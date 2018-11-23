{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Monad (msum, when)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteArray.Encoding as BE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool, withResource)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple
import qualified Database.SQLite.Simple as SQLite
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.Environment (getEnvironment)
import System.Exit (die)
import qualified Web.Routing.Combinators as R
import qualified Web.Routing.SafeRouting as R

import qualified Broch.PostgreSQL as BP
import qualified Broch.SQLite as BS
import qualified CoinberryUI as UI
import Broch.Server
import Broch.Server.Config
import Broch.Server.Internal
import Broch.Server.Session (defaultLoadSession, getKey)

data BackEnd = POSTGRES | SQLITE deriving (Read, Show)

data BrochOpts = BrochOpts
    { issuer  :: T.Text
    , externalLogin :: T.Text
    , port    :: Int
    , connStr :: String
    , webRoot :: FilePath
    , keysPath :: FilePath
    , backEnd :: BackEnd
    }

backEndOption :: Parser BackEnd
backEndOption = option auto
    ( long "back-end"
   <> metavar "(POSTGRES or SQLITE)"
   <> value POSTGRES
   <> help "the database backend to use for storage"
    )

textOption :: Mod OptionFields String -> Parser T.Text
textOption x = T.pack <$> strOption x

parser :: String -> String -> String -> Int -> String -> String -> Parser BrochOpts
parser issuer externalLogin db port webroot keysPath = BrochOpts
    <$> textOption
        ( long "issuer"
       <> help "The OP's issuer URL"
       <> metavar "ISSUER"
       <> value issuer)
    <*> textOption
        ( long "external-login"
       <> help "URI for external login page"
       <> metavar "EXTERNAL_LOGIN"
       <> value externalLogin)
    <*> option auto
        ( long "port"
       <> metavar "PORT"
       <> value port
       <> help "The port to listen on")
    <*> strOption
        ( long "connection-string"
       <> help "The postgresql connection string or sqlite database file name"
       <> metavar "DATABASE"
       <> value db)
    <*> strOption
        ( long "web-root"
       <> help "The directory from which to serve static content"
       <> metavar "WEBROOT"
       <> value webroot)
    <*> strOption
        ( long "keys-path"
       <> help "The directory used to store JWKS and session key."
       <> metavar "KEYS_PATH"
       <> value keysPath)
    <*> backEndOption

main :: IO ()
main = do
    env <- getEnvironment
    let issuer  = fromMaybe "http://localhost:3000" $ lookup "ISSUER" env
        externalLogin = fromMaybe "http://localhost:3000/login" $ lookup "EXTERNAL_LOGIN" env
        port    = maybe 3000 read                   $ lookup "PORT" env
        db      = fromMaybe "default"               $ lookup "DATABASE" env
        webroot = fromMaybe "webroot"               $ lookup "WEBROOT" env
        keysPath = fromMaybe "."               $ lookup "KEYS_PATH" env
        desc    = fullDesc <> progDesc "Run an OpenID Connect server"
    sidSalt <- decodeSalt $ lookup "SUBJECT_ID_SALT" env
    opts <- setConnStr <$> execParser (info (helper <*> parser issuer externalLogin db port webroot keysPath) desc)
    when (isNothing sidSalt) $ putStrLn "Subject identifiers will be shared between clients. Set SUBJECT_ID_SALT to use pairwise identifiers)"
    runWithOptions opts sidSalt
  where
    setConnStr opts
        | connStr opts == "default" = opts { connStr = defaultConnStr (backEnd opts)}
        | otherwise = opts
    defaultConnStr be = case be of
        POSTGRES -> "dbname=broch"
        SQLITE   -> "broch.db3"

decodeSalt :: Maybe String -> IO (Maybe ByteString)
decodeSalt Nothing = return Nothing
decodeSalt (Just s) = case bs of
    Left  _ -> die "salt value must be hex or base64 encoded"
    Right b -> return (Just b)
  where
    bs = let b = BC.pack s in msum [BE.convertFromBase BE.Base64 b, BE.convertFromBase BE.Base16 b]

keyRingParams :: FilePath -> KeyRingParams
keyRingParams keysPath = KeyRingParams (keysPath <> "/jwks.json") 128 5 5

keyFile :: FilePath -> FilePath
keyFile keysPath = keysPath <> "/session_key.json"

runWithOptions :: BrochOpts -> Maybe ByteString -> IO ()
runWithOptions BrochOpts {..} sidSalt = do
    sessionKey <- getKey $ keyFile keysPath
    kr <- getKeyRing $ keyRingParams keysPath
    rotateKeys kr True
    (mkBackEnd, passwordAuthenticate) <- case backEnd of
        POSTGRES -> do
            pool <- createPool (connectPostgreSQL (BC.pack connStr)) close 1 60 20
            return (BP.postgreSQLBackend pool, BP.passwordAuthenticate pool)
        SQLITE   -> do
            pool <- createPool (SQLite.open connStr) SQLite.close 1 60 20
            withResource pool $ \c -> BS.createSchema c
            return (BS.sqliteBackend pool, \v u p -> withResource pool $ \c -> BS.passwordAuthenticate c v u p)

    config <- mkBackEnd <$> inMemoryConfig issuer kr sidSalt
    let app = staticApp (defaultWebAppSettings "webroot")
        loginURI = "http://localhost:3000/login"
        baseRouter = brochServer config UI.approvalPage authenticatedSubject (authenticateSubjectWithURI loginURI)
        authenticate username password = passwordAuthenticate validatePassword username (TE.encodeUtf8 password)
        extraRoutes =
            [ ("/home",   text "Hello, I'm the home page")
            , ("/login",  passwordHandler loginURI authenticate)
            , ("/logout", invalidateSession >> text "You have been logged out")
            ]
        router = foldl (\pathMap (r, h) -> R.insertPathMap' (R.toInternalPath (R.static r)) (const h) pathMap) baseRouter extraRoutes
        broch = routerToMiddleware (defaultLoadSession 3600 sessionKey) issuer router

    run port (logStdoutDev (broch app))
