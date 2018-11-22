{ rev                             # The Git revision of nixpkgs to fetch
, outputSha256 ? null             # The SHA256 fixed-output hash
}:

  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  }
