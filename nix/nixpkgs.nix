let
  fetchNixpkgs = import ./fetchNixpkgs.nix;
in

fetchNixpkgs {
  rev          = "18.09";
  outputSha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
}
