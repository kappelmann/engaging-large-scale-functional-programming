with import (builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "https://github.com/NixOS/nixpkgs/archive/257cbbcd3ab7bd96f5d24d50adc807de7c82e06d.tar.gz";
  sha256 = "0g3n725kjk2fc9yn9rvdjwci4mrx58yrdgp3waby9ky3d5xhcaw4";
}) {};

haskell.lib.buildStackProject {
  name = "ex10";
  buildInputs = [ ghc zlib freeglut libGL libGLU ];
}
