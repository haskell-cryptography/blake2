{
  description = "Haskell bindings for BLAKE2";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
    };

    packageName = "blake2";

    project = pkgs.haskellPackages.developPackage {
      root = ./.;
      name = packageName;
    };
  in {
    packages.${packageName} = project;
    defaultPackage = self.packages.${system}.${packageName};
    devShells.default = pkgs.mkShell {
      inputsFrom = [ project.env ];
      buildInputs = [ pkgs.haskell-language-server pkgs.cabal-install ];
    };
  });
}
