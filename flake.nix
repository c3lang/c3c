{
  description = "C3 compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... } @ inputs: inputs.flake-utils.lib.eachDefaultSystem 
  (system: 
    let pkgs = import inputs.nixpkgs { inherit system; };
        c3c  = pkgs.callPackage ./nix/default.nix {}; 
    in {
      packages = {
        default = self.packages.${system}.c3c;
        inherit c3c;
      };

      devShells.default = pkgs.mkShell {
        buildInputs = [ c3c ];
      };
    }
  );
}
