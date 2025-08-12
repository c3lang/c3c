{
  description = "C3 compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... }@inputs: inputs.flake-utils.lib.eachDefaultSystem 
  (system: 
    let pkgs = import inputs.nixpkgs { inherit system; }; 
        c3cBuild = set: pkgs.callPackage ./nix/default.nix (set // { 
          rev = self.rev or "unknown"; 
        });
    in {
      packages = {
        default = self.packages.${system}.c3c;

        c3c = c3cBuild {};
        
        c3c-checks = c3cBuild { 
          checks = true; 
        };

        c3c-debug = c3cBuild { 
          debug = true; 
        };

        c3c-debug-checks = c3cBuild { 
          debug = true; 
          checks = true; 
        };
      };

      devShells = {
        default = pkgs.callPackage ./nix/shell.nix {
          c3c = self.packages.${system}.c3c-debug; 
        };
      };
    }
  );
}
