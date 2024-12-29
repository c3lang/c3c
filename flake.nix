{
  description = "C3 compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... } @ inputs: inputs.flake-utils.lib.eachDefaultSystem 
  (system: 
    let pkgs = import inputs.nixpkgs { inherit system; }; 
        call = set: pkgs.callPackage ./nix/default.nix ( 
          set // { 
            rev = self.rev or "unknown"; 
          } 
        );
    in {
      packages = {
        default = self.packages.${system}.c3c;

        c3c = call {};
        
        c3c-checks = pkgs.callPackage ./nix/default.nix { 
          checks = true; 
        };

        c3c-debug = pkgs.callPackage ./nix/default.nix { 
          debug = true; 
        };

        c3c-debug-checks = pkgs.callPackage ./nix/default.nix { 
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
