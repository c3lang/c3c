{
  description = "C3 compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... }@inputs:
    {
      overlays = {
        default = inputs.nixpkgs.lib.composeExtensions 
          (import ./nix/overlay)
          (_: prev: {
            # rev from Git is a flake-only feature
            c3c = prev.c3c.overrideScope (_: _: { rev = self.rev or "unknown"; });
          });
      };
    } 
    // inputs.flake-utils.lib.eachDefaultSystem 
      (system: 
        let 
          pkgs = import inputs.nixpkgs { 
            inherit system; 
            overlays = [
              self.overlays.default
            ];
          };
        in {
          packages = {
            inherit (pkgs.c3c)
             c3c
             c3c-checks
             c3c-debug
             c3c-debug-checks
             default;
          };

          devShells = {
            default = pkgs.callPackage ./nix/shell.nix {
              c3c = self.packages.${system}.c3c-debug;
            };
          };
        }
      );
}
