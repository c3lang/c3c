{
  description = "C3 compiler flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... } @ inputs: inputs.flake-utils.lib.eachDefaultSystem 
  (system: 
    let pkgs = import inputs.nixpkgs { inherit system; }; in 
    {
      packages = {
        default = self.packages.${system}.c3c;
        c3c = pkgs.callPackage ./nix/default.nix {};
        c3c-debug = pkgs.callPackage ./nix/default.nix { debug = true; };
        c3c-nochecks = pkgs.callPackage ./nix/default.nix { debug = true; checks = false; };
      };
      # devShells.default = pkgs.mkShell {
      #   packages = [ 
      #     self.packages.${system}.c3c-nochecks
      #     pkgs.clang-tools 
      #   ];
      # };
    }
  );
}
