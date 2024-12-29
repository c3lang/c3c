{
  lib,
  mkShell,
  clang-tools,
  c3c,
}: 
mkShell.override { 
  inherit (c3c) stdenv; 
} {
  name = "c3c-shell";

  inputsFrom = [
    c3c
  ];

  packages = [ 
    clang-tools 
  ];

  # Usage: 'cmake . -Bbuild $C3_CMAKE_FLAGS' or 'cmake . -Bbuild $=C3_CMAKE_FLAGS' on zsh
  C3_CMAKE_FLAGS = lib.concatStringsSep " " c3c.cmakeFlags;
}
