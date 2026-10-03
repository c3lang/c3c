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

  # Usage: 'meson setup build $C3_MESON_FLAGS' or 'meson setup build $=C3_MESON_FLAGS' on zsh
  C3_MESON_FLAGS = lib.concatStringsSep " " c3c.mesonFlags;
}