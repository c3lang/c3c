{
  mkShell,
  clang-tools,
  c3c,
}:

mkShell {
  inputsFrom = [
    c3c
  ];

  packages = [ 
    clang-tools 
    c3c
  ];

  shellHook = ''
    ln -sf ${c3c}/compile_commands.json compile_commands.json
  '';
}
