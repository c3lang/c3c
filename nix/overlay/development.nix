_final: prev: {
  c3c = prev.c3c.overrideScope (cfinal: _cprev: {
    shell = cfinal.callPackage ../shell.nix { c3c = cfinal.c3c-debug; };
  });
}
