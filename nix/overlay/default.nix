final: _prev: {
  c3c = final.lib.makeScope final.newScope (self: {
    c3c = self.callPackage ../default.nix { };

    c3c-checks = self.c3c.override { checks = true; };

    c3c-debug = self.c3c.override { debug = true; };

    c3c-debug-checks = self.c3c.override {
      checks = true;
      debug = true;
    };
    
    default = self.c3c;
  });
}
