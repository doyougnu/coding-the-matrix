{
  description = "A flake for CL!";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs { inherit system; };

            lispCompiler = pkgs.lispPackages_new.sbclWithPackages
              (ps: with ps; [ alexandria
                              arrow-macros
                              vgplot # works!
                              eazy-gnuplot
                            ]);
        in
          rec
            {
            packages = {
                default = import ./cl-ctm.nix { inherit pkgs;
                                                lisp = lispCompiler;
                                              };
              };
            devShells = { # default = packages.default;
                default = import ./ctm-shell.nix { inherit pkgs;
                                                   lisp   = lispCompiler;
                                                 };
              };
            }
      );
}
