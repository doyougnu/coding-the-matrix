{ pkgs, lisp }:

pkgs.mkShell {
  buildInputs = [
    pkgs.gnuplot
    lisp
  ];

  shellHook = '' export CL_SOURCE_REGISTRY=/home/doyougnu/programming/cl/coding-the-matrix
              '';
}
