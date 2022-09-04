{ pkgs, lisp }:

pkgs.mkShell {
  nativeBuildInputs = [
    lisp
  ];

  shellHook = '' export CL_SOURCE_REGISTRY=/home/doyougnu/programming/cl/coding-the-matrix
              '';
}
