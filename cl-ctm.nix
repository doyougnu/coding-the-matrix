{ pkgs, lisp }:

let
  sbcl-start = "${lisp}/bin/sbcl -- script";
in
pkgs.lispPackages_new.build-asdf-system {
  pname = "cl-ctm";
  version = "0.0.1";
  src = pkgs.fetchFromGitHub {
    owner = "doyougnu";
    repo  = "coding-the-matrix";
    rev   = "3108bb633805f27a2d3f87524d798becf090db04";
    sha256 = "2FexArxHlLmSShJTxSQihO17SHxh5X7dKPbGDFEs6Sc=";
  };
  lisp = sbcl-start;
  lispLibs = with pkgs.lispPackages_new.sbclPackages; [ alexandria arrow-macros lisp-stat ];
}
