{ config, lib, pkgs, ... }:

let
  sbcl = "$pkgs.sbcl}/bin/sbcl -- script";

in
pkgs.lisPackages_new.build-asdf-system {
  pname = "cl-ctm";
  version = "0.0.1";
  src = pkgs.fetchFromGithub {
    url = "https://github.com/doyougnu/coding-the-matrix.git";
  };
  lisp = pkgs.lispPackages_new.lispWithPackages sbcl;
  # lispLibs = [ arrows closer-mop lisp-stat ];
}
