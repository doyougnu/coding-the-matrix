{ pkgs, ... }:

pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.lispPackages_new.sbclWithPackages
      (ps: with ps; [ alexandria str dexador cl-ppcre sqlite arrow-macros jzon ]))
  ];
}
