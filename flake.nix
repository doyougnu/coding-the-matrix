{
  description = "A flake for CL!";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs { inherit system; };
        in
          rec
            {
            packages =
              {
                default = import ./cl-ctm.nix { inherit pkgs; };
              };
            devShells =
              {
                default = packages.default;
              };
            }
      );
}
