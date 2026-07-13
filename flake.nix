{
  inputs = {
    superbol-overlay.url = "git+ssh://git@gitlab.ocamlpro.com:30022/pierre.villemot/superbol-overlay.git";
    flake-parts.follows = "superbol-overlay/flake-parts";
    nixpkgs.follows = "superbol-overlay/nixpkgs";
  };

  outputs =
    { self, ... }@inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      perSystem =
        { pkgs, system, ... }:
        {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            overlays = [ self.inputs.superbol-overlay.overlays.default ];
          };

          formatter = pkgs.nixfmt-tree;

          devShells.default = pkgs.mkShell {
            packages =
              (with pkgs; [
                gnucobol_4
                nodejs_26
                yarn
                vscode
              ])
              ++ (with pkgs.ocamlPackages; [
                dune_3
                ocaml
                menhir
                # vendor dependencies
                gen_js_api
                js_of_ocaml
              ])
              ++ (with pkgs.ocamlProPackages; [
                drom
              ]);

            inputsFrom = [
              pkgs.superbolPackages.superbol-studio-oss
              pkgs.superbolPackages.goblint-cil
              pkgs.superbolPackages.autofonce
              pkgs.superbolPackages.ezlibcob
            ];

            shellHook = ''
              export LD_LIBRARY_PATH="${pkgs.gnucobol_4}/lib"
            '';
          };
        };
    };
}
