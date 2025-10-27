{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs)
        busybox
        gnused
        importNpmLock
        lib
        nodejs
        writeShellScriptBin
        writeText
        ;
      inherit (builtins) readFile toString;

      # https://nixos.org/manual/nixpkgs/stable/#javascript-buildNpmPackage-importNpmLock.buildNodeModules
      node_modules = importNpmLock.buildNodeModules {
        npmRoot = ./.;
        inherit nodejs;
      };

      help = writeText "restyler-prettier-help" ''
        Usage:
          restyler-prettier help
          restyler-prettier version
          restyler-prettier -- [file1] [file2] ...
      '';

    in
    {
      packages.${system} = {
        default = inputs.self.packages.${system}.restyler-prettier;

        restyler-prettier = writeShellScriptBin "restyler-prettier" ''

          case "$1" in
            help)
              cat ${help}
              ;;
            version)
              cd ${node_modules}
              ${nodejs}/bin/node node_modules/.bin/prettier --version | ${gnused}/bin/sed 's/^/v/; s/$/-3/'
              ;;
            --)
              targets=( $(${busybox}/bin/realpath "''\${@:2}") )
              cd ${node_modules}
              ${nodejs}/bin/node node_modules/.bin/prettier --write -- "''\${targets[@]}"
              ;;
            *)
              cat ${help}
              exit 1
              ;;
          esac
        '';
      };

      checks.${system} = {
        inherit (inputs.self.packages.${system}) restyler-prettier;
      };
    };
}
