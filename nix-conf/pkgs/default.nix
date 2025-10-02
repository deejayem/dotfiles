pkgs:
let

  inherit (pkgs) lib;

  skip = [ ];

  dir = ./.;

  # Include every foo/default.nix in this directory, unless it begins with . or _
  # or has been explicitly skipped
  packageDirs = builtins.attrNames (
    lib.filterAttrs
      (name: type:
        type == "directory"
        && !(builtins.elem name skip)
        && builtins.substring 0 1 name != "."
        && builtins.substring 0 1 name != "_"
        && builtins.pathExists (dir + "/${name}/default.nix"))
      (builtins.readDir dir)
  );
in
  lib.genAttrs packageDirs (name: pkgs.callPackage (dir + "/${name}") { })
