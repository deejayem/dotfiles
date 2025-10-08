pkgs:
let
  # Functions copied from lib, to avoid infinite recursion (when using pkgs.lib)
  filterAttrs =
    pred: set:
    builtins.removeAttrs set (builtins.filter (name: !pred name set.${name}) (builtins.attrNames set));
  genAttrs' = xs: f: builtins.listToAttrs (map f xs);
  nameValuePair = name: value: { inherit name value; };
  genAttrs = names: f: genAttrs' names (n: nameValuePair n (f n));

  skip = [ ];

  dir = ./.;

  # Include every foo/default.nix in this directory, unless it begins with . or _
  # or has been explicitly skipped
  packageDirs = builtins.attrNames (
    filterAttrs (
      name: type:
      type == "directory"
      && !(builtins.elem name skip)
      && builtins.substring 0 1 name != "."
      && builtins.substring 0 1 name != "_"
      && builtins.pathExists (dir + "/${name}/default.nix")
    ) (builtins.readDir dir)
  );
in
genAttrs packageDirs (name: pkgs.callPackage (dir + "/${name}") { })
