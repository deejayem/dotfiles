{ pkgs, lib }:
let
  inherit (builtins) readDir;

  inherit (lib)
    attrNames
    concatStringsSep
    filter
    hasSuffix
    head
    isString
    listToAttrs
    pathExists
    removeSuffix
    split
    substring
    tail
    toLower
    toUpper
    ;

  capitalise = s: (toUpper (substring 0 1 s)) + (substring 1 (-1) s);

  capitaliseWord =
    word:
    let
      overrides = {
        github = "GitHub";
        gitlab = "GitLab";
      };
    in
    overrides.${toLower word} or (capitalise word);

  kebabToCamel =
    str:
    let
      parts = filter isString (split "-" str);
    in
    (head parts) + concatStringsSep "" (map capitaliseWord (tail parts));

  discoverPackages =
    path:
    {
      nameTransform ? (name: name),
    }:
    let
      contents = readDir path;

      isPackage =
        name: type:
        (type == "directory" && pathExists (path + "/${name}/default.nix"))
        || (type == "regular" && name != "default.nix" && hasSuffix ".nix" name);

      packageName = name: type: if type == "directory" then name else removeSuffix ".nix" name;

      packageEntries = filter (name: isPackage name contents.${name}) (attrNames contents);
    in
    listToAttrs (
      map (
        name:
        let
          type = contents.${name};
        in
        {
          name = nameTransform (packageName name type);
          value = pkgs.callPackage (path + "/${name}") { };
        }
      ) packageEntries
    );
in
discoverPackages ./. { }
// discoverPackages ./build-support { nameTransform = kebabToCamel; }
// discoverPackages ./scripts { }
