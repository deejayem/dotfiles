{ pkgs, lib }:
let
  inherit (builtins) readDir;

  inherit (lib)
    attrNames
    concatStringsSep
    elemAt
    filter
    genList
    head
    isString
    length
    listToAttrs
    pathExists
    replaceStrings
    split
    stringLength
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

  isPackageDir =
    path: name: type:
    type == "directory" && pathExists (path + "/${name}/default.nix");

  discoverPackages =
    path:
    {
      nameTransform ? (name: name),
    }:
    let
      contents = readDir path;
      packageNames = filter (name: isPackageDir path name contents.${name}) (attrNames contents);
    in
    listToAttrs (
      map (name: {
        name = nameTransform name;
        value = pkgs.callPackage (path + "/${name}") { };
      }) packageNames
    );
in
discoverPackages ./. { } // discoverPackages ./build-support { nameTransform = kebabToCamel; }
