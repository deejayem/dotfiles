pkgs:
let
  inherit (builtins)
    attrNames
    concatStringsSep
    elemAt
    filter
    genList
    isString
    length
    listToAttrs
    pathExists
    readDir
    replaceStrings
    split
    stringLength
    substring
    ;

  # String utilities copied from lib.strings (using lib here causes infinite recursion)
  stringToCharacters = s: genList (i: substring i 1 s) (stringLength s);
  lowerChars = stringToCharacters "abcdefghijklmnopqrstuvwxyz";
  upperChars = stringToCharacters "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  toUpper = replaceStrings lowerChars upperChars;
  capitalize = s: (toUpper (substring 0 1 s)) + (substring 1 (-1) s);

  kebabToCamel =
    str:
    let
      parts = filter isString (split "-" str);
      len = length parts;
      indexed = genList (i: {
        inherit i;
        word = elemAt parts i;
      }) len;
      transformed = map (x: if x.i == 0 then x.word else capitalize x.word) indexed;
    in
    concatStringsSep "" transformed;

  dir = ./.;

  isPackageDir =
    path: name: type:
    type == "directory" && pathExists (path + "/${name}/default.nix");

  discoverPackages =
    subdir: nameTransform:
    let
      path = if subdir == null then dir else dir + "/${subdir}";
      contents = readDir path;
      packageNames = filter (name: isPackageDir path name contents.${name}) (attrNames contents);
    in
    listToAttrs (
      map (name: {
        name = nameTransform name;
        value = pkgs.callPackage (path + "/${name}") { };
      }) packageNames
    );

  packages = discoverPackages null (name: name);
  buildSupport = discoverPackages "build-support" kebabToCamel;
in
packages
// buildSupport
// {
  # Yuck!
  fetchFromPrivateGitHub = buildSupport.fetchFromPrivateGithub or null;
}
