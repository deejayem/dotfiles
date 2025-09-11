pkgs: {
  json-table = pkgs.callPackage ./json-table { };
  ssh-over-ssm = pkgs.callPackage ./ssh-over-ssm { };
}
