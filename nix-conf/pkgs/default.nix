pkgs: {
  doll = pkgs.callPackage ./doll { };
  json-table = pkgs.callPackage ./json-table { };
  orbstack = pkgs.callPackage ./orbstack {  };
  ssh-over-ssm = pkgs.callPackage ./ssh-over-ssm { };
}
