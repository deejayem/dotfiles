{ config, lib, pkgs, ... }:
let
  zscaler-cert = ''
    -----BEGIN CERTIFICATE-----
    MIIE0zCCA7ugAwIBAgIJANu+mC2Jt3uTMA0GCSqGSIb3DQEBCwUAMIGhMQswCQYD
    VQQGEwJVUzETMBEGA1UECBMKQ2FsaWZvcm5pYTERMA8GA1UEBxMIU2FuIEpvc2Ux
    FTATBgNVBAoTDFpzY2FsZXIgSW5jLjEVMBMGA1UECxMMWnNjYWxlciBJbmMuMRgw
    FgYDVQQDEw9ac2NhbGVyIFJvb3QgQ0ExIjAgBgkqhkiG9w0BCQEWE3N1cHBvcnRA
    enNjYWxlci5jb20wHhcNMTQxMjE5MDAyNzU1WhcNNDIwNTA2MDAyNzU1WjCBoTEL
    MAkGA1UEBhMCVVMxEzARBgNVBAgTCkNhbGlmb3JuaWExETAPBgNVBAcTCFNhbiBK
    b3NlMRUwEwYDVQQKEwxac2NhbGVyIEluYy4xFTATBgNVBAsTDFpzY2FsZXIgSW5j
    LjEYMBYGA1UEAxMPWnNjYWxlciBSb290IENBMSIwIAYJKoZIhvcNAQkBFhNzdXBw
    b3J0QHpzY2FsZXIuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA
    qT7STSxZRTgEFFf6doHajSc1vk5jmzmM6BWuOo044EsaTc9eVEV/HjH/1DWzZtcr
    fTj+ni205apMTlKBW3UYR+lyLHQ9FoZiDXYXK8poKSV5+Tm0Vls/5Kb8mkhVVqv7
    LgYEmvEY7HPY+i1nEGZCa46ZXCOohJ0mBEtB9JVlpDIO+nN0hUMAYYdZ1KZWCMNf
    5J/aTZiShsorN2A38iSOhdd+mcRM4iNL3gsLu99XhKnRqKoHeH83lVdfu1XBeoQz
    z5V6gA3kbRvhDwoIlTBeMa5l4yRdJAfdpkbFzqiwSgNdhbxTHnYYorDzKfr2rEFM
    dsMU0DHdeAZf711+1CunuQIDAQABo4IBCjCCAQYwHQYDVR0OBBYEFLm33UrNww4M
    hp1d3+wcBGnFTpjfMIHWBgNVHSMEgc4wgcuAFLm33UrNww4Mhp1d3+wcBGnFTpjf
    oYGnpIGkMIGhMQswCQYDVQQGEwJVUzETMBEGA1UECBMKQ2FsaWZvcm5pYTERMA8G
    A1UEBxMIU2FuIEpvc2UxFTATBgNVBAoTDFpzY2FsZXIgSW5jLjEVMBMGA1UECxMM
    WnNjYWxlciBJbmMuMRgwFgYDVQQDEw9ac2NhbGVyIFJvb3QgQ0ExIjAgBgkqhkiG
    9w0BCQEWE3N1cHBvcnRAenNjYWxlci5jb22CCQDbvpgtibd7kzAMBgNVHRMEBTAD
    AQH/MA0GCSqGSIb3DQEBCwUAA4IBAQAw0NdJh8w3NsJu4KHuVZUrmZgIohnTm0j+
    RTmYQ9IKA/pvxAcA6K1i/LO+Bt+tCX+C0yxqB8qzuo+4vAzoY5JEBhyhBhf1uK+P
    /WVWFZN/+hTgpSbZgzUEnWQG2gOVd24msex+0Sr7hyr9vn6OueH+jj+vCMiAm5+u
    kd7lLvJsBu3AO3jGWVLyPkS3i6Gf+rwAp1OsRrv3WnbkYcFf9xjuaf4z0hRCrLN2
    xFNjavxrHmsH8jPHVvgc1VD0Opja0l/BRVauTrUaoW6tE+wFG5rEcPGS80jjHK4S
    pB5iDj2mUZH1T8lzYtuZy0ZPirxmtsk3135+CKNa2OCAhhFjE0xd
    -----END CERTIFICATE-----
  '';

  internal-cert = ''
    -----BEGIN CERTIFICATE-----
    MIIDpzCCAo+gAwIBAgIRAPimIVPUvFeeWdKoTVr/KaowDQYJKoZIhvcNAQELBQAw
    bTELMAkGA1UEBhMCR0IxGDAWBgNVBAoMD29udGhlbWFya2V0LmNvbTELMAkGA1UE
    CwwCSVQxDzANBgNVBAgMBkxvbmRvbjEVMBMGA1UEAwwMaW50ZXJuYWwub3RtMQ8w
    DQYDVQQHDAZMb25kb24wHhcNMjQwNTIxMTIyNTUzWhcNMzQwNTIxMTMyNTQ1WjBt
    MQswCQYDVQQGEwJHQjEYMBYGA1UECgwPb250aGVtYXJrZXQuY29tMQswCQYDVQQL
    DAJJVDEPMA0GA1UECAwGTG9uZG9uMRUwEwYDVQQDDAxpbnRlcm5hbC5vdG0xDzAN
    BgNVBAcMBkxvbmRvbjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKMb
    +jxbONNYRWXFYLHOVsKkTBVY/SkPo9VYv63Xzp8YF5CC3GMNKVvtdfwDLxdB4yDE
    71kOngybxIRTeX+UdZCfhmcgpmu6trT8RB27SzpOVkrVz+wCzYx/3qE4xSQok474
    komOtHkuwoL1MMqTH1WOPqUL3RaNkK3YSq2M8JPfjG9w6eboT0i+c7GG9OEk9BwW
    35M+tdiI9fjAK95yMU9DjVI7PqTfqBVT5pUoyzAKhTikZlC6O8X8U98NJojwhaT4
    RJcbbd1bdNqcxdpshIiP1kWAE4CKp2+tMzzz9yqwgQ1igbsm2j37TxI74JoEV9+k
    95tFwgXLT7Bih3MFuI0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4E
    FgQUuA22mh+yV3W8D3mpiouhO2Y/2c4wDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3
    DQEBCwUAA4IBAQCJgkhzAWW0Rh4EWyAdVIoUnjHIr032Qu61cXiNqvvRS9GIF5gs
    oynXjIIj2scBeNlkG3oSy0G3wWyFzng6ixwuol2fRhDsllvm2bgeaObdbccbLbWx
    8OTobCqWTfZvEn8dYs7Qbx/9l4yBH6pYptnOmDt+Ze2hOVZyTuiVq91CEn+on9FG
    2V6Bjuu8dNpz2CC8na7H4wsqUNRfBVSTSKgdeeiLj1zdueWgOtA1PNOZp5wi452U
    mpb61I1k/Xfe6ECUn8QEh9oEB4MprNvlvLVmmnstcBmqU9SvONtmSrn8ekI2OO69
    R7pRciveNTEVrJRPqOfL4fjfQbjtpKx6Gk5m
    -----END CERTIFICATE-----
  '';

  internal-staging-cert = ''
    -----BEGIN CERTIFICATE-----
    MIIDpzCCAo+gAwIBAgIRANXYUsUWHHGL/LgpcIY3zlUwDQYJKoZIhvcNAQELBQAw
    bTELMAkGA1UEBhMCR0IxGDAWBgNVBAoMD29udGhlbWFya2V0LmNvbTELMAkGA1UE
    CwwCSVQxDzANBgNVBAgMBkxvbmRvbjEVMBMGA1UEAwwMaW50ZXJuYWwub3RtMQ8w
    DQYDVQQHDAZMb25kb24wHhcNMjQwNTI0MDc1MDQ3WhcNMzQwNTI0MDg1MDQ0WjBt
    MQswCQYDVQQGEwJHQjEYMBYGA1UECgwPb250aGVtYXJrZXQuY29tMQswCQYDVQQL
    DAJJVDEPMA0GA1UECAwGTG9uZG9uMRUwEwYDVQQDDAxpbnRlcm5hbC5vdG0xDzAN
    BgNVBAcMBkxvbmRvbjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAImM
    FljdqdQ4M0tRYAiRU6WPWiIKFHwZSTsdLohNXikjnSar1xnN1LQLLH1mzPpztnpA
    eCnADo9Dc1Nsm+dt6WREL6n57oQSG3d5eM+br6MIm2qWIXQhtJtpKFcbSuOlaB4z
    uWNmk3R09+3GaGNhpYBmEbh3Nvc5it0/p6EUOVWigF3ghr0NO2JSOhPtGhNSPyyS
    9Q7DZSwdaGeix9yKWKDh3X4ikZvjm4xqkogFFdyFHKA1qmsaCsT+NP1iH6HNb6pB
    xOb1ZyzR1EcFKAP+8uOgoI3bF0iJswNtkSc2kqf0vNQ+K/qoNL8OH7VyKCfeQqNL
    2b8lV+FwHIBD2ZwhsuUCAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4E
    FgQUxbubUk2Kf9k68OUOSwrdgGuAtJYwDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3
    DQEBCwUAA4IBAQBNRKzWrQQBh1RAU023u0pgNjfk0OV5mTlb2024jCLmqG7U/eSC
    X8LoO/Gm3yVuj5RsoH8H5ftNU1j71c+dHg7+UVvQZRCOfgVIMnPCxuMvcaljRnLG
    qHaRCra5G3IOLrBtJDg9DgKg6/gUbg5DvZwiO5J21yzJWxy6wmoRBmy43DZBa2RV
    /rO3WOM0uuMp4DHqBIYx55d+4mdtshikZoys3TCiFH3C11xrUwkIdNEqvMcjl/Pr
    5WrzfbTpDzvo/GCkPhA0thVgUBx4LXB8HizVmDZgGbSuh7ic7LHyh1ahE0fqGX9C
    ZvHif3XTcAZlNkilVHvF3pM4EIosFEc6dHFy
    -----END CERTIFICATE-----
  '';

  aws-cert = (builtins.readFile
    "${pkgs.awscli2}/lib/python${pkgs.awscli2.python.pythonVersion}/site-packages/awscli/botocore/cacert.pem")
    + zscaler-cert;

  full-cert = (builtins.readFile /etc/ssl/cert.pem) + aws-cert + internal-cert
    + internal-staging-cert;

  zscaler-cert-file = pkgs.writeText "zscaler-cert.pem" zscaler-cert;
  aws-cert-file = pkgs.writeText "aws-cert.pem" aws-cert;
  full-cert-file = pkgs.writeText "full-cert.pem" full-cert;

  zscaler-jdk = pkgs.jdk.overrideAttrs (old: {
    # passthru.home must be set to ensure JAVA_HOME is set correctly
    # See https://github.com/nix-community/home-manager/blob/086f619dd991a4d355c07837448244029fc2d9ab/modules/programs/java.nix#L39-L41
    # and https://github.com/NixOS/nixpkgs/blob/4877ea239f4d02410c3516101faf35a81af0c30e/pkgs/development/compilers/openjdk/jre.nix#L32
    passthru.home = "${zscaler-jdk}"; # make sure JAVA_HOME is set
    installPhase = old.installPhase + ''
      $out/bin/keytool -import -noprompt -trustcacerts -alias zscalerrootca -keystore $out/lib/security/cacerts <<< "${zscaler-cert}"
    '';
  });

  zscaler-lein = pkgs.leiningen.override { jdk = zscaler-jdk; };
in {
  imports = [ ./includes/darwin.nix ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "dmorgan";
  home.homeDirectory = "/Users/dmorgan";

  home.sessionPath = [ "$HOME/.costar/auth2aws" ];

  home.sessionVariables = {
    AWS_DEFAULT_REGION = "eu-west-1";
    AWS_PROFILE = "aws_otm_dev_developers";
    AM_PROFILE = "staging";
    AWS_CA_BUNDLE = "${aws-cert-file}";
    CURL_CA_BUNDLE = "${full-cert-file}";
    NIX_SSL_CERT_FILE = "${full-cert-file}";
    NODE_EXTRA_CA_CERTS = "${zscaler-cert-file}";
  };

  home.shellAliases = {
    notify_success = ''
      ( osascript -e 'display notification "The command finished" with title "Success"' && afplay /System/Library/Sounds/Ping.aiff && say done  )
    '';
    notify_failure = ''
      ( osascript -e 'display notification "The command failed" with title "Failure"' && afplay /System/Library/Sounds/Sosumi.aiff && say failed  )
    '';
    notify = "notify_success || notify_failure";
    auth =
      "auth2aws login -r aws_otm_dev_developers,aws_otm_prd_developers && osascript -e 'tell app \"iTerm\" to activate'";
    yarn_build =
      "aws codeartifact login --tool npm --repository otm-js --domain otm --domain-owner 103567893073 --region eu-west-1 --profile aws_otm_dev_developers && yarn && yarn build && notify";
  };

  home.packages = with pkgs; [ zscaler-lein ];

  home.file = {
    "certs/zscaler-cert.pem".source = zscaler-cert-file;
    "certs/aws-cert.pem".source = aws-cert-file;
    "certs/full-cert.pem".source = full-cert-file;
    "certs/internal-ca.pem".text = internal-cert;
    "certs/staging-internal-ca.pem".text = internal-staging-cert;
  };

  sops.secrets = {
    "git_email_config/otm" = { };
    "ssh_config/otm" = { };
  };

  programs.java = {
    enable = true;
    package = zscaler-jdk;
  };

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    includes = lib.mkForce [
      { path = config.sops.secrets."git_email_config/otm".path; }
      { path = config.sops.secrets."git_email_config/default".path; condition = "gitdir:~/src/personal/"; }
      { path = config.sops.secrets."git_email_config/default".path; condition = "gitdir:~/dotfiles/"; }
      { contents = { commit.gpgSign = true; tag.gpgSign = true; }; condition = "gitdir:~/src/personal/"; }
      { contents = { commit.gpgSign = true; tag.gpgSign = true; }; condition = "gitdir:~/dotfiles/"; }
    ];
    extraConfig = {
      github.user = "david-morgan-otm";
      http.sslcainfo = "${full-cert-file}";
    };
    ignores = [
      ".envrc"
      ".clj-kondo"
      "shell.nix"
      ".direnv"
      ".dir-locals.el"
      "browser-tests/package-lock.json"
      "resources/react-app/package-lock.json"
      "resources/next/package-lock.json"
    ];
  };
  programs.ssh = {
    includes = [ config.sops.secrets."ssh_config/otm".path ];
    matchBlocks = {
      "github.com" = lib.mkForce {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_rsa";
        identitiesOnly = true;
      };
      "github.com-personal" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
        identitiesOnly = true;
      };
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
