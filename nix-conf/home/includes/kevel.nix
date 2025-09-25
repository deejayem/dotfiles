{
  config,
  lib,
  pkgs,
  ...
}:
let
  envSecrets = {
    ADZERK_GITHUB_PACKAGES_AUTH_TOKEN = "adzerk-packages-token";
    OPENAI_API_TOKEN = "openai-api-token";
  };

  dollPlistXml = pkgs.writeText "com.xiaogd.Doll.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
            <key>KeyboardShortcuts_toggleConfigWindow</key>
            <string>{"carbonKeyCode":2,"carbonModifiers":768}</string>
            <key>SETTINGS_Show_As_Red_Badge</key>
            <true/>
            <key>SETTING_MONITORED_APP_IDS</key>
            <string>com.tinyspeck.slackmacgap</string>
    </dict>
    </plist>
  '';

  dollPlistBinary =
    pkgs.runCommand "com.xiaogd.Doll.plist" { nativeBuildInputs = [ pkgs.libplist ]; }
      # Should be compatible with Apple's binary plist format
      ''
        plistutil -i ${dollPlistXml} -o $out -f binary1
      '';
in
{
  imports = [ ./darwin.nix ];

  programs.home-manager.enable = true;

  home.username = "djm";
  home.homeDirectory = "/Users/djm";

  home.sessionVariables = {
    AWS_DEFAULT_SSO_REGION = "us-east-1";
    AWS_DEFAULT_SSO_START_URL = "https://kevel.awsapps.com/start";
    TERRAFORM_BINARY_NAME = "tofu";
  };

  home.sessionPath = [
    "$HOME/.npm-global/bin"
    "$HOME/src/kevel/cli-tools/micha"
  ];

  home.packages = with pkgs; [
    aws-sso-util
    copilot-language-server
    coffeescript
    google-cloud-sdk
    nodejs
    nodePackages.aws-cdk
    opentofu
  ];

  # TODO
  home.file = {
    ".npmrc".text = ''
      @adzerk:registry=https://npm.pkg.github.com/
      //npm.pkg.github.com/:_authToken=''${ADZERK_GITHUB_PACKAGES_AUTH_TOKEN}
      prefix=~/.npm-global
    '';
    "Library/Preferences/com.xiaogd.Doll.plist".source = dollPlistBinary;
  };

  sops.secrets = {
    "git_email_config/kevel" = { };
    "ssh_config/kevel" = { };
  }
  // lib.mapAttrs' (_: secretName: lib.nameValuePair "env/${secretName}" { }) envSecrets;

  programs.java = {
    enable = true;
  };

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    includes = lib.mkForce [
      { path = config.sops.secrets."git_email_config/kevel".path; }
      {
        path = config.sops.secrets."git_email_config/default".path;
        condition = "gitdir:~/src/ext/";
      }
      {
        path = config.sops.secrets."git_email_config/default".path;
        condition = "gitdir:~/dotfiles/";
      }
      {
        contents = {
          commit.gpgSign = true;
          tag.gpgSign = true;
        };
        condition = "gitdir:~/src/ext/";
      }
      {
        contents = {
          commit.gpgSign = true;
          tag.gpgSign = true;
        };
        condition = "gitdir:~/dotfiles/";
      }
    ];
    ignores = [
      ".envrc"
      ".clj-kondo"
      "shell.nix"
      ".direnv"
      ".dir-locals.el"
    ];
  };
  programs.ssh = {
    includes = [ config.sops.secrets."ssh_config/kevel".path ];
    matchBlocks = {
      "i-*" = {
        user = "ubuntu";
        proxyCommand = "ssh-ssm.sh %h %r";
        identityFile = "~/.ssh/ssm-ssh-tmp";
        userKnownHostsFile = "/dev/null";
        forwardAgent = true;
        serverAliveInterval = 5;
        sendEnv = [
          "AWS_*"
          "ADZERK_*"
        ];
        extraOptions = {
          "ConnectTimeout" = "30";
          "BatchMode" = "yes";
          "LogLevel" = "QUIET";
          "StrictHostKeyChecking" = "no";
        };
      };
      "*.orb.local" = {
        identityFile = "~/.orbstack/ssh/id_ed25519";
        forwardAgent = true;
        sendEnv = [
          "AWS_*"
          "ADZERK_*"
        ];
      };
    };
  };

  programs.granted = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh.envExtra = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (envName: secretName: ''
      if [ -e ${config.sops.secrets."env/${secretName}".path} ]; then
        export ${envName}=$(<${config.sops.secrets."env/${secretName}".path})
      fi
    '') envSecrets
  );

  programs.zsh.initContent = ''
    source "${pkgs.google-cloud-sdk}/share/zsh/site-functions/_gcloud"
  '';

}
