{ config, ... }:
let
  ssoRegion = "us-east-1";
  inherit (config.host.private) accounts ssoStartUrl;

  mkSsoProfile =
    {
      accountName,
      roleName,
      accountId,
    }:
    let
      profileName = "${accountName}.${roleName}";
    in
    {
      name = "profile ${profileName}";
      value = {
        sso_start_url = ssoStartUrl;
        sso_region = ssoRegion;
        sso_account_name = accountName;
        sso_account_id = accountId;
        sso_role_name = roleName;
        region = ssoRegion;
        credential_process = "aws-sso-util credential-process --profile ${profileName}";
        sso_auto_populated = true;
      };
    };

  mkEscalationProfile =
    { accountName }:
    let
      readOnlyProfileName = "${accountName}.ReadOnlyDevOps";
      fullAccessProfileName = "${accountName}.DevOps";
    in
    {
      name = "profile ${fullAccessProfileName}";
      value = {
        credential_process = "pacs -x -p ${readOnlyProfileName}";
      };
    };

  mkAwsProfile =
    {
      accountName,
      accountId,
      readOnly ? false,
      enableEscalation ? false,
    }:
    let
      roleName = if readOnly then "ReadOnlyDevOps" else "DevOps";
      ssoProfile = mkSsoProfile { inherit accountName roleName accountId; };
      escalationProfile =
        if readOnly && enableEscalation then mkEscalationProfile { inherit accountName; } else null;
    in
    builtins.listToAttrs (
      [ ssoProfile ] ++ (if escalationProfile != null then [ escalationProfile ] else [ ])
    );

  accountProfiles = builtins.foldl' (
    acc: accountName: acc // (mkAwsProfile ({ inherit accountName; } // accounts.${accountName}))
  ) { } (builtins.attrNames accounts);
in
{
  programs.awscli = {
    enable = true;
    settings = {
      "sso-session kevel-sso" = {
        sso_start_url = ssoStartUrl;
        sso_region = ssoRegion;
        sso_registration_scopes = "sso:account:access";
      };
    }
    // accountProfiles;
  };
}
