{ config, pkgs, ... }:
let
  ssoRegion = "us-east-1";

  encryptedConfig = "hosts/home-manager/modules/orgs/kevel/secrets/age/aws/config.age";

  accountsPath = config.age.secrets."kevel/aws/accounts".path;
  ssoStartUrlPath = config.age.secrets."kevel/env/sso-start-url".path;

  # Re-creates config.age from accounts.age (another switch is needed to update the actual aws config)
  regenAwsSecret = pkgs.writeShellApplication {
    name = "regenerate-aws-secret";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.jq
      pkgs.rage
    ];
    text = ''
      set -euo pipefail
      umask 077

      encrypted_config="${encryptedConfig}"

      if [ ! -f ${accountsPath} ]; then
        printf '%s\n' "Missing decrypted accounts JSON: ${accountsPath}" >&2
        exit 1
      fi

      if [ ! -f ${ssoStartUrlPath} ]; then
        printf '%s\n' "Missing decrypted sso-start-url: ${ssoStartUrlPath}" >&2
        exit 1
      fi

      SSO_START_URL="$(<${ssoStartUrlPath})"

      tmp="$(mktemp)"
      trap 'rm -f "$tmp"' EXIT

      printf '%s\n' \
        "[sso-session kevel-sso]" \
        "sso_start_url=$SSO_START_URL" \
        "sso_region=${ssoRegion}" \
        "sso_registration_scopes=sso:account:access" \
        "" \
        > "$tmp"

      jq -r \
        --arg ssoStartUrl "$SSO_START_URL" \
        --arg ssoRegion "${ssoRegion}" '
        to_entries
        | sort_by(.key)
        | .[]
        | .key as $name
        | .value as $a
        | ($a.readOnly // false) as $readOnly
        | ($a.enableEscalation // false) as $enableEscalation
        | (if $readOnly then "ReadOnlyDevOps" else "DevOps" end) as $role
        | ($name + "." + $role) as $profile
        | [
            "[profile \($profile)]\n"
            + "credential_process=aws-sso-util credential-process --profile \($profile)\n"
            + "region=\($ssoRegion)\n"
            + "sso_account_id=\($a.accountId)\n"
            + "sso_account_name=\($name)\n"
            + "sso_auto_populated=true\n"
            + "sso_region=\($ssoRegion)\n"
            + "sso_role_name=\($role)\n"
            + "sso_start_url=\($ssoStartUrl)",

            (if ($readOnly and $enableEscalation) then
              "[profile \($name).DevOps]\n"
              + "credential_process=pacs-aws -x -p \($name).ReadOnlyDevOps"
            else empty end)
        ]
        | map(select(. != ""))
        | join("\n\n")
        | . + "\n"
        ' < ${accountsPath} >> "$tmp"

      mkdir -p "$(dirname "$encrypted_config")"
      rage -e -i ~/.ssh/agenix < "$tmp" >| "$encrypted_config"

      printf '%s\n' "Wrote $encrypted_config" >&2
    '';
  };
in
{
  age.secrets."kevel/aws/config".path = "${config.home.homeDirectory}/.aws/config";

  home.packages = [ regenAwsSecret ];

  programs.awscli.enable = true;
}
