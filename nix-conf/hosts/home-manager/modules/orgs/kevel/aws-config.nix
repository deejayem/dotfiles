{
  config,
  lib,
  pkgs,
  ...
}:
let
  ssoRegion = "us-east-1";
  outputPath = "hosts/home-manager/modules/orgs/kevel/secrets/age/aws/config.age";
  accountsPath = config.age.secrets."kevel/aws/accounts".path;
  ssoStartUrlPath = config.age.secrets."kevel/env/aws-default-sso-start-url".path;
  rage = lib.getExe pkgs.rage;

  regenAwsSecret = pkgs.writers.writeBabashkaBin "regenerate-aws-secret" { } ''
    (ns regenerate-aws-secret
      (:require [babashka.fs :as fs]
                [babashka.process :as p]
                [clojure.edn :as edn]
                [clojure.string :as str]))

    (defn die! [msg]
      (binding [*out* *err*]
        (println msg))
      (System/exit 1))

    (defn check-file! [path desc]
      (when-not (fs/exists? path)
        (die! (str "Missing " desc ": " path))))

    (defn ->sso-profile [account-name {:keys [account-id]} role sso-start-url]
      (str/join "\n"
        [(str "[profile " account-name "." role "]")
         (str "credential_process=aws-sso-util credential-process --profile " account-name "." role)
         "region=${ssoRegion}"
         (str "sso_account_id=" account-id)
         (str "sso_account_name=" account-name)
         "sso_auto_populated=true"
         "sso_region=${ssoRegion}"
         (str "sso_role_name=" role)
         (str "sso_start_url=" sso-start-url)]))

    (defn ->escalation-profile [account-name]
      (str/join "\n"
        [(str "[profile " account-name ".DevOps]")
         (str "credential_process=pacs-aws -x -p " account-name ".ReadOnlyDevOps")]))

    (defn ->account-profiles [sso-start-url [account-key {:keys [read-only enable-escalation] :as account}]]
      (let [account-name (name account-key)
            role (if read-only "ReadOnlyDevOps" "DevOps")]
        (cond-> [(->sso-profile account-name account role sso-start-url)]
          (and read-only enable-escalation) (conj (->escalation-profile account-name)))))

    (defn ->header [sso-start-url]
      (str/join "\n"
        ["[sso-session kevel-sso]"
         (str "sso_start_url=" sso-start-url)
         "sso_region=${ssoRegion}"
         "sso_registration_scopes=sso:account:access"]))

    (defn generate-config []
      (check-file! "${accountsPath}" "decrypted accounts EDN")
      (check-file! "${ssoStartUrlPath}" "decrypted sso-start-url")
      (let [sso-start-url (str/trim (slurp "${ssoStartUrlPath}"))
            header (->header sso-start-url)
            profiles (->> (edn/read-string (slurp "${accountsPath}"))
                          (sort-by key)
                          (mapcat #(->account-profiles sso-start-url %))
                          (str/join "\n\n"))]
        (str header "\n\n" profiles "\n")))

    (defn encrypt-to-file! [content path]
      (fs/create-dirs (fs/parent path))
      (let [{:keys [exit err]} (p/shell {:in content :err :string :continue true}
                                        "${rage}" "-e" "-i"
                                        (str (fs/expand-home "~/.ssh/agenix"))
                                        "-o" path)]
        (if (zero? exit)
          (binding [*out* *err*]
            (println (str "Wrote " path)))
          (die! (str "rage encryption failed: " err)))))

    (encrypt-to-file! (generate-config) "${outputPath}")
  '';
in
{
  age.secrets."kevel/aws/config".path = "${config.home.homeDirectory}/.aws/config";

  home.packages = with pkgs; [
    aws-sso-util
    regenAwsSecret
  ];

  programs.zsh.shellAliases = {
    pacs-open = ''open "$(pacs-aws -l -f)"'';
    pacs-incognito = ''open -na "Google Chrome" --args --incognito "$(pacs-aws -l -f)"'';
  };

  home.sessionVariables = {
    AWS_DEFAULT_SSO_REGION = "us-east-1";
  };

  programs.ssh = {
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
    };
  };
}
