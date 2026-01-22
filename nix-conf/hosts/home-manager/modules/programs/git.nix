{
  config,
  lib,
  pkgs,
  ...
}:
let
  less = lib.getExe pkgs.less;
in
{
  home.packages = with pkgs; [
    diff-so-fancy
    difftastic
    git-crypt
  ];

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      line-numbers = true;
      navigate = true;
      light = false;
      file-style = "bold yellow ul";
      hunk-header-line-number-style = "brightyellow";
    };
  };

  programs.git = {
    enable = true;
    includes = [ { path = config.age.secrets."git/user".path; } ];
    attributes = [
      "*.el diff=elisp"
      "*.clj diff=clojure"
    ];
    settings = {
      alias = {
        # difftastic
        logt = "!sh -c 'GIT_EXTERNAL_DIFF=\"difft --background=dark\" git log -p --ext-diff'";
        showt = "!show() { GIT_EXTERNAL_DIFF=difft git show \${1} --ext-diff; }; show";
        difft = "difftool";
        # "raw" output
        rlog = "!git -c delta.raw=true -c core.pager=${less} log"; # usually used with -p
        rshow = "!git -c delta.raw=true -c core.pager=${less} show";
        rdiff = "!git -c delta.raw=true -c core.pager=${less} diff";
        #  copiable output (without line numbers or +/- indicators)
        clog = "!git -c delta.line-numbers=false log"; # usually used with -p
        cshow = "!git -c delta.line-numbers=false show";
        cdiff = "!git -c delta.line-numbers=false diff";
        # diff-so-fancy
        flog = ''!git -c core.pager="diff-so-fancy | ${less}" log''; # usually used with -p
        fshow = ''!git -c core.pager="diff-so-fancy | ${less}" show'';
        fdiff = ''!git -c core.pager="diff-so-fancy | ${less}" diff'';

        upstream = "!git push -u origin HEAD";
        update-master = "!git fetch origin master:master";
        update-main = "!git fetch origin main:main";

        checkout-pr = "!f() { git fetch \${2:-upstream} pull/$1/head:pr-$1 && git switch pr-$1; }; f";
        check-stash = "!f() { git stash show -p stash@{\${1:-0}} | git apply --check --verbose; }; f";
        reset-branch = "!git fetch && git reset --hard origin/$(git branch --show-current)";
      };
      core.editor = "vim";
      diff = {
        tool = "difftastic";
        colorMoved = "default";
        elisp = {
          xfuncname = "^\\((((def\\S+)|use-package)\\s+\\S+)";
        };
        clojure = {
          xfuncname = "^\\((def\\S+\\s+\\S+)";
        };
      };
      difftool = {
        prompt = false;
        difftastic = {
          cmd = ''difft "$LOCAL" "$REMOTE"'';
        };
      };
      pull = {
        ff = "only";
        rebase = false;
      };
      push.autoSetupRemote = true;
      rebase = {
        autostash = true;
      };
      github.user = "deejayem";
      user.name = "David Morgan";
    };
    ignores = [
      ".lein-repl-history"
      ".lsp"
      ".rebel_readline_history"
      ".cider-repl-history"
      "nohup.out"
      "*.elc"
      "*.eln"
      "*~"
    ];
    signing = {
      key = "9B436B1477A879C26CDB6604C171251002C200F2";
      signByDefault = true;
    };
  };
}
