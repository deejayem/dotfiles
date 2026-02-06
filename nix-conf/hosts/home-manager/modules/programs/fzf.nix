{
  config,
  pkgs,
  lib,
  ...
}:
let
  bat = lib.getExe pkgs.bat;
  eza = lib.getExe pkgs.eza;
  fd = lib.getExe pkgs.fd;
  fzf = lib.getExe pkgs.fzf;
  fzfTmux = lib.getExe' pkgs.fzf "fzf-tmux";
  head = lib.getExe' pkgs.coreutils "head";
  nvim = lib.getExe pkgs.neovim;
  rg = lib.getExe pkgs.ripgrep;
  stty = lib.getExe' pkgs.coreutils "stty";

  show_file_or_dir_preview = "if [ -d {} ]; then ${eza} --tree --color=always {} | ${head} -200; else ${bat} -n --color=always --line-range :500 {}; fi";
in
{
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    changeDirWidgetCommand = "${fd} --type=d --hidden --strip-cwd-prefix --exclude .git --exclude node_modules"; # FZF_ALT_C_COMMAND
    changeDirWidgetOptions = [ "--preview '${eza} --tree --color=always {} | ${head} -200'" ]; # FZF_ALT_C_OPTS
    defaultCommand = "${fd} --hidden --strip-cwd-prefix --exclude .git --exclude node_modules"; # FZF_DEFAULT_COMMAND
    defaultOptions = [
      "--bind=ctrl-t:toggle-all"
      "--bind=ctrl-j:jump"
    ]; # FZF_DEFAULT_OPTS
    fileWidgetCommand = config.programs.fzf.defaultCommand; # FZF_CTRL_T_COMMAND
    fileWidgetOptions = [ "--preview '${show_file_or_dir_preview}'" ]; # FZF_CTRL_T_OPTS
    historyWidgetOptions = [
      "--preview 'echo {}'"
      "--preview-window down:3:hidden:wrap"
      "--bind 'ctrl-t:toggle-preview'"
    ]; # FZF_CTRL_R_OPTS
  };

  programs.zsh = {
    shellAliases = {
      ".," = "dotcomma";
      fb = "fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'";
    };

    siteFunctions = {
      fcd = "cd $(${fd} -L --max-depth=\${1:-4} --type=d 2>/dev/null | ${fzfTmux})";

      fif = ''
        if [ ! "$#" -gt 0 ]; then
          echo "usage: fif <SEARCH_TERM>"
          return 1
        fi
        ${rg} --files-with-matches --no-messages "$1" | ${fzf} $FZF_PREVIEW_WINDOW --preview "${rg} --ignore-case --pretty --context 10 '$1' {}"
      '';

      fe = ''
        IFS=$'\n' files=($(${fzfTmux} --query="$1" --multi --select-1 --exit-0))
        [[ -n "$files" ]] && ''${EDITOR:-${nvim}} "''${files[@]}"
      '';

      "dotcomma" = ''
        local declare dirs=()
        get_parent_dirs() {
          if [[ -d "$1" ]]; then dirs+=("$1"); else return; fi
          if [[ "$1" == '/' ]]; then
            for _dir in "''${dirs[@]}"; do echo $_dir; done
          else
            get_parent_dirs $(dirname "$1")
          fi
        }
        local DIR=$(get_parent_dirs $(realpath "$PWD/..") | ${fzfTmux})
        cd "$DIR"
      '';
    };

    initContent = ''
      enable-fzf-tab

      # preview directory's content with eza when completing cd
      zstyle ':fzf-tab:complete:cd:*' fzf-preview "${eza} -1 --color=always $realpath"
      # switch group using `,` and `.`
      zstyle ':fzf-tab:*' switch-group ',' '.'

      # functions modified from https://www.josean.com/posts/7-amazing-cli-tools
      _fzf_compgen_path () {
        ${fd} --hidden --exclude .git --exclude node_modules . "$1"
      }
      _fzf_compgen_dir () {
        ${fd} --type=d --hidden --exclude .git --exclude node_modules . "$1"
      }
      _fzf_comprun () {
        local command=$1
        shift

        case "$command" in
          cd)           ${fzf} --preview '${eza} --tree --color=always {} | head -200' "$@" ;;
          export|unset) ${fzf} --preview "eval 'echo $'{}"         "$@" ;;
          ssh)          ${fzf} --preview 'dig {}'                   "$@" ;;
          *)            ${fzf} --preview "${show_file_or_dir_preview}" "$@" ;;
        esac
      }

      # disable flow control (so that fzf-git.sh's ^g^s can work)
      ${stty} -ixon
    '';

    plugins = with pkgs; [
      {
        name = "zsh-fzf-tab";
        src = zsh-fzf-tab;
        file = "share/fzf-tab/fzf-tab.zsh";
      }
      {
        name = "fzf-git.sh";
        src = fzf-git-sh;
        file = "share/fzf-git-sh/fzf-git.sh";
      }
    ];
  };
}
