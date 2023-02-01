{ config, pkgs, lib, ... }:
{

  imports = [
    ./common.nix
    ./clojure.nix
  ];

  home.packages = with pkgs; [
    docker
    docker-compose
    ffmpeg
    gopass-jsonapi
    mpv
    mu
    neovim
  ];

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    prefix = "C-x";
    plugins = with pkgs; [
      tmuxPlugins.copy-toolkit
      tmuxPlugins.copycat
      tmuxPlugins.extrakto
      tmuxPlugins.fuzzback
      tmuxPlugins.fzf-tmux-url
      tmuxPlugins.jump
      {
        plugin = tmuxPlugins.open;
        extraConfig = ''
          set -g @open-S 'https://www.duckduckgo.com/?q='
        '';
      }
      {
        plugin = tmuxPlugins.tmux-thumbs;
        extraConfig = ''
          set -g @thumbs-reverse enabled
          set -g @thumbs-unique enabled
          set -g @thumbs-position right
          set -g @thumbs-contrast 1
          #${lib.optionalString pkgs.stdenv.isLinux "set -g @thumbs-upcase-command 'xargs xdg-open {}'"}
          #${lib.optionalString pkgs.stdenv.isDarwin "set -g @thumbs-upcase-command 'xargs open {}'"}
        '';
      }
    ];
    extraConfig = ''
     unbind-key R
     bind-key R run-shell ' \
       tmux source-file ~/.config/tmux/tmux.conf > /dev/null; \
       tmux display-message "Sourced .config/tmux/tmux.conf!"'

      bind-key £ split-window -h

      set-option -g status-bg '#666666'
      set-option -g status-fg '#aaaaaa'
      set-option -g status-left-length 50
      set-option -g status-right " %a, %b %d - %H:%M "

      ${lib.optionalString pkgs.stdenv.isLinux ''
        bind-key -T copy-mode y send-keys -X copy-pipe-and-cancel "xsel -i -p && xsel -o -p | xsel -i -b"
        bind-key C-y run "xsel -o | tmux load-buffer - ; tmux paste-buffer"
      ''}
      ${lib.optionalString pkgs.stdenv.isDarwin ''
        bind-key -T copy-mode y send-keys -X copy-pipe-and-cancel "reattach-to-user-namespace pbcopy"
        bind-key C-y run "reattach-to-user-namespace pbpaste | tmux load-buffer - ; tmux paste-buffer"
      ''}
    '';
  };

}

