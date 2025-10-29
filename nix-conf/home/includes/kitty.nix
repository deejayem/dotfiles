{ ... }:
{
  programs.kitty = {
    enable = true;
    package = null;

    settings = {
      window_title = "%d";
      copy_on_select = true;
      macos_option_as_alt = true;
    };

    darwinLaunchOptions = [
      "--single-instance"
    ];

    environment = {
      TERM = "tmux-256color";
    };

    shellIntegration = {
      enableZshIntegration = true;
    };
  };
}
