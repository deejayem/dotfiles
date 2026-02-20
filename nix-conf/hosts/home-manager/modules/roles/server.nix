{ pkgs, ... }:
{
  # Ensure we can ssh in using ghostty
  home.packages = with pkgs; [ ghostty.terminfo ];
}
