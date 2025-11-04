{ ... }:
{
  services.libinput = {
    enable = true;
    touchpad = {
      clickMethod = "clickfinger";
      naturalScrolling = true;
      tappingButtonMap = "lmr";
    };
  };
}
