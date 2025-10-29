{ ... }:
{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      format = ''
        $os$username$hostname$localip$shlvl$singularity$kubernetes$nats$directory$vcsh$fossil_branch$fossil_metrics$git_branch$git_commit$git_state$git_metrics$git_status$hg_branch$pijul_channel$docker_context$package$bun$c$cmake$cobol$cpp$daml$dart$deno$dotnet$elixir$elm$erlang$fennel$gleam$golang$gradle$haskell$haxe$helm$java$julia$kotlin$lua$mojo$nim$nodejs$ocaml$odin$opa$perl$php$pulumi$purescript$python$quarto$raku$rlang$red$ruby$rust$scala$solidity$swift$terraform$typst$vlang$vagrant$zig$buf$guix_shell$nix_shell$conda$pixi$meson$spack$memory_usage$openstack$azure$direnv$env_var$mise$crystal$custom$sudo$jobs$battery$time$container$netns$shell$fill$status$cmd_duration$aws$gcloud$line_break$character
      '';
      fill.symbol = " ";
      aws = {
        format = ''[$symbol($profile )(\($region\) )(\[$duration\] )]($style)'';
        symbol = "󰸏 ";
      };
      cmake.format = "[$symbol($version )]($style)";
      directory = {
        format = "[$path]($style)[$read_only]($read_only_style) ";
        read_only = " 󰌾";
        style = "bold fg:#00afff"; # xterm 39 (deepskyblue1)
        truncate_to_repo = false;
        truncation_length = 10;
        truncation_symbol = "…/";
      };
      direnv.disabled = false;
      docker_context = {
        format = "[$symbol$context]($style) ";
        symbol = " ";
      };
      gcloud = {
        format = ''[$symbol@$project (\($region\))]($style) '';
        symbol = "󱇶 ";
      };
      golang = {
        format = "[$symbol($version )]($style)";
        symbol = " ";
      };
      git_branch = {
        format = "[$symbol$branch(:$remote_branch)]($style) ";
        style = "green";
        symbol = " ";
      };
      git_status = {
        format = ''([$all_status$ahead_behind]($style))'';
        stashed = "[\\\$\${count} ](fg:#5fd700)"; # xterm 76 (chartreuse3)
        ahead = "[⇡\${count} ](fg:#87d700)"; # xterm 118 (chartreuse2)
        behind = "[⇣\${count} ](fg:#ff5f5f)"; # xterm 203 (indianred1)
        diverged = "[⇕\${count} ](fg:#af87ff)"; # xterm 141 (mediumpurple1)
        conflicted = "[=\${count} ](bold fg:#ff0000)"; # xterm 196 (red1)
        deleted = "[✘\${count} ](bold fg:#d70000)"; # xterm 160 (red3)
        renamed = "[»\${count} ](fg:#00afff)"; # xterm 39 (deepskyblue1)
        modified = "[!\${count} ](fg:#d7af00)"; # xterm 178 (gold3)
        staged = "[+\${count} ](fg:#5fd700)"; # xterm 76 (chartreuse3)
        untracked = "[?\${count} ](fg:#00afff)"; # xterm 39 (deepskyblue1)
      };
      haskell.symbol = " ";
      hostname = {
        format = "[$ssh_symbol$hostname]($style) ";
        ssh_symbol = " ";
      };
      java = {
        format = "[$symbol($version )]($style)";
        symbol = " ";
        style = "#ffa500";
      };
      lua.format = "[$symbol($version )]($style)";
      memory_usage.symbol = "󰍛 ";
      nix_shell.symbol = " ";
      nodejs = {
        format = "[$symbol($version )]($style)";
        symbol = " ";
      };
      os = {
        disabled = false;
        symbols = {
          Macos = " ";
          NixOS = " ";
        };
      };
      package.format = "[$symbol$version]($style) ";
      python.format = "[\${symbol}\${pyenv_prefix}(\${version} )(\($virtualenv\) )]($style)";
      ruby.format = "[$symbol($version )]($style)";
      status.disabled = false;
      username.format = "[$user]($style) ";
      terraform.format = "[$symbol$workspace]($style) ";
      time = {
        disabled = false;
        format = "[ $time]($style) ";
        style = "bright-black"; # TODO is "white dimmed" better?
      };
    };
  };
}
