{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
      molokai
      nvim-autopairs
      nvim-lastplace
      nvim-surround
      plenary-nvim
      telescope-nvim
      vim-pasta
      vim-repeat
      vim-sleuth
      (nvim-treesitter.withPlugins (
        ps: with ps; [
          tree-sitter-bash
          tree-sitter-lua
          tree-sitter-vim
          tree-sitter-vimdoc
          tree-sitter-json
          tree-sitter-yaml
          tree-sitter-nix
          tree-sitter-markdown
          tree-sitter-regex
          tree-sitter-clojure
          tree-sitter-java
          tree-sitter-typescript
          tree-sitter-javascript
          tree-sitter-elisp
          tree-sitter-make
          tree-sitter-dockerfile
          tree-sitter-markdown-inline
        ]
      ))
    ];

    extraLuaConfig = ''
      vim.opt.termguicolors = true
      vim.cmd('colorscheme molokai')
      vim.cmd('highlight Normal ctermfg=white ctermbg=black')

      vim.opt.background = "dark"
      vim.opt.copyindent = true
      vim.opt.expandtab = true
      vim.opt.ignorecase = true
      vim.opt.number = true
      vim.opt.shiftwidth = 4
      vim.opt.smartcase = true
      vim.opt.tabstop = 4
      vim.opt.hlsearch = true
      vim.opt.showmatch = true
      vim.api.nvim_set_hl(0, "Normal", { bg = "#000000" })

      vim.api.nvim_create_autocmd('BufEnter', {
        callback = function() vim.opt.formatoptions:remove({'c','r','o'}) end
      })

      -- Keymaps (Lua)
      local map = vim.keymap.set
      local silent = { silent = true }

      map({'n','i'}, '<F3>', function() vim.cmd('silent nohlsearch') end, silent)

      map({'n','i'}, '<F4>', function()
        vim.cmd('silent setlocal spell spelllang=en_gb')
      end, silent)

      map({'n','i'}, '<F5>', function()
        vim.cmd('silent setlocal nospell')
      end, silent)

      map({'n','i'}, '<F6>', function()
        vim.cmd('silent set diffopt+=iwhite')
      end, silent)

      map({'n','i'}, '<F7>', function()
        vim.cmd('silent set diffopt-=iwhite')
      end, silent)

      require('nvim-lastplace').setup({})
      require('nvim-treesitter.configs').setup({
        highlight = { enable = true },
        indent    = { enable = true },
      })
      require('telescope').setup({})
      require('nvim-autopairs').setup({})
      require('nvim-surround').setup({})
    '';
  };

}
