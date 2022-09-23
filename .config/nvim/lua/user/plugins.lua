local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end
--
-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

return packer.startup(function(use)

use { "wbthomason/packer.nvim"}
use { "nvim-lua/plenary.nvim"}
use { "windwp/nvim-autopairs"}
use { "kyazdani42/nvim-web-devicons"}
use { "kyazdani42/nvim-tree.lua"}
use { "moll/vim-bbye"}
use { "nvim-lualine/lualine.nvim"}
use { "lewis6991/impatient.nvim"}
use { "lukas-reineke/indent-blankline.nvim"}
use { "L3MON4D3/LuaSnip" }
use { "nvim-treesitter/nvim-treesitter"}
use { "dracula/vim" }
use { "akinsho/bufferline.nvim"}


use { "neovim/nvim-lspconfig"}
use { "williamboman/nvim-lsp-installer"}
use { "j-hui/fidget.nvim"}


-- use { "jose-elias-alvarez/null-ls.nvim"}
use { "numToStr/Comment.nvim"}

use{"https://github.com/junegunn/fzf.vim.git"}

use { "hrsh7th/nvim-cmp"}
  use { "hrsh7th/cmp-buffer"}
  use { "hrsh7th/cmp-path"}
  use { "saadparwaiz1/cmp_luasnip"}
  use { "hrsh7th/cmp-nvim-lsp"}
  use { "hrsh7th/cmp-nvim-lua"}
  use { "hrsh7th/cmp-cmdlIne" }

use {"unblevable/quick-scope"}

use {"KeitaNakamura/tex-conceal.vim"}
use {'goolord/alpha-nvim'}

use {'nvim-telescope/telescope.nvim', tag = '0.1.0'}
use {'glepnir/galaxyline.nvim'}
use {
  'lewis6991/gitsigns.nvim',
  config = function()
    require('gitsigns').setup()
  end
}

use 'ggandor/lightspeed.nvim'

use({
	'mvllow/modes.nvim',
	tag = 'v0.2.0',
	config = function()
		require('modes').setup()
	end
})

-- use {
--   'phaazon/hop.nvim',
--   branch = 'v2', -- optional but strongly recommended
--   config = function()
--     -- you can configure Hop the way you like here; see :h hop-config
--     require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
--   end
-- }

if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
