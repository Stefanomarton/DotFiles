local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

-- Disable default plugin
local disabled_built_ins = {
	"netrw",
	"netrwPlugin",
	"netrwSettings",
	"netrwFileHandlers",
	"gzip",
	"zip",
	"zipPlugin",
	"tar",
	"tarPlugin",
	"getscript",
	"getscriptPlugin",
	"vimball",
	"vimballPlugin",
	"2html_plugin",
	"logipat",
	"rrhelper",
	"spellfile_plugin",
	"matchit",
}

for _, plugin in pairs(disabled_built_ins) do
	vim.g["loaded_" .. plugin] = 1
end

local nocode = function()
	return vim.fn.exists("g:vscode") == 0
end

return packer.startup(function(use)
	use({ "wbthomason/packer.nvim" })
	use({ "nvim-lua/plenary.nvim" })
	use({ "lewis6991/impatient.nvim" })

	use({
		"windwp/nvim-autopairs",
		config = function()
			require("user.autopairs")
		end,
	})

	use({ "kyazdani42/nvim-web-devicons" })

	use({ "moll/vim-bbye" })

	use({
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			require("user.indentline")
		end,
	})

	use({ "L3MON4D3/LuaSnip" })

	use({
		"nvim-treesitter/nvim-treesitter",
		config = function()
			require("user.treesitter")
		end,
		run = ":TSUpdate",
	})

	use({
		"nvim-treesitter/nvim-treesitter-context",
		after = "nvim-treesitter",
		config = function()
			require("user.treesitter-context")
		end,
	})

	use({
		"lewis6991/spellsitter.nvim",
		after = "nvim-treesitter",
		config = function()
			require("spellsitter").setup({
				enable = true,
			})
		end,
	})
	use({ "nvim-treesitter/playground" })

	--Colorscheme and deco stuff
	use({
		"feline-nvim/feline.nvim",
		after = "tokyonight.nvim",
		config = function()
			require("user.deco.feline")
		end,
	})
	use("shaunsingh/nord.nvim")

	use({ "folke/tokyonight.nvim" })
	-- If you are using Packer
	use({
		"romgrk/barbar.nvim",
		config = function()
			require("user.deco.barbar")
		end,
	})
	-- use({ "j-hui/fidget.nvim"})
	-- use({ "rcarriga/nvim-notify" })
	use({
		"Pocco81/true-zen.nvim",
		config = function()
			require("user.deco.colorscheme")
		end,
	})
	-- use({
	-- 	"glepnir/galaxyline.nvim",
	-- 	config = function()
	-- 		require("user.deco.galaxyline")
	-- 	end,
	-- })
	use({
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
		end,
	})

	use({
		"folke/todo-comments.nvim",
		config = function()
			require("user.todo")
		end,
	})

	use({
		"glepnir/dashboard-nvim",
		event = "BufEnter",
		config = function()
			require("user.deco.dashboard")
		end,
	})

	--[[ 	
The Core plugins
]]

	use({
		"ggandor/leap.nvim",
		config = function()
			require("leap").set_default_keymaps()
		end,
	})
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("user.comment")
		end,
	})
	use({ "hrsh7th/nvim-cmp" })
	use({ "kdheepak/cmp-latex-symbols", ft = "tex" })
	--use({ "KeitaNakamura/tex-conceal.vim", ft = "tex" })
	use({ "hrsh7th/cmp-buffer" })
	use({ "hrsh7th/cmp-path" })
	use({ "saadparwaiz1/cmp_luasnip" })
	use({ "hrsh7th/cmp-nvim-lsp" }) --
	use({ "hrsh7th/cmp-nvim-lua" })
	use({ "hrsh7th/cmp-cmdlIne" })
	use({ "f3fora/cmp-spell" }) --
	use({ "hrsh7th/cmp-omni" })

	use({ "williamboman/nvim-lsp-installer" })
	use({
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"neovim/nvim-lspconfig",
	})

	use({ "unblevable/quick-scope" })
	use({ "lervag/vimtex" })

	--[[ 	telescope and telescope integrations ]]

	use({ "junegunn/fzf.vim" })
	use({
		"nvim-telescope/telescope.nvim",
		config = function()
			require("user.telescope")
		end,
	})

	use({
		"crispgm/telescope-heading.nvim",
		config = function()
			require("telescope").load_extension("heading")
		end,
		ft = { "tex", "md" },
	})

	use({
		"nvim-telescope/telescope-packer.nvim",
		config = function()
			require("telescope").load_extension("packer")
		end,
	})

	use({
		"nvim-telescope/telescope-file-browser.nvim",
		config = function()
			require("telescope").load_extension("file_browser")
		end,
	})

	-- Luasnip integration with Telescope to check the snippet in a hurry
	use({
		"benfowler/telescope-luasnip.nvim",
		config = function()
			require("telescope").load_extension("luasnip")
		end,
	})

	use({
		"keyvchan/telescope-find-pickers.nvim",
		config = function()
			require("telescope").load_extension("find_pickers")
		end,
	})

	use({
		"mvllow/modes.nvim",
		config = function()
			require("modes").setup()
		end,
	})

	use({
		"phaazon/hop.nvim",
		branch = "v2", -- optional but strongly recommended
		config = function()
			-- you can configure Hop the way you like here; see :h hop-config
			require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
		end,
	})

	use({
		"folke/which-key.nvim",
		config = function()
			require("user.whichkey")
		end,
	})
	use({ "jose-elias-alvarez/null-ls.nvim" })
	use({
		"folke/trouble.nvim",
		config = function()
			require("user.trouble")
		end,
	})
	use({ "NvChad/nvim-colorizer.lua" })

	use({
		"kylechui/nvim-surround",
		tag = "main",
		config = function()
			require("nvim-surround").setup({})
		end,
	})

	use({
		"abecodes/tabout.nvim",
		config = function()
			require("tabout").setup({
				tabkey = "<Tab>", -- key to trigger tabout, set to an empty string to disable
				backwards_tabkey = "<S-Tab>", -- key to trigger backwards tabout, set to an empty string to disable
				act_as_tab = true, -- shift content if tab out is not possible
				act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
				default_tab = "<C-t>", -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
				default_shift_tab = "<C-d>", -- reverse shift default action,
				enable_backwards = true, -- well ...
				completion = true, -- if the tabkey is used in a completion pum
				tabouts = {
					{ open = "'", close = "'" },
					{ open = '"', close = '"' },
					{ open = "`", close = "`" },
					{ open = "(", close = ")" },
					{ open = "[", close = "]" },
					{ open = "{", close = "}" },
				},
				ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
				exclude = {}, -- tabout will ignore these filetypes
			})
		end,
	})

	use({
		"epwalsh/obsidian.nvim",
		config = function()
			require("obsidian").setup({
				dir = "~/GoogleDrive/Obsidian/",
				completion = {
					nvim_cmp = true, -- if using nvim-cmp, otherwise set to false
				},
			})
		end,
	})

	use({
		"ryleelyman/latex.nvim",
		config = function()
			require("latex").setup({
				conceals = {
					enabled = {
						"greek",
						"math",
						-- "script",
						-- "delim",
						-- "font",
					},
					add = {},
				},
				imaps = {
					enabled = false,
					add = {},
					default_leader = "`",
				},
				surrounds = {
					enabled = false,
					command = "c",
					environment = "e",
				},
			})
		end,
	})

	use("kdheepak/lazygit.nvim")

	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)

-- T O D O
