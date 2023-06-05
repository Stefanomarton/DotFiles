return {
	-- the colorscheme should be available when starting Neovim
	{
		"shaunsingh/nord.nvim",
		config = function()
			vim.cmd.colorscheme("nord")
			vim.g.nord_borders = true
			vim.g.nord_contrast = true
			-- require("nord").set()
		end,
	},
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("lualine").setup({
				options = {
					theme = "nord",
				},
			})
		end,
	},
	{
		"kyazdani42/nvim-web-devicons",
	},
	{
		-- 	"lukas-reineke/indent-blankline.nvim",
		-- 	config = function()
		-- 		require("indent_blankline").setup({
		-- 			char = "▏",
		-- 			show_trailing_blankline_indent = false,
		-- 			show_end_of_line = false,
		-- 			show_first_indent_level = true,
		-- 			use_treesitter = true,
		-- 			show_current_context = true,
		-- 			show_current_context_start = true,
		-- 			buftype_exclude = { "terminal", "nofile" },
		-- 			filetype_exclude = {
		-- 				"help",
		-- 				"packer",
		-- 				"NvimTree",
		-- 			},
		-- 		})
		--
				-- vim.opt.list = true
				-- vim.opt.listchars:append("eol:↴")
				-- vim.opt.listchars:append("space:⋅")
			-- end,
	},
	{
		--
		-- 	"petertriho/nvim-scrollbar",
		-- 	config = function()
		-- 		require("scrollbar").setup({
		-- 			show = true,
		-- 			show_in_active_only = true,
		-- 			set_highlights = true,
		-- 			folds = 1000, -- handle folds, set to number to disable folds if no. of lines in buffer exceeds this
		-- 			max_lines = false, -- disables if no. of lines in buffer exceeds this
		-- 			hide_if_all_visible = true, -- Hides everything if all lines are visible
		-- 			throttle_ms = 100,
		-- 			handle = {
		-- 				text = " ",
		-- 				color = nil,
		-- 				cterm = nil,
		-- 				highlight = "CursorColumn",
		-- 				hide_if_all_visible = true, -- Hides handle if all lines are visible
		-- 			},
		-- 			marks = {
		-- 				Cursor = {
		-- 					text = "•",
		-- 					priority = 0,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "Normal",
		-- 				},
		-- 				Search = {
		-- 					text = { "-", "=" },
		-- 					priority = 1,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "Search",
		-- 				},
		-- 				Error = {
		-- 					text = { "-", "=" },
		-- 					priority = 2,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "DiagnosticVirtualTextError",
		-- 				},
		-- 				Warn = {
		-- 					text = { "-", "=" },
		-- 					priority = 3,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "DiagnosticVirtualTextWarn",
		-- 				},
		-- 				Info = {
		-- 					text = { "-", "=" },
		-- 					priority = 4,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "DiagnosticVirtualTextInfo",
		-- 				},
		-- 				Hint = {
		-- 					text = { "-", "=" },
		-- 					priority = 5,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "DiagnosticVirtualTextHint",
		-- 				},
		-- 				Misc = {
		-- 					text = { "-", "=" },
		-- 					priority = 6,
		-- 					color = nil,
		-- 					cterm = nil,
		-- 					highlight = "Normal",
		-- 				},
		-- 				-- GitAdd = {
		-- 				-- 	text = "┆",
		-- 				-- 	priority = 7,
		-- 				-- 	color = nil,
		-- 				-- 	cterm = nil,
		-- 				-- 	highlight = "GitSignsAdd",
		-- 				-- },
		-- 				-- GitChange = {
		-- 				-- 	text = "┆",
		-- 				-- 	priority = 7,
		-- 				-- 	color = nil,
		-- 				-- 	cterm = nil,
		-- 				-- 	highlight = "GitSignsChange",
		-- 				-- },
		-- 				-- GitDelete = {
		-- 				-- 	text = "▁",
		-- 				-- 	priority = 7,
		-- 				-- 	color = nil,
		-- 				-- 	cterm = nil,
		-- 				-- 	highlight = "GitSignsDelete",
		-- 				-- },
		-- 			},
		-- 			excluded_buftypes = {
		-- 				"terminal",
		-- 				"mason",
		-- 			},
		-- 			excluded_filetypes = {
		-- 				"prompt",
		-- 				"TelescopePrompt",
		-- 				"noice",
		-- 				"mason",
		-- 				"packer",
		-- 			},
		-- 			autocmd = {
		-- 				render = {
		-- 					"BufWinEnter",
		-- 					"TabEnter",
		-- 					"TermEnter",
		-- 					"WinEnter",
		-- 					"CmdwinLeave",
		-- 					"TextChanged",
		-- 					"VimResized",
		-- 					"WinScrolled",
		-- 				},
		-- 				clear = {
		-- 					"BufWinLeave",
		-- 					"TabLeave",
		-- 					"TermLeave",
		-- 					"WinLeave",
		-- 				},
		-- 			},
		-- 			handlers = {
		-- 				cursor = true,
		-- 				diagnostic = true,
		-- 				gitsigns = false, -- Requires gitsigns
		-- 				handle = true,
		-- 				search = false, -- Requires hlslens
		-- 				ale = false, -- Requires ALE
		-- 			},
		-- 		})
		-- 	end,
	},
	{
		"declancm/cinnamon.nvim",
		config = function()
			require("cinnamon").setup({
				default_keymaps = true, -- Create default keymaps.
				extra_keymaps = false, -- Create extra keymaps.
				extended_keymaps = true, -- Create extended keymaps.
				override_keymaps = true, -- The plugin keymaps will override any existing keymaps.
				always_scroll = true, -- Scroll the cursor even when the window hasn't scrolled.
				centered = true, -- Keep cursor centered in window when using window scrolling.
				disabled = false, -- Disables the plugin.
				default_delay = 5, -- The default delay (in ms) between each line when scrolling.
				hide_cursor = true, -- Hide the cursor while scrolling. Requires enabling termguicolors!
				horizontal_scroll = true, -- Enable smooth horizontal scrolling when view shifts left or right.
				max_length = 300, -- Maximum length (in ms) of a command. The line delay will be
				-- re-calculated. Setting to -1 will disable this option.
				scroll_limit = 150, -- Max number of lines moved before scrolling is skipped. Setting
				-- to -1 will disable this option.
			})
		end,
	},
	{
		-- 	"glepnir/dashboard-nvim",
		-- 	config = function()
		-- 		require("dashboard").setup()
		-- 	end,
	},
}
