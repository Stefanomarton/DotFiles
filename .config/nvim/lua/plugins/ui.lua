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
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			require("indent_blankline").setup({
				char = "▏",
				show_trailing_blankline_indent = false,
				show_end_of_line = false,
				show_first_indent_level = true,
				use_treesitter = true,
				show_current_context = true,
				show_current_context_start = true,
				buftype_exclude = { "terminal", "nofile" },
				filetype_exclude = {
					"help",
					"packer",
					"NvimTree",
				},
			})

			vim.opt.list = true
			vim.opt.listchars:append("eol:↴")
			vim.opt.listchars:append("space:⋅")
		end,
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
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			-- add any options here
		},
		dependencies = {
			-- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
			"MunifTanjim/nui.nvim",
			-- OPTIONAL:
			--   `nvim-notify` is only needed, if you want to use the notification view.
			--   If not available, we use `mini` as the fallback
			-- "rcarriga/nvim-notify",
		},
		config = function()
			require("noice").setup({
				lsp = {
					-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
					override = {
						["vim.lsp.util.convert_input_to_markdown_lines"] = true,
						["vim.lsp.util.stylize_markdown"] = true,
						["cmp.entry.get_documentation"] = true,
					},
				},
				-- you can enable a preset for easier configuration
				presets = {
					bottom_search = false, -- use a classic bottom cmdline for search
					command_palette = true, -- position the cmdline and popupmenu together
					long_message_to_split = true, -- long messages will be sent to a split
					inc_rename = true, -- enables an input dialog for inc-rename.nvim
					lsp_doc_border = false, -- add a border to hover docs and signature help
				},
			})
		end,
	},
}
