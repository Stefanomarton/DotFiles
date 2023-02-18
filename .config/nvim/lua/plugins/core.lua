return {
	{
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({
				check_ts = true, -- treesitter integration
				disable_filetype = { "TelescopePrompt" },
			})

			-- local cmp_autopairs = require("nvim-autopairs.completion.cmp")
			-- local cmp_status_ok, cmp = pcall(require, "cmp")
			-- if not cmp_status_ok then
			-- 	return
			-- end
			-- cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done({}))
		end,
	},
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup({
				---Add a space b/w comment and the line
				padding = true,
				---Whether the cursor should stay at its position
				sticky = true,
				---Lines to be ignored while (un)comment
				ignore = nil,
				---LHS of toggle mappings in NORMAL mode
				toggler = {
					---Line-comment toggle keymap
					line = "gcc",
					---Block-comment toggle keymap
					block = "gbc",
				},
				---LHS of operator-pending mappings in NORMAL and VISUAL mode
				opleader = {
					---Line-comment keymap
					line = "gc",
					---Block-comment keymap
					block = "gb",
				},
				---LHS of extra mappings
				extra = {
					---Add comment on the line above
					above = "gcO",
					---Add comment on the line below
					below = "gco",
					---Add comment at the end of line
					eol = "gcA",
				},
				---Enable keybindings
				mappings = {
					---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
					basic = true,
					---Extra mapping; `gco`, `gcO`, `gcA`
					extra = true,
				},
				---Function to call before (un)comment
				pre_hook = nil,
				---Function to call after (un)comment
				post_hook = nil,
			})
		end,
	},
	{
		"folke/todo-comments.nvim",
		config = function()
			require("todo-comments").setup({
				signs = true, -- show icons in the signs column
				sign_priority = 8, -- sign priority
				-- keywords recognized as todo comments
				keywords = {
					FIX = {
						icon = " ", -- icon used for the sign, and in search results
						color = "error", -- can be a hex color, or a named color (see below)
						alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
						-- signs = false, -- configure signs for some keywords individually
					},
					TODO = { icon = " ", color = "warning" },
					HACK = { icon = " ", color = "warning" },
					WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
					PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
					NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
					TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
				},
				gui_style = {
					fg = "NONE", -- The gui style to use for the fg highlight group.
					bg = "BOLD", -- The gui style to use for the bg highlight group.
				},
				merge_keywords = true, -- when true, custom keywords will be merged with the defaults
				-- highlighting of the line containing the todo comment
				-- * before: highlights before the keyword (typically comment characters)
				-- * keyword: highlights of the keyword
				-- * after: highlights after the keyword (todo text)
				highlight = {
					multiline = true, -- enable multine todo comments
					multiline_pattern = "^.", -- lua pattern to match the next multiline from the start of the matched keyword
					multiline_context = 10, -- extra lines that will be re-evaluated when changing a line
					before = "", -- "fg" or "bg" or empty
					keyword = "wide", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty. (wide and wide_bg is the same as bg, but will also highlight surrounding characters, wide_fg acts accordingly but with fg)
					after = "fg", -- "fg" or "bg" or empty
					pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
					comments_only = true, -- uses treesitter to match keywords in comments only
					max_line_len = 400, -- ignore lines longer than this
					exclude = {}, -- list of file types to exclude highlighting
				},
				-- list of named colors where we try to extract the guifg from the
				-- list of highlight groups or use the hex color if hl not found as a fallback
				colors = {
					error = { "DiagnosticError", "ErrorMsg", "#DC2626" },
					warning = { "DiagnosticWarn", "WarningMsg", "#FBBF24" },
					info = { "DiagnosticInfo", "#2563EB" },
					hint = { "DiagnosticHint", "#10B981" },
					default = { "Identifier", "#7C3AED" },
					test = { "Identifier", "#FF00FF" },
				},
				search = {
					command = "rg",
					args = {
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--line-number",
						"--column",
					},
					-- regex that will be used to match keywords.
					-- don't replace the (KEYWORDS) placeholder
					pattern = [[\b(KEYWORDS)]], -- ripgrep regex
					-- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
				},
			})
		end,
	},
	{

		"abecodes/tabout.nvim",
		config = function()
			require("tabout").setup({
				tabkey = "<Tab>", -- key to trigger tabout, set to an empty string to disable
				backwards_tabkey = "<S-Tab>", -- key to trigger backwards tabout, set to an empty string to disable
				act_as_tab = false, -- shift content if tab out is not possible
				act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
				default_tab = "<C-t>", -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
				default_shift_tab = "<C-d>", -- reverse shift default action,
				enable_backwards = true, -- well ...
				completion = false, -- if the tabkey is used in a completion pum
				tabouts = {
					{ open = "'", close = "'" },
					{ open = '"', close = '"' },
					{ open = "`", close = "`" },
					{ open = "(", close = ")" },
					{ open = "[", close = "]" },
					{ open = "{", close = "}" },
					{ open = "<", close = ">" },
				},
				ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
				exclude = {}, -- tabout will ignore these filetypes
			})
		end,
	},
	{ "NvChad/nvim-colorizer.lua" },
	{
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({
				plugins = {
					marks = true, -- shows a list of your marks on ' and `
					registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
					spelling = {
						enabled = false, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
						suggestions = 20, -- how many suggestions should be shown in the list?
					},
					-- the presets plugin, adds help for a bunch of default keybindings in Neovim
					-- No actual key bindings are created
					presets = {
						operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
						motions = true, -- adds help for motions
						text_objects = true, -- help for text objects triggered after entering an operator
						windows = true, -- default bindings on <c-w>
						nav = true, -- misc bindings to work with windows
						z = true, -- bindings for folds, spelling and others prefixed with z
						g = true, -- bindings for prefixed with g
					},
				},
				-- add operators that will trigger motion and text object completion
				-- to enable all native operators, set the preset / operators plugin above
				operators = { gc = "Comments" },
				key_labels = {
					-- override the label used to display some keys. It doesn't effect WK in any other way.
					-- For example:
					-- ["<space>"] = "SPC",
					-- ["<cr>"] = "RET",
					-- ["<tab>"] = "TAB",
				},
				icons = {
					breadcrumb = "", -- symbol used in the command line area that shows your active key combo
					separator = "", -- symbol used between a key and its label
					group = "+", -- symbol prepended to a grouplocal
				},
				popup_mappings = {
					scroll_down = "<c-d>", -- binding to scroll down inside the popup
					scroll_up = "<c-u>", -- binding to scroll up inside the popup
				},
				window = {
					border = "none", -- none, single, double, shadow
					position = "bottom", -- bottom, top
					margin = { 0, 0, 1.5, 0 }, -- extra window margin [top, right, bottom, left]
					padding = { 0, 0, 3, 0 }, -- extra window padding [top, right, bottom, left]
					winblend = 5,
				},
				layout = {
					height = { min = 4, max = 25 }, -- min and max height of the columns
					width = { min = 20, max = 50 }, -- min and max width of the columns
					spacing = 3, -- spacing between columns
					align = "center", -- align columns left, center or right
				},
				ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
				hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
				show_help = false, -- show help message on the command line when the popup is visible
				triggers = "auto", -- automatically setup triggers
				-- triggers = {"<leader>"} -- or specify a list manually
				triggers_blacklist = {
					-- list of mode / prefixes that should never be hooked by WhichKey
					-- this is mostly relevant for key maps that start with a native binding
					-- most people should not need to change this
					i = { "j", "k" },
					v = { "j", "k" },
				},
				-- disable the WhichKey popup for certain buf types and file types.
				-- Disabled by deafult for Telescope
				disable = {
					buftypes = {},
					filetypes = { "TelescopePrompt" },
				},
			})
		end,
	},
	{
		"kdheepak/lazygit.nvim",
	},
	{
		"ggandor/flit.nvim",
		config = function()
			require("flit").setup({
				keys = { f = "f", F = "F", t = "t", T = "T" },
				labeled_modes = "v",
				multiline = false,
				opts = {},
			})
		end,
	},
	{
		"https://github.com/ggandor/leap-spooky.nvim.git",
		config = function()
			require("leap-spooky").setup({
				affixes = {
					-- These will generate mappings for all native text objects, like:
					-- (ir|ar|iR|ar|im|am|iM|aM){obj}.
					-- Special line objects will also be added, by repeating the affixes.
					-- E.g. `yrr<leap>` and `ymm<leap>` will yank a line in the current
					-- window.
					-- You can also use 'rest' & 'move' as mnemonics.
					remote = { window = "r", cross_window = "R" },
					magnetic = { window = "m", cross_window = "M" },
				},
				paste_on_remote_yank = true,
			})
		end,
	},
	{
		"ggandor/leap.nvim",
		require = "tpope/vim-repeat",
		config = function()
			require("leap").add_default_mappings()
		end,
	},
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
		end,
	},
	{
		"kevinhwang91/nvim-hlslens",
		config = function()
			require("hlslens").setup()
		end,
	},
	{ "L3MON4D3/LuaSnip" },
	{
		"tamago324/lir.nvim",
		config = function()
			local actions = require("lir.actions")
			local mark_actions = require("lir.mark.actions")
			local clipboard_actions = require("lir.clipboard.actions")

			require("lir").setup({
				show_hidden_files = false,
				ignore = {}, -- { ".DS_Store", "node_modules" } etc.
				devicons = {
					enable = false,
					highlight_dirname = false,
				},
				mappings = {
					["l"] = actions.edit,
					["<C-s>"] = actions.split,
					["<C-v>"] = actions.vsplit,
					["<C-t>"] = actions.tabedit,

					["h"] = actions.up,
					["q"] = actions.quit,

					["K"] = actions.mkdir,
					["N"] = actions.newfile,
					["R"] = actions.rename,
					["@"] = actions.cd,
					["Y"] = actions.yank_path,
					["."] = actions.toggle_show_hidden,
					["D"] = actions.delete,

					["J"] = function()
						mark_actions.toggle_mark()
						vim.cmd("normal! j")
					end,
					["C"] = clipboard_actions.copy,
					["X"] = clipboard_actions.cut,
					["P"] = clipboard_actions.paste,
				},
				float = {
					winblend = 0,
					curdir_window = {
						enable = false,
						highlight_dirname = false,
					},

					-- -- You can define a function that returns a table to be passed as the third
					-- -- argument of nvim_open_win().
					-- win_opts = function()
					--   local width = math.floor(vim.o.columns * 0.8)
					--   local height = math.floor(vim.o.lines * 0.8)
					--   return {
					--     border = {
					--       "+", "─", "+", "│", "+", "─", "+", "│",
					--     },
					--     width = width,
					--     height = height,
					--     row = 1,
					--     col = math.floor((vim.o.columns - width) / 2),
					--   }
					-- end,
				},
				hide_cursor = true,
			})

			vim.api.nvim_create_autocmd({ "FileType" }, {
				pattern = { "lir" },
				callback = function()
					-- use visual mode
					vim.api.nvim_buf_set_keymap(
						0,
						"x",
						"J",
						':<C-u>lua require"lir.mark.actions".toggle_mark("v")<CR>',
						{ noremap = true, silent = true }
					)

					-- echo cwd
					vim.api.nvim_echo({ { vim.fn.expand("%:p"), "Normal" } }, false, {})
				end,
			})
		end,
	},
}
