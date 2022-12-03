-- Keymaps for latex only files
local keymap = vim.keymap.set
local wk = require("which-key") -- requiring which-key plugin to create keybinding

-- Silent keymap option
local opts = { silent = true, noremap = true }

wk.register({
	-- Open PDF preview with zathura
	p = {
		":!zathura %:r.pdf > /dev/null 2>&1 &<cr><cr>",
		"PDF view of the current file",
		opts,
	},
	-- Show list of headings with Telescope integrations
	s = { ":Telescope heading <cr>", "Table of contents of the current file", opts },
	-- Show list of available luasnip
	l = { ":Telescope luasnip theme=get_cursor<cr>", "Available snippets", opts },
}, { prefix = "<Space>", mode = "n" })

-- Delete env, cmd, math and delimiters
wk.register({
	d = {
		name = "Delete",
		e = { "<plug>(vimtex-env-delete)", "Delete enviroment", opts },
		c = { "<plug>(vimtex-cmd-delete)", "Delete command", opts },
		m = { "<plug>(vimtex-env-delete-math)", "Delete Math ", opts },
		d = { "<plug>(vimtex-delim-delete)", "Delete delimiter", opts },
	},
}, { prefix = "<leader>", mode = "n" })

-- Change env, cmd, math and delimiters
wk.register({
	c = {
		name = "Change",
		e = { "<plug>(vimtex-env-change)", "Change enviroment", opts },
		c = { "<plug>(vimtex-cmd-change)", "Change command", opts },
		m = { "<plug>(vimtex-env-change-math)", "Change Math ", opts },
		d = { "<plug>(vimtex-delim-change)", "Change delimiter", opts },
	},
}, { prefix = "<leader>"})

-- Toggle env, cmd, math and delimiters
wk.register({
	t = {
		name = "Toggle",
		f = { "<plug>(vimtex-cmd-toggle-frac)", "Toggle enviroment", opts },
		c = { "<plug>(vimtex-cmd-toggle-star)", "Toggle command", opts },
		e = { "<plug>(vimtex-env-toggle-star)", "Toggle Math ", opts },
		m = { "<plug>(vimtex-env-toggle-math)", "Toggle delimiter", opts },
	},
})

-- Surround current enviroment with a new enviroment
wk.register({
	a = {
		name = "Surround",
		e = { "v<plug>(vimtex-ae) <plug>(vimtex-env-surround-visual)", "Surround enviroment", opts },
	},
}	,{ prefix = "<leader>", mode = "v" })

-- Surround current line with a new enviroment
wk.register({
	a = {
		name = "Surround",
		l = { "<plug>(vimtex-env-surround-line)", "Surround line  command", opts },
	},
		},{ prefix = "<leader>", mode = "n" }
)

keymap("n", "vam", "v<plug>(vimtex-a$)", opts) -- Visual select (a)inside math
keymap("n", "vim", "v<plug>(vimtex-i$)", opts) -- Visual select inside math
keymap("n", ",", "<plug>(vimtex-%)", opts) -- Move between delimiters
keymap("n", ";", "<plug>(vimtex-doc-package)", opts) -- Move between delimiters

vim.cmd([[imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>']])

--[[ other useful command  ]]
-- ac               |<plug>(vimtex-ac)| aside command
-- ic               |<plug>(vimtex-ic)| iside command
-- ad               |<plug>(vimtex-ad)| aside delimiters
-- id               |<plug>(vimtex-id)| iside command
-- ae               |<plug>(vimtex-ae)| aside env
-- ie               |<plug>(vimtex-ie)| iside command
-- aP               |<plug>(vimtex-aP)| aside paragraph
-- iP               |<plug>(vimtex-iP)| iside paragraph
-- ]]               |<plug>(vimtex-delim-close)|
-- <F8>             |<plug>(vimtex-delim-add-modifiers)|
-- am               |<plug>(vimtex-am)|
-- im               |<plug>(vimtex-im)|
-- ]]               |<plug>(vimtex-]])| Next heading
-- ][               |<plug>(vimtex-][)|
-- []               |<plug>(vimtex-[])|
-- [[               |<plug>(vimtex-[[)|
