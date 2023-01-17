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
-- vim.opt.listchars:append("space:⋅")
