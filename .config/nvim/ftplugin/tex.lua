-- Keymaps for latex only files
local wk = require("which-key") -- requiring which-key plugin to create keybinding
wk.register({
	-- c = { ":VimtexCompile<cr>", "Compile", noremap = true, silent = true },
	p = { ":!zathura %:r.pdf > /dev/null 2>&1 &<cr><cr>", "Preview", noremap = true, silent = true },
	s = { ":Telescope heading <cr>", "Toc", noremap = true, silent = true },
	l = { ":Telescope luasnip theme=get_cursor<cr>", "Snippets", noremap = true, silent = true },
	r = { ":<F6><cr>", "Snippets", noremap = true, silent = true },
}, { prefix = "<leader>" })
