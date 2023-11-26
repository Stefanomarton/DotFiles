-- Shorten function name
local keymap = vim.keymap.set

-- Silent keymap option
local opts = { silent = true, noremap = true }

keymap("n", "h", "/", opts)

keymap("n", "/", "<right>", opts)
keymap("n", "l", "<up>", opts)
keymap("n", "k", "<down>", opts)
keymap("n", "j", "<left>", opts)

keymap("v", "/", "<right>", opts)
keymap("v", "l", "<up>", opts)
keymap("v", "k", "<down>", opts)
keymap("v", "j", "<left>", opts)