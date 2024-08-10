local M = {}

local vscode = require('vscode')

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

keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

local function notify(cmd)
    return string.format("<cmd>call VSCodeNotify('%s')<CR>", cmd)
end


-- keymap('n', '<leader>ff', notify 'workbench.action.quickOpen', { silent = true }) -- find files
keymap('n', '<leader>', notify 'whichkey.show', { silent = true }) -- find files

return M
