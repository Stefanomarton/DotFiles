-- Shorten function name
local keymap = vim.keymap.set

-- Silent keymap option
local opts = { silent = true, noremap = true }

-- Require WhichKey
local wk = require("which-key")

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Normal --
-- Faster movements
keymap("n", "q", "b", opts)
keymap("n", "b", "q", opts)
keymap("x", "q", "b", opts)
keymap("n", "Q", "B", opts)
keymap("x", "Q", "B", opts)
keymap("n", "1", "$", opts)

keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")

-- keymap("n", "<C-d>", "<C-D>zz")
-- keymap("n", "<C-u>", "<C-u>zz")
keymap("n", "J", "mzJ`z") -- The cursor remain in the same position
keymap("n", "n", "nzzzv")
keymap("n", "N", "Nzzzv")

-- Delete
keymap("n", "dq", "db", opts)
keymap("n", "dQ", "dB", opts)
keymap("x", "<leader>d", '"_d', opts)

-- Paste
keymap("x", "<leader>p", '"_dP', opts)

--Save and close
keymap("n", "<C-x><C-s>", ":w<cr>", opts)
keymap("n", "<leader>q", "<cmd>BufferClose<CR>", opts)

-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Clear highlights
keymap("n", "<esc>", "<cmd>nohlsearch<CR>", opts)

-- Close buffers
keymap("n", "<leader>qa", ":x<cr>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

wk.register({
	e = { ":Telescope file_browser<CR>", "File explorer", opts },
}, { prefix = "<leader>" })

wk.register({
	b = { ":Telescope buffers<CR>", "Telescope buffer", opts },
}, { prefix = "<leader>" })

wk.register({
	f = {
		name = "Files", -- Create Group name
		f = { "<cmd>Telescope find_files<cr>", "Find File", noremap = true, silent = true },
		r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File", noremap = true, silent = true },
		b = { "<cmd>Telescope buffers<cr>", "Open current buffers", noremap = true, silent = true },
		g = { "<cmd>Telescope live_grep<cr>", "Open live grep", noremap = true, silent = true },
	},
}, { prefix = "<leader>" })

wk.register({
	g = { ":LazyGitCurrentFile<cr>", "Open live grep", opts },
}, { prefix = "<leader>" })

keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
keymap("n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
keymap("n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
keymap("n", "<leader>pf", "<cmd>lua vim.lsp.buf.format({async= true})<cr>", opts)
keymap("n", "<leader>pi", "<cmd>LspInfo<cr>", opts)
keymap("n", "<leader>pI", "<cmd>Mason<cr>", opts)
keymap("n", "<leader>pa", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
keymap("n", "<leader>pj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
keymap("n", "<leader>pk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
keymap("n", "<leader>pr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
-- keymap("n", "<leader>ps", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
keymap("n", "<leader>pq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

vim.api.nvim_set_keymap("n", "*", [[*<Cmd>lua require('hlslens').start()<CR>]], opts)
vim.api.nvim_set_keymap("n", "#", [[#<Cmd>lua require('hlslens').start()<CR>]], opts)

vim.keymap.set("n", "s", function()
	local current_window = vim.fn.win_getid()
	require("leap").leap({ target_windows = { current_window } })
end)
