-- Shorten function name
local keymap = vim.keymap.set

-- Silent keymap option
local opts = { silent = true, noremap = true }

-- Require WhichKey
local wk = require("which-key")

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --

-- Normal --

-- Faster movements
keymap("n", "K", "2k", opts)
keymap("n", "J", "2j", opts)
keymap("n", "q", "b", opts)
keymap("x", "q", "b", opts)
keymap("n", "Q", "B", opts)
keymap("x", "Q", "B", opts)

-- Delete
keymap("n", "dq", "db", opts)
keymap("n", "dQ", "dB", opts)

--Save and close
keymap("n", "<leader>w", ":w<cr>", opts)
keymap("n", "<leader>q", ":x<cr>", opts)

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
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)

-- Close buffers
keymap("n", "<C-q>", "<cmd>Bdelete!<CR>", opts)

-- Better paste
keymap("v", "p", '"_dP', opts)

-- Better search
keymap("n", "<leader>s", "V :s/")
keymap("v", "s", "V :s/")

-- Insert --

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Plugins --

-- NvimTree
keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

-- Git
-- keymap("n", "<leader>gg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", opts)

-- Comment
keymap("n", "<leader>/", "<cmd>lua require('Comment.api').toggle.linewise.current()<CR>", opts)
keymap("x", "<leader>/", '<ESC><CMD>lua require("Comment.api").toggle.linewise_op(vim.fn.visualmode())<CR>', opts)

-- -- DAP
-- keymap("n", "<leader>db", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", opts)
-- keymap("n", "<leader>dc", "<cmd>lua require'dap'.continue()<cr>", opts)
-- keymap("n", "<leader>di", "<cmd>lua require'dap'.step_into()<cr>", opts)
-- keymap("n", "<leader>do", "<cmd>lua require'dap'.step_over()<cr>", opts)
-- keymap("n", "<leader>dO", "<cmd>lua require'dap'.step_out()<cr>", opts)
-- keymap("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", opts)
-- keymap("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<cr>", opts)
-- keymap("n", "<leader>du", "<cmd>lua require'dapui'.toggle()<cr>", opts)
-- keymap("n", "<leader>dt", "<cmd>lua require'dap'.terminate()<cr>", opts)

--Latex
-- keymap("n", "<leader>c",
--   ":!pdflatex -synctex=1 -shell-escape %:r.tex && pdflatex -shell-escape %:r.tex && pdflatex -shell-escape %:r.tex && rm %:r.aux %:r.log %:r.blg %:r.bbl %:r.fls %:r.fdb_latexmk<cr><cr>"
--   , opts)

-- keymap("n", "<leader>p", ":!zathura %:r.pdf > /dev/null 2>&1 &<cr><cr>", opts)

-- Hop
keymap("x", "F", ":HopWord<Cr>", opts)
keymap("n", "m", ":HopChar1<Cr>", opts)


wk.register({
  f = {
    name = "Files", -- Create Group name
    f = { "<cmd>Telescope find_files<cr>", "Find File", noremap = true, silent = true },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File", noremap = true, silent = true },
    b = { "<cmd>Telescope buffers<cr>", "Open current buffers", noremap = true, silent = true },
    g = { "<cmd>Telescope live_grep<cr>", "Open live grep", noremap = true, silent = true },
  }
},
  { prefix = "<leader>" })

wk.register({
  l = {
    name = "LaTex",
    c = { ":VimtexCompile", "Compile", noremap = true, silent = true },
    p = { ":!zathura %:r.pdf > /dev/null 2>&1 &<cr><cr>", "Preview", noremap = true, silent = true },
    t = { ":VimtexTocToggle", "Toc", noremap = true, silent = true }
  }
},
  { prefix = "<leader>" })

keymap("n", "DD", '"_dd', opts)
keymap("v", "p", '"_p', opts)
keymap("v", "P", '"_P', opts)

keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
keymap("n", "gI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
keymap("n", "gl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
keymap("n", "<leader>pf", "<cmd>lua vim.lsp.buf.format({async= true})<cr>", opts)
keymap("n", "<leader>pi", "<cmd>LspInfo<cr>", opts)
keymap("n", "<leader>pI", "<cmd>LspInstallInfo<cr>", opts)
keymap("n", "<leader>pa", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
keymap("n", "<leader>pj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
keymap("n", "<leader>pk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
keymap("n", "<leader>pr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
keymap("n", "<leader>ps", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
keymap("n", "<leader>pq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
