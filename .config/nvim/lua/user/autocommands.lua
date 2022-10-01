-- Use 'q' to quit from common plugins
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "qf", "help", "man", "lspinfo", "spectre_panel", "lir" },
  callback = function()
    vim.cmd [[
      nnoremap <silent> <buffer> q :close<CR> 
      set nobuflisted 
    ]]
  end,
})

-- Remove statusline and tabline when in Alpha
vim.api.nvim_create_autocmd({ "User" }, {
  pattern = { "AlphaReady" },
  callback = function()
    vim.cmd [[
      set showtabline=0 | autocmd BufUnload <buffer> set showtabline=2
      set laststatus=0 | autocmd BufUnload <buffer> set laststatus=3
    ]]
  end,
})

-- Set wrap and spell in markdown and gitcommit
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "gitcommit", "markdown" },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
  end,
})

-- NvimTree enter file directory automaticall
vim.cmd "autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif"

-- Fixes Autocomment
vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
  callback = function()
    vim.cmd "set formatoptions-=cro"
  end,
})

-- Highlight Yanked Text
vim.api.nvim_create_autocmd({ "TextYankPost" }, {
  callback = function()
    vim.highlight.on_yank { higroup = "Visual", timeout = 200 }
  end,
})

-- Latex compile on save
vim.cmd('autocmd BufWritePost *.tex !pdflatex -shell-escape %:r.tex && pdflatex -shell-escape %:r.tex && pdflatex -shell-escape %:r.tex && rm %:r.aux %:r.log %:r.blg %:r.bbl %:r.fls %:r.fdb_latexmk')

-- Autocd for NvimTree
vim.cmd([[autocmd BufEnter * if &ft != 'help' | silent! cd %:p:h | endif]])

-- Autoformat on save
vim.cmd('autocmd BufWritePre * lua vim.lsp.buf.format()')

-- Autocmd for Colorizer
vim.cmd('autocmd BufEnter * :ColorizerToggle')
