-- Vimtex
vim.g.vimtex_view_method = "zathura"
vim.g.livepreview_previewer = "zathura"
vim.g.vimtex_compiler_method = "tectonic"
vim.g.vimtex_compiler_enabled = 1
vim.g.vimtex_compiler_silent = 1
vim.vimtex_syntax_conceal_disable = 1
vim.g.vimtex_syntax_conceal = {
	accents = 1,
	ligatures = 1,
	cites = 1,
	fancy = 1,
	greek = 1,
	math_bounds = 1,
	math_delimiters = 1,
	math_fracs = 1,
	math_super_sub = 1,
	math_symbols = 1,
	sections = 0,
	styles = 1,
}
vim.g.vimtex_default_mappings = 0
vim.g.vimtex_format_enabled = 1
vim.gvimtex_complete_enabled = 1
vim.g.vimtex_doc_enabled = 1
vim.g.vimtex_syntax_enabled = 1
vim.g.vimtex_quickfix_enabled = 1
vim.g.vimtex_env_change_autofill = "gather*"

vim.g.vimtex_imaps_leader = ";"
vim.g.vimtex_imaps_enabled = 1
vim.g.tex_IgnoredWarnings = 1
--vim.g.vimtex_imaps_list = 1
-- vim.vimtex_log_ignore = 1
vim.g.vimtex_toc_show_preamble = 0
vim.cmd([[
" TOC settings
let g:vimtex_toc_config = {
      \ 'name' : 'TOC',
      "\ 'layers' : ['content', 'todo', 'include'],
      \ 'layers' : ['content'],
      \ 'resize' : 0,
      \ 'split_width' : 40,
      \ 'todo_sorted' : 0,
      \ 'show_help' : 0,
      \ 'show_numbers' : 1,
      \ 'mode' : 1,
      \}

let g:vimtex_toc_show_preamble = 0

]])

-- for TOC
--:call vimtex#fzf#run('c')
