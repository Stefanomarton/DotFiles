-- Vimtex
vim.g.vimtex_view_method = "zathura"
vim.g.livepreview_previewer = "zathura"
vim.g.vimtex_compiler_method = "tectonic"
vim.g.vimtex_compiler_enabled = 1
vim.g.vimtex_compiler_silent = 1
vim.g.vimtex_syntax_conceal = vim.cmd([[
    let g:vimtex_syntax_conceal = {
          \ 'accents': 1,
          \ 'ligatures': 1,
          \ 'cites': 1,
          \ 'fancy': 1,
          \ 'greek': 1,
          \ 'math_bounds': 1,
          \ 'math_delimiters': 1,
          \ 'math_fracs': 1,
          \ 'math_super_sub': 1,
          \ 'math_symbols': 1,
          \ 'sections': 0,
          \ 'styles': 1,
          \}

]])
-- vim.g.vimtex_default_mappings = 0

vim.g.tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]"
vim.g.tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]"
vim.g.tex_conceal = "abdgm"
