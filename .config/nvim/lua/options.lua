vim.opt.backup = false -- creates a backup file
vim.opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
vim.opt.cmdheight = 1 -- more space in the neovim command line for displaying messages
-- vim.opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
vim.opt.conceallevel = 2 -- so that `` is visible in markdown files
vim.opt.fileencoding = "utf-8" -- the encoding written to a file
vim.opt.hlsearch = true -- highlight all matches on previous search pattern
vim.opt.ignorecase = true -- ignore case in search patterns
vim.opt.mouse = "a" -- allow the mouse to be used in Neovim
vim.opt.pumheight = 10 -- pop up menu height
vim.opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
-- vim.opt.colorcolumn = "80"
vim.opt.showtabline = 0 -- always show tabs
vim.opt.smartcase = true -- smart case
vim.opt.smartindent = true -- make indenting smarter again
vim.opt.splitbelow = true -- force all horizontal splits to go below current window
vim.opt.splitright = true -- force all vertical splits to go to the right of current window
vim.opt.swapfile = false -- creates a swapfile
vim.opt.termguicolors = true -- set term GUI colors (most terminals support this)
vim.opt.timeoutlen = 1000 -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.undofile = true -- enable persistent undo
vim.opt.updatetime = 300 -- faster completion (4000ms default)
vim.opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
vim.opt.expandtab = false -- convert tabs to spaces
vim.opt.shiftwidth = 4 -- the number of spaces inserted for each indentation
vim.opt.tabstop = 2 -- insert 2 spaces for a tab
vim.opt.cursorline = true -- highlight the current line
vim.opt.relativenumber = true -- set numbered lines
vim.opt.number = true -- set numbered line
vim.opt.laststatus = 3
vim.opt.showcmd = false
vim.opt.ruler = false
vim.opt.numberwidth = 4 -- set number column width to 2 {default 4}
vim.opt.signcolumn = "yes" -- always show the sign column, otherwise it would shift the text each time
vim.opt.wrap = true -- display lines as one long line
vim.opt.scrolloff = 12 -- is one of my fav
vim.opt.sidescrolloff = 12
vim.opt.guifont = "JetBrainsMono Nerd Font:h17" -- the font used in graphical neovim applications
vim.opt.fillchars.eob = " "
vim.opt.shortmess:append("c")
vim.opt.whichwrap:append("<,>,[,],h,l")
vim.opt.iskeyword:append("-")
vim.opt.spell = true
vim.opt.spelllang = { "en_us", "it" }
vim.opt.shell = "/bin/bash"
vim.opt.lazyredraw = false
vim.opt.shadafile = "NONE"
vim.opt.fillchars = { eob = " " } -- Remove useless tilde at the end of the file

-- TreeSitter context background color
--vim.api.nvim_set_hl(0, "TreesitterContext", { fg = "#ffffff", bg = "#414868" })

-- GUI settings for Neovide
vim.g.neovide_scale_factor = 0.8
vim.g.neovide_refresh_rate = 144

-- Colorscheme
-- vim.api.nvim_set_hl(0, 'CursorLine', { underline = false})
-- vim.cmd('highlight Normal guibg=NONE')
-- vim.cmd('highlight CursorLineNr guibg=NONE')
-- vim.cmd('highlight CursorLine guifg=NONE')
-- vim.cmd('highlight LineNr guibg=NONE')
-- vim.cmd('highlight SignColumn guibg=NONE')
-- vim.cmd('highlight NonText guibg=NONE')
-- vim.cmd('highlight EndOfBuffer guibg=NONE')
-- vim.cmd('highlight TabLine guibg=NONE')
-- vim.cmd('highlight TabLineFill guibg=NONE')
-- vim.cmd('highlight TabLinelSel guibg=NONE')
-- vim.cmd('highlight IndentBlanklineIndent guibg=NONE')
-- vim.cmd('highlight Conceal guibg=NONE')
