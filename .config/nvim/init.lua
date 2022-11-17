-- UI Configs
require("alpha").setup(require("user.UI.alpha").config)
require("user.UI.colorscheme")
require("user.UI.galaxyline")
require("user.UI.zen")
--require("user.UI.noice")
require("user.indentline")
require("user.bufferline")

-- Plugins
require("user.plugins")
require("user.impatient")

-- Base configs
require("user.keymaps")
require("user.options")
require("user.autocommands")

-- Core plugins
require("user.lsp")
require("user.cmp")
require("user.trouble")
require("user.treesitter")
require("luasnip-latex-snippets").setup({ use_treesitter = true })
require("user.treesitter-context")
require("user.vimtex")

-- Confort plugins configs
require("user.autopairs")
require("user.whichkey")
require("user.comment")
require("user.telescope")
