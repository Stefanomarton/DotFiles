--[[ deco Configs ]]
--require("alpha").setup(require("user.deco.alpha").config)
require("user.deco.colorscheme")

--[[ Plugins ]]
require("user.plugins")

----[[ Base configs ]]
require("user.keymaps")
require("user.options")
require("user.autocommands")

----[[ Core plugins ]]
require("user.lsp")
require("user.cmp")
require("snippets.latex").setup({ use_treesitter = true })
require("user.vimtex")

--[[Confort plugins configs ]]
--require("user.whichkey")
