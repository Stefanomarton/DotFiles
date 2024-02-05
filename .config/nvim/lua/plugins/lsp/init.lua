return {
	{
		"williamboman/mason.nvim",
    config = function ()
		require("mason").setup()
    end
	},
	-- -- {
	-- -- 	"jose-elias-alvarez/null-ls.nvim",
	-- -- 	config = function()
	-- -- 		require("plugins.lsp.null-ls")
	-- -- 	end,
	-- -- },
	{
		"williamboman/mason-lspconfig.nvim",
    config = function ()
		require("mason-lspconfig").setup()
    end
	},
	{
    "neovim/nvim-lspconfig",
		config = function ()
-- Setup language servers.
local lspconfig = require('lspconfig')
lspconfig.pyright.setup {}
lspconfig.tsserver.setup {}
lspconfig.lua_ls.setup{}
lspconfig.rust_analyzer.setup {
  -- Server-specific settings. See `:help lspconfig-setup`
  settings = {
    ['rust-analyzer'] = {},
  },
}
end
	}
}
