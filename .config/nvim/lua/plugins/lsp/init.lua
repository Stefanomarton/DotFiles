return {
	{
		"williamboman/mason.nvim",
		config = function()
			require("plugins.lsp.mason")
			require("plugins.lsp.handlers").setup()
		end,
	},
	{
		"jose-elias-alvarez/null-ls.nvim",
		config = function()
			require("plugins.lsp.null-ls")
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
	},
}
