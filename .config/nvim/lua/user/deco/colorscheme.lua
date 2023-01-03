function CScheme(color)
	color = color or "nord"
	vim.cmd.colorscheme(color)
	vim.api.nvim_set_hl(10, "Normal", { bg = "#2e3440" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#2e3440" })
end

CScheme()
