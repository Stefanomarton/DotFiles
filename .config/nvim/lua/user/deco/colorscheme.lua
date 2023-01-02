function CScheme(color)
color = color or "nord"
vim.cmd.colorscheme(color)
vim.api.nvim_set_hl(20, "Normal", { bg = "none" })
vim.api.nvim_set_hl(10, "NormalFloat", { bg = "none" })
end

CScheme()
