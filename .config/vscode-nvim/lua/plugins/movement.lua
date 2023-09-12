return	{
	{
		"ggandor/leap.nvim",
		require = "tpope/vim-repeat",
		config = function()
			require("leap").add_default_mappings()
		end,
	},
	{
		"ggandor/flit.nvim",
		config = function()
			require("flit").setup({
				keys = { f = "f", F = "F", t = "t", T = "T" },
				labeled_modes = "v",
				multiline = false,
				opts = {},
			})
		end,
	},
	{
		"https://github.com/ggandor/leap-spooky.nvim.git",
		config = function()
			require("leap-spooky").setup({
				affixes = {
					-- These will generate mappings for all native text objects, like:
					-- (ir|ar|iR|ar|im|am|iM|aM){obj}.
					-- Special line objects will also be added, by repeating the affixes.
					-- E.g. `yrr<leap>` and `ymm<leap>` will yank a line in the current
					-- window.
					-- You can also use 'rest' & 'move' as mnemonics.
					remote = { window = "r", cross_window = "R" },
					magnetic = { window = "m", cross_window = "M" },
				},
				paste_on_remote_yank = true,
			})
		end,
	},
}

