return {

	{
		"echasnovski/mini.surround",
		config = function()
			require("mini.surround").setup({
				custom_surroundings = nil,
				highlight_duration = 500,
				mappings = {
					add = ",",
					delete = ",d",
					find = "",
					find_left = "",
					highlight = "",
					replace = ",r",
					update_n_lines = "",
					suffix_last = "",
					suffix_next = "",
				},
				n_lines = 20,
				search_method = "cover",
			})
		end,
	},

	{
		"echasnovski/mini.ai",
		config = function()
			require("mini.ai").setup({
				custom_textobjects = nil,
				mappings = {
					around = "a",
					inside = "i",
					around_next = "an",
					inside_next = "in",
					around_last = "al",
					inside_last = "il",
					goto_left = "g[",
					goto_right = "g]",
				},
				n_lines = 50,
				search_method = "cover_or_next",
			})
		end,
	},
	{
		"echasnovski/mini.move",
		config = function()
			require("mini.move").setup( -- No need to copy this inside `setup()`. Will be used automatically.
				{
					mappings = {
						-- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
						left = "<M-h>",
						right = "<M-l>",
						down = "<M-j>",
						up = "<M-k>",

						-- Move current line in Normal mode
						line_left = "<M-h>",
						line_right = "<M-l>",
						line_down = "<M-j>",
						line_up = "<M-k>",
					},
				}
			)
		end,
	},
}
