return {
	settings = {
		texlab = {
			rootDirectory = nil,
			-- bibtexFormatter = "texlab",
			build = {
				executable = "tectonic",
				args = { "%f" },
				forwardSearchAfter = false,
				onSave = true,
			},
			chktex = {
				onEdit = true,
				onOpenAndSave = true,
			},
			diagnosticsDelay = 300,
			formatterLineLength = 100,
			forwardSearch = {
				args = {},
			},
			latexFormatter = "latexindent",
			latexindent = {
				modifyLineBreaks = true,
			},
		},
	},
}
