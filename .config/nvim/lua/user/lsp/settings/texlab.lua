return {
	settings = {
		texlab = {
			rootDirectory = nil,
			-- bibtexFormatter = "texlab",
			build = {
				executable = "tectonic",
				args = { "-x", "compile" },
				forwardSearchAfter = false,
				onSave = false,
			},
			chktex = {
				onEdit = true,
				onOpenAndSave = true,
			},
			diagnosticsDelay = 300,
			formatterLineLength = 80,
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
