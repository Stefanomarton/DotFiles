return {
  texlab = {
    rootDirectory = nil,
    bibtexFormatter = "texlab",
    build = {
      args = { "-pdf", "-c", "-interaction=nonstopmode", "-synctex=1", "%f" },
      executable = "latexmk",
      forwardSearchAfter = true,
      onSave = true
    },
    chktex = {
      onEdit = false,
      onOpenAndSave = true
    },
    diagnosticsDelay = 300,
    formatterLineLength = 80,
    forwardSearch = {
      args = {}
    },
    latexFormatter = "latexindent",
    latexindent = {
      modifyLineBreaks = true
    }
  }
}
