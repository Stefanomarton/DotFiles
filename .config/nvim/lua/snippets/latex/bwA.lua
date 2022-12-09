local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local bwA = {
	s({ trig = "ali", name = "Align" }, { t({ "\\begin{align*}", "\t" }), i(1), t({ "", ".\\end{align*}" }) }),

	ls.parser.parse_snippet(
		{ trig = "nw", name = "begin{} / end{}" },
		"\\begin{$1}\n${2:${TM_SELECTED_TEXT}}\n\\end{$1}"
	),
	ls.parser.parse_snippet({ trig = "case", name = "cases" }, "\\begin{cases}\n\t$1\n\\end{cases}"),
}

return bwA
