local ls = require("luasnip")
local conds = require("luasnip.extras.expand_conditions")
local utils = require("snippets.latex.util.utils")
local pipe = utils.pipe
local no_backslash = utils.no_backslash
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require("luasnip.util.types")
local conds_expand = require("luasnip.extras.conditions.expand")

local M = {}

local default_opts = {
	use_treesitter = true,
}

M.setup = function(opts)
	opts = vim.tbl_deep_extend("force", default_opts, opts or {})

	local is_math = utils.with_opts(utils.is_math, opts.use_treesitter)
	local not_math = utils.with_opts(utils.not_math, opts.use_treesitter)

	ls.config.setup({ enable_autosnippets = true })

	ls.add_snippets("tex", {
		ls.parser.parse_snippet({ trig = "pac", name = "Package" }, "\\usepackage[]{${1:package}}$0"),
		ls.parser.parse_snippet({ trig = ";s", name = "Section" }, "\\section{${1}}"),
		ls.parser.parse_snippet({ trig = ";ss", name = "Subsection" }, "\\subsection{${1}}"),
		ls.parser.parse_snippet({ trig = ";sss", name = "Subsubsection" }, "\\subsubsection{${1}}"),
	})

	local math_i = require("snippets.latex.math_i")
	for _, snip in ipairs(math_i) do
		snip.condition = pipe({ is_math })
		snip.show_condition = is_math
		snip.wordTrig = false
	end

	ls.add_snippets("tex", math_i, { default_priority = 0 })

	local autosnippets = {}

	for _, snip in ipairs(require("snippets.latex.math_wRA_no_backslash")) do
		snip.regTrig = true
		snip.condition = pipe({ is_math, no_backslash })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex.math_rA_no_backslash")) do
		snip.wordTrig = false
		snip.regTrig = true
		snip.condition = pipe({ is_math, no_backslash })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex.normal_wA")) do
		snip.condition = pipe({ not_math })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex.math_wrA")) do
		snip.regTrig = true
		snip.condition = pipe({ is_math })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex.math_wA_no_backslash")) do
		snip.condition = pipe({ is_math, no_backslash })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex/math_iA")) do
		snip.wordTrig = false
		snip.condition = pipe({ is_math })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex/math_iA_no_backslash")) do
		snip.wordTrig = false
		snip.condition = pipe({ is_math, no_backslash })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex/math_bwA")) do
		snip.condition = pipe({ conds.line_begin, is_math })
		table.insert(autosnippets, snip)
	end

	for _, snip in ipairs(require("snippets.latex/bwA")) do
		snip.condition = pipe({ conds.line_begin, not_math })
		table.insert(autosnippets, snip)
	end

	ls.add_snippets("tex", autosnippets, {
		type = "autosnippets",
		default_priority = 0,
	})
end

ls.filetype_extend("markdown", { "tex" }) -- Addd tex snippets to markdown

-- Needed keyconfig for selection snippets to work
ls.config.set_config({
	store_selection_keys = "<leader>u",
})

return M
