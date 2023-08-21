set background=dark
if exists('g:colors_name')
hi clear
if exists('syntax_on')
syntax reset
endif
endif
let g:colors_name = 'lushwal'
highlight Normal guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight! link User Normal
highlight Bold guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=bold
highlight Boolean guifg=#74789A guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Character guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight CmpItemAbbr guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight CmpItemAbbrDeprecated guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=strikethrough
highlight CmpItemAbbrMatch guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight CmpItemAbbrMatchFuzzy guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight CmpItemKind guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight CmpItemMenu guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight ColorColumn guifg=#696A6D guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Comment guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=italic
highlight Conceal guifg=#464749 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight! link Whitespace Conceal
highlight Conditional guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Constant guifg=#74789A guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Cursor guifg=#090A0C guibg=#C1C1C2 guisp=NONE blend=NONE gui=NONE
highlight CursorColumn guifg=#696A6D guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight CursorLine guifg=#464749 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight CursorLineNr guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Debug guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Define guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Delimiter guifg=#6A7CA0 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight DiagnosticError guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight DiagnosticHint guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight DiagnosticInfo guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight DiagnosticUnderlineError guifg=#68799C guibg=NONE guisp=#68799C blend=NONE gui=underline
highlight DiagnosticUnderlineHint guifg=#7BA2C1 guibg=NONE guisp=#7BA2C1 blend=NONE gui=underline
highlight DiagnosticUnderlineInfo guifg=#5D81AC guibg=NONE guisp=#5D81AC blend=NONE gui=underline
highlight DiagnosticUnderlineWarn guifg=#847C98 guibg=NONE guisp=#847C98 blend=NONE gui=underline
highlight DiagnosticWarn guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight DiffAdd guifg=#5C7CA3 guibg=#464749 guisp=NONE blend=NONE gui=bold
highlight! link DiffAdded DiffAdd
highlight DiffChange guifg=#959697 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight DiffDelete guifg=#68799C guibg=#464749 guisp=NONE blend=NONE gui=bold
highlight! link DiffRemoved DiffDelete
highlight! link diffRemoved DiffDelete
highlight DiffFile guifg=#68799C guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight DiffLine guifg=#5D81AC guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight DiffNewFile guifg=#5C7CA3 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight DiffText guifg=#5D81AC guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight Directory guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight EndOfBuffer guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Error guifg=#68799C guibg=#C1C1C2 guisp=NONE blend=NONE gui=NONE
highlight ErrorMsg guifg=#68799C guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Exception guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Float guifg=#74789A guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight FoldColumn guifg=#5D81AC guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Folded guifg=#C1C1C2 guibg=#464749 guisp=NONE blend=NONE gui=italic
highlight Function guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight GitSignsAdd guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight GitSignsChange guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight GitSignsDelete guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Identifier guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight IncSearch guifg=#464749 guibg=#74789A guisp=NONE blend=NONE gui=NONE
highlight Include guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight IndentBlanklineChar guifg=#464749 guibg=NONE guisp=NONE blend=NONE gui=nocombine
highlight IndentBlanklineContextChar guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=nocombine
highlight IndentBlanklineContextStart guifg=NONE guibg=NONE guisp=#C1C1C2 blend=NONE gui=underline
highlight Italic guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=italic
highlight Keyword guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Label guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight LineNr guifg=#959697 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Macro guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight MatchParen guifg=#C1C1C2 guibg=#959697 guisp=NONE blend=NONE gui=NONE
highlight ModeMsg guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight MoreMsg guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight NonText guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Number guifg=#74789A guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Operator guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight PMenu guifg=#C1C1C2 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight PMenuSel guifg=#C1C1C2 guibg=#5D81AC guisp=NONE blend=NONE gui=NONE
highlight PmenuSbar guifg=#696A6D guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight PmenuThumb guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight PreProc guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Question guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Repeat guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Search guifg=#959697 guibg=#847C98 guisp=NONE blend=NONE gui=NONE
highlight SignColumn guifg=#696A6D guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight Special guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight SpecialChar guifg=#6A7CA0 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight SpecialKey guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight SpellBad guifg=#68799C guibg=NONE guisp=#68799C blend=NONE gui=underline
highlight SpellCap guifg=#847C98 guibg=NONE guisp=#847C98 blend=NONE gui=underline
highlight SpellLocal guifg=#7BA2C1 guibg=NONE guisp=#7BA2C1 blend=NONE gui=underline
highlight SpellRare guifg=#C2A7D2 guibg=NONE guisp=#C2A7D2 blend=NONE gui=underline
highlight Statement guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight StatusLine guifg=#C1C1C2 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight StatusLineNC guifg=#696A6D guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight StatusLineTerm guifg=#5C7CA3 guibg=#5C7CA3 guisp=NONE blend=NONE gui=NONE
highlight StatusLineTermNC guifg=#847C98 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight StorageClass guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight String guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Structure guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TabLine guifg=#959697 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight TabLineFill guifg=#959697 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight TabLineSel guifg=#5C7CA3 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight Tag guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TelescopeBorder guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TelescopeMatching guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TelescopeMultiSelection guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight TelescopePromptBorder guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TelescopePromptCounter guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight TelescopeSelection guifg=#7BA2C1 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight TelescopeSelectionCaret guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Title guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=bold
highlight Todo guifg=#847C98 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight TooLong guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Type guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Typedef guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight Underlined guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight VertSplit guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight! link WinSeparator VertSplit
highlight Visual guifg=#090A0C guibg=#696A6D guisp=NONE blend=NONE gui=NONE
highlight VisualNOS guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WarningMsg guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKey guifg=#9DB0C8 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKeyDesc guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKeyFloat guifg=NONE guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight WhichKeyGroup guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKeySeparator guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKeySeperator guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WhichKeyValue guifg=#959697 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight WildMenu guifg=#C1C1C2 guibg=#5D81AC guisp=NONE blend=NONE gui=NONE
highlight WinBar guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight WinBarNC guifg=#696A6D guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight gitCommitOverflow guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight gitCommitSummary guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight helpCommand guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight helpExample guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight mkdBold guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight! link markdownBold mkdBold
highlight mkdCode guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight! link markdownCode mkdCode
highlight mkdCodeBlock guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight! link markdownCodeBlock mkdCodeBlock
highlight mkdCodeDelimiter guifg=#6A7CA0 guibg=NONE guisp=NONE blend=NONE gui=italic
highlight! link markdownCodeDelimiter mkdCodeDelimiter
highlight mkdError guifg=#C1C1C2 guibg=#090A0C guisp=NONE blend=NONE gui=NONE
highlight! link markdownError mkdError
highlight mkdH1 guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=bold
highlight! link markdownH1 mkdH1
highlight mkdH2 guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=bold
highlight! link markdownH2 mkdH2
highlight mkdHeadingDelimiter guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight! link markdownHeadingDelimiter mkdHeadingDelimiter
highlight mkdItalic guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=italic
highlight! link markdownItalic mkdItalic
highlight @attribute guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @boolean guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @character guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @character.special guifg=#6A7CA0 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @comment guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=italic
highlight @conditional guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @constant guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @constant.builtin guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @constant.macro guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @constructor guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @debug guifg=NONE guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @define guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @exception guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @field guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @float guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @function guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @function.builtin guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @function.macro guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @include guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @keyword guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @keyword.function guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @keyword.operator guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @label guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @method guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @namespace guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @none guifg=NONE guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @number guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @operator guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @parameter guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @preproc guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @property guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @punctuation.bracket guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @punctuation.delimiter guifg=#C1C1C2 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @punctuation.special guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight @repeat guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @storageclass guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @string guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @string.escape guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @string.regex guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @string.special guifg=#6A7CA0 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @symbol guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @tag guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @tag.attribute guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @tag.delimiter guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.bold guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=bold
highlight @text.danger guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.diff.add guifg=#5C7CA3 guibg=#464749 guisp=NONE blend=NONE gui=bold
highlight @text.diff.delete guifg=#68799C guibg=#464749 guisp=NONE blend=NONE gui=bold
highlight @text.emphasis guifg=#C2A7D2 guibg=NONE guisp=NONE blend=NONE gui=italic
highlight @text.environment guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.environment.name guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.literal guifg=#5C7CA3 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.math guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.note guifg=#7BA2C1 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.reference guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @text.strike guifg=NONE guibg=NONE guisp=NONE blend=NONE gui=strikethrough
highlight @text.title guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=bold
highlight @text.todo guifg=#847C98 guibg=#464749 guisp=NONE blend=NONE gui=NONE
highlight @text.underline guifg=NONE guibg=NONE guisp=NONE blend=NONE gui=underline
highlight @text.uri guifg=NONE guibg=#464749 guisp=NONE blend=NONE gui=underline
highlight @type guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @type.builtin guifg=#5D81AC guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @type.definition guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @variable guifg=#847C98 guibg=NONE guisp=NONE blend=NONE gui=NONE
highlight @variable.builtin guifg=#68799C guibg=NONE guisp=NONE blend=NONE gui=NONE
