local bling = require("module.bling")
local rubato = require("module.rubato") -- Totally optional, only required if you are using animations.
local awful = require("awful")

local function centered_gemotery(width, height)
	width = width or awful.screen.focused().geometry.width * 0.6
	height = height or awful.screen.focused().geometry.height * 0.9

	return {
		x = (awful.screen.focused().geometry.width / 2) - (width / 2),
		y = (awful.screen.focused().geometry.height / 2) - (height / 2),
		width = width,
		height = height,
	}
end

-- These are example rubato tables. You can use one for just y, just x, or both.
-- The duration and easing is up to you. Please check out the rubato docs to learn more.
local anim_y = rubato.timed({
	pos = 500,
	rate = 144,
	easing = rubato.quadratic,
	intro = 0.1,
	duration = 0.3,
	awestore_compat = true, -- This option must be set to true.
})

local anim_x = rubato.timed({
	pos = -500,
	rate = 144,
	easing = rubato.quadratic,
	intro = 0.1,
	duration = 0.3,
	awestore_compat = true, -- This option must be set to true.
})

-- Ranger
ranger = bling.module.scratchpad({
	command = "kitty --class ranger -e ranger",
	rule = { instance = "ranger" },
	sticky = true,
	autoclose = true,
	floating = true,
	geometry = centered_gemotery(),
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})

-- ncspot
music = bling.module.scratchpad({
	command = "kitty --class ncspot -e ncspot",
	rule = { instance = "ncspot" },
	sticky = true,
	autoclose = true,
	floating = true,
	geometry = centered_gemotery(),
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})

-- lazygit
lazygit = bling.module.scratchpad({
	command = "kitty --class lazygit -e lazygit",
	rule = { instance = "lazygit" },
	sticky = true,
	autoclose = true,
	floating = true,
	geometry = centered_gemotery(),
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})

repo = bling.module.scratchpad({
	command = 'kitty --class rp -e lazygit --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"',
	rule = { instance = "rp" },
	sticky = true,
	autoclose = true,
	floating = true,
	geometry = centered_gemotery(),
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})

emacs = bling.module.scratchpad({
	command = 'emacs',
	rule = { instance = "emacs" },
	sticky = false,
	autoclose = false,
	floating = false,
	geometry = centered_gemotery(),
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})
