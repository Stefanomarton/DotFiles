local bling = require("module.bling")
local rubato = require("module.rubato") -- Totally optional, only required if you are using animations.

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
	geometry = { x = 1100, y = 280, height = 900, width = 1200 },
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})

-- Terminal
music = bling.module.scratchpad({
	command = "kitty --class ncspot -e ncspot",
	rule = { instance = "ncspot" },
	sticky = true,
	autoclose = true,
	floating = true,
	geometry = { x = 1100, y = 280, height = 900, width = 1200 },
	dont_focus_before_close = true,
	-- rubato = { x = anim_x, y = anim_y },
})
