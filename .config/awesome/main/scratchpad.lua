local bling = require("bling")

-- Ranger
term_scratch = bling.module.scratchpad {
  command                 = "kitty --class spad -e ranger",
  rule                    = { instance = "spad" },
  sticky                  = true,
  autoclose               = true,
  floating                = true,
  geometry                = { x = 1100, y = 280, height = 900, width = 1200 },
  dont_focus_before_close = true,
}

-- Terminal
term = bling.module.scratchpad {
  command                 = "kitty --class termscratch",
  rule                    = { instance = "termscratch" },
  sticky                  = true,
  autoclose               = true,
  floating                = true,
  geometry                = { x = 1100, y = 280, height = 900, width = 1200 },
  dont_focus_before_close = true,
}

-- Mixer
mixer = bling.module.scratchpad {
  command                 = "kitty --class mixer -e pulsemixer",
  rule                    = { instance = "termscratch" },
  sticky                  = true,
  autoclose               = true,
  floating                = true,
  geometry                = { x = 1100, y = 280, height = 900, width = 1200 },
  dont_focus_before_close = true,
}


galculator = bling.module.scratchpad {
  command                 = "galculator",
  rule                    = { instance = "galculator" },
  sticky                  = true,
  autoclose               = true,
  floating                = true,
  geometry                = { x = 1300, y = 350, height = 600, width = 900 },
  dont_focus_before_close = true,
}


ytm = bling.module.scratchpad {
  command                 = "youtube-music",
  rule                    = { instance = "youtube music" },
  sticky                  = true,
  autoclose               = true,
  floating                = true,
  geometry                = { x = 1300, y = 350, height = 600, width = 900 },
  dont_focus_before_close = true,
}
