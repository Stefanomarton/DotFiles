-- Standard awesome library
local awful = require("awful")
local lain = require("lain")
local layout = require("main.layouts")

local _M = {}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

lain.layout.termfair.center.nmaster = 3

lain.layout.termfair.ncol = 1

function _M.get ()
  local tags = {}

awful.tag.add(" ", {
  layout            = awful.layout.suit.spiral,
  gap_single_client = true,
  gap               = 5,
  screen            = 1,
  selected          = true,
})

awful.tag.add(" ", {
  layout            = awful.layout.suit.spiral,
  gap_single_client = true,
  gap               = 5,
  screen            = 1,
})

awful.tag.add(" ", {
  layout            = awful.layout.suit.spiral,
  gap_single_client = true,
  gap               = 5,
  screen            = 1,
})

awful.tag.add(" ", {
  layout            =     awful.layout.suit.spiral,
  gap_single_client = true,
  gap               = 5,
  screen            = 1,
})

  return tags
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
