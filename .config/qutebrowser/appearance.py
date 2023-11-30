import qutellast.draw
qutellast.draw.color(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})



## Font used for the context menu. If set to null, the Qt default is
c.fonts.contextmenu =  '10pt JetBrainsMono Nerd Font'

## Font used for the debugging console.
c.fonts.debug_console = '10pt JetBrainsMono Nerd Font'

## Default font families to use.
c.fonts.default_family = ["JetBrainsMono Nerd Font"]

## Default font size to use.
c.fonts.default_size = '10pt'

## Font used for the downloadbar.
## Type: Font
c.fonts.downloads = 'default_size default_family'

## Font used for the hints.
## Type: Font
c.fonts.hints = 'bold default_size default_family'

## Font used in the keyhint widget.
## Type: Font
c.fonts.keyhint = 'default_size default_family'

## Font used for error messages.
## Type: Font
c.fonts.messages.error = 'default_size default_family'

## Font used for info messages.
## Type: Font
c.fonts.messages.info = 'default_size default_family'

## Font used for warning messages.
## Type: Font
c.fonts.messages.warning = 'default_size default_family'

## Font used for prompts.
## Type: Font
c.fonts.prompts = 'default_size sans-serif'

## Font used in the statusbar.
## Type: Font
c.fonts.statusbar = 'default_size default_family'

## Font used for selected tabs.
## Type: Font
c.fonts.tabs.selected = 'default_size default_family'

## Font used for unselected tabs.
## Type: Font
c.fonts.tabs.unselected = 'default_size default_family'

## Font used for tooltips. If set to null, the Qt default is used.
## Type: Font
c.fonts.tooltip = None

## Font family for cursive fonts.
## Type: FontFamily
c.fonts.web.family.cursive = ''

## Font family for fantasy fonts.
## Type: FontFamily
c.fonts.web.family.fantasy = ''

## Font family for fixed fonts.
## Type: FontFamily
c.fonts.web.family.fixed = ''

## Font family for sans-serif fonts.
## Type: FontFamily
c.fonts.web.family.sans_serif = ''

## Font family for serif fonts.
## Type: FontFamily
c.fonts.web.family.serif = ''

## Font family for standard fonts.
## Type: FontFamily
c.fonts.web.family.standard = ''

## Default font size (in pixels) for regular text.
## Type: Int
c.fonts.web.size.default = 16

## Default font size (in pixels) for fixed-pitch text.
## Type: Int
c.fonts.web.size.default_fixed = 13

## Hard minimum font size (in pixels).
## Type: Int
c.fonts.web.size.minimum = 0

## Minimum logical font size (in pixels) that is applied when zooming
## out.
## Type: Int
c.fonts.web.size.minimum_logical = 6
