require("bookmarks"):setup({
    save_last_directory = true,
	notify = {
		enable = false,
		timeout = 1,
		message = {
			new = "New bookmark '<key>' -> '<folder>'",
			delete = "Deleted bookmark in '<key>'",
			delete_all = "Deleted all bookmarks",
		},
	},
})

function Status:name()
	local h = cx.active.current.hovered
	if h == nil then
		return ui.Span("")
	end

	local linked = ""
	if h.link_to ~= nil then
		linked = " -> " .. tostring(h.link_to)
	end
	return ui.Span(" " .. h.name .. linked)
end

function Manager:render(area)
	self.area = area

	local chunks = ui.Layout()
		:direction(ui.Layout.HORIZONTAL)
		:constraints({
			ui.Constraint.Ratio(MANAGER.ratio.parent, MANAGER.ratio.all),
			ui.Constraint.Ratio(MANAGER.ratio.current, MANAGER.ratio.all),
			ui.Constraint.Ratio(MANAGER.ratio.preview, MANAGER.ratio.all),
		})
		:split(area)

	local bar = function(c, x, y)
		x, y = math.max(0, x), math.max(0, y)
		return ui.Bar(ui.Rect { x = x, y = y, w = ya.clamp(0, area.w - x, 1), h = math.min(1, area.h) }, ui.Bar.TOP)
			:symbol(c)
	end

	return ya.flat {
		-- Borders
		ui.Border(area, ui.Border.ALL):type(ui.Border.ROUNDED),
		ui.Bar(chunks[1], ui.Bar.RIGHT),
		ui.Bar(chunks[3], ui.Bar.LEFT),

		bar("┬", chunks[1].right - 1, chunks[1].y),
		bar("┴", chunks[1].right - 1, chunks[1].bottom - 1),
		bar("┬", chunks[2].right, chunks[2].y),
		bar("┴", chunks[2].right, chunks[1].bottom - 1),

		-- Parent
		Parent:render(chunks[1]:padding(ui.Padding.xy(1))),
		-- Current
		Current:render(chunks[2]:padding(ui.Padding.y(1))),
		-- Preview
		Preview:render(chunks[3]:padding(ui.Padding.xy(1))),
	}
    end

function Status:position()
end

function Status:percentage()
end

function Status:size()
	local h = cx.active.current.hovered
	if h == nil then
		return ui.Line {}
	end

	local style = self.style()
	return ui.Line {
		ui.Span(" " .. ya.readable_size(h:size() or h.cha.length) .. " "):fg(style.bg):bg(THEME.status.separator_style.bg),
		ui.Span(THEME.status.separator_close):fg(THEME.status.separator_style.fg),
	}
end


require("zoxide"):setup {
    update_db = true,
                        }

require("session"):setup {
	sync_yanked = true,
                         }

require("parent-arrow")
