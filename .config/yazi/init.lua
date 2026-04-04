-- show link path
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

-- remove position from status bar
function Status:permissions()
end

-- remove percentage from status bar
function Status:percentage()
end

-- full-border plugin
require("full-border"):setup()

-- zoxide plugin
require("zoxide"):setup {
    update_db = true
}
-- sync clipboard plugin
require("session"):setup {
    sync_yanked = true
}

require("augment-command"):setup(
    {
        prompt = true,
        default_item_group_for_prompt = "hovered",
        smart_enter = true,
        smart_paste = false,
        smart_tab_create = true,
        smart_tab_switch = true,
        open_file_after_creation = false,
        enter_directory_after_creation = false,
        use_default_create_behaviour = false,
        enter_archives = false,
        extract_retries = 3,
        recursively_extract_archives = true,
        preserve_file_permissions = false,
        must_have_hovered_item = true,
        skip_single_subdirectory_on_enter = true,
        skip_single_subdirectory_on_leave = true,
        wraparound_file_navigation = false
    }
)

-- You can configure your bookmarks using simplified syntax
local bookmarks = {
  { tag = "uni", path = "~/uni", key = "u" },
  { tag = "inbox", path = "~/inbox", key = "i" },
}
