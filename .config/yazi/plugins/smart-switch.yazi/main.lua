--- @sync entry
local function entry(_, job)
    local cur = cx.active.current
    for _ = #cx.tabs, job.args[1] do
        ya.manager_emit("tab_create", {cur.cwd})
        if cur.hovered then
            ya.manager_emit("reveal", {cur.hovered.url})
        end
    end
    ya.manager_emit("tab_switch", {job.args[1]})
end

return {entry = entry}
