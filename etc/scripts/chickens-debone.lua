api = require("data.scripts.lualib.api")

itms = api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Cleaned Chicken" end))
id = api.widget.next_wdg_id()

while #itms > 0 do
  for i=1, #itms do
    api.item.interact(itms[i], api.const.mf_none)
    api.widget.ui_force_wdgmsg(id, "cl", {0, api.const.mf_none})
    id = id + 1 + 2 -- May screw up if we get a wishbone.. still faster to do this way
  end
  --reset and  process remaining
  script:sleep(500)
  itms = api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Cleaned Chicken" end))
  id = api.widget.next_wdg_id()
end
