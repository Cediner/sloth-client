api = require("data.scripts.lualib.api")

function get_itms()
  return api.inventory.invs_get_items_by_filter((function(itm) return itm:rnm() == "Head of Lettuce" end))
end


itms = get_itms()
while #itms > 0 do
  for i=1, #itms do
    api.item.interact(itms[i], api.const.mf_none)
    api.flowermenu.wait_for_flowermenu()
    api.flowermenu.select(1)
    api.flowermenu.wait_for_no_flowermenu()
  end
  script:sleep(500)
  itms = get_itms()
end
