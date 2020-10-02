--print('Test!')
--print(script)
--print(session)
--print(session:username())
--print(session:chrname())
--print(session:getUI())
--print(session.shp)
--print(session.hhp)
--print(session.mhp)
--chat = session:getUI().gui.chat
--chat.area:send(session:username())
--chat.area:send(session:chrname())


api = require("data.scripts.lualib.api")

horse = api.gob.get_by_name("gfx/kritter/horse/stallion")

while true do
  print("click horse")
  horse:click(3, api.const.mf_none, 0, -1)
  print("wait on flowermenu")
  api.flowermenu.wait_for_flowermenu()
  print("select opt")
  id = api.widget.next_wdg_id()
  api.flowermenu.select(0)
  script:sleep(250)
  print("close  window")
  api.widget.ui_force_wdgmsg(id, "close")
  script:sleep(50)
end
