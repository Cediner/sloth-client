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

crate = api.gob.get_by_name("gfx/terobjs/crate")
me = api.gob.mygob()
print(me)
print(api.gob.is_moving(me))


if crate then
  api.mv.smart_move_to_gob(crate)
end
