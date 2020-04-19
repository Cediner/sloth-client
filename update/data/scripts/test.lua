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

api.chat.chat_send_message(api.chat.village_chat(), "test")
--api.chat.chat_send_message(api.chat.realm_chat(), "test")
