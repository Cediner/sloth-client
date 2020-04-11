print('Test!')
print(script)
print(session)
print(session:username())
print(session:chrname())
print(session:getUI())
print(session.shp)
print(session.hhp)
print(session.mhp)
chat = session:getUI().gui.chat
chat.area:send(session:username())
chat.area:send(session:chrname())

while true do
      chat.area:send("Tick")
      script:sleep(500)
end