-- This API is mostly optional. With how the lua binding works you can access methods/fields directly
-- if you understand the client class layout. This API just simplifies that for people who don't.

local api = {}

--------------------------------------------------
-- Basic constants
--------------------------------------------------
api.const = {
  tilesz = 11,

  leftbutton   = 1,
  mid_button   = 2,
  right_button = 3,

  mf_none           = 0,
  mf_shift          = 1,
  mf_ctrl           = 2,
  mf_shift_ctrl     = 3,
  mf_alt            = 4,
  mf_shift_alt      = 5,
  mf_ctrl_alt       = 6,
  mf_shift_ctrl_alt = 7,

  speed = {
    -- These speeds are based off how many tiles
    -- you *should* go in a single server tick at
    -- the given speed in units.
    -- tiles are made up of units and can be thought of
    -- tilesz x tilesz square units
   
    -- If you want  to get units/s just do:
    -- (speed / tick_rate) * 1000
    tick_rate   = 60,

    crawl       = 1,
    walk        = 2,
    run         = 3,
    sprint      = 4,

    dugout      = 2,
    boat        = 3,
    snekkja_min = 5,
    snekkja_max = 8,
    knarr_min   = 4,
    knarr_max   = 8
  }
}

--------------------------------------------------
-- Some core helping functions and shortcuts
--------------------------------------------------
api.core = {
  apply = function(arr, fun)
    for i = 1, #arr do
      fun(arr[i])
    end
  end,


  waituntil = function(fun, timeout, refresh)
    refresh = refresh or 100
    start = script:gettime()
    while not fun() do
      script:sleep(refresh)
      if timeout and script:gettime() - start > timeout then
        return false
      end
    end
    return true
  end,

  gui = function()
    return session:getUI().gui
  end,

  mv = function()
    return session:getUI().gui.map
  end,

  oc = function()
    return session:getUI().sess.glob.oc
  end,

  mc = function()
    return session:getUI().sess.glob.mc
  end,
}



--------------------------------------------------
-- Some basic session details about you
--------------------------------------------------
api.session =  {
  username = function()
    return session:username()
  end,

  chrname = function()
    return session:chrname()
  end,

  -- Script Message Queue
  -- Note: filter is a regex filtering on `msg`
  startListening = function(filter, allow_external)
    filter = filter or ".+"
    allow_external = allow_external or false
    session:listen(filter, allow_external)
  end,

  stopListening = function()
    session:stopListening()
    session:clearmsgs()
  end,

  clearMessages = function()
    session:clearmsgs()
  end,

  hasMessage = function()
    return session:hasMessage()
  end,


  -- Messages returned are a class with fields:
  -- `sender` - Widget who sent it to us
  -- `msg`    - The message subject
  -- `args`   - List of arguments based off `msg`
  --
  -- Known Messages
  --- Chat Related
  ---- Bot Chat
  ----- `msg`  : msg
  ----- `args` : (text)
  ---- Area Chat
  ----- `msg`  : area-msg
  ----- `args` : (text, from-name)
  ---- Realm Chat
  ----- `msg`  : realm-msg
  ----- `args` : (text, from-name)
  ---- Village Chat
  ----- `msg`  : village-msg
  ----- `args` : (text, from-name)
  ---- Party Chat
  ----- `msg`  : party-msg
  ----- `args` : (text, from-name)
  ---- Private chat
  ----- Inbound
  ------ `msg` : priv-in-msg
  ------ `args`: (text, from-name)
  ----- Outbound
  ------ `msg` : priv-out-msg
  ------ `args`: (text)
  --- Tile Selection
  ---- `msg`   : bot-select
  ---- `args`  : (starting_coord, ending_coord)
  --- Marking Gobs
  ---- `msg`   : click-gob
  ---- `args`  : (gob_obj)
  --- Marking Tiles
  ---- `msg`   : click-tile
  ---- `args`  : (tile_coord)
  pollMessage = function()
    return session:pollmsg()
  end
}


--------------------------------------------------
-- ObjectCache functionality
--------------------------------------------------
api.oc = {
  getgob = function(id)
    return api.core.oc():getgob(id)
  end,

  getallgobs = function()
    return api.core.oc():getallgobs()
  end,

  posres = function(mc)
    return mc:floor(api.core.oc().posres)
  end
}

--------------------------------------------------
-- MCache functionality
--------------------------------------------------
api.mc = {
  get_tile_z = function(c)
    mc = api.core.mc()
    return mc:getz_safe(c.div(mc.tilesz))
  end,

  get_tile = function(c)
    mc = api.core.mc()
    return mc:gettile_safe(c.div(mc.tilesz))
  end,

  get_grid_id = function(c)
    return api.core.mc():getgridid(c)
  end,

  get_tile_offset = function(c)
    return api.core.mc():gettileoffset(c)
  end,

  tilify = function(c)
    mc = api.core.mc()
    return c.div(mc.tilesz).mul(mc.tilesz).add(tilesz.div(2.0))
  end
}

--------------------------------------------------
-- Character List Widget
--------------------------------------------------
api.charlst = {
  charlst = function()
    return session:getCharlist()
  end,

  login = function(name)
    api.charlst.charlst():wdgmsg('play', name)
  end,

  logout = function()
    api.core.gui():wdgmsg('act', 'lo', 'cs')
  end,
}


--------------------------------------------------
-- Coord helpers
--------------------------------------------------
api.coord = {
  -- x, y are integers
  coord2i = function(x, y)
    return luajava.newInstance("haven.Coord", x, y)
  end,

  -- x, y are doubles
  coord2d = function(x, y)
    return luajava.newInstance("haven.Coord2d", x, y)
  end,

  -- x, y, z are floats
  coord3f = function(x, y, z)
    return luajava.newInstance("haven.Coord3f", x, y, z)
  end

  --Unlike the Lisp API lua makes it easier to just use
  -- class methods, below is the files for Coord, Coord2d,
  -- and Coord3f. You can use any of their `public` functions
  -- Example:
  -- cc = api.coord.coord2i(5, 5)
  -- cc:add(5, 5) -- Coord(10, 10)
  -- cc:mul(5.0)  -- Coord2d(25.0, 25.0)
  --
  -- Coord   : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord.java
  -- Coord2d : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord2d.java
  -- Coord3f : https://gitlab.com/Boshaw/sloth-client/-/blob/sloth/src/haven/Coord3f.java
}

--------------------------------------------------
-- BoundingBox / MapMod functionality
--------------------------------------------------
api.bbox = {
  trigger = function()
    gui = api.core.gui()
    pos = api.coord.coord2i(50, 50)
    gui:add(luajava.newInstance("haven.MapMod", pos))
  end,

  make = function(ul, ec)
    bb = {
      ul = ul,
      ec = ec,
      sz = ec:sub(ul),
      dir = api.coord.coord2i(1, 1)
    }

    if sz.x < 0 then
      dir.x = -1
    end
    if sz.y < 0 then
      dir.y = -1
    end

    return bb
  end,

  ec = function(bb)
    bb.ul:add(api.coord.coord2i( bb.sz.x * session.east, bb.sz.y * session.south ))
  end,

  within = function(bb, c)
    tsz = api.core.mc().tilesz2
    sc = bb.ul:mul(tsz)
    ec = bb.ec:mul(tsz)
    br = tsz.mul(session.east, session.south)
      :add(api.coord.coord2i( math.max(sc.x, ec.x), math.max(sc.y, ec.y) ))
    ul = api.coord.coord2i( math.min(sc.x, ec.x), math.min(sc.y, ec.y) )

    return (((c.x >= ul.x) and (c.x <= br.x)) or ((c.x >= br.x) and (c.x <= ul.x)))
      and (((c.y >= ul.y) and (c.y <= br.y)) or ((c.y >= br.y) and (c.y <= ul.y)))
  end,

  gobs = function(bb)
    gobs = api.oc.getallgobs()
    ret = {}

    for i = 1, #gobs do
      if api.bbox.within(bb, gobs[i].rc) then
        ret.insert(gobs[i])
      end
    end

    return ret
  end,

  tiles = function(bb)
    tiles = {}
    ul = bb.ul:mul(api.core.mc().tilesz2)
    off = bb.dir:mul(session.east, session.south):mul(api.core.mc().tilesz2)

    for x = 0, math.abs(bb.sz.x) do
      for y = 0, math.abs(bb.sz.y) do
        tiles.insert(api.mc.tilify(ul:add(off:mul(x, y))))
      end
    end

    return tiles
  end,

  dots = function(bb)
    dots = {}
    ul = bb.ul:mul(api.core.mc().tilesz2)
    off = bb.dir:mul(session.east, session.south):mul(api.core.mc().tilesz2)

    local function contains(a, e)
      for i = 1, #a do
        if a[i] == e then
          return true
        end
      end
      return false
    end

    local function pushnew(a, e)
      if not contains(a, e) then
        a.insert(e)
      end
    end


    for x = 0, math.abs(bb.sz.x)+1 do
      for y = 0, math.abs(bb.sz.y)+1 do
        base = api.mc.tilify(ul:add(off:mul(x, y)))

        pushnew(dots, base.add(6,6))
        pushnew(dots, base.add(-5, 6))
        pushnew(dots, base.add(6, -5))
        pushnew(dots, base.add(-5, -5))
      end
    end

    return dots
  end
}


--------------------------------------------------
-- Discord functionality
--------------------------------------------------
api.discord = {
  start_discord_session = function(token)
    session:startDiscord(token)
  end,

  end_discord_session = function()
    session:endDiscord()
  end,

  send_discord_message = function(channel, msg)
    session:sendDiscordMessage(channel, msg)
  end,

  prompt_for_discord_info = function()
    api.session.startListening("discord")
    token = nil
    role = nil

    api.core.gui().add(luajava.newInstance("haven.sloth.gui.DiscordHelper"),
                       api.coord.coord2i(50, 50))

    repeat
      script:sleep(1000)
      while api.session.hasMessage() do
        msg = api.session:pollMessage()
        token = msg.args[1]
        role  = msg.args[2]
      end
    until (token and role)

    api.session.stopListening()
    return { token, role }
  end
}


--------------------------------------------------
-- Logging and Chat functionality
--------------------------------------------------
api.chat = {
  const = {
    area = "Area Chat",
    village = "Village",
    party = "Party",
    bot = "Bot-Chat"
  },

  chat = function()
    return api.core.gui().chat
  end,

  area_chat = function()
    return api.chat.chat().area
  end,

  party_chat = function()
    return api.chat.chat().party
  end,

  village_chat = function()
    return api.chat.chat().village
  end,

  bot_chat = function()
    return api.core.gui().botlog
  end,

  realm_chat = function()
    return api.chat.chat().realm
  end,

  privchats = function()
    return api.chat.chat():privchats()
  end,

  privchat_by_name = function(name)
    chats = api.chat.privchats()
    for i = 1, #chats do
      if name == chats[i]:name() then
        return chat[i]
      end
    end
    return nil
  end,

  sysprint = function(msg)
    api.core.gui():msg(msg)
  end,

  log = function(msg)
    script:log(msG)
  end,

  chat_send_message = function(chat, msg)
    if chat == api.chat.bot_chat() then
      color = luajava.bindClass("java.awt.Color")
      chat:uimsg("msg", msg.format("[Bot] %s", msg), color.RED, 1)
    else
      chat:send(msg)
    end
  end
}

--------------------------------------------------
--  Some basic widget functionality
--------------------------------------------------
api.widget = {
  wdgmsg = function(wdg, msg, ...)
    wdg:wdgmsg(msg, ...)
  end,

  uimsg = function(wdg, msg, ...)
    wdg:uimsg(msg, ...)
  end,

  id = function(wdg)
    return wdg:wdgid()
  end,

  add = function(wdg, child, pos)
    pos = pos or api.coord.coord2i(50, 50)
    wdg:add(child, pos)
  end,

  next_wdg_id = function()
    return session:getUI().next_predicted_id
  end,

  ui_force_wdgmsg = function(id, msg, ...)
    session:getUI():wdgmsg(id, msg, ...)
  end
}

--------------------------------------------------
-- Hotkey functionality
--------------------------------------------------
api.hotkey = {
  const = {
    hk_1 = 141,
    hk_2 = 142,
    hk_3 = 143
  },

  use_item = function(slot, mflags)
    api.core.gui():wdgmsg("belt", slot, 1, mflags)
  end,

  set_item = function(slot)
    api.core.gui():wdgmsg("setbelt", slot, 0)
  end,

  unset_item = function(slot)
    api.core.gui():wdgmsg("setbelt", slot, 1)
  end,

  is_hotkey_set = function(slot)
    return api.core.gui().belt[slot]
  end,

  wait_until_hotkey_is_set = function(slot)
    api.core.waituntil((function() return is_hotkey_set(slot) end))
  end,

  wait_until_hotkey_is_unset = function(slot)
    api.core.waituntil((function() return not is_hotkey_set(slot) end))
  end
}

--------------------------------------------------
-- Gob
-- Gob objects have a few fields you can directly
-- access:
--  * id   [long]    (id)
--  * rc   [Coord2d] (Position)
--  * type [Type]    (Type of gob)
--  * a    [double]  (angle)
-- Ex: g.id to get id
--------------------------------------------------
api.gob = {
  plgobid = function()
    return api.core.mv().rlplgob
  end,

  mygob = function()
    return api.oc.getgob(api.gob.plgobid())
  end,

  is_moving = function(gob)
    return gob:moving() or (gob.id == api.gob.plgobid() and api.mv.has_moves())
  end,

  ----------------------------------------------------
  -- There's a few gob functions you can call directly
  -- Given Gob `g` these are:
  --
  -- g:getc()  returns Coord2d
  --   - Gets position as seen on the client
  -- g:name()  returns String
  --   - Returns the resource name of this gob
  -- g:getv()  returns double
  --   - Returns the velocity that the gob  is going
  -- g:sdt()   returns int
  --   - Returns the sdt number, used mostly to determine
  --     rendering state of gob (ie: crop stage, drying frame status)
  -- g:overlays()   returns Overlay[]
  --   - Returns and array of current overlays
  --   - For a given Overlay `o` you can do:
  --     o:id()  returns long - Overlay id
  --     o:name() returns String - Overlay resource name
  --
  -- States:
  -- g:isDead()
  -- g:isFriendly()
  -- g:isDangerous()
  --
  -- For humans:
  -- g:kinname()
  -- g:equipment()
  --
  -- For Crops:
  -- g:multistageplant()
  -- - Crops that can be harvested 1 before the max stage
  -- g:fallowplant()
  -- - Dead winter plant..
  -- g:getMaxStage()
  --
  ----------------------------------------------------

  ------------------
  -- Overlay helpers
  ------------------
  has_overlay = function(gob, name)
    ols = gob:overlays()
    for i = 1, #ols do
      if ols[i]:name() == name then
        return true
      end
    end
    return false
  end,

  get_overlay_by_name = function(gob, name)
    ols = gob:overlays()
    for i = 1, #ols do
      if ols[i]:name() == name then
        return ols[i]
      end
    end
    return nil
  end,

  -----------------------------
  -- The many ways to get gobs
  -----------------------------
  get_by_name = function(name)
    gobs = api.oc.getallgobs()
    for i = 1, #gobs  do
      if string.find(gobs[i]:name(), name) then
        return gobs[i]
      end
    end
    return nil
  end,

  get_all_by_filter = function (filter)
    gobs = api.oc.getallgobs()
    ret = {}
    for i = 1, #gobs do
      if filter(gobs[i]) then
        table.insert(ret, gobs[i])
      end
    end
    return ret
  end,

  get_all_by_name = function(name)
    return get_all_by_filter((function(g) string.find(g:name(), name) end))
  end,

  get_closest_by_filter_and_path = function(filter)
    me = api.gob.mygob()
    if me then
      gobs = api.gob.get_all_by_filter(filter)
      best = nil
      bestdist = 999999999
      mc = me.rc
      for i = 1, #gobs do
        path = api.mv.find_path_to_gob(gobs[i])
        if path then
          dist = api.mv.path_distance(path)
          if best == nil or dist < bestdist then
            best = gobs[i]
            bestdist = dist
          end
        end
      end
      return best
    else
      return nil
    end
  end,

  get_closest_by_filter = function(filter)
    me = api.gob.mygob()
    if me then
      gobs = api.gob.get_all_by_filter(filter)
      best = nil
      mc = me.rc
      if #gobs > 0 then
        best = gobs[1]
        bestdist = gobs[1].rc:dist(mc)
        for i = 2, #gobs do
          if gobs[i].rc:dist(mc) < bestdist then
            best = gobs[i]
            bestdist = gobs[i].rc:dist(mc)
          end
        end
      end
      return best
    else
      return nil
    end
  end,

  get_closest_by_name_and_path = function(name)
    return api.gob.get_closest_by_filter_and_path((function (g) return string.find(g:name(), name) end))
  end,

  get_closest_by_name = function(name)
    return api.gob.get_closest_by_filter((function (g) return string.find(g:name(), name) end))
  end
}


--------------------------------------------------
-- MapView (movement, pathfinding)
--------------------------------------------------
api.mv = {
  wait_for_movement = function(gob)
    gob = gob or api.gob.mygob()
    api.core.waituntil((function() return api.gob.is_moving(gob) end), 2000)
    api.core.waituntil((function() return not api.gob.is_moving(gob) end))
  end,

  -------------------------
  --  Queued movement
  -------------------------
  clear_moves = function()
    api.core.mv():clearmovequeue()
  end,

  queue_move = function(c)
    api.core.mv():queuemove(c)
  end,

  has_moves = function()
    return api.core.mv():hasmoves()
  end,

  -------------------------
  -- Instant Movement
  -------------------------
  move_to = function(c)
    api.core.mv():moveto(c)
  end,

  move_to_rel = function(c)
    api.core.mv():relMove(c)
  end,

  -------------------------
  -- Pathfinding
  -------------------------
  los = function(c)
    return api.core.mv():los(c)
  end,

  los_gob = function(gob)
    return api.core.mv():los(gob)
  end,

  find_path = function(c)
    return api.core.mv():findpath(c)
  end,

  find_path_to_gob = function(gob)
    return api.core.mv():findpath(gob)
  end,

  path_distance = function(moves)
    len = 0

    if #moves > 0 then
      len = len + moves[1]:dest():dist(api.gob.mygob().rc)
    end

    for i = 2, #moves do
      lne = len + moves[i-1]:dest():dist(moves[i]:dest())
    end
    return len
  end,

  walk_path = function(moves)
    mv = api.core.mv()
    for i = 1, #moves do
      moves[i]:apply(mv)
      api.mv.wait_for_movement()
    end
  end,

  reverse_path = function(move, goalc)
    i = #move
    nmoves = {}

    while i > 0 do
      nmoves.insert(moves[i])
      i = i - 1
    end

    nmoves.insert(goalc)
    return nmoves
  end,

  path_to = function(c)
    api.mv.walk_path(api.mv.find_path(c))
  end,

  path_to_gob = function(gob)
    api.mv.walk_path(api.mv.find_path_to_gob(gob))
  end,

  smart_move = function(c)
    if api.mv.los(c) then
      api.mv.move_to(c)
      api.mv.wait_for_movement()
    else
      api.mv.path_to(c)
    end
  end,

  smart_move_to_gob = function(gob)
    if api.mv.los_gob(gob) then
      api.mv.move_to(gob.rc)
      api.mv.wait_for_movement()
    else
      api.mv.path_to_gob(gob)
    end
  end,


  -------------------------
  -- Interacting with gobs
  -------------------------
  interact_held_item_with_gob = function(gob, mflags, overlayid, fastmeshid)
    overlayid = overlayid or 0
    fashmeshid = fastmeshid or -1

    api.core.mv():wdgmsg("itemact", api.coord.coord2i(1, 1), api.oc.posres(gob.rc), mflags,
                         overlayid, gob.id, api.oc.posres(gob.rc), overlayid, fastmeshid)
  end,

  interact_held_item_with_tile = function(tile, mflags)
    api.core.mv():wdgmsg("itemact", api.coord.coord2i(1, 1), api.oc.posres(tile), mflags)
  end,

  select_area = function(sc, ec)
    api.core.mv():wdgmsg("sel", sc, ec, api.const.mf_none)
  end,

  drop = function(c, mflags)
    api.core.mv():wdgmsg("drop", api.coord.coord2i(1, 1), api.oc.posres(c), mflags)
  end,

  placing_gob = function()
    return api.core.mv():placing()
  end,

  place_gob = function(c, angle, btn, mflags)
    api.core.mv():wdgmsg("place", api.oc.posres(c), angle, btn, mflags)
  end,

  wait_for_placing_gob = function()
    waituntil((function() return api.mv.placing_gob() end))
  end,

  wait_for_placing_gob_to_be_gone = function()
    waituntil((function() return api.mv.placing_gob() == nil end))
  end
}

--------------------------------------------------
--------------------------------------------------

return api
