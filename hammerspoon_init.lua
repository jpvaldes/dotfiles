--- Hammerspoon config: a fork of Mjolnir
--- I find Mjolnir less verbose through the use of packages but Hammerspoon docs are better
--- and there are some modules like grid that can be used although losing some control

--- fast animations, 0 for no animation
hs.window.animationDuration = 0

--- keyboard combos, better than writing the whole {} all the time
local cact = {"cmd", "alt", "ctrl"}
local ca = {"cmd", "alt"}
local cas = {"cmd", "alt", "shift"}

--- say something on the screen
hs.hotkey.bind(cact, "W", function()
    hs.alert.show("Hi!")
end)

--- apps launcher!
appDefs = {
  i = 'iterm',
  c = 'Google Chrome',
  e = 'Emacs',
  m = 'Mail',
  x = 'KeePassX'
}
-- launch the apps
for key, app in pairs(appDefs) do
  hs.hotkey.bind(cact, key, function() hs.application.launchOrFocus(app) end)
end

--- auto reload configuration after saving
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

--- window movement with cact + h j k l y u b n
hs.hotkey.bind(cact, "K", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.y = f.y - 30
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "J", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.y = f.y + 30
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + 30
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - 30
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "Y", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - 20
  f.y = f.y - 20
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "U", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + 20
  f.y = f.y - 20
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "B", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x - 20
  f.y = f.y + 20
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "N", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()

  f.x = f.x + 20
  f.y = f.y + 20
  win:setFrame(f)
end)

--- maximaize only to left or right side of screen: cact + left/right arrow
hs.hotkey.bind(cact, "Left", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind(cact, "Right", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

--- grid in quarters of screen: cact + 1234
hs.hotkey.bind(cact, "1", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()
               local screen = win:screen()
               local max = screen:frame()

               f.x = max.x
               f.y = max.y
               f.w = max.w / 2
               f.h = max.h / 2
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "2", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()
               local screen = win:screen()
               local max = screen:frame()

               f.x = max.x + (max.w / 2)
               f.y = max.y
               f.w = max.w / 2
               f.h = max.h / 2
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "3", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()
               local screen = win:screen()
               local max = screen:frame()

               f.x = max.x
               f.y = max.y + (max.h / 2)
               f.w = max.w / 2
               f.h = max.h / 2
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "4", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()
               local screen = win:screen()
               local max = screen:frame()

               f.x = max.x + (max.w / 2)
               f.y = max.y + (max.h / 2)
               f.w = max.w / 2
               f.h = max.h / 2
               win:setFrame(f)
end)

--- resize windows
hs.hotkey.bind(cact, "=", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()

               f.h = f.h + 20
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "-", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()

               f.h = f.h - 20
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "]", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()

               f.w = f.w + 20
               win:setFrame(f)
end)

hs.hotkey.bind(cact, "[", function()
               local win = hs.window.focusedWindow()
               local f = win:frame()

               f.w = f.w - 20
               win:setFrame(f)
end)

--- move focused window to left screen on multimonitor set up
hs.hotkey.bind(cact, "r", function()
                 local win = hs.window.focusedWindow()
                 local leftScreen = hs.screen.allScreens()[2]:name()

                 win:moveToScreen(leftScreen)
end)

hs.hotkey.bind(cact, "t", function()
                 local win = hs.window.focusedWindow()
                 local rightScreen = hs.screen.allScreens()[1]:name()

                 win:moveToScreen(rightScreen)
end)

--- caffeinate control
local caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle("AWAKE")
    else
        caffeine:setTitle("SLEEPY")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("systemIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("systemIdle"))
end
