-- A global variable for the Hyper Mode
k = hs.hotkey.modal.new({}, "F17")

-- HYPER+L: Open news.google.com in the default browser
-- lfun = function()
--   news = "app = Application.currentApplication(); app.includeStandardAdditions = true; app.doShellScript('open http://news.google.com')"
--   hs.osascript.javascript(news)
--   k.triggered = true
-- end
-- k:bind('', 'l', nil, lfun)

apps = { 
  "Figma",
  "Preview",
  "Messages",
  space = "Emacs",
  e = "iTerm",
  t = "iTerm",
  s = "Slack",
  c = "Google Chrome",
  a = "Safari",
  w = "Firefox Developer Edition",
  p = "Spotify",
  f = "Finder",
}

for key, value in pairs(apps) do
  k:bind('', tostring(key), function()
    k.triggered = true
    hs.application.launchOrFocus(value)
  end)
end

switcher_browsers = hs.window.switcher.new{'Firefox Developer Edition','Safari','Google Chrome'}

k:bind('', 'tab', function()switcher_browsers:next() end)


local WindowResizer = require("window-resizer")
hs.window.animationDuration = 0
k:bind("", "left", function()
  k.triggered = true
  WindowResizer.moveWindowLeft()
end)
k:bind("", "right", function()
  k.triggered = true
  WindowResizer.moveWindowRight()
end)

k:bind("", "up", function()
  k.triggered = true
  WindowResizer.maximizeWindow()
end)
k:bind("", "down", function()
  k.triggered = true
  WindowResizer.centerWindow()
end)
k:bind("shift", "up", function()
  k.triggered = true
  WindowResizer.maximizeWindow()
end)
k:bind("shift", "down", function()
  k.triggered = true
  WindowResizer.centerWindow()
end)
k:bind("cmd", "up", function()
  k.triggered = true
  WindowResizer.maximizeWindow()
end)
k:bind("cmd", "down", function()
  k.triggered = true
  WindowResizer.centerWindow()
end)

k:bind("shift", "left", function()
  k.triggered = true
  WindowResizer.moveWindowUpperLeft()
end)
k:bind("shift", "right", function()
  k.triggered = true
  WindowResizer.moveWindowUpperRight()
end)
k:bind("cmd", "left", function()
  k.triggered = true
  WindowResizer.moveWindowBottomLeft()
end)
k:bind("cmd", "right", function()
  k.triggered = true
  WindowResizer.moveWindowBottomRight()
end)
k:bind("shift", "space", function()
  k.triggered = true
  WindowResizer.moveWindowToNextMonitor()
end)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-- Bind alt space
-- alt = hs.hotkey.bind('alt', 'space', null, function()
--   hs.application.launchOrFocus("iTerm")
-- end)

