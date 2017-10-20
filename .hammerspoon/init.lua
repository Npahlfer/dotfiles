-- A global variable for the Hyper Mode
k = hs.hotkey.modal.new({}, "F17")
-- s = hs.hotkey.modal.new({}, "space")

-- HYPER+L: Open news.google.com in the default browser
-- lfun = function()
--   news = "app = Application.currentApplication(); app.includeStandardAdditions = true; app.doShellScript('open http://news.google.com')"
--   hs.osascript.javascript(news)
--   k.triggered = true
-- end
-- k:bind('', 'l', nil, lfun)


-- s:bind('', 't', function() hs.application.launchOrFocus("iTerm") end)
k:bind('', 'space', function() hs.application.launchOrFocus("iTerm") end)
k:bind('', 't', function() hs.application.launchOrFocus("iTerm") end)
k:bind('', '3', function() hs.application.launchOrFocus("FirefoxDeveloperEdition") end)
k:bind('', 's', function() hs.application.launchOrFocus("Slack") end)
k:bind('', 'w', function() hs.application.launchOrFocus("Google Chrome") end)
k:bind('', 'p', function() hs.application.launchOrFocus("Spotify") end)
k:bind('', 'f', function() hs.application.launchOrFocus("Finder") end)
k:bind('', '1', function() hs.application.launchOrFocus("Sketch") end)
k:bind('', '2', function() hs.application.launchOrFocus("Preview") end)

-- hyper = {"ctrl", "alt", "cmd"}
-- hypershift = {"ctrl", "alt", "cmd", "shift"}
-- hyper = { "F17" }
-- hypershift = {"F17" , "shift"}

-- require('keyboard')
require('watcher')
require('position')

-- function setIterm2Profile(profile)
--    hs.execute("echo -e '\033]50;SetProfile=" .. profile .. "\x7'")
-- end

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

-- modal = hs.hotkey.modal.new( {}, 'f20' );

-- --  Vim like directions: hjkl
-- --   ensure that selection is made if shift is pressed
-- k:bind( '', 'j',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'down' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'down' ) 
-- 	end
-- )

-- k:bind( { 'shift' }, 'j',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'down' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'down' ) 
-- 	end
-- )

-- k:bind( '', 'k',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'up' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'up' ) 
-- 	end
-- )
-- k:bind( { 'shift' }, 'k',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'up' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'up' ) 
-- 	end
-- )

-- k:bind( '', 'h',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'left' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'left' ) 
-- 	end
-- )
-- k:bind( { 'shift' }, 'h',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'left' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'left' ) 
-- 	end
-- )

-- k:bind( '', 'l',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'right' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'right' ) 
-- 	end
-- )
-- k:bind( { 'shift' }, 'l',
-- 	function()
-- 		k.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'right' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'right' ) 
-- 	end
-- )


-- modal:bind( '', 'j',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'down' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'down' ) 
-- 	end
-- )

-- modal:bind( { 'shift' }, 'j',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'down' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'down' ) 
-- 	end
-- )

-- modal:bind( '', 'k',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'up' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'up' ) 
-- 	end
-- )
-- modal:bind( { 'shift' }, 'k',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'up' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'up' ) 
-- 	end
-- )

-- modal:bind( '', 'h',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'left' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'left' ) 
-- 	end
-- )
-- modal:bind( { 'shift' }, 'h',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'left' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'left' ) 
-- 	end
-- )

-- modal:bind( '', 'l',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( {}, 'right' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( {}, 'right' ) 
-- 	end
-- )
-- modal:bind( { 'shift' }, 'l',
-- 	function()
-- 		modal.triggered = true;
-- 		hs.eventtap.keyStroke( { 'shift' }, 'right' ) 
-- 	end,
-- 	nil,
-- 	function()
-- 		hs.eventtap.keyStroke( { 'shift' }, 'right' ) 
-- 	end
-- )

 -- -- Space
-- modal:bind( '', 'b',
	-- function() 
		-- modal.triggered = true;
	  	-- space:disable();
		-- hs.eventtap.keyStroke( {}, 'space' ); 
		-- space:enable()
	-- end,
	-- nil, 
	-- function()
		-- space:disable();
		-- hs.eventtap.keyStroke( {}, 'space' ); 
		-- space:enable() 
	-- end 
-- )

-- -- Enter Hyper Mode when Spacebar is pressed
-- pressedSpace = function()
--   modal.triggered = false
--   modal:enter()
-- end

-- -- Leave Hyper Mode when Spacebar is pressed,
-- --   send space if no other keys are pressed.
-- releasedSpace = function()
--   modal:exit()
--   if not modal.triggered then
--   	space:disable() -- hs seems to be triggered by his onw keyStroke, so needs to be disables before we can emit the space
--     hs.eventtap.keyStroke( {}, 'space' )
--     space:enable()
--   end
-- end

-- -- Main binding on spacebar
-- space = hs.hotkey.bind( {}, 'space', pressedSpace, releasedSpace )


