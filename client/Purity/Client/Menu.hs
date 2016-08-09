module Purity.Client.Menu where

keyboardCallback :: GLFW.KeyCallback
keyboardCallback win key _ keystate mods = do
  putStrLn $ "Ya Did Da " ++ show key ++ " Key like a " ++ show keystate ++ " While ya held da " ++ show mods
  when (key == GLFW.Key'Escape && keystate == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose win True

mouseCallback :: GLFW.MouseButtonCallback
mouseCallback win button buttonState mods = 
