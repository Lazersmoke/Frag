module FragCommands where

import FragData

performUC :: Player -> ServerState -> UserCommand -> ServerState
performUC p oldss uc = ($ oldss) . ($ p) $ case command uc of
  "+forward" -> playerMove (Vector (0,0,1))
  "-forward" -> playerMove (Vector (0,0,-1))


type UCAction = Player -> ServerState -> ServerState
playerMove :: Direction -> UCAction
playerMove newwish p = modifyPlayer p (transformObject (\o -> o {wish = newwish}) p)
