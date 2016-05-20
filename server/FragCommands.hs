module FragCommands where

import FragData

performUC :: Player -> ServerState -> UserCommand -> ServerState
performUC p oldss uc = ($ oldss) . ($ p) $ case command uc of
  "+forward" -> playerMove (setVecZ 1)
  "-forward" -> playerMove (setVecZ 0)

  "+back" -> playerMove (setVecZ (-1))
  "-back" -> playerMove (setVecZ 0)

  "+right" -> playerMove (setVecX 1)
  "-right" -> playerMove (setVecX 0)

  "+left" -> playerMove (setVecX (-1))
  "-left" -> playerMove (setVecX 0)
  _ -> noAct

type UCAction = Player -> ServerState -> ServerState
playerMove :: (Vector -> Vector) -> UCAction
playerMove f p = modifyPlayer p $ transformObject (transformWish f) p

noAct :: UCAction
noAct _ = id
