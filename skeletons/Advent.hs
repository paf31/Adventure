{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad(unless)
import Data.List(delete)
import Data.Maybe
import Data.Monoid

type GameObject = String
type RoomName = String
type Input = [String]
type Output = String
type RoomState = [GameObject]
type Inventory = [GameObject]

type Result = (GameState, RoomName, Output)

type Action = RoomState -> GameState -> Input -> First Result

data GameState = GameState {inventory :: Inventory, roomMap :: [(RoomName, Room)]}

type RoomFun = GameState -> Input -> Result
data Room = Room {
      roomName :: RoomName
    , inRoom :: RoomFun
    , description :: String
    }


-- main eval loop
player :: (GameState, RoomName, Output) -> IO ()
player (gs, roomNm, output) = do
    unless (null output) $ putStrLn output
    putStr "> "
    inp <- words <$> getLine
    if inp == ["quit"]  
      then return () 
      else player $ inRoom room gs inp 
  where room = fromMaybe (error $ "unknown room: "++roomNm) . lookup roomNm $ roomMap gs


-- require an object to be present in the room in order to do something.
requireRm :: GameObject -> Action -> Action
requireRm obj act st | obj `elem` st = act st
                     | otherwise = \_ _ -> First Nothing

-- require an object to be in inventory in order to do something.
requireInv :: GameObject -> Action -> Action
requireInv obj act st gs | obj `elem` inventory gs = act st gs
                         | otherwise = \_ -> First Nothing

-- note that we return the result of passing the new room the input ["look"]
move :: String -> RoomName -> Action
move dir roomNm _st gs ["move", dir'] | dir == dir' = 
    First . Just . (\r -> inRoom r gs ["look"]) . fromMaybe (error $ "internal error: missing " ++ roomNm) . lookup roomNm $ roomMap gs
move _ _ _ _ _ = First Nothing
    

-- a generic room which can be extended with some specific actions
genericRoom :: Room -> [Action] -> RoomState -> GameState -> Input -> Result
genericRoom room acts st gs = 
    fromMaybe (gs, roomNm, "You can't do that now.") . 
    getFirst . 
    mconcat (acts ++ [commonAction]) st gs
  where 
    room' st = room{inRoom = genericRoom room acts st}
    roomNm = roomName room
    commonAction st gs inp = First $ case inp of 

        ["get", obj] | obj `elem` st -> Just (gs', roomNm, "You got it.")
                     | otherwise -> Just (gs, roomNm, "There is no " ++ obj ++ " here.") 
            where gs' = gs{ inventory = obj : inventory gs
                          , roomMap = (roomNm, room' $ delete obj st) : roomMap gs
                          }

        ["put", obj] | obj `elem` inventory gs -> Just (gs', roomNm, "You put it.")
                     | otherwise -> Just (gs, roomNm, "You don't have " ++ obj ++ ".") 
            where gs' = gs{ inventory = delete obj (inventory gs)
                          , roomMap = (roomNm, room' $ obj : st) : roomMap gs
                          }

        ["look"] -> Just (gs, roomNm, out)
            where out = description room ++ "\n" ++ 
                        (unlines . map (("You see " ++) . (++ " here.")) $ st)
                  
        ["inventory"] -> Just (gs, roomNm, out) 
            where out = unlines . map (("You have " ++) . (++ ".")) $ inventory gs
                     
        _ -> Nothing


-- example rooms
aud = Room{ roomName = "auditorium"
          , description = "A room full of people."
          , inRoom = inrm ["table"]
          }
  where inrm = genericRoom aud [move "north" $ roomName lounge] 

lounge = Room{ roomName = "lounge"
             , description = "A room with a view."
             , inRoom = inrm ["computer", "disk_a", "disk_z"]
             }
  where inrm = genericRoom lounge [move "south" $ roomName aud, requireInv "disk_a" playGame]
        playGame st gs inp = First $ case inp of
            ["use", "disk_a"] -> Just (gs{inventory = delete "disk_a" (inventory gs)}, "advent", "Something strange happened...")
            _ -> Nothing

advent = Room{ roomName = "advent"
             , description = "You are standing at the end of a road before a small brick building. Around you is a forest. A small stream flows out of the building and down a gully."
             , inRoom = genericRoom advent [xyzzy] []
             } 
  where xyzzy st gs inp = First $ case inp of
            ["xyzzy"] -> Just (gs{inventory = "disk_a" : inventory gs}, "lounge", "Welcome back.")
            _ -> Nothing

-- a sample main
main = player $ inRoom aud gs ["look"]
  where gs = GameState {inventory = [], roomMap = map (\r -> (roomName r, r)) [aud, lounge, advent]}
