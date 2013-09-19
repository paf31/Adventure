Adventure Game Engine
====

Design and development of a toy engine in Haskell.

Introduction and example of higher-order functions, laziness, and types.

Imperative Design
----

Big state machine with one big global state.

Simple.

Doesn't match logical situation-- no isolation of logically separate room states.

Functional Design
----

Rooms are evolving processes.

No externally accessible state.

Each room defined separately with its own actions.

An action produces some output and the next room to be in. 

*Actions are the only things with access to room state.*


#### Rooms

~~~{.haskell}
type Result = (GameState, RoomName, Output)

data GameState = GameState {inventory :: Inventory, roomMap :: [(RoomName, Room)]}

type RoomFun = GameState -> Input -> Result
data Room = Room {
      roomName :: RoomName
    , inRoom :: RoomFun
    , description :: String
    }
~~~

#### The Player

~~~{.haskell}
player :: (GameState, RoomName, Output) -> IO ()
player (gs, roomNm, output) = do
    unless (null output) $ putStrLn output
    putStr "> "
    inp <- words <$> getLine
    if inp == ["quit"]  
      then return () 
      else player $ inRoom room gs inp 
  where room = fromMaybe (error $ "unknown room: "++roomNm) . lookup roomNm $ roomMap gs
~~~

