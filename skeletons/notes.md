Adventure Game Engine
====

Design and development of a toy engine in Haskell.

* Introduction and example of higher-order functions, laziness, and types.

* Brief demonstration os STM.

Imperative Design
----

* Big state machine with one big global state.

* Simple.

* Doesn't match logical situation-- no isolation of logically separate room states.

Functional Design
----

* Rooms are evolving processes.

* No externally accessible state.

* Each room defined separately with its own actions.


### Rooms 

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

Simple "pure" references for the rooms.

But where is the `RoomState`?

### The Player

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

### Creating Rooms

All rooms support some common actions, e.g. get, put, look, etc...

#### Actions

~~~{.haskell}
type Action = RoomState -> GameState -> Input -> First Result
~~~

Performing an action produces a `Result`, i.e. some output and the next room to be in. 

*Actions are the only things with access to room state.*

Use `First` monoid to conveniently string together possible actions.

##### First Monoid (for the curious)
Comes with standard libraries in `Data.Monoid`.
~~~{.haskell}
newtype First a = First {getFirst :: Maybe a}
~~~
~~~
mappend (First (Just x)) (First (Just y)) == x

mappend (First Nothing) x == mappend x (First Nothing) == x
~~~

#### Generic Rooms

Have a builder function which creates a room with the common actions, along with some passed in room specific actions.

~~~{.haskell}
genericRoom :: Room -> [Action] -> RoomState -> GameState -> Input -> Result
~~~

The function should try the room specific actions before the common actions and provide a failure message if no action could handle the given input.

~~~{.haskell}
genericRoom room acts st gs = 
    fromMaybe (gs, roomNm, "You can't do that now.") . 
    getFirst . 
    mconcat (acts ++ [commonAction]) st gs
~~~

If an action is performed, the current room must be updated to reflect possible changed state.

~~~{.haskell}
where 
    room' st = room{inRoom = genericRoom room acts st}
~~~

Now define some common actions.

~~~{.haskell}
    roomNm = roomName room

    commonAction st gs inp = First $ case inp of 

        ["get", obj] | obj `elem` st -> Just (gs', roomNm, "You got it.")
                     | otherwise -> Just (gs, roomNm, "There is no " ++ obj ++ " here.") 
            where gs' = gs{ inventory = obj : inventory gs
                          , roomMap = (roomNm, room' $ delete obj st) : roomMap gs
                          }

        ["inventory"] -> Just (gs, roomNm, out) 
            where out = unlines . map (("You have " ++) . (++ ".")) $ inventory gs
                     
        _ -> Nothing
~~~

### Some Other Actions

Move from current room to another room. 

Note we return the result of passing the new room the input ["look"].

~~~{.haskell}
move :: String -> RoomName -> Action
move dir roomNm _st gs ["move", dir'] | dir == dir' = 
    First . Just . (\r -> inRoom r gs ["look"]) . 
    fromMaybe (error $ "internal error: missing " ++ roomNm) . lookup roomNm $ 
    roomMap gs
move _ _ _ _ _ = First Nothing
~~~

Require an object to be in inventory in order to perform an action.

~~~{.haskell}
requireInv :: GameObject -> Action -> Action
requireInv obj act st gs | obj `elem` inventory gs = act st gs
                         | otherwise = \_ -> First Nothing
~~~

### An Actual Room

~~~{.haskell}
aud = Room{ roomName = "auditorium"
          , description = "A room full of people."
          , inRoom = genericRoom aud [move "north" $ roomName lounge] ["table"]
          }

lounge = Room{ roomName = "lounge"
             , description = "A room with a view."
             , inRoom = genericRoom lounge [move "south" $ roomName aud] ["computer", "disk"]
             }
~~~

Adding More Players
---

### So Far
We have a simple adventure game engine. 

Rooms are independent processes with their own state.

The player wanders around from room to room, altering a room's state as well as her own state.

*It should be easy to add more players.*

### Software Transactional Memory

Could use explicit mutable refs with locking (MVars) to ensure rooms are atomically updated.

But we can do even better with STM.
* STM computations are not committed if underlying memory changed.

Can allow multiple players with exact same architecture and minimal syntactic change.

### Previous Types
~~~{.haskell}
type Result = (GameState, RoomName, Output)

type Action = RoomState -> GameState -> Input -> First Result

data GameState = GameState {inventory :: Inventory, roomMap :: [(RoomName, Room)]}

type RoomFun = GameState -> Input -> Result
data Room = Room {
      roomName :: RoomName
    , inRoom :: RoomFun
    , description :: String
    }
~~~

### Types for Multiple Players
~~~{.haskell}
type Result = STM (GameState, TVar Room, Output)

type Action = RoomState -> GameState -> Input -> First Result

data GameState = GameState {inventory :: Inventory, playerId :: PlayerId} 

type RoomFun = GameState -> Input -> Result
data Room = Room {
      roomName :: RoomName
    , inRoom' :: RoomFun
    , description :: String
    }
~~~

Useful convenience function:
~~~{.haskell}
inRoom :: TVar Room -> RoomFun
inRoom roomTV gs inp = do 
    room <- readTVar roomTV 
    inRoom' room gs inp
~~~

### Previous Generic Room
~~~{.haskell}
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

        ["inventory"] -> Just (gs, roomNm, out) 
            where out = unlines . map (("You have " ++) . (++ ".")) $ inventory gs
                     
        _ -> Nothing
~~~

### Generic Room for Multiple Players
~~~{.haskell}
genericRoom :: TVar Room -> [Action] -> RoomState -> GameState -> Input -> Result
genericRoom roomTV acts st gs inp = do
    room <- readTVar roomTV
    let 
        room' st = room{inRoom' = genericRoom roomTV acts st}
        commonAction st gs inp = First $ case inp of 

            ["get", obj] | Thing obj `elem` st -> Just $ do 
                               writeTVar roomTV $ room' (delete (Thing obj) st)
                               let gs' = gs{ inventory = obj : inventory gs}
                               return (gs', roomTV, "You got it.")
                         | otherwise -> Just $ return (gs, roomTV, "There is no object " ++ obj ++ " here.") 

            ["inventory"] -> Just $ return (gs, roomTV, out) 
                where out = unlines . map (("You have " ++) . (++ ".") . show) $ inventory gs

            _ -> Nothing

    fromMaybe (return (gs, roomTV, "You can't do that now.")) . getFirst $ 
      mconcat (acts ++ [commonAction]) st gs inp
~~~

Distinguish between things and players so that you can't get and put another player.

### Players

~~~{.haskell}
data Player = Player {play :: Input -> STM (Output, Player)}

type Players = [(PlayerId, Player)]

-- Uses a bit of a hack to get the players into and out of the room states.
playerFn :: PlayerId -> GameState -> TVar Room -> Player
playerFn name gs roomTV = Player $ \inp -> do
    let gs0 = gs{playerId = name}
    (gs1, newRoomTV, out) <- inRoom roomTV gs0 inp 
    (gs2, _, _) <- inRoom roomTV gs1 ["exit "++name]
    (gs', _, _) <- inRoom newRoomTV gs2 ["enter "++name]
    return (out, playerFn name gs' newRoomTV) 
~~~
