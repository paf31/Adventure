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


