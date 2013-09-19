{-# LANGUAGE QuasiQuotes, RecursiveDo, ScopedTypeVariables, TupleSections #-}

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.List(delete, intercalate)
import Data.Maybe
import Data.Monoid
import Happstack.Server hiding (Input)
import Language.Javascript.JMacro
import Safe
import Text.XHtml.Strict as X

import Debug.Trace

traceMsgIt msg x = trace ("\n"++msg++show x) x

type ThingId = String
--type GameObject = String
data GameObject = Person PlayerId | Thing ThingId deriving (Eq)
instance Show GameObject where 
    show (Person pid) = pid
    show (Thing obj) = obj
type RoomName = String
type PlayerId = String
type Input = [String] 
type Output = String
type RoomState = [GameObject]
type Inventory = [ThingId]

-- result is (new inventory, updated old room, output, new room)
type Result = STM (GameState, TVar Room, Output)

type Action = RoomState -> GameState -> Input -> First Result

data GameState = GameState {inventory :: Inventory, playerId :: PlayerId} 
gsInit = GameState [] ""

type RoomFun = GameState -> Input -> Result
data Room = Room {
      roomName :: RoomName
    , inRoom' :: RoomFun
    , description :: String
    }
inRoom :: TVar Room -> RoomFun
inRoom roomTV gs inp = do 
    room <- readTVar roomTV 
    inRoom' room gs inp

-- require an object to be present in the room in order to do something.
requireRm :: GameObject -> Action -> Action
requireRm obj act st | obj `elem` st = act st
                     | otherwise = \_ _ -> First Nothing

-- require an object to be in inventory in order to do something.
requireInv :: ThingId -> Action -> Action
requireInv obj act st gs | obj `elem` inventory gs = act st gs
                         | otherwise = \_ -> First Nothing


-- note that we return the result of passing the new room the input ["look"]
move :: String -> TVar Room -> Action
move dir roomTV _st gs ["move", dir'] | dir == dir' = First . Just $ inRoom roomTV gs ["look"]
move _ _ _ _ _ = First Nothing


-- a generic room which can be extended with some specific actions
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

            ["put", obj] | obj `elem` inventory gs -> Just $ do 
                               writeTVar roomTV $ room' (Thing obj : st)
                               let gs' = gs{ inventory = delete obj (inventory gs)}
                               return (gs', roomTV, "You put it.")
                         | otherwise -> Just $ return (gs, roomTV, "You don't have " ++ obj ++ ".") 

            -- These two are hacks since each element of user input can't have a space.
            -- But it would be easy enough to change Input type to have internal and external input.
            [enter] | ["enter", nm] <- words enter -> Just $ do
                          writeTVar roomTV $ room' (Person nm : st)
                          return (gs, roomTV, "")
            
            [exit] | ["exit", nm] <- words exit -> Just $ do
                          writeTVar roomTV $ room' (delete (Person nm) st)
                          return (gs, roomTV, "")
                     
            ["look"] -> Just $ return (gs, roomTV, out)
                where out = description room ++ "\n" ++ 
                            (unlines . map (("You see " ++) . (++ " here.") . show) . filter (/= Person (playerId gs)) $ st)
                  
            ["inventory"] -> Just $ return (gs, roomTV, out) 
                where out = unlines . map (("You have " ++) . (++ ".") . show) $ inventory gs

            _ -> Nothing

    fromMaybe (return (gs, roomTV, "You can't do that now.")) . getFirst $ 
      mconcat (acts ++ [commonAction]) st gs inp



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


myMain = mdo 
  lounge <- mdo 
      roomTV <- newTVarIO room
      let room = Room{ roomName = "lounge"
                     , description = "A room with a view."
                     , inRoom' = genericRoom roomTV [move "south" aud, requireInv "disk_a" playGame] $ map Thing ["computer", "disk_a", "disk_z"]
                     }
          playGame st gs inp = First $ case inp of
            ["use", "disk_a"] -> Just $ return (gs{inventory = delete "disk_a" (inventory gs)}, advent, "Something strange happened...")
            _ -> Nothing
      return roomTV

  aud <- mdo
      roomTV <- newTVarIO room
      let room = Room{ roomName = "auditorium"
                     , description = "A room full of people."
                     , inRoom' = genericRoom roomTV [move "north" lounge] [Thing "table"]
                     }
      return roomTV

  advent <- mdo
      roomTV <- newTVarIO room
      let room =  Room{ roomName = "advent"
                      , description = "You are standing at the end of a road before a small brick building. Around you is a forest. A small stream flows out of the building and down a gully."
                      , inRoom' = genericRoom roomTV [xyzzy] []
                      } 
          xyzzy st gs inp = First $ case inp of
            ["xyzzy"] -> Just $ return (gs{inventory = "disk_a" : inventory gs}, lounge, "Welcome back.")
            _ -> Nothing
      return roomTV

  players <- newMVar []

  simpleHTTP nullConf . dir "adventure" . path $ \name -> 
    msum [ 
           do 
             inp <- words <$> look "input"
             player <- liftIO . withMVar players $ return . fromMaybe (error $ "bad name: " ++ name) . lookup name
             (out, player') <- liftIO . atomically $ play player inp
             liftIO . modifyMVar_ players $ return . ((name, player') :)
             ok (toResponse out)
         , do
             okName <- liftIO $ modifyMVar players $ \ps -> 
                       if name `elem` map fst ps 
                           then return (ps, False) 
                           else return ((name, playerFn name gsInit aud):ps, True)
             if okName then playerPage 
                       else ok . toResponse $ "The name " ++ name ++ " is already taken."
         ]

playerPage :: ServerPartT IO Response
playerPage = ok . toResponse $
    header << [ script ! [src "//ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js"] << noHtml
              , script ! [thetype "text/javascript"] << primHtml scr
              ] +++
    X.body << noHtml
  where
    scr = show $ renderJs [jmacro|
              var br = $("<br/>");
              var scrn = $("<div></div>");
              var askServer = \inp {
                                var serverLoc = window.location.href.split(/[?#]/)[0];
                                $.get( serverLoc+"?input="+inp
                                     , \msg { 
                                              scrn.append $("<p>" + msg + "</p>"); 
                                              scrn.scrollTop scrn[0].scrollHeight;
                                            }
                                     )
                              };
              var inp = $("<input></input>");
              var lbl = $("<span>Input:</span>");
              var btn = $("<button>Go</button>").click(\ { 
                            var x = $.trim inp.val();
                            inp.val("");
                            if (x.length > 0) {
                                scrn.append($("<p>" + "> " + x + "</p>"));                            
                                askServer x;
                            };
                        });
              inp.keyup(\e { if( e.which == 13 ) {btn.click()}; });

              $(\ {
                  $("body").append(scrn, br, br, lbl, inp, btn);
                  scrn.css("border","2px solid black");
                  scrn.css("height","300px");
                  scrn.css("width","500px");
                  scrn.css("overflow","auto");
                  scrn.css("resize","both");
                  askServer "look";
              });
          |]


