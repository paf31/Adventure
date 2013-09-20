~~~{.haskell}
aud = Room{ roomName = "auditorium"
          , description = "A room full of people."
          , inRoom = genericRoom aud [move "north" $ roomName lounge] ["table"]
          }


lounge = Room{ roomName = "lounge"
             , description = "A room with a view."
             , inRoom = genericRoom lounge myActs ["computer", "disk_a", "disk_z"]
             }
  where 
    myActs = [move "south" $ roomName aud, requireInv "disk_a" playGame]
    playGame st gs inp = First $ 
      case inp of
        ["use", "disk_a"] -> Just (gs{inventory = delete "disk_a" (inventory gs)}, "advent", "Something strange happened...")
        _ -> Nothing


advent = Room{ roomName = "advent"
             , description = "You are standing at the end of a road before a small brick building." ++ 
                             "Around you is a forest." ++ 
                             "A small stream flows out of the building and down a gully."
             , inRoom = genericRoom advent [xyzzy] []
             } 
  where 
    xyzzy st gs inp = First $ 
      case inp of
        ["xyzzy"] -> Just (gs{inventory = "disk_a" : inventory gs}, "lounge", "Welcome back.")
        _ -> Nothing
~~~
