

(kern-mk-place 
 'p_tutorial_wilderness
 "The Tutorial Wilderness"
 nil          ; sprite 
 (kern-mk-map
  nil 19 19 pal_expanded
  (list
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ ^^ ^^ "
      "__ __ __ .. .. .. .. .. .. .. .. .. .. .. __ __ ^^ .. ^^ "
      "__ __ .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ ee __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. ee __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. ^^ ^^ ^^ .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. ^^ .. ^^ .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. ^^ .. ^^ .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ .. .. .. .. .. .. .. .. .. .. .. .. .. __ .. .. .. __ "
      "__ .. .. .. .. .. .. .. ^^ .. ^^ .. .. .. __ .. .. .. __ "
      "__ .. .. .. .. .. .. ^^ ^^ .. ^^ ^^ .. .. __ .. .. .. __ "
      "__ __ .. .. .. .. .. .. ^^ ^^ ^^ .. .. .. __ __ .. __ __ "
      "__ __ __ .. .. .. .. .. .. .. .. .. .. .. __ __ __ __ __ "
      "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
  )  )
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces:
 (list
  (list p_tutorial_town 9 9)
  )

 nil ; neighborss

 ;; objects:
 (list

  (put (mk-step-clue "A)ttack the bandits to the south. "
                     "Combat will take place on a special map. "
                     "When combat is over press '<' or just walk off the edge of the map.")
       9 10)
  (put (mk-npc-party 'bandit-party-l1) 9 13)
  (put (mk-step-clue "You can K)amp in the wilderness to heal your party. Try it now.") 9 13)
 
  (put (mk-step-clue "Enter the cave below by stepping onto it.")
       9 14)
  (put (mk-dungeon 'p_tutorial_cave 7 17) 9 15)

  (put (mk-step-clue "B)oard the ship by stepping onto it and pressing 'b'. "
                     "Then sail to the dock in the northeast corner using the arrow keys.") 14 5)
  (put (mk-step-clue "F)ire your cannons at the bandits on the island by pressing 'f' and an arrow key. "
                     "A ship can only fire broadside, so you may have to maneuver a bit.") 
       15 4)
  (put (mk-step-clue "F)ire your cannons at the bandits on the island by pressing 'f' and an arrow key. "
                     "A ship can only fire broadside, so you may have to maneuver a bit.") 
       15 6)
  (put (mk-step-clue "F)ire your cannons at the bandits on the island by pressing 'f' and an arrow key. "
                     "A ship can only fire broadside, so you may have to maneuver a bit.") 
       16 5)

  (put (mk-ship) 15 5)
  (put (mk-npc-party 'bandit-party-l1) 16 14)

  (put (mk-step-clue "That's it for the tutorial. There's lots more info in the USERS_GUIDE. "
                     "You'll find one in the doc directory where you installed the game, or "
                     "you can find one online at the project website. "
                     "You may exit the tutorial now by Q)uitting the game with the 'q' key. "
                     "Have fun!") 17 1)
       
  ) ;; end of objects

 nil ; hooks
 nil ; edge entrances
 )
