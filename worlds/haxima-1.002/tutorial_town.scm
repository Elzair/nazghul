(mk-tower
 'p_tutorial_town "Tutorial Town"
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx ,, ,, xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-chest 
       nil 
       (list 
        (list 10 t_torch)
        (list 1 t_sword)
        (list 1 t_armor_leather)
        (list 1 t_shield)
        (list 1 t_halberd)
        ))
      5 6)
 ;; (put (spawn-pt 'snake) 11 13)
 (put (kern-tag 'p1 (mk-portcullis)) 6 11)
 (put (mk-lever 'p1) 5 10)
 (put (kern-tag 'p2 (mk-portcullis)) 10 13)
 (put (mk-lever 'p2) 9 14)
 (put (mk-step-clue "To the left is a chest. O)pen it by pressing the 'O' key followed by an arrow key. "
                    "Then G)et the items by pressing the 'G' key followed by an arrow.") 
      6 6)
 (put (mk-step-clue "You better R)eady your weapons and armor. "
                    "Press the 'R' key. In the window above you can select weapons and armor to wear or "
                    "remove by pressing the SPACEBAR. Experiment with this a bit. "
                    "When you're done amusing yourself press the ESC key."
                    )
      6 8)
 (put (mk-step-clue "To check your equipment Z)tatus, hit the Z key. "
                    "Scroll through the panes with the arrow keys. "
                    "When you're done hit the ESC key."
                    )
      6 9)
 (put (mk-step-clue "To open the portcullis you need to H)andle the lever to the left. "
                    "Press 'H', move the crosshair over the lever with the arrow keys, "
                    "and then press enter.")
      6 10)
 (put (mk-step-clue "If you want to identify the objects around you, use the X)amine command by pressing 'X'. "
                    "The cursor over whatever you want to look at. "
                    "Non-player characters NPCs like the snake will be highlighted with a box. "
                    "Red means hostile, yellow means neutral and green means friendly. "
                    "Hostile NPC's will attack you on sight. "
                    "When done hit the ESC key.")
      6 13)
 (put (mk-step-clue "Once you open the portcullis you'll have to fight the snake. "
                    "To A)ttack press 'A' and cursor over the target, then press ENTER.")
      9 13)

 )
