(mk-tower
 'p_tutorial_town "Tutorial Town"
 (list
      "xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx ,, ,, ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx xx ,, xx xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx ,, xx xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx ,, ,, xx xx xx xx ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx xx xx xx ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx ,, xx xx xx xx xx xx ,, xx xx xx xx xx "
      "xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "

  )
  (list ;entrances
  	(list north 9 0)
  )
 (put (mk-chest 
       nil 
       '( 
        ( 10 t_torch)
        ( 1 t_sword)
        ( 1 t_armor_leather)
        ( 1 t_shield)
        ( 1 t_halberd)
        ))
      5 6)
 (put (spawn-pt 'snake) 11 13)
 (put (kern-tag 'p1 (mk-portcullis)) 6 11)
 (put (mk-lever 'p1) 5 10)
 (put (kern-tag 'p2 (mk-portcullis)) 10 13)
 (put (mk-lever 'p2) 9 14)
 (put (mk-step-clue "To the left is a chest. O)pen it by pressing the 'o' key followed by an arrow key. "
                    "Then G)et the items by pressing the 'g' key followed by an arrow.") 
      6 6)
 (put (mk-step-clue "You better R)eady your weapons and armor. "
                    "Press the 'r' key. In the window above you can select weapons and armor to wear or "
                    "remove by pressing the SPACEBAR. Experiment with this a bit. "
                    "When you're done amusing yourself press the ESC key."
                    )
      6 8)
 (put (mk-step-clue "To check your equipment Z)tatus, hit the 'z' key. "
                    "Scroll through the panes with the arrow keys. "
                    "When you're done hit the ESC key."
                    )
      6 9)
 (put (mk-step-clue "To open the portcullis you need to H)andle the lever to the left. "
                    "Press 'h', move the crosshair over the lever with the arrow keys, "
                    "and then press enter.")
      6 10)
 (put (mk-step-clue "If you want to identify the objects around you, use the X)amine command by pressing 'x'. "
                    "The cursor over whatever you want to look at. "
                    "Non-player characters (NPCs) like the snake will be highlighted with a box. "
                    "Red means hostile, yellow means neutral and green means friendly. "
                    "Hostile NPC's will attack you on sight. "
                    "When done hit the ESC key.")
      6 13)
 (put (mk-step-clue "Once you open the portcullis you'll have to fight the snake. "
                    "To A)ttack press 'a' and cursor over the target, then press ENTER.")
      9 13)

 (put (mk-step-clue "To O)pen a door just try to step through it.") 13 13)
 (put (mk-door) 13 12)

 (put (mk-step-clue "S)earch corpses using the 's' key followed by an arrow key. You can also S)earch for invisible items with this command.") 12 10)
 (put (mk-corpse2 (list (list 20 't_picklock) 
                        (list 10 'sulphorous_ash)
                        (list 10 'blood_moss)
                        )) 11 10)

 (put (mk-locked-door) 13 8)
 (put (mk-step-clue "U)se the picklocks from the corpse to unlock this door. "
                    "Press 'u' for U)se, select the item, then target the door. "
                    "If your picklock breaks, just try again!"
                    ) 13 9)

 (put (kern-tag 'p3 (mk-portcullis)) 12 13)
 (put (mk-lever 'p3) 11 14)

 (put (mk-magic-locked-door) 13 4)
 (put (mk-thorald) 12 5)
 (put (mk-step-clue "T)alk to NPC's by hitting the 't' key and targeting them. "
                    "Then enter keywords to ask them questions. Most NPC's respond to NAME and JOB. "
                    "NPC's usually give you clues to more keywords in their responses. "
                    "To end a conversation type 'bye' or just hit ENTER. "
                    "If you think an NPC would be a good addition to your party, ask them to JOIN. "
                    "Now talk to Thorald and ask him about the door. ")
      13 7)


 (put (mk-step-clue "You're about to leave town and enter the wilderness. "
                    "In the wilderness your entire party will appear as a single icon. ")
      9 0)
 )
