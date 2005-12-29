;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(mk-dungeon-room
 'p_tutorial_cave "Tutorial Cave"
 (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr %% %% rr rr tt .. .. .. tt rr rr rr {{ rr rr rr rr rr "
      "rr %% rr rr || .. .. .. .. .. || rr rr tt {{ rr rr rr rr "
      "rr %% rr tt %% bb .. .. .. bb %% || rr .. tt rr rr rr rr "
      "rr %% rr %% %% %% %% .. .. %% %% tt rr .. tt rr rr rr rr "
      "rr rr rr tt %% bb %% .. .. bb %% %% rr .. {{ rr rr rr rr "
      "rr .. || || %% %% .. .. .. %% %% %% rr {{ {{ rr rr rr rr "
      "rr .. rr || %% bb .. %% %% bb %% %% rr {{ {{ rr rr rr rr "
      "rr tt rr tt %% .. .. .. %% %% tt tt rr .. {{ rr rr rr rr "
      "rr tt rr %% %% bb .. .. .. bb || || rr .. {{ rr rr rr rr "
      "rr tt rr rr tt %% .. .. .. tt || rr rr .. tt rr rr rr rr "
      "rr .. tt rr rr rr .. .. .. rr rr rr .. .. tt rr rr rr rr "
      "rr .. .. .. && rr .. .. .. rr .. .. .. tt || rr rr rr rr "
      "rr .. .. .. .. rr .. .. .. rr .. .. tt || rr rr rr rr rr "
      "rr rr tt .. .. rr rr .. rr rr tt tt || rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'bat) 7 7)
 (put (mk-step-clue "Now would be a good time to U)se a torch!") 7 15)
 (put (mk-step-clue "Kill the bat, then enter F)ollow mode and look around. "
                    "When you're done, go back to the ladder and press E)nter to climb back up.")
      7 14)
 (put (mk-ladder-up 'p_tutorial_wilderness 9 15) 7 17)
 )
