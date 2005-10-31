;; Aka, reagent cave -- generate rare reagents here
(mk-dungeon-room
 'p_dank_cave "Dank Cave"
 (list 
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
  "rr rr rr rr {{ rr rr rr .. .. .. rr rr rr {{ rr rr rr rr "
  "rr rr rr {{ {{ {{ rr {{ {{ .. {{ {{ rr {{ {{ {{ rr rr rr "
  "rr rr {{ {{ .. {{ {{ {{ {{ {{ {{ {{ {{ {{ .. {{ {{ rr rr "
  "rr {{ {{ .. .. .. .. {{ {{ rr {{ {{ .. .. .. .. {{ {{ rr "
  "rr {{ {{ .. .. .. {{ {{ rr rr rr {{ {{ .. .. .. {{ {{ rr "
  "rr rr {{ {{ .. {{ {{ rr rr rr rr rr {{ {{ .. {{ {{ rr rr "
  "rr rr rr {{ {{ {{ rr rr rr rr rr rr rr {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ rr rr rr rr rr rr rr rr rr {{ rr rr rr rr "
  "rr rr rr {{ {{ {{ rr rr rr rr rr rr rr {{ {{ {{ rr rr rr "
  "rr rr {{ {{ .. {{ {{ rr rr rr rr rr {{ {{ .. {{ {{ rr rr "
  "rr {{ {{ .. .. .. {{ {{ rr rr rr {{ {{ .. .. .. {{ {{ rr "
  "rr {{ {{ .. .. .. .. {{ {{ rr {{ {{ .. .. .. .. {{ {{ rr "
  "rr rr {{ {{ .. .. .. .. {{ {{ {{ .. .. .. .. {{ {{ rr rr "
  "rr rr rr {{ {{ .. .. {{ {{ rr {{ {{ .. .. {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ {{ rr rr rr {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr {{ {{ rr rr rr rr rr {{ {{ rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-up 'p_shamans_grove 17 11) 9 1)
 )
