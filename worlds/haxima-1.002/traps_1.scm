;; ----------------------------------------------------------------------------
;; Level 1 of the Thief's Ladder
;; ----------------------------------------------------------------------------
(mk-dungeon-room
 'p_traps_1 "Riddles in the Dark"
 (list
  "rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx xx xx rr rr "
  "rr rr rr xx xx xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, x! rr rr "
  "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx rr rr "
  "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
  "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
  "rr rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
  "rr rr x! ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
  "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx rr rr rr "
  "rr rr xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr "
  )
 (put (mk-riddle 'egg 't_lava 3 5 3 9 #f
                 "All who would pass must answer the riddle:\n\n"
                 "  In a marble hall white as milk\n"
                 "  Lined with skin soft as silk\n"
                 "  Within a fountain crystal clear\n"
                 "  A golden apple doth appear\n"
                 "  No doors there are to this stronghold\n"
                 "  Yet thieves break in to steal its gold.\n"
                 ) 4 14)
 (put (mk-riddle 'few 't_lava 8 5 3 9 #f
                 "All who would pass must answer the riddle:\n\n"
                 "  I know a word of letters three.\n"
                 "  Add two, and fewer there will be."
                 ) 9 4)
 (put (mk-riddle 'eye 't_lava 13 5 3 9 #f
                 "All who would pass must answer the riddle:\n\n"
                 "  Pronounced as one letter,\n"
                 "  but written with three.\n"
                 "  Two letters there are\n"
                 "  and two only in me.\n"
                 "  I'm double, and single,\n"
                 "  and black, blue and gray.\n"
                 "  When read from both ends\n"
                 "  I'm the same either way."
                 ) 14 14)
 (put (mk-ladder-down 'p_traps_2 9 15) 14 2)
 (put (mk-ladder-up 'p_bole 43 6) 4 16)
 )

(mk-place-music p_traps_1 'ml-dungeon-town)

(kern-place-add-on-entry-hook p_traps_1 'quest-thiefrune-den1)
