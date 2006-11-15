(kern-load "anaxes.scm")

;;----------------------------------------------------------------------------
;; Entrance to Brundegardt
(kern-mk-place 
 'p_brundegardt     ; tag
 "Brundegardt"      ; name
 s_mountains           ; sprite
 (kern-mk-map 
  nil 19 19 pal_expanded
  (list
   ".. .. .. .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
   ".. .. .. .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
   ".. .. .. .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
   ".. .. .. .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. ^^ "
   ".. .. .. .. .. .. .. .. tt tt tt ^^ ^^ ^^ ^^ ^^ || .. ^^ "
   ".. .. .. .. .. .. .. tt tt tt tt ^^ ^^ ^^ ^^ || || ^^ ^^ "
   ".. .. .. .. .. tt tt tt tt tt || ^^ ^^ ^^ ^^ || ^^ ^^ ^^ "
   ".. .. .. .. tt tt tt tt tt || || || ^^ ^^ ^^ || ^^ ^^ ^^ "
   ".. .. .. .. .. tt tt || || || || || ^^ ^^ ^^ || ^^ ^^ ^^ "
   ".. .. .. tt tt tt tt || || || || || || ^^ || || ^^ ^^ ^^ "
   ".. .. tt tt tt tt tt tt || || || || || || || ^^ ^^ ^^ ^^ "
   ".. .. tt tt tt tt tt tt tt bb .. bb .. bb ^^ ^^ ^^ ^^ ^^ "
   ".. .. tt tt || || tt tt .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ "
   ".. .. tt tt || || || tt .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ "
   ".. .. tt tt || || || || bb .. .. && .. .. .. ^^ ^^ ^^ ^^ "
   ".. .. tt tt tt || || || .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ "
   ".. .. tt tt tt || || || bb .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ "
   ".. .. .. tt tt tt || || || bb .. bb .. bb ^^ ^^ ^^ ^^ ^^ "
   ".. .. .. tt tt tt tt || || || || || || ^^ ^^ ^^ ^^ ^^ ^^ "
   ))
 #f                 ; wraps
 #f                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;; objects
 (list
  (put (spawn-pt 'troll) 12 14)
  (put (spawn-pt 'troll) 10 14)
  (put (spawn-pt 'troll) 11 15)
  (put (mk-riddle 'noor 't_doorway 18 3 1 1 #t
                  "All who would pass must speak the password!") 17 3)
  )

 nil ;; hooks

 ;; edge entrances
 (list
  (list west 18 3)
  )
 )

;;----------------------------------------------------------------------------
;; Brundegardt Keep
(kern-mk-place 
 'p_brundegardt_keep     ; tag
 "Brundegardt Keep"      ; name
 nil
 (kern-mk-map 
  nil 25 25 pal_expanded
  (list
   "rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr gg gg gg rr "
   "rr rr rr rr xx cc cc cc xx rr rr rr rr rr rr rr rr rr rr rr rr rr gg rr rr "
   "rr rr rr rr xx cc cc cc ?? gg gg gg gg gg gg gg gg gg gg rr rr rr gg rr rr "
   "rr rr rr rr xx cc cc cc xx rr rr xx xx xx xx xx xx xx gg rr rr rr gg rr rr "
   "rr rr rr rr xx xx cc xx xx xx xx xx cc cc cc cc cc xx gg gg gg gg gg rr rr "
   "rr rr rr rr xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx xx xx rr rr rr rr "
   "rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr rr rr "
   "rr rr rr rr xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx cc xx rr rr rr rr "
   "rr rr rr rr xx xx cc xx xx xx xx xx cc cc cc cc cc xx xx cc xx rr rr rr rr "
   "rr rr rr rr xx cc cc cc w+ cc xx xx xx xx cc xx xx xx xx cc xx xx xx xx xx "
   "rr rr rr rr xx cc cc cc w+ cc xx cc cc xx cc xx cc cc cc cc cc cc cc cc xx "
   "xx xx xx xx xx cc cc cc w+ cc xx cc cc xx cc xx cc [[ ]] cc [[ ]] cc cc xx "
   "cc cc cc cc cc cc cc cc w+ cc cc cc cc cc cc xx cc cc cc cc cc cc cc cc xx "
   "xx xx xx xx xx cc cc cc w+ cc xx cc cc xx cc xx cc [[ ]] cc [[ ]] cc cc xx "
   "rr rr rr rr xx cc cc cc w+ cc xx cc cc xx cc xx cc cc cc cc cc cc cc cc xx "
   "rr rr rr rr xx cc cc cc w+ cc xx xx xx xx cc xx xx xx xx cc xx xx cc xx xx "
   "rr rr rr rr xx xx cc xx xx xx xx xx cc cc cc cc cc xx xx cc xx cc cc cc xx "
   "rr rr rr rr xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx cc xx cc cc cc xx "
   "rr rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx cc cc cc xx "
   "rr rr rr rr xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx xx xx cc cc cc xx "
   "xx xx xx xx xx xx cc xx xx xx xx xx cc cc cc cc cc xx rr rr xx xx xx xx xx "
   "xx cc cc cc w+ cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
   "xx cc cc cc w+ cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
   "xx cc cc cc w+ cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
   "xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
   ))
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;; objects
 (list

  ;; mechanisms
  (put (mk-locked-windowed-door) 4 12)
  (put (mk-locked-windowed-door) 6 4)
  (put (kern-tag 'bgk-p1 (mk-portcullis)) 6 20)
  (put (kern-tag 'bgk-p2 (mk-portcullis)) 8 22)
  (put (kern-tag 'bgk-p3 (mk-portcullis)) 6 16)
  (put (kern-tag 'bgk-p4 (mk-portcullis)) 6 8)
  (put (mk-lever-on 'bgk-p1) 5 21)
  (put (mk-lever 'bgk-p2) 1 21)
  (put (mk-lever-on 'bgk-p3) 5 18)
  (put (mk-lever-on 'bgk-p4) 5 6)
  (put (mk-door) 7 6)
  (put (mk-door) 11 6)
  (put (mk-door) 14 9)
  (put (mk-door) 13 12)
  (put (mk-door) 10 12)
  (put (mk-door) 14 15)
  (put (mk-door) 11 18)
  (put (mk-door) 7 18)
  (put (mk-door) 17 18)
  (put (mk-door) 19 15)
  (put (mk-locked-door) 22 15)
  (put (mk-door) 19 9)
  (put (mk-door) 17 6)

  ;; ladders
  (put (mk-ladder-down 'p_ratling_warren 6 2) 6 2)
  (put (mk-ladder-down 'p_ratling_warren 6 22) 6 22)
  (put (mk-ladder-down 'p_ratling_warren 2 22) 2 22)
  (put (mk-ladder-down 'p_ratling_warren 22 19) 22 19)

  )

 nil ;; hooks

 ;; edge entrances
 (list
  (list west 24 22)
  (list south 22 0)
  )
 )

;;----------------------------------------------------------------------------
;; Cave Shrine
(mk-dungeon-room
 'p_cave_shrine "Cave Shrine"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr ,V ,I ,G ,I ,L ,A ,N ,C ,E rr rr rr rr rr "
  "rr rr rr rr rr !! !! !! !! !! !! !! !! !! rr rr rr rr rr "
  "rr rr rr gg gg gg gg gg gg gg gg gg gg gg gg gg rr rr rr "
  "rr rr rr gg gg gg dd bb dd dd dd bb dd gg gg gg rr rr rr "
  "rr rr gg gg gg gg dd dd dd dd dd dd dd gg gg gg gg rr rr "
  "rr rr gg gg dd dd dd dd dd dd dd dd dd dd dd gg gg rr rr "
  "rr rr gg gg bb dd dd dd dd dd dd dd dd dd bb gg gg rr rr "
  "rr !! gg gg dd dd dd dd gg gg gg dd dd dd dd gg gg !! rr "
  "rr !! gg gg dd dd dd dd gg aa gg dd dd dd dd gg gg !! rr "
  "rr !! gg gg dd dd dd dd gg gg gg dd dd dd dd gg gg !! rr "
  "rr rr gg gg bb dd dd dd dd gg dd dd dd dd bb gg gg rr rr "
  "rr rr gg gg dd dd dd dd dd gg dd dd dd dd dd gg gg rr rr "
  "rr rr gg gg gg gg dd dd dd gg dd dd dd gg gg gg gg rr rr "
  "rr rr rr gg gg gg dd bb dd gg dd bb dd gg gg gg rr rr rr "
  "rr rr rr gg gg gg gg gg gg gg gg gg gg gg gg gg rr rr rr "
  "rr rr rr rr rr gg gg gg gg gg gg gg gg gg rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr gg gg gg rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr gg gg gg rr rr rr rr rr rr rr rr "
  )
 (put (mk-anaxes) 9 8)
 )

;;----------------------------------------------------------------------------
;; Ratling Warren
(kern-mk-place 
 'p_ratling_warren     ; tag
 "Ratling Warren"      ; name
 nil
 (kern-mk-map 
  nil 25 25 pal_expanded
  (list
   "rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
   "rr rr rr rr xx cc cc cc xx rr rr bb dd dd dd bb rr rr rr bb dd dd bb rr rr "
   "rr rr rr rr xx cc cc cc xx rr dd dd dd dd dd dd dd rr bb dd dd dd dd bb rr "
   "rr rr rr rr xx cc cc cc xx rr rr bb dd dd dd bb rr rr dd dd dd dd dd dd rr "
   "rr rr rr rr xx xx cc xx xx rr rr rr rr dd rr rr rr rr dd dd dd dd dd dd rr "
   "rr rr rr rr %% .. dd dd dd bb rr rr bb dd bb rr rr rr bb dd dd dd dd dd rr "
   "rr rr dd dd .. dd dd %% dd dd dd dd dd dd dd dd bb rr rr rr dd dd dd bb rr "
   "rr dd dd bb dd dd %% bb dd bb dd dd dd dd dd dd dd dd bb rr rr dd rr rr rr "
   "rr rr dd .. dd dd rr rr rr rr .. .. dd dd dd dd dd dd dd dd dd dd rr rr rr "
   "rr rr rr %% .. rr rr rr rr bb %% %% .. dd dd dd dd dd bb rr rr rr rr rr rr "
   "rr rr rr rr rr rr rr rr rr dd .. .. dd dd dd dd dd dd rr rr rr rr rr rr rr "
   "rr rr rr rr rr rr rr rr bb dd dd dd dd dd dd dd dd dd rr rr rr rr rr rr rr "
   "rr rr %% .. dd dd dd dd dd dd dd dd dd dd dd dd dd dd rr rr rr rr rr rr rr "
   "rr rr .. rr rr rr rr rr dd dd dd dd dd dd dd dd dd dd rr rr rr rr rr rr rr "
   "rr rr dd rr rr rr rr rr rr dd dd dd dd dd dd dd dd dd bb rr rr rr rr rr rr "
   "rr rr dd rr rr rr rr rr rr dd .. .. dd dd dd dd dd dd dd dd dd dd rr rr rr "
   "rr rr dd rr rr rr rr rr rr bb %% %% .. dd dd dd dd dd bb rr rr dd rr rr rr "
   "rr rr dd rr rr rr rr rr rr rr %% .. dd dd dd dd dd dd rr rr rr dd dd rr rr "
   "rr rr dd rr rr rr rr rr rr rr .. dd dd dd dd dd dd rr rr rr dd dd dd dd rr "
   "rr rr dd rr rr rr rr rr rr rr bb dd dd dd dd dd bb rr rr rr dd dd dd dd rr "
   "xx bb dd bb xx rr rr xx xx rr rr rr bb dd bb rr rr rr rr rr dd dd dd dd rr "
   "xx cc cc cc w+ bb cc cc bb rr rr rr rr dd rr rr rr rr rr rr rr dd dd rr rr "
   "xx cc cc cc rr cc cc cc dd dd dd dd dd dd rr rr rr rr rr rr rr rr rr rr rr "
   "xx cc cc bb w+ cc cc cc bb rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
   "xx xx rr xx xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr " 
   ))
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;; objects
 (list

  ;; mechs
  (put (kern-tag 'rw-p1 (mk-portcullis)) 6 4)
  (put (mk-lever 'rw-p1) 7 1)

  ;; prisoners (long dead)
  (put (mk-corpse) 1 7)
  (put (mk-corpse) 5 6)
  (put (mk-corpse) 4 9)

  ;; ladders
  (put (mk-ladder-up 'p_brundegardt_keep 6 2) 6 2)
  (put (mk-ladder-up 'p_brundegardt_keep 6 22) 6 22)
  (put (mk-ladder-up 'p_brundegardt_keep 2 22) 2 22)
  (put (mk-ladder-up 'p_brundegardt_keep 22 19) 22 19)

  )

 nil ;; hooks
 nil ;; edge entrances
 )

;;----------------------------------------------------------------------------
;; Black River
(mk-dungeon-room
 'p_black_canal "Black Canal"
 (list
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "xx xx xx xx xx oo ee ee ee oo ee ee ee oo xx xx xx xx xx "
  "cc cc cc cc cc ee ee ee ee ee ee _s ee ee cc cc cc cc cc "
  "xx xx xx xx xx oo ee _s ee oo ee _s _s oo xx xx xx xx xx "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr == rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  "rr rr rr rr rr _s _s _s _s _s _s _s _s _s rr rr rr rr rr "
  ) 
 )

;;----------------------------------------------------------------------------
;; Tunnels
(mk-dungeon-room
 'p_tunnels "Tunnels"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx cc cc cc xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx cc pp cc xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx cc cc cc xx rr rr rr rr rr rr rr "
  "xx xx xx xx xx xx xx xx cc pp cc xx xx xx xx xx xx xx xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
  "xx xx xx xx xx xx xx xx cc pp cc xx xx xx xx xx xx xx xx "
  "rr rr rr rr rr rr rr xx cc cc cc xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx cc pp cc xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx cc cc cc xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-locked-windowed-door) 7 9)
 (put (mk-locked-windowed-door) 11 9)
 (put (mk-corpse-with-loot) 8 8)
 (put (mk-corpse-with-loot) 8 10)
 (put (mk-corpse-with-loot) 10 8)
 (put (mk-corpse-with-loot) 10 10)
 )

;;----------------------------------------------------------------------------
;; Chasm Drawbridge
(mk-dungeon-room
 'p_wide_chasm "Wide Chasm"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr vv vv vv vv vv rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr vv vv vv vv vv vv vv rr rr rr rr rr rr "
  "rr rr rr rr rr vv vv vv vv vv vv vv vv vv rr rr rr rr rr "
  "rr rr rr xx xx vv vv vv vv vv vv vv vv vv xx xx rr rr rr "
  "rr rr xx xx cc vv vv vv vv ee vv vv vv vv cc xx xx rr rr "
  "rr xx xx cc cc vv vv vv vv ee vv vv vv vv cc cc xx xx rr "
  "xx xx cc cc cc vv vv vv vv ee vv vv vv vv cc cc cc xx xx "
  "xx cc cc cc cc vv vv vv ee ee ee vv vv vv cc cc cc cc xx "
  "cc cc cc cc cc vv vv vv ee oo ee vv vv vv cc cc cc cc cc "
  "xx cc cc cc cc vv vv vv ee ee ee vv vv vv cc cc cc cc xx "
  "xx xx cc cc cc vv vv vv vv ee vv vv vv vv cc cc cc xx xx "
  "rr xx xx cc cc vv vv vv vv ee vv vv vv vv cc cc xx xx rr "
  "rr rr xx xx cc vv vv vv vv ee vv vv vv vv cc xx xx rr rr "
  "rr rr rr xx xx vv vv vv vv vv vv vv vv vv xx xx rr rr rr "
  "rr rr rr rr rr vv vv vv vv vv vv vv vv vv rr rr rr rr rr "
  "rr rr rr rr rr rr vv vv vv vv vv vv vv rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr vv vv vv vv vv rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

;;----------------------------------------------------------------------------
;; Tower L1
(mk-dungeon-room
 'p_brundegardt_tower_1 "BrundeGardt Tower L1"
 (list
  "rr rr rr rr rr xx xx xx xx xx xx xx xx xx rr rr rr rr rr "
  "rr rr rr xx xx xx cc cc cc cc cc cc cc xx xx xx rr rr rr "
  "rr rr xx xx cc cc cc cc cc cc cc cc cc cc cc xx xx rr rr "
  "rr xx xx cc cc cc cc pp cc cc cc pp cc cc cc cc xx xx rr "
  "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr "
  "xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc pp cc cc cc pp cc cc cc pp cc cc cc pp cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc pp cc cc cc pp cc cc cc pp cc cc cc pp cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx "
  "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr "
  "rr xx xx cc cc cc cc pp cc cc cc pp cc cc cc cc xx xx rr "
  "rr rr xx xx cc cc cc cc cc cc cc cc cc cc cc xx xx rr rr "
  "rr rr rr xx xx xx cc cc cc cc cc cc cc xx xx xx rr rr rr "
  "rr rr rr rr rr xx xx xx xx xx xx xx xx xx rr rr rr rr rr "
  )
 (put (mk-ladder-up 'p_brundegardt_tower_2 10 9) 10 9)
 )

;;----------------------------------------------------------------------------
;; Tower L2
(mk-dungeon-room
 'p_brundegardt_tower_2 "BrundeGardt Tower L2"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr xx xx xx xx xx xx xx rr rr rr rr rr rr "
  "rr rr rr rr xx xx xx cc cc cc cc cc xx xx xx rr rr rr rr "
  "rr rr rr xx xx cc cc cc cc cc cc cc cc cc xx xx rr rr rr "
  "rr rr xx xx cc cc cc pp cc cc cc pp cc cc cc xx xx rr rr "
  "rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
  "rr xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx rr "
  "rr xx cc cc pp cc cc pp cc cc cc pp cc cc pp cc cc xx rr "
  "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr "
  "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr "
  "rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr "
  "rr xx cc cc pp cc cc pp cc cc cc pp cc cc pp cc cc xx rr "
  "rr xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx rr "
  "rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
  "rr rr xx xx cc cc cc pp cc cc cc pp cc cc cc xx xx rr rr "
  "rr rr rr xx xx cc cc cc cc cc cc cc cc cc xx xx rr rr rr "
  "rr rr rr rr xx xx xx cc cc cc cc cc xx xx xx rr rr rr rr "
  "rr rr rr rr rr rr xx xx xx xx xx xx xx rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_1 10 9) 10 9)
 (put (mk-ladder-up 'p_brundegardt_tower_3 8 9) 8 9)
 )

;;----------------------------------------------------------------------------
;; Tower L3
(mk-dungeon-room
 'p_brundegardt_tower_3 "BrundeGardt Tower L3"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr xx xx xx cc cc cc xx xx xx rr rr rr rr rr "
  "rr rr rr rr xx xx cc cc cc cc cc cc cc xx xx rr rr rr rr "
  "rr rr rr xx xx cc cc pp cc cc cc pp cc cc xx xx rr rr rr "
  "rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc xx rr rr rr "
  "rr rr xx xx cc pp cc pp cc cc cc pp cc pp cc xx xx rr rr "
  "rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
  "rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
  "rr rr xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rr rr "
  "rr rr xx xx cc pp cc pp cc cc cc pp cc pp cc xx xx rr rr "
  "rr rr rr xx cc cc cc cc cc cc cc cc cc cc cc xx rr rr rr "
  "rr rr rr xx xx cc cc pp cc cc cc pp cc cc xx xx rr rr rr "
  "rr rr rr rr xx xx cc cc cc cc cc cc cc xx xx rr rr rr rr "
  "rr rr rr rr rr xx xx xx cc cc cc xx xx xx rr rr rr rr rr "
  "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_2 8 9) 8 9)
 (put (mk-ladder-up 'p_brundegardt_tower_4 10 9) 10 9)
 )

;;----------------------------------------------------------------------------
;; Tower L4
(mk-19x19-town
 'p_brundegardt_tower_4 "BrundeGardt Tower L4" nil
 (list
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. bb .. .. .. .. .. .. .. bb ^. ^. ^. ^. ^. "
  "^. ^. ^. bb .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. ^. "
  "^. ^. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. "
  "^. ^. .. .. .. .. xx w+ xx w+ xx w+ xx .. .. .. .. ^. ^. "
  "^. bb .. .. .. xx xx cc cc cc cc cc xx xx .. .. .. bb ^. "
  "^. .. .. .. xx xx cc cc cc cc cc cc cc xx xx .. .. .. ^. "
  "^. .. .. .. w+ cc cc pp cc cc cc pp cc cc w+ .. .. .. bb "
  "^. .. .. .. xx cc cc cc cc cc cc cc cc cc xx .. .. .. .. "
  "^. .. .. .. w+ cc cc cc cc cc cc cc cc cc cc .. .. .. .. "
  "^. .. .. .. xx cc cc cc cc cc cc cc cc cc xx .. .. .. .. "
  "^. .. .. .. w+ cc cc pp cc cc cc pp cc cc w+ .. .. .. bb "
  "^. .. .. .. xx xx cc cc cc cc cc cc cc xx xx .. .. .. ^. "
  "^. bb .. .. .. xx xx cc cc cc cc cc xx xx .. .. .. bb ^. "
  "^. ^. .. .. .. .. xx w+ xx w+ xx w+ xx .. .. .. .. ^. ^. "
  "^. ^. bb .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. "
  "^. ^. ^. bb .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. ^. "
  "^. ^. ^. ^. bb bb .. .. .. .. .. .. .. bb ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_3 10 9) 10 9)
 (put (mk-ladder-up 'p_brundegardt_tower_5 8 9) 8 9)
 )

;;----------------------------------------------------------------------------
;; Tower L5
(mk-19x19-town
 'p_brundegardt_tower_5 "BrundeGardt Tower L5" nil
 (list
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. bb .. .. .. .. .. .. .. bb ^. ^. ^. ^. ^. "
  "^. ^. ^. bb .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. ^. "
  "^. ^. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. "
  "^. ^. .. .. .. .. ee ee ee x. ee ee ee .. .. .. .. ^. ^. "
  "^. bb .. .. .. ee ee x. x. x. x. x. ee ee .. .. .. bb ^. "
  "^. .. .. .. ee ee x. x. cc cc cc x. x. ee ee .. .. .. ^. "
  "^. .. .. .. ee x. x. cc cc cc cc cc x. x. ee .. .. .. bb "
  "^. .. .. .. ee x. cc cc cc cc cc cc cc x. ee .. .. .. .. "
  "^. .. .. .. x. x. cc cc cc cc cc cc cc x. x. .. .. .. .. "
  "^. .. .. .. ee x. cc cc cc cc cc cc cc x. ee .. .. .. .. "
  "^. .. .. .. ee x. x. cc cc cc cc cc x. x. ee .. .. .. bb "
  "^. .. .. .. ee ee x. x. cc cc cc x. x. ee ee .. .. .. ^. "
  "^. bb .. .. .. ee ee x. x. x. x. x. ee ee .. .. .. bb ^. "
  "^. ^. .. .. .. .. ee ee ee x. ee ee ee .. .. .. .. ^. ^. "
  "^. ^. bb .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. "
  "^. ^. ^. bb .. .. .. .. .. .. .. .. .. .. .. bb ^. ^. ^. "
  "^. ^. ^. ^. bb bb .. .. .. .. .. .. .. bb ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_4 8 9) 8 9)
 )

;;----------------------------------------------------------------------------
;; Griffin Peak
(mk-19x19-town
 'p_griffin_peak_s "South Face of Griffin Peak" nil
 (list
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ bb .. .. .. bb ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ ^^ bb .. dd .. dd .. bb ^^ ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ bb .. dd dd dd .. dd .. bb ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ .. dd dd dd dd dd dd dd .. ^^ ^^ ^^ ^^ ^^ "
  "^^ ^^ ^^ ^^ ^^ bb dd dd dd dd dd dd dd bb ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^^ ^^ ^^ .. dd .. dd dd dd dd dd .. ^^ ^^ ^^ ^. ^. "
  ".. ^. ^. ^^ ^^ bb .. dd dd dd dd dd .. bb ^^ ^^ ^. ^. .. "
  ".. .. ^. ^. ^^ ^^ bb .. dd .. dd .. bb ^^ ^^ ^. ^. .. .. "
  ".. .. .. ^. ^. ^^ ^^ bb .. dd .. bb ^^ ^^ ^^ ^. .. .. .. "
  "^. .. .. .. ^. ^. ^^ ^^ ^^ bb ^^ ^^ ^^ ^^ ^. ^. .. .. ^. "
  "^. ^. .. .. .. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. .. ^. "
  "^. ^. ^. .. .. ^. ^. ^. ^^ ^^ ^^ ^^ ^. ^. .. .. .. .. ^. "
  "^. ^. ^. .. .. .. .. ^. ^. ^^ ^. ^. ^. .. .. .. bb .. ^. "
  "^. ^. ^. .. bb .. .. .. ^. ^. ^. .. .. .. .. bb .. .. ^. "
  "^. ^. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. "
  "^. ^. ^. .. ^. ^. ^. .. .. .. .. .. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
 (put (mk-corpse) 6 15)
 (put (mk-corpse) 11 15)
 (put (mk-corpse) 14 13)
 (put (mk-corpse) 7 5)
 (put (mk-corpse) 13 7)
 (put (mk-corpse) 8 8)
 (put (mk-corpse) 10 1)
 )

(mk-19x19-town
 'p_griffin_peak_se "Southeast Face of Griffin Peak" nil
 (list
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. .. ^. ^. ^. ^. {{ ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. ^. ^. ^. {{ {{ ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. ^. ^. ^. ^. ^. {{ {{ "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. .. ^. ^. ^. ^. ^. {{ ^. "
  "^. ^. ^. ^. ^. ^^ ^^ ^^ ^. .. .. .. ^. ^. ^. {{ {{ ^. ^. "
  "^. .. .. .. ^. ^. ^^ ^. ^. .. .. ^. ^. ^. ^. ^. {{ ^. ^. "
  ".. .. .. .. .. ^. ^. ^. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. "
  ".. .. .. .. .. .. .. .. .. .. .. .. ^. ^. ^. ^. ^. ^. ^. "
  ".. ^. ^. ^. .. .. .. .. .. .. ^. .. .. .. .. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. .. .. ^. ^. .. .. .. .. .. .. .. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. .. .. .. .. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. && .. .. .. ^. ^. "
  "^. {{ ^. ^. ^. ^. {{ ^. ^. ^. .. .. .. .. .. .. .. ^. ^. "
  "^. ^. {{ {{ {{ {{ ^. ^. ^. ^. ^. .. .. .. .. .. ^. ^. ^. "
  "^. {{ tt tt tt tt {{ ^. ^. ^. ^. ^. .. .. .. ^. ^. ^. ^. "
  "{{ tt tt |. |. tt tt {{ ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "{{ tt |. |. |. |. tt {{ ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. {{ "
  )
 )

(mk-19x19-town
 'p_griffin_peak_ne "Northeast Face of Griffin Peak" nil
 (list
  "^. ^. ^. {{ tt |. |. |. tt {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. {{ tt |. tt {{ ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. {{ tt {{ {{ ^. ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. {{ ^. {{ ^. {{ {{ {{ ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. {{ ^. ^. {{ ^. ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. {{ ^. ^. ^. ^. ^. ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. {{ ^. ^. ^. ^. ^. ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^^ "
  ".. .. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  ".. .. .. .. .. .. .. ^. ^. ^. ^. ^. ^. ^. {{ ^. ^. ^. ^. "
  ".. .. ^. ^. .. .. .. .. ^. ^. ^. ^. ^. {{ {{ ^. ^. {{ ^. "
  "^. ^. ^. ^. ^. ^. .. .. .. .. ^. ^. ^. ^. ^. {{ {{ ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^. ^. .. .. .. ^. ^. ^. ^. ^. {{ {{ ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. .. ^. ^. ^. ^. ^. {{ ^. ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. ^. ^. ^. ^. ^. {{ {{ ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. ^. ^. ^. ^. ^. {{ {{ ^. "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. ^. ^. ^. {{ ^. ^. {{ {{ "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. .. .. ^. ^. ^. ^. {{ {{ {{ {{ "
  "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. .. .. .. ^. ^. ^. ^. ^. ^. {{ {{ "
  )
 )

(mk-19x19-town
 'p_griffin_peak_n "North Face of Griffin Peak" nil
 (list
  "^. tt |. ~* |. tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. tt |. ~* |. tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. tt |. ~* ~* |. tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. tt ~* |. |. tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. tt ~* ~* ~* |. tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. tt |. ~* _! _! ~* tt ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. tt |. ~* _! _! ~* ~* ~* ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. tt tt ~* ~* ^. ^. ~* ~* ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ~* ^. ^. ^. ^. ~* ^. ^. ^. ^. ^. ^. ^. ^. .. "
  "^. ^. ^. ~* ~* ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. "
  "^. ^. ~* ~* ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. "
  "^. ^. ~* ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. .. .. ^. "
  "^. ^. ~* ~* ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. .. ^. ^. ^. "
  "^. ^. ^. ~* ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. ^. ^. ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. ^. ^. ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. ^. ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. ^. ^. ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. ^^ ^^ ^^ ^^ ^^ ^^ "
  "^. ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ "
  )
 )

;; Note: Griffin Peak is a copy of the South Face, but with different mountain
;; visibility to give the player the illusion that he has climbed up to the top
;; and is now looking down on the path he trod to get here. Object placement
;; will vary at runtime and may break the illusion, but try to initialize
;; object placement the same at the start of play.
(mk-19x19-town
 'p_griffin_peak "Griffin Peak" nil
 (list
  "^. ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. bb .. .. .. bb ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. bb .. dd .. dd .. bb ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. bb .. dd dd dd .. dd .. bb ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. .. dd dd dd dd dd dd dd .. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. bb dd dd dd dd dd dd dd bb ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. .. dd .. dd dd dd dd dd .. ^. ^. ^. ^. ^. "
  ".. ^. ^. ^. ^. bb .. dd dd dd dd dd .. bb ^. ^. ^. ^. .. "
  ".. .. ^. ^. ^. ^. bb .. dd .. dd .. bb ^. ^. ^. ^. .. .. "
  ".. .. .. ^. ^. ^. ^. bb .. dd .. bb ^. ^. ^. ^. .. .. .. "
  "^. .. .. .. ^. ^. ^. ^. ^. bb ^. ^. ^. ^. ^. ^. .. .. ^. "
  "^. ^. .. .. .. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. ^. "
  "^. ^. ^. .. .. ^. ^. ^. ^. ^. ^. ^. ^. ^. .. .. .. .. ^. "
  "^. ^. ^. .. .. .. .. ^. ^. ^. ^. ^. ^. .. .. .. bb .. ^. "
  "^. ^. ^. .. bb .. .. .. ^. ^. ^. .. .. .. .. bb .. .. ^. "
  "^. ^. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb ^. "
  "^. ^. ^. .. ^. ^. ^. .. .. .. .. .. ^. ^. ^. ^. ^. ^. ^. "
  "^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
 (put (mk-corpse) 6 15)
 (put (mk-corpse) 11 15)
 (put (mk-corpse) 14 13)
 (put (mk-corpse) 7 5)
 (put (mk-corpse) 13 7)
 (put (mk-corpse) 8 8)
 (put (mk-corpse) 10 1)
 )

;;----------------------------------------------------------------------------
;; Assemble the lower levels into a dungeon complex
(mk-dungeon-level 
 (list nil           p_cave_shrine     )
 (list p_brundegardt p_brundegardt_keep p_black_canal p_tunnels p_wide_chasm p_brundegardt_tower_1)
 )

;;----------------------------------------------------------------------------
;; Assemble the top levels into a complex
(mk-dungeon-level
 (list nil                   nil              p_griffin_peak_ne)
 (list p_brundegardt_tower_4 p_griffin_peak_s p_griffin_peak_se)
 )

;; Manually paste together the final spiral of griffin peak
(kern-place-set-neighbor west p_griffin_peak_ne p_griffin_peak_n)
(kern-place-set-neighbor south p_griffin_peak_n p_griffin_peak)