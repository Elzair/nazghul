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
  (list west 18 3)
  (list south 22 0)
  )
 )

(mk-dungeon-room
 'p_cave_shrine "Cave Shrine"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr gg gg gg rr rr rr rr rr rr rr "
  )
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
;; Assemble the levels into a dungeon complex
(mk-dungeon-level 
 (list nil           p_cave_shrine     )
 (list p_brundegardt p_brundegardt_keep)
 )

