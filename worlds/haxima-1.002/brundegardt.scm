(kern-load "anaxes.scm")

;;----------------------------------------------------------------------------
;; Entrance to Brundegardt
(kern-mk-place 
 'p_brundegardt     ; tag
 "Brundegardt"      ; name
 s_dungeon          ; sprite
 (kern-mk-map 
  nil 19 19 pal_expanded
  (list
		".. .. .. .. .. .. .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. .. .. .. .. .. .. .. .. {4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. .. .. .. .. .. .. .. .. {4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		".. .. .. .. .. .. .. .. .. {4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ {7 ^^ "
		".. .. .. .. .. .. .. tC t3 tt t5 ^^ ^^ ^^ ^^ ^c |& {c ^^ "
		".. .. .. .. .. .. tC t3 tt tt tt ^^ ^^ ^^ ^^ |# |C ^^ ^^ "
		".. .. .. .. tC t3 tt tt tt tt || ^a ^^ ^^ ^^ || ^3 ^^ ^^ "
		".. .. .. .. tb tt tt tt tt || || |% ^^ ^^ ^^ || ^^ ^^ ^^ "
		".. .. .. .. tD tt tt || || || || || ^a ^^ ^c || ^^ ^^ ^^ "
		".. .. tC t3 tt tt tt || || || || || |% ^e |# |C ^^ ^^ ^^ "
		".. .. t3 tt tt tt tt tt || || || || || || || ^3 ^^ ^^ ^^ "
		".. .. tt tt tt tt tt tt tc bb .. bb .. bb ^3 ^^ ^^ ^^ ^^ "
		".. .. tt tt || || tt tt t# .. .. .. .. {4 ^^ ^^ ^^ ^^ ^^ "
		".. .. tt tt || || || tt .. .. .. .. .. .. {5 ^^ ^^ ^^ ^^ "
		".. .. tt tt || || || || bb .. .. && .. .. {4 ^^ ^^ ^^ ^^ "
		".. .. tt tt tt || || || .. .. .. .. .. .. {c ^^ ^^ ^^ ^^ "
		".. .. ta tt tt || || || bb .. .. .. .. {4 ^^ ^^ ^^ ^^ ^^ "
		".. .. t% tt tt tt || || || bb .. bb .. bb ^^ ^^ ^^ ^^ ^^ "
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
  (put (mk-monman) 0 0)
  (put (spawn-pt 'troll) 12 14)
  (put (spawn-pt 'troll) 11 15)
  (put (let ((kriddle (mk-riddle 'noor 't_doorway 18 3 1 1 #t
                                 "All who would pass must speak the password!")))
         (riddle-set-rm-on-wrong! (gob kriddle) #f)
         kriddle)
       17 3)
  )

 (list 'on-entry-to-dungeon-room) ; hooks

 ;; edge entrances
 (list
  (list west 18 3)
  )
 )

(mk-place-music p_brundegardt 'ml-dungeon-adventure)

;;----------------------------------------------------------------------------
;; Brundegardt Keep
(kern-mk-place 
 'p_brundegardt_keep     ; tag
 "Brundegardt Keep"      ; name
 nil
 (kern-mk-map 
  nil 25 25 pal_expanded
  (list
		"rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn rn rn rn rn r4 gg gg gg r2 "
		"rn rn rn rn xx cc cc cc xx r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 rn rn r5 gg r3 rn "
		"rn rn rn rn xx cc cc cc cc gg gg gg gg gg gg gg gg gg gg r2 rn r4 gg r2 rn "
		"rn rn rn rn xx cc cc cc xx r1 r1 xx xx xx xx xx xx xx gg ra r8 rc gg r2 rn "
		"rn rn rn rn xx xx cc xx xx xx xx xx cc cc cc cc cc xx gg gg gg gg gg r2 rn "
		"rn rn rn rn xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx xx xx xx xx xx xx "
		"rn rn rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx cc cc cc xx "
		"rn rn rn rn xx xx gg xx cc cc cc xx cc cc cc cc cc xx xx cc cc cc cc cc xx "
		"rn rn rn rn xx xx cc xx xx xx xx xx cc cc cc cc cc xx xx cc xx cc cc cc xx "
		"rn rn xx xx xx cc cc cc w+ cc xx xx xx xx cc xx xx xx xx cc xx xx xx xx xx "
		"rn xx xx cc cc cc cc cc w+ cc xx xx xx xx cc xx cc cc cc cc cc cc cc cc xx "
		"xx xx cc cc cc cc cc cc w+ cc xx cc cc xx cc xx cc [[ ]] cc [[ ]] cc cc xx "
		"cc cc cc cc cc cc cc cc w+ cc cc cc cc cc cc xx cc cc cc cc cc cc cc cc xx "
		"xx xx cc cc cc cc cc cc w+ cc xx cc cc xx cc xx cc [[ ]] cc [[ ]] cc cc xx "
		"rn xx xx cc cc cc cc cc w+ cc xx xx xx xx cc xx cc cc cc cc cc cc cc cc xx "
		"rn rn xx xx xx cc cc cc w+ cc xx xx xx xx cc xx xx xx xx cc xx xx cc xx xx "
		"rn rn rn rn xx xx cc xx xx xx xx xx cc cc cc cc cc xx xx cc xx cc cc cc xx "
		"rn rn rn rn xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx cc xx cc cc cc xx "
		"rn rn rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx cc cc cc xx "
		"rn rn rn rn xx xx cc xx cc cc cc xx cc cc cc cc cc xx xx xx xx cc cc cc xx "
		"xx xx xx xx xx xx cc xx xx xx xx xx cc cc cc cc cc xx rn rn xx xx xx xx xx "
		"xx cc cc cc w+ cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
		"xx cc cc cc w+ cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
		"xx cc cc cc w+ cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
		"xx xx xx xx xx xx xx xx xx rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   ))
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;; objects
 (list
  (put (mk-monman) 0 0)

  ;; mechanisms
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
  (put (mk-magic-locked-door) 20 7)

  ;; ladders
  (put (mk-ladder-down 'p_ratling_warren 6 2) 6 2)
  (put (mk-ladder-down 'p_ratling_warren 6 22) 6 22)
  (put (mk-ladder-down 'p_ratling_warren 2 22) 2 22)
  (put (mk-ladder-down 'p_ratling_warren 22 19) 22 19)

  ;; npc generators
  (put (spawn-pt 'skeletal-warrior) 6 14)
  (put (spawn-pt 'skeletal-warrior) 6 10)
  (put (spawn-pt 'skeletal-warrior) 6 22)
  (put (spawn-pt 'skeletal-warrior) 6 2)
  (put (spawn-pt 'skeletal-archer) 9 10)
  (put (spawn-pt 'skeletal-archer) 9 10)
  (put (spawn-pt 'skeletal-archer) 3 22)
  (put (spawn-pt 'ghast) 9 6)
  (put (spawn-pt 'ghast) 14 6)
  (put (spawn-pt 'ghast) 12 12)
  (put (spawn-pt 'ghast) 9 18)
  (put (spawn-pt 'ghast) 14 18)
  (put (spawn-pt 'ghast) 18 12)
  (put (spawn-pt 'ghast) 21 12)

  ;; loot
  (put (kern-mk-obj t_sword 1) 8 5)
  (put (kern-mk-obj t_sword 1) 9 5)
  (put (kern-mk-obj t_sword 1) 10 5)
  (put (kern-mk-obj t_shield 1) 8 7)
  (put (kern-mk-obj t_shield 1) 9 7)
  (put (kern-mk-obj t_shield 1) 10 7)
  (put (kern-mk-obj t_sword 1) 8 17)
  (put (kern-mk-obj t_sword 1) 9 17)
  (put (kern-mk-obj t_sword 1) 10 17)
  (put (kern-mk-obj t_shield 1) 8 19)
  (put (kern-mk-obj t_shield 1) 9 19)
  (put (kern-mk-obj t_shield 1) 10 19)
  (put (kern-mk-obj t_bow 1) 11 11)
  (put (kern-mk-obj t_bow 1) 12 11)
  (put (kern-mk-obj t_arrow 20) 11 13)
  (put (kern-mk-obj t_arrow 20) 12 13)
  (put (mk-chest nil
                 '((10 t_food))) 21 16)
  (put (mk-mimic) 21 17)
  (put (mk-mimic) 16 4)
  (put (mk-chest nil
                 '((5 t_wine))) 23 16)
  (put (mk-chest nil
                 '((5 garlic)
                   (5 ginseng))) 23 17)
  (put (mk-chest 'bomb-trap
                 '((1 t_an_tym_scroll)
                   (1 t_in_mani_corp_scroll)
                   (1 t_wis_quas_scroll)
                   (1 t_wis_an_ylem_scroll))) 23 6)
  (put (mk-chest 'lightning-trap
                 '((5 blood_moss)
                   (5 black_pearl)
                   (3 nightshade)
                   (3 mandrake))) 23 7)
  (put (mk-chest 'poison-trap
                 '((1 t_staff)
                   (1 t_dagger)
                   (1 t_slime_vial)
                   (3 t_heal_potion)
                   (3 t_mana_potion))) 23 8)
  )

 (list 'on-entry-to-dungeon-room) ; hooks

 ;; edge entrances
 (list
  (list west 24 22)
  (list south 22 0)
  )
 )

(mk-place-music p_brundegardt_keep 'ml-dungeon-adventure)

;;----------------------------------------------------------------------------
;; Cave Shrine
(mk-dungeon-room
 'p_cave_shrine "Cave Shrine"
 (list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn ,V ,I ,G ,I ,L ,A ,N ,C ,E rn rn rn rn rn "
		"rn rn rn r8 rc !! !! !! !! !! !! !! !! !! ra r8 rn rn rn "
		"rn rn r4 gg gg gg gg gg gg gg gg gg gg gg gg gg r2 rn rn "
		"rn rn rc gg gg gg dd bb dd dd dd bb dd gg gg gg ra rn rn "
		"rn r4 gg gg gg gg dd dd dd dd dd dd dd gg gg gg gg r2 rn "
		"rn r4 gg gg dd dd dd dd dd dd dd dd dd dd dd gg gg r2 rn "
		"rn rc gg gg bb dd dd dd dd dd dd dd dd dd bb gg gg ra rn "
		"r4 !! gg gg dd dd dd dd gg gg gg dd dd dd dd gg gg !! r2 "
		"r4 !! gg gg dd dd dd dd gg aa gg dd dd dd dd gg gg !! r2 "
		"r4 !! gg gg dd dd dd dd gg gg gg dd dd dd dd gg gg !! r2 "
		"rn r5 gg gg bb dd dd dd dd gg dd dd dd dd bb gg gg r3 rn "
		"rn r4 gg gg dd dd dd dd dd gg dd dd dd dd dd gg gg r2 rn "
		"rn r4 gg gg gg gg dd dd dd gg dd dd dd gg gg gg gg r2 rn "
		"rn rn r5 gg gg gg dd bb dd gg dd bb dd gg gg gg r3 rn rn "
		"rn rn r4 gg gg gg gg gg gg gg gg gg gg gg gg gg r2 rn rn "
		"rn rn rn r1 r5 gg gg gg gg gg gg gg gg gg r3 r1 rn rn rn "
		"rn rn rn rn rn r1 r1 r5 gg gg gg r3 r1 r1 rn rn rn rn rn "
		"rn rn rn rn rn rn rn r4 gg gg gg r2 rn rn rn rn rn rn rn "
  )
 (put (mk-anaxes) 9 8)
 )

(mk-place-music p_cave_shrine 'ml-dungeon-adventure)
 
;;----------------------------------------------------------------------------
;; Ratling Warren
(kern-mk-place 
 'p_ratling_warren     ; tag
 "Ratling Warren"      ; name
 nil
 (kern-mk-map 
  nil 25 25 pal_expanded
  (list
		"rn rn rn rn xx xx xx xx xx rn rn r8 r8 r8 r8 r8 rn rn rn r8 r8 r8 r8 rn rn "
		"rn rn rn rn xx cc cc cc xx rn rc bb dd dd dd bb ra rn rc bb dd dd bb ra rn "
		"rn rn rn rn xx cc cc cc xx r4 dd dd dd dd dd dd dd r6 bb dd dd dd dd bb r2 "
		"rn rn rn rn xx cc cc cc xx rn rd bb dd dd dd bb r3 r4 dd dd dd dd dd dd r2 "
		"rn rn rn rn xx xx cc xx xx rc dd dd dd dd bb r3 rn r4 dd dd dd dd dd dd r2 "
		"rn rn r8 rc %f .. dd .. dd dd dd dd bb dd dd r2 r8 r4 bb dd dd dd dd dd r2 "
		"rn rc dd dd .. dd .. %f .. dd r3 r9 r9 r9 r9 rc bb ra rd bb dd dd dd bb r2 "
		"r4 dd dd bb dd .. %f bb .. r3 rc dd dd bb bb dd dd dd dd dd dd dd dd r3 rn "
		"rn r5 dd .. dd dd r3 r1 r1 rc dd dd dd dd dd dd dd dd dd dd dd dd dd r2 rn "
		"rn rn r5 %f .. r3 rn rn rc dd dd dd dd dd dd dd dd dd dd dd dd dd bb r2 rn "
		"rn rn r8 r9 r9 r8 r8 rc bb dd dd dd dd dd dd dd dd dd bb r3 r1 r1 r1 rn rn "
		"rn rc %7 .. dd dd dd dd dd dd dd dd dd dd dd dd dd dd r3 rn rn rn rn rn rn "
		"r4 %b %c .. dd dd dd dd dd dd dd dd dd dd dd dd dd dd r2 rn rn rn rn rn rn "
		"r4 .. .. dd dd dd dd dd dd dd dd dd dd dd dd dd dd dd ra rn rn rn rn rn rn "
		"r4 dd dd dd dd r3 r1 r5 bb dd dd dd dd dd dd dd dd dd bb r2 rn rn rn rn rn "
		"r4 dd dd dd r3 rn rn rn r5 .. .. .. dd dd dd dd dd dd dd r2 r8 r8 rn rn rn "
		"r4 dd dd .. r2 rn rn rn r4 bb %3 %d .. dd dd dd dd dd bb r6 dd dd ra rn rn "
		"r4 dd .. %f r2 rn rn rn rn r5 %e .. dd dd dd dd dd dd r3 rc dd dd dd ra rn "
		"r4 dd dd .. r2 rn rn rn rn r4 .. dd dd dd dd dd dd r3 r4 dd dd dd dd dd r2 "
		"r4 bb dd bb r2 rn rn rn rn r4 bb dd dd dd dd dd bb r2 r4 dd dd dd dd dd r2 "
		"xx rd dd rb xx r8 r8 xx xx r8 rd bb dd dd dd r3 r1 rn r4 dd dd dd dd dd r2 "
		"xx cc cc cc w+ bb cc cc re bb dd dd dd dd dd r2 rn rn rn r5 dd dd dd dd r2 "
		"xx cc cc cc rf cc cc cc dd dd dd dd dd .. dd r2 rn rn rn rn r5 dd dd r3 rn "
		"xx cc cc bb w+ cc cc cc r7 bb dd dd .. %f bb r2 rn rn rn rn rn r1 r1 rn rn "
		"xx xx r1 xx xx xx xx xx xx r1 r1 r1 r1 r1 r1 rn rn rn rn rn rn rn rn rn rn "
   ))
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 ;; objects
 (list
  (put (mk-monman) 0 0)

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

  ;; loot chamber
  (put (mk-chest nil '((5 t_gem))) 20 16)
  (put (mk-chest nil '((100 t_gold_coins))) 21 16)
  (put (mk-chest nil '((10 sulphorous_ash))) 19 18)
  (put (mk-mimic) 19 19)
  (put (mk-chest nil '((10 ginseng))) 19 20)
  (put (mk-chest nil '((10 garlic))) 20 21)
  (put (spawn-pt 'skeletal-warrior) 20 19)

  ;; ratlings!
  (put (guard-pt 'ratling-sorcerer) 13 13)
  (put (guard-pt 'ratling-sorcerer) 12 13)
  (put (guard-pt 'ratling-sorcerer) 14 14)
  (put (spawn-pt 'ratling-swarmer) 2 22)
  (put (spawn-pt 'ratling-swarmer) 20 4)
  (put (spawn-pt 'ratling-swarmer) 2 22)
  (put (spawn-pt 'ratling-swarmer) 13 15)

  ;; ratling stash
  (put (kern-mk-obj t_heal_potion 1) 20 1)
  (put (kern-mk-obj t_torch 1) 21 1)
  (put (kern-mk-obj t_dagger 1) 19 2)
  (put (kern-mk-obj garlic 1) 20 2)
  (put (kern-mk-obj t_cure_potion 1) 21 2)
  (put (kern-mk-obj t_food 1) 22 2)
  (put (kern-mk-obj t_gold_coins 1) 18 3)
  (put (kern-mk-obj t_in_an_scroll 1) 19 3)
  (put (kern-mk-obj ginseng 1) 20 3)
  (put (kern-mk-obj t_picklock 1) 21 3)
  (put (kern-mk-obj t_bolt 1) 22 3)
  (put (mk-corpse) 23 3)

  (put (kern-mk-obj t_int_potion 1) 18 4)
  (put (mk-broken-clock s_clock_hand_s s_clock_hand_n "The clock reads 6:00") 19 4)
  (put (kern-mk-obj t_mana_potion 1) 20 4)
  (put (kern-mk-obj t_gem 1) 21 4)
  (put (kern-mk-obj t_gold_coins 2) 22 4)
  (put (kern-mk-obj t_arrow 1) 23 4)

  (put (kern-mk-obj t_wine 1) 19 5)
  (put (kern-mk-obj t_poison_immunity_potion 1) 20 5)
  (put (kern-mk-obj t_pick 1) 21 5)
  (put (kern-mk-obj t_oil 1) 22 5)

  )
  (list 'on-entry-to-dungeon-room) ; hooks
 nil ;; edge entrances
 )

(mk-place-music p_ratling_warren 'ml-dungeon-adventure)
 
;;----------------------------------------------------------------------------
;; Tunnel Turn
(kern-mk-place 
 'p_tunnel_turn "Tunnel Turn"
 nil
 (kern-mk-map 
  nil 19 19 pal_expanded
  (list
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn xx xx xx xx xx xx xx rn rn rn rn rn rn "
   "rn rn rn rn rn rn xx cc cc cc cc cc xx rn rn rn rn rn rn "
   "rn rn rn rn rn rn xx cc pp cc pp cc xx xx xx xx xx xx xx "
   "rn rn rn rn rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc "
   "rn rn rn rn rn rn xx cc pp cc pp cc xx xx xx xx xx xx xx "
   "rn rn rn rn rn rn xx cc cc cc cc cc xx rn rn rn rn rn rn "
   "rn rn rn rn rn rn xx xx xx cc xx xx xx rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn xx cc xx rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn xx cc xx rn rn rn rn rn rn rn rn "
   "xx xx xx xx xx xx xx xx xx cc xx rn rn rn rn rn rn rn rn "
   "cc cc cc cc cc cc cc cc cc cc xx rn rn rn rn rn rn rn rn "
   "xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn rn rn rn rn "
   "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
   ))
 #f                 ; wraps
 #t                 ; underground
 #f                 ; large-scale (wilderness)
 #f                 ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list
  (put (mk-monman) 0 0)
  (put (mk-windowed-door) 9 12)
  (put (mk-windowed-door) 12 9)
  (put (spawn-pt 'carabid) 9 9)
  )
  (list 'on-entry-to-dungeon-room) ; hooks
 ;; edge entrances
 (list
  (list east 0 16)
  )
 )

(mk-place-music p_tunnel_turn 'ml-dungeon-adventure)
 
;; randomly scatter gems through the rocks
(put-random-stuff p_tunnel_turn
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_nat_rock))
                  (lambda (loc)
                    (kern-obj-put-at (kern-mk-obj t_gem 1)
                                     loc))
                  10)         
                  
;;----------------------------------------------------------------------------
;; Black River
(mk-dungeon-room
 'p_black_canal "Black Canal"
 (list
		"rn rn r8 r8 r8 rn rn r8 r8 r8 r8 r8 rn rn rn rn rn rn rn "
		"rn rc gg gg gg ra rc bb ~s _s _s _s ra rn rn rn rn rn rn "
		"r4 gg gg bb gg gg ~s ~s ~s bb ~s _s _s ra rn rn rn rn rn "
		"r4 gg gg gg gg ~s _s _s ~s ~s ~s _s _s _s ra r8 rn rn rn "
		"rn r5 gg gg ~s _s _s _s _s _s ~s bb ~s _s _s _s r2 rn rn "
		"rn rn r1 rd _s _s _s _s _s _s ~s ~s ~s bb ~s _s r2 rn rn "
		"rn rn rc _s _s _s _s _s _s _s _s _s ~s ~s ~s _s r2 rn rn "
		"xx xx _s _s _s _s _s _s _s _s _s _s _s ~s _s _s xx xx rn "
		"xx ee ee oo ee ee ee oo ~s ee ee oo ee ee oo ee ee xx xx "
		"cc ee ee ee ee ~s ee ee ee ee ee ee ee ee ee ee ee cc cc "
		"xx ee ee oo ee ee ee oo ee ee ee oo ~s ee oo ee ee xx xx "
		"xx xx _s ~s _s _s _s ~s _s _s _s ~s _s _s ~s _s xx xx rn "
		"rn rn r5 ~s bb ~s _s _s _s _s _s _s _s _s _s rb rn rn rn "
		"rn rn rc ~s ~s ~s bb ~s _s _s _s _s _s _s _s ~s ra rn rn "
		"rn r4 ~s _s _s _s ~s _s _s _s _s ~s bb ~s _s _s ~s r2 rn "
		"rn r4 _s _s _s _s _s _s _s _s _s _s ~s _s _s _s _s r2 rn "
		"rn rn r5 _s _s _s _s _s _s _s _s _s _s _s _s _s r3 rn rn "
		"rn rn rn r1 r5 _s _s r3 r1 r1 r1 r5 _s _s r3 r1 rn rn rn "
		"rn rn rn rn rn r1 r1 rn rn rn rn rn r1 r1 rn rn rn rn rn "
  ) 
 (put (spawn-pt 'sludge-kraken) 9 4)
 )

(mk-place-music p_black_canal 'ml-dungeon-adventure)

;; random loot corpses
(put-random-stuff p_black_canal
                  (mk-rect 1 1 4 4)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_gravel))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  8)
                  
;;----------------------------------------------------------------------------
;; Tunnels
(mk-dungeon-room
 'p_tunnels "Tunnels"
 (list
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx cc cc cc xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx cc pp cc xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx cc cc cc xx rn rn rn rn rn rn rn "
	  "xx xx xx xx xx xx xx xx cc pp cc xx xx xx xx xx xx xx xx "
	  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
	  "xx xx xx xx xx xx xx xx cc pp cc xx xx xx xx xx xx xx xx "
	  "rn rn rn rn rn rn rn xx cc cc cc xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx cc pp cc xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx cc cc cc xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  )
 (put (mk-locked-windowed-door) 7 9)
 (put (mk-locked-windowed-door) 11 9)
 (put (mk-corpse-with-loot) 8 8)
 (put (mk-corpse-with-loot) 8 10)
 (put (mk-corpse-with-loot) 10 8)
 (put (mk-corpse-with-loot) 10 10)
 (put (spawn-pt 'carabid) 9 7)
 (put (spawn-pt 'carabid) 9 11)
 )

(mk-place-music p_tunnels 'ml-dungeon-adventure)

;; randomly scatter gems through the rocks
(put-random-stuff p_tunnels
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_nat_rock))
                  (lambda (loc)
                    (kern-obj-put-at (kern-mk-obj t_gem 1)
                                     loc))
                  10)

;;----------------------------------------------------------------------------
;; Chasm Drawbridge
(mk-dungeon-room
 'p_wide_chasm "Wide Chasm"
 (list
		"rn rn rn rn rn rn rn r8 r8 r8 r8 r8 rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rc *3 *1 *1 *1 *5 ra rn rn rn rn rn rn "
		"rn rn rn rn rn rc *3 vv vv vv vv vv *5 ra rn rn rn rn rn "
		"rn rn rn rn r4 *3 vv vv vv vv vv vv vv *5 r2 rn rn rn rn "
		"rn rn rn xx xx *2 vv vv vv vv vv vv vv *4 xx xx rn rn rn "
		"rn rn xx xx cc *2 vv vv vv ee vv vv vv *4 cc xx xx rn rn "
		"rn xx xx cc cc *2 vv vv *4 ee *2 vv vv *4 cc cc xx xx rn "
		"xx xx cc cc cc *2 vv vv *c ee *a vv vv *4 cc cc cc xx xx "
		"xx cc cc cc cc *2 vv vv ee ee ee vv vv *4 cc cc cc cc xx "
		"cc cc cc cc cc *2 vv *4 ee oo ee *2 vv *4 cc cc cc cc cc "
		"xx cc cc cc cc *2 vv vv ee ee ee vv vv *4 cc cc cc cc xx "
		"xx xx cc cc cc *2 vv vv *5 ee *3 vv vv *4 cc cc cc xx xx "
		"rn xx xx cc cc *2 vv vv *4 ee *2 vv vv *4 cc cc xx xx rn "
		"rn rn xx xx cc *2 vv vv vv ee vv vv vv *4 cc xx xx rn rn "
		"rn rn rn xx xx *2 vv vv vv vv vv vv vv *4 xx xx rn rn rn "
		"rn rn rn rn r4 *a vv vv vv vv vv vv vv *c r2 rn rn rn rn "
		"rn rn rn rn rn r5 *a vv vv vv vv vv *c r3 rn rn rn rn rn "
		"rn rn rn rn rn rn r5 *a *8 *8 *8 *c r3 rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn r1 r1 r1 r1 r1 rn rn rn rn rn rn rn "
  )
  (put (mk-weather-vane) 9 9)
  (put (mk-wind-bridge) 5 5)
  (put (spawn-pt 'bat) 9 8)
  (put (spawn-pt 'bat) 9 9)
  (put (spawn-pt 'bat) 8 8)
  (put (spawn-pt 'bat) 8 10)
  (put (spawn-pt 'bat) 10 9)
  (put (spawn-pt 'bat) 10 10)
  (put (mk-chest 'spike-trap
       '((3 sulphorous_ash)
         (3 blood_moss))) 14 7)
 )

(mk-place-music p_wide_chasm 'ml-dungeon-adventure)

(kern-place-add-on-entry-hook p_wide_chasm 'set-wind-north)

;;----------------------------------------------------------------------------
;; Tower L1
(mk-dungeon-room
 'p_brundegardt_tower_1 "BrundeGardt Tower L1"
 (list
  "rn rn rn rn rn xx xx xx xx xx xx xx xx xx rn rn rn rn rn "
  "rn rn rn xx xx xx xx xx cc cc cc xx xx xx xx xx rn rn rn "
  "rn rn xx xx xx cc cc xx cc cc cc xx cc cc cc cc xx rn rn "
  "rn xx xx cc cc cc cc xx cc cc cc cc cc cc xx cc xx xx rn "
  "rn xx xx cc xx xx xx xx xx cc xx xx xx xx xx cc cc xx rn "
  "xx xx cc cc xx cc cc xx cc cc cc xx cc cc xx cc cc xx xx "
  "xx xx cc cc xx cc cc xx cc cc cc cc cc cc xx cc cc xx xx "
  "xx xx xx cc xx xx cc xx xx xx xx xx cc xx xx xx xx xx xx "
  "xx cc cc cc xx cc cc cc cc cc cc xx cc cc xx cc cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc xx cc cc xx cc cc cc xx "
  "xx cc cc cc xx cc cc cc cc cc cc xx cc cc xx cc cc cc xx "
  "xx xx xx cc xx xx xx xx xx xx xx xx xx cc xx xx cc xx xx "
  "xx xx cc cc xx cc cc xx cc cc cc cc cc cc xx cc cc xx xx "
  "xx xx cc cc cc cc cc cc cc cc cc xx cc cc xx cc cc xx xx "
  "rn xx ?? xx xx xx xx xx xx cc xx xx xx xx xx cc cc xx rn "
  "rn xx cc xx cc cc cc xx cc cc cc xx cc cc xx cc xx xx rn "
  "rn rn xx xx cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
  "rn rn rn xx xx xx xx xx cc cc cc xx xx xx xx xx rn rn rn "
  "rn rn rn rn rn xx xx xx xx xx xx xx xx xx rn rn rn rn rn "
  )

 ;; doors
 (put (mk-door) 4 9)
 (put (mk-door) 3 7)
 (put (mk-door) 3 11)
 (put (mk-door) 4 13)
 (put (mk-windowed-door) 7 13)
 (put (mk-door) 11 16)
 (put (mk-locked-door) 14 16)
 (put (mk-door) 15 15)
 (put (mk-magic-locked-door) 16 11)
 (put (mk-door) 11 12)
 (put (mk-door) 13 11)
 (put (mk-windowed-door) 12 7)
 (put (mk-windowed-door) 11 6)
 (put (mk-locked-door) 9 4)
 (put (mk-door) 11 3)
 (put (mk-locked-windowed-door) 15 3)
 (put (mk-door) 3 4)

 (put (mk-portcullis) 7 8)
 (put (mk-portcullis) 7 10)
 (put (kern-tag 'bt1_p1 (mk-portcullis)) 7 9)
 (put (kern-tag 'bt1_p2 (mk-portcullis)) 4 3)
 (put (kern-tag 'bt1_p3 (mk-portcullis)) 7 16)
 (put (kern-tag 'bt1_p4 (mk-portcullis)) 6 7)
 
 (put (mk-lever 'bt1_p1) 16 6)
 (put (mk-lever 'bt1_p3) 8 10)
 (put (mk-lever 'bt1_p4) 2 15)

 (put (mk-ladder-up 'p_brundegardt_tower_2 10 9) 10 9)

 (put (spawn-pt 'yellow-slime) 8 16)
 (put (spawn-pt 'yellow-slime) 10 16)
 (put (spawn-pt 'yellow-slime) 9 13)

 (put (spawn-pt 'ratling-sorcerer) 15 12)
 (put (spawn-pt 'ratling-sorcerer) 16 12)

 (put (spawn-pt 'insect) 5 12)
 (put (spawn-pt 'insect) 6 12)
 (put (spawn-pt 'insect) 5 13)
 (put (spawn-pt 'insect) 6 13)

 (put (guard-pt 'skeletal-warrior) 5 8)
 (put (guard-pt 'skeletal-warrior) 5 10)
 (put (guard-pt 'skeletal-spear-thrower) 8 9)
 (put (guard-pt 'skeletal-spear-thrower) 9 9)

 (put (spawn-pt 'giant-spider) 8 12)
 (put (spawn-pt 'giant-spider) 8 6)

 (put (spawn-pt 'rat) 2 5)
 (put (spawn-pt 'rat) 3 5)

 (put (spawn-pt 'giant-spider) 9 2)
 (put (spawn-pt 'giant-spider) 8 1)

 (put (spawn-pt 'giant-spider) 13 3)

 (put (mk-corpse-with-loot) 5 5)
 (put (mk-corpse-with-loot) 5 2)
 (put (mk-corpse-with-loot) 6 3)
 (put (mk-corpse-with-loot) 5 16)
 (put (mk-corpse-with-loot) 2 9)

 ;; demon room
 (put (kern-mk-obj F_energy_perm 1) 15 9)
 (put (kern-mk-obj F_energy_perm 1) 16 9)
 (put (kern-mk-obj F_energy_perm 1) 17 9)
 (put (mk-npc 'demon 4) 16 8)
 (put (mk-chest 'bomb-trap
                '((1 t_an_tym_scroll)
                  (1 t_in_mani_corp_scroll)
                  (1 t_xen_corp_scroll)
                  (1 t_sanct_lor_scroll)
                  (100 t_gold_coins)))
      15 8)
 (put (mk-chest 'poison-trap
                '((1 t_str_potion)
                  (1 t_dex_potion)
                  (1 t_int_potion)
                  (100 t_gold_coins)))
      17 8)
 )

(mk-place-music p_brundegardt_tower_1 'ml-dungeon-adventure)

;; random loot corpses
(put-random-stuff p_brundegardt_tower_1
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_cobblestone))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  5)
                  
;;----------------------------------------------------------------------------
;; Tower L2
(mk-dungeon-room
 'p_brundegardt_tower_2 "BrundeGardt Tower L2"
 (list
		"rn rn r8 r8 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rc bb bb ra r8 xx xx xx xx xx xx xx rn rn rn rn rn rn "
		"r4 bb dd dd bb bb dd cc cc cc cc cc xx xx xx rn rn rn rn "
		"r4 bb dd dd dd dd cc cc cc cc cc cc cc cc xx xx rn rn rn "
		"rn r5 bb dd && dd cc pp cc cc cc pp cc cc cc xx xx rn rn "
		"rn r4 bb dd dd dd cc cc cc cc cc cc cc cc cc cc xx rn rn "
		"rn xx dd cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx rn "
		"rn xx cc cc pp cc cc pp cc cc cc pp cc cc pp cc cc xx rn "
		"rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn "
		"rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn "
		"rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn "
		"rn xx cc cc pp cc cc pp cc cc cc pp cc cc pp cc cc xx rn "
		"rn xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx rn "
		"rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
		"rn rn xx xx cc cc cc pp cc cc cc pp cc cc cc xx xx rn rn "
		"rn rn rn xx xx cc cc cc cc cc cc cc cc cc xx xx rn rn rn "
		"rn rn rn rn xx xx xx cc cc cc cc cc xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn xx xx xx xx xx xx xx rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_1 10 9) 10 9)
 (put (mk-ladder-up 'p_brundegardt_tower_3 8 9) 8 9)
 (put (spawn-pt 'troll-geomancer) 3 4)
 (put (spawn-pt 'troll) 4 3)
 (put (spawn-pt 'troll) 5 4)
 (put (spawn-pt 'troll) 3 5)
 )

(mk-place-music p_brundegardt_tower_2 'ml-dungeon-adventure)

;;----------------------------------------------------------------------------
;; Tower L3
(mk-dungeon-room
 'p_brundegardt_tower_3 "BrundeGardt Tower L3"
 (list
  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  "rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn "
  "rn rn rn rn rn xx xx xx cc cc cc xx xx xx rn rn rn rn rn "
  "rn rn rn rn xx xx cc cc cc cc cc cc cc xx xx rn rn rn rn "
  "rn rn rn xx xx cc cc pp cc cc cc pp cc cc xx xx rn rn rn "
  "rn rn rn xx cc cc cc cc cc cc cc cc cc cc cc xx rn rn rn "
  "rn rn xx xx cc pp cc pp cc cc cc pp cc pp cc xx xx rn rn "
  "rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
  "rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
  "rn rn xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx rn rn "
  "rn rn xx xx cc pp cc pp cc cc cc pp cc pp cc xx xx rn rn "
  "rn rn rn xx cc cc cc cc cc cc cc cc cc cc cc xx rn rn rn "
  "rn rn rn xx xx cc cc pp cc cc cc pp cc cc xx xx rn rn rn "
  "rn rn rn rn xx xx cc cc cc cc cc cc cc xx xx rn rn rn rn "
  "rn rn rn rn rn xx xx xx cc cc cc xx xx xx rn rn rn rn rn "
  "rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn "
  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  "rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
  )
 (put (mk-ladder-down 'p_brundegardt_tower_2 8 9) 8 9)
 (put (mk-ladder-up 'p_brundegardt_tower_4 10 9) 10 9)
 (put (spawn-pt 'queen-spider) 12 9)
 (put (spawn-pt 'queen-spider) 9 12)
 )

(mk-place-music p_brundegardt_tower_3 'ml-dungeon-adventure)

;;----------------------------------------------------------------------------
;; Tower L4
(kern-load "tim.scm")
(mk-19x19-town
 'p_brundegardt_tower_4 "BrundeGardt Tower L4" nil
 (list
		"^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
		"^. ^. ^. ^. ^. bb {1 {5 {{ {{ {3 {1 {1 bb ^. ^. ^. ^. ^. "
		"^. ^. ^. bb {1 .. .. .. {1 {1 .. {8 {4 {{ {3 bb ^. ^. ^. "
		"^. ^. {3 .. {8 {8 .. .. .. .. {4 {{ {2 {1 .. .. bb ^. ^. "
		"^. ^. {2 {c {{ {{ xx w+ xx w+ xx w+ xx .. .. {c {{ ^. ^. "
		"^. bb {4 {{ {{ xx xx cc cc cc cc cc xx xx {4 {{ {{ bb ^. "
		"^. {a .. {1 xx xx xx xx xx cc cc cc cc xx xx {5 {{ {6 ^. "
		"^. {{ {2 .. w+ cc cc cc xx cc cc pp cc cc w+ .. {1 .. bb "
		"^. {3 .. .. xx cc cc cc xx cc cc cc cc cc xx .. .. .. .. "
		"^. {a {8 .. w+ cc cc cc cc cc cc cc cc cc cc .. .. .. .. "
		"^. {{ {{ {2 xx cc cc cc xx cc cc cc cc cc xx .. .. .. .. "
		"^. {3 {1 .. w+ cc cc cc xx cc cc pp cc cc w+ .. {8 {8 bb "
		"^. {2 .. .. xx xx xx xx xx cc cc cc cc xx xx {4 {{ {{ ^. "
		"^. bb .. .. .. xx xx cc cc cc cc cc xx xx .. .. {1 bb ^. "
		"^. ^. {2 .. .. .. xx w+ xx w+ xx w+ xx .. .. {8 {4 ^. ^. "
		"^. ^. bb .. {8 .. .. .. .. {4 {{ {{ {2 .. {4 {{ bb ^. ^. "
		"^. ^. ^. bb {{ {2 .. {8 .. .. {1 {1 .. .. {8 bb ^. ^. ^. "
		"^. ^. ^. ^. bb bb {c {{ {a {8 {8 {8 {8 bb ^. ^. ^. ^. ^. "
		"^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
    nil ;;entrances
 (put (mk-ladder-down 'p_brundegardt_tower_3 10 9) 10 9)
 (put (mk-ladder-up 'p_brundegardt_tower_5 6 9) 6 9)
 (put (mk-tim) 16 9)
 (put (mk-corpse) 9 8)
 (put (mk-corpse) 9 10)
 (put (mk-corpse) 11 5)
 (let ((kdoor (mk-door)))
   (lock-door-with-key kdoor 't_brundegardt_tower_4_key)
   (put kdoor 8 9))
 )

(mk-place-music p_brundegardt_tower_4 'ml-outdoor-adventure)

;;----------------------------------------------------------------------------
;; Tower L5
(kern-load "eye-of-brune.scm")
(mk-19x19-town
 'p_brundegardt_tower_5 "BrundeGardt Tower L5" nil
 (list
  		"^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
		"^. ^. ^. ^. ^. bb {1 {5 {{ {{ {3 {1 {1 bb ^. ^. ^. ^. ^. "
		"^. ^. ^. bb {1 .. .. .. {1 {1 .. {8 {4 {{ {3 bb ^. ^. ^. "
		"^. ^. {3 .. {8 {8 .. .. .. .. {4 {{ {2 {1 .. .. bb ^. ^. "
		"^. ^. {2 {c {{ {{ ee ee ee x. ee ee ee .. .. {c {{ ^. ^. "
		"^. bb {4 {{ {{ ee ee x. x. x. x. x. ee ee {4 {{ {{ bb ^. "
		"^. {a .. {1 ee ee x. x. cc cc cc x. x. ee ee {5 {{ {6 ^. "
		"^. {{ {2 .. ee x. x. cc cc cc cc cc x. x. ee .. {1 .. bb "
		"^. {3 .. .. ee x. cc cc cc cc cc cc cc x. ee .. .. .. .. "
		"^. {a {8 .. x. x. cc cc cc cc cc cc cc x. x. .. .. .. .. "
		"^. {{ {{ {2 ee x. cc cc cc cc cc cc cc x. ee .. .. .. .. "
		"^. {3 {1 .. ee x. x. cc cc cc cc cc x. x. ee .. {8 {8 bb "
		"^. {2 .. .. ee ee x. x. cc cc cc x. x. ee ee {4 {{ {{ ^. "
		"^. bb .. .. .. ee ee x. x. x. x. x. ee ee .. .. {1 bb ^. "
		"^. ^. {2 .. .. .. ee ee ee x. ee ee ee .. .. {8 {4 ^. ^. "
		"^. ^. bb .. {8 .. .. .. .. {4 {{ {{ {2 .. {4 {{ bb ^. ^. "
		"^. ^. ^. bb {{ {2 .. {8 .. .. {1 {1 .. .. {8 bb ^. ^. ^. "
		"^. ^. ^. ^. bb bb {c {{ {a {8 {8 {8 {8 bb ^. ^. ^. ^. ^. "
		"^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. ^. "
  )
    nil ;;entrances
 (put (mk-ladder-down 'p_brundegardt_tower_4 6 9) 6 9)
 (put (kern-mk-obj t_eye_of_brune 1) 9 9)
 (put (mk-corpse-with-loot) 8 9)
 (put (mk-corpse-with-loot) 9 8)
 (put (mk-corpse-with-loot) 10 10)
 (put (kern-mk-obj t_spell_book_divination 1) 9 10)
 (put (mk-ambient-sound 'sound-wind) 18 18)
 )

(mk-place-music p_brundegardt_tower_5 'ml-outdoor-adventure)

(block-teleporting 
 p_brundegardt_tower_5
 (list
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# ee ee ee x. ee ee ee x# x# x# x# x# x# "
  "x# x# x# x# x# ee ee x. x. x. x. x. ee ee x# x# x# x# x# "
  "x# x# x# x# ee ee x. x. cc cc cc x. x. ee ee x# x# x# x# "
  "x# x# x# x# ee x. x. cc cc cc cc cc x. x. ee x# x# x# x# "
  "x# x# x# x# ee x. cc cc cc cc cc cc cc x. ee x# x# x# x# "
  "x# x# x# x# x. x. cc cc cc cc cc cc cc x. x. x# x# x# x# "
  "x# x# x# x# ee x. cc cc cc cc cc cc cc x. ee x# x# x# x# "
  "x# x# x# x# ee x. x. cc cc cc cc cc x. x. ee x# x# x# x# "
  "x# x# x# x# ee ee x. x. cc cc cc x. x. ee ee x# x# x# x# "
  "x# x# x# x# x# ee ee x. x. x. x. x. ee ee x# x# x# x# x# "
  "x# x# x# x# x# x# ee ee ee x. ee ee ee x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  )
)                  

;;----------------------------------------------------------------------------
;; Griffin Peak
(mk-19x19-town
 'p_griffin_peak_s "South Face of Griffin Peak" nil
 (list
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {2 .. {4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {2 .. {4 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^c bb .. .. .. bb ^a ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^c bb .. dd .. dd .. bb ^a ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ bb .. dd dd dd .. dd .. bb ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ .. dd dd dd dd dd dd dd .. ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ bb dd dd dd dd dd dd dd bb ^^ ^^ ^^ ^^ ^^ "
		"^. ^. ^^ ^^ ^^ .. dd .. dd dd dd dd dd .. ^^ ^^ ^^ ^. ^. "
		"{5 ^. ^. ^^ ^^ bb .. dd dd dd dd dd .. bb ^^ ^^ ^. ^. {3 "
		"{4 {{ ^. ^. ^^ ^5 bb .. dd .. dd .. bb ^3 ^^ ^. ^. {{ {2 "
		".. {1 {5 ^. ^. ^^ ^5 bb .. dd .. bb ^3 ^^ ^^ ^. {{ {3 .. "
		"^v .. .. {5 ^. ^. ^^ ^^ ^^ bb ^^ ^^ ^^ ^^ ^. ^. {3 .. ^v "
		"^v ^v .. {4 {{ ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {{ {2 .. ^v "
		"^v ^v ^v .. {5 ^. ^. ^. ^^ ^^ ^^ ^^ ^. ^. {3 {9 .. .. ^v "
		"^v ^v ^v .. .. {1 {5 ^. ^. ^^ ^. ^. ^. {3 {c {C bb .. ^v "
		"^v ^v ^v {8 bb .. {4 {{ ^. ^. ^. {{ {3 {4 {{ bb .. .. ^v "
		"^v ^v {4 {{ {2 .. .. {1 {1 {1 {1 {1 .. .. {1 .. .. bb ^v "
		"^v ^v ^v {1 ^v ^v ^v .. .. .. .. .. ^v ^v ^v ^v ^v ^v ^v "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v "
  )
    nil ;;entrances
 (put (spawn-pt 'griffin) 9 13)
 (put (spawn-pt 'griffin) 3 3)
 (put (spawn-pt 'griffin) 9 17)
 (put (mk-ambient-sound 'sound-wind) 10 10)
 )

(mk-place-music p_griffin_peak_s 'ml-outdoor-adventure)

;; random loot corpses
(put-random-stuff p_griffin_peak_s
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_grass))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  20)

(block-teleporting 
 p_griffin_peak_s
 (list
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ x# x# x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ x# x# x# x# x# ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ x# ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  "~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ "
  ))
                   

(mk-19x19-town
 'p_griffin_peak_se "Southeast Face of Griffin Peak" nil
 (list
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {a .. .. ^v ^v ^v ^v ^v ^v ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {a .. ^v ^v ^v ^v ^v ^v ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {{ {2 .. ^v ^v ^v ^v {{ ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {2 .. ^v ^v ^v {{ {{ ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {2 .. ^v ^v ^v ^v ^v {{ {{ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {{ {2 .. ^v ^v ^v ^v ^v {{ ^v "
		"^. ^. ^. ^. ^. ^^ ^^ ^^ ^. {3 .. .. ^v ^v ^v {{ {{ ^v ^v "
		"^. {{ {7 {{ ^. ^. ^^ ^. ^. {2 .. ^v ^v ^v ^v ^v {{ ^v ^v "
		"{1 {1 .. {1 {5 ^. ^. ^. {{ {2 .. ^v ^v ^v ^v ^v ^v ^v ^v "
		".. .. .. .. .. {1 {1 {1 {1 .. .. .. bb ^v ^v ^v ^v ^v ^v "
		".. ^v ^v ^v .. .. .. .. .. .. bb .. .. .. .. ^v ^v ^v ^v "
		"^v ^v ^v ^v ^v .. .. ^v ^v .. .. .. {8 .. .. bb ^v ^v ^v "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v ^v .. {4 {{ {2 .. .. .. ^v ^v "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v ^v .. .. {1 && .. .. .. ^v ^v "
		"^v {{ ^v ^v ^v ^v {{ ^v ^v ^v bb .. .. .. .. .. .. ^v ^v "
		"^v ^v {{ {{ {{ {{ ^v ^v ^v ^v ^v .. .. .. .. bb ^v ^v ^v "
		"^v {C t3 tt tt t5 {A ^v ^v ^v ^v ^v bb .. .. ^v ^v ^v ^v "
		"{{ t3 tt |. |. tt t5 {{ ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v "
		"{{ tt |. |. |. |. tt {{ ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v {{ "
  )
    nil ;;entrances
 (put (spawn-pt 'griffin) 8 7)
 (put (spawn-pt 'griffin) 7 8)
 (put (spawn-pt 'griffin) 9 9)
 (put (spawn-pt 'griffin) 12 6)
 (put (spawn-pt 'troll) 12 13)
 (put (spawn-pt 'troll) 13 12)
 (put (spawn-pt 'troll-geomancer) 14 13)
 (put (spawn-pt 'troll) 13 14)
 (put (mk-ambient-sound 'sound-wind) 13 13)
 )

(mk-place-music p_griffin_peak_se 'ml-outdoor-adventure)

;; random loot corpses
(put-random-stuff p_griffin_peak_se
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_grass))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  5)

(mk-19x19-town
 'p_griffin_peak_ne "Northeast Face of Griffin Peak" nil
 (list
		"^v ^v ^v {{ ta |. |. |. tc {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v {% ta |. tc {# ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v ^v {% te {# {{ ^v ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v {{ ^v {{ ^v {{ {{ {{ ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v ^v ^v ^v ^v {{ ^v ^v {{ ^. ^^ ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v ^v ^v ^v ^v {{ ^v ^v ^. ^. ^. ^^ ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v {{ ^v ^. ^. ^. ^. ^^ ^^ ^^ ^^ "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^. ^. ^. ^. ^. ^^ "
		".. {8 .. .. .. ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^. ^. ^. ^. "
		"{4 {{ {a {8 {8 .. .. .. .. .. ^v ^v ^v ^v {{ ^v ^. ^. ^. "
		"{8 {d ^. ^. {{ {a .. .. .. .. .. ^v ^v {{ {{ ^v ^v {{ ^. "
		"^. ^. ^. ^. ^. ^. {a {8 .. .. .. ^v ^v ^v ^v {{ {{ ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^. ^. {{ {2 .. .. ^v ^v ^v ^v {{ {{ ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {a .. .. ^v ^v ^v ^v ^v {{ ^v ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {a .. ^v ^v ^v ^v ^v {{ {{ ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {{ {2 ^v ^v ^v ^v ^v {{ {{ ^v "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {3 .. ^v ^v ^v {{ ^v ^v {{ {{ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. {2 .. ^v ^v ^v ^v {{ {{ {{ {{ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^. {{ {2 .. ^v ^v ^v ^v ^v ^v {{ {{ "
  )
    nil ;;entrances
    (put (spawn-pt 'bat) 18 2)
    (put (spawn-pt 'bat) 15 4)
    (put (spawn-pt 'bat) 17 6)
    (put (spawn-pt 'bat) 13 3)
    (put (mk-landslide 6 11 3 3) 8 11)
    (put (mk-ambient-sound 'sound-wind) 13 9)
 )

(mk-place-music p_griffin_peak_ne 'ml-outdoor-adventure)

(mk-19x19-town
 'p_griffin_peak_n "North Face of Griffin Peak" nil
 (list
		"^v ta |. ~a ~5 tt ^v ^v ^. ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ ^. ^. "
		"^v ^v tt |. ~~ |. t5 ^v ^v ^. ^. ^. ^. ^^ ^^ ^^ ^. ^. ^. "
		"^v tb |. ~3 ~c |. tt ^v ^v ^v ^. ^. ^. ^^ ^^ ^. ^. ^. ^v "
		"^v ^v tt ~6 |. |. tt ^v ^v ^v ^v ^v ^. ^. ^. ^. ^. ^v ^v "
		"^v ^v tt ~~ ~~ ~5 |. t5 ^v ^v ^v ^v ^. ^. ^. ^v ^v ^v ^v "
		"^v t3 |. ~~ _! _! ~5 tJ ^v ^v ^v ^v ^v ^. ^v ^v ^v ^v ^v "
		"^v ta |. ~a _! _! ~~ ~~ ~5 ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v "
		"^v ^v ta td ~~ ~c ^v ^v ~a ~5 ^v ^v ^v ^v ^v ^v ^v ^v ^v "
		"^v ^v ^v ^v ~6 ^v ^v ^v ^v ~e ^v ^v ^v ^v ^v ^v ^v ^v .. "
		"^v ^v ^v ~3 ~c ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v .. {8 "
		"^v ^v ~3 ~c ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v .. {4 {{ "
		"^v ^v ~~ ^v ^v ^v ^. ^. ^v ^v ^v ^v ^v .. .. {8 {8 {c ^. "
		"^v ^v ~a ~5 ^v ^. ^. ^. ^v ^v ^v ^v .. {8 {c {{ ^. ^. ^. "
		"^v ^v ^v ~e ^v ^v ^. ^. ^v ^v ^v .. {c {{ ^. ^. ^. ^^ ^^ "
		"^. ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v {4 {{ ^. ^. ^^ ^^ ^^ ^^ "
		"^. ^. ^v ^v ^v ^v ^v ^v ^v ^v ^v {4 {{ ^. ^^ ^^ ^^ ^^ ^^ "
		"^^ ^. ^. ^v ^v ^v ^v ^v ^v ^v .. {c ^. ^. ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^. ^. ^v ^v ^v ^v ^v {8 {4 {{ ^. ^^ ^^ ^^ ^^ ^^ ^^ "
		"^^ ^^ ^. ^. ^. ^v ^v ^v {4 {{ {6 ^. ^. ^^ ^^ ^^ ^^ ^^ ^^ "
  )
    nil ;;entrances
   (put (spawn-pt 'sea-serpent) 8 7)
   (put (mk-ambient-sound 'sound-wind) 9 9)
 )

(mk-place-music p_griffin_peak_n 'ml-outdoor-adventure)

;; Note: Griffin Peak is a copy of the South Face, but with different mountain
;; visibility to give the player the illusion that he has climbed up to the top
;; and is now looking down on the path he trod to get here. Object placement
;; will vary at runtime and may break the illusion, but try to initialize
;; object placement the same at the start of play.
(mk-19x19-town
 'p_griffin_peak "Griffin Peak" nil
 (list
  		"^. ^. ^. ^. ^v ^v ^v ^v {2 .. {4 ^v ^v ^v ^v ^. ^. ^. ^. "
		"^. ^. ^. ^v ^v ^v ^v ^v {2 .. {4 ^v ^v ^v ^v ^. ^. ^. ^. "
		"^. ^. ^. ^v ^v ^v ^v bb .. .. .. bb ^v ^v ^v ^v ^. ^. ^. "
		"^. ^. ^v ^v ^v ^v bb .. dd .. dd .. bb ^v ^v ^v ^v ^. ^. "
		"^. ^v ^v ^v ^v bb .. dd dd dd .. dd .. bb ^v ^v ^v ^. ^. "
		"^v ^v ^v ^v ^v .. dd dd dd dd dd dd dd .. ^v ^v ^v ^v ^. "
		"^v ^v ^v ^v ^v bb dd dd dd dd dd dd dd bb ^v ^v ^v ^v ^v "
		"^v ^v ^v ^v ^v .. dd .. dd dd dd dd dd .. ^v ^v ^v ^v ^v "
		"{5 ^v ^v ^v ^v bb .. dd dd dd dd dd .. bb ^v ^v ^v ^v {3 "
		"{4 {{ ^v ^v ^v ^v bb .. dd .. dd .. bb ^v ^v ^v ^v {{ {2 "
		".. {1 {5 ^v ^v ^v ^v bb .. dd .. bb ^v ^v ^v ^v {{ {3 .. "
		"^v .. .. {5 ^v ^v ^v ^v ^v bb ^v ^v ^v ^v ^v ^v {3 .. ^v "
		"^v ^v .. {4 {{ ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v {{ {2 .. ^v "
		"^v ^v ^v .. {5 ^v ^v ^v ^v ^v ^v ^v ^v ^v {3 {9 .. .. ^v "
		"^v ^v ^v .. .. {1 {5 ^v ^v ^v ^v ^v ^v {3 {c {C bb .. ^v "
		"^v ^v ^v {8 bb .. {4 {{ ^v ^v ^v {{ {3 {4 {{ bb .. .. ^v "
		"^v ^v {4 {{ {2 .. .. {1 {1 {1 {1 {1 .. .. {1 .. .. bb ^v "
		"^v ^v ^v {1 ^v ^v ^v .. .. .. .. .. ^v ^v ^v ^v ^v ^v ^v "
		"^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v ^v "
  )
    nil ;;entrances
 (put (mk-key t_brundegardt_tower_4_key) 9 9)
(put (kern-mk-obj t_spell_book_force_magick_winds 1) 9 10)
 (put (spawn-pt 'griffin-chick) 8 5)
 (put (spawn-pt 'griffin-chick) 10 5)
 (put (spawn-pt 'griffin-chick) 8 7)
 (put (spawn-pt 'griffin-chick) 10 7)
 (put (spawn-pt 'griffin) 9 6)
 (put (mk-ambient-sound 'sound-wind) 9 9)
 )

(mk-place-music p_griffin_peak 'ml-outdoor-adventure)

;; random loot corpses
(put-random-stuff p_griffin_peak
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (or (eqv? (kern-place-get-terrain loc)
                              t_dirt)
                        (eqv? (kern-place-get-terrain loc)
                              t_grass)))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  20)

(block-teleporting 
 p_griffin_peak
 (list
  "x# x# x# x# x# x# x# x# .. .. .. x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# .. .. .. x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# bb .. .. .. bb x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# bb .. dd .. dd .. bb x# x# x# x# x# x# "
  "x# x# x# x# x# bb .. dd dd dd .. dd .. bb x# x# x# x# x# "
  "x# x# x# x# x# .. dd dd dd dd dd dd dd .. x# x# x# x# x# "
  "x# x# x# x# x# bb dd dd dd dd dd dd dd bb x# x# x# x# x# "
  "x# x# x# x# x# .. dd .. dd dd dd dd dd .. x# x# x# x# x# "
  "x# x# x# x# x# bb .. dd dd dd dd dd .. bb x# x# x# x# x# "
  "x# x# x# x# x# x# bb .. dd .. dd .. bb x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# bb .. dd .. bb x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# bb x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  "x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# x# "
  ))
                   

;;----------------------------------------------------------------------------
;; Assemble the lower levels into a dungeon complex
(mk-dungeon-level 
 (list nil           p_cave_shrine     )
 (list p_brundegardt p_brundegardt_keep p_tunnel_turn p_black_canal p_tunnels p_wide_chasm p_brundegardt_tower_1)
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
