;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map
 'm_angriss_lair 19 19 pal_expanded
 (list
  "|| || || || || || || || tt tt tt || || || tt || || || || "
  "|| || || || || || || || || tt || || || tt tt tt || || || "
  "|| || || || || || bb bb bb bb || || || bb .. bb || || || "
  "|| || || || bb bb bb tt tt bb bb || bb .. .. .. bb || || "
  "|| || || bb .. .. tt || || tt bb .. .. .. .. .. bb || || "
  "|| || || bb .. .. tt || || tt .. .. .. .. .. bb || || || "
  "|| || || bb bb bb .. .. .. .. .. .. .. bb bb || || || || "
  "|| || || || bb bb .. bb .. .. .. tt tt tt bb || || || || "
  "tt || || || bb || || || bb .. tt || || tt tt bb || || tt "
  "tt tt || || bb || .. || bb .. .. || || tt .. bb || tt tt "
  "tt || || || bb || || || bb bb .. .. .. .. bb || || || tt "
  "|| || || || bb bb bb bb .. .. .. bb .. bb bb bb || || || "
  "|| || || bb .. .. tt tt .. .. .. bb .. || || bb || || || "
  "|| || || bb .. tt || || tt .. bb .. .. .. || bb || || || "
  "|| || || bb .. tt || || .. .. bb .. .. .. .. bb || || || "
  "|| || || || bb .. .. .. .. bb || bb bb .. bb || || || || "
  "|| || || || || bb .. bb bb || || || tt tt tt || || || || "
  "|| || || || || tt tt tt || tt || || || tt || || || || || "
  "|| || || || || || tt || tt tt tt || || || || || || || || "
  )
 )
;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_angriss_lair ; tag
 "Entrance to Angriss's Lair"   ; name
 s_spider_web     ; sprite
 m_angriss_lair  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-monman) 0 0)
  (put (mk-ladder-down 'p_spider_cave 6 9) 6 9)
  (put (mk-ladder-down 'p_spider_cave 15 4) 15 4)
  
  (put (spawn-pt 'giant-spider faction-spider) 6 9)
  (put (spawn-pt 'giant-spider faction-spider) 15 4)
  (put (spawn-pt 'giant-spider faction-spider) 7 4)
  (put (spawn-pt 'giant-spider faction-spider) 6 13)
  (put (spawn-pt 'giant-spider faction-spider) 14 12)

  )

 (list 'on-entry-to-dungeon-room) ; hooks
 nil ; edge entrances
 )

(kern-load "angriss.scm")

(mk-dungeon-room
 'p_angriss_throne_room  "Angriss's Throne Room"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
  "rr rr .. .. .. .. bb .. .. .. .. .. .. .. .. .. .. rr rr "
  "rr .. .. .. .. .. bb bb .. bb bb bb .. .. .. .. .. .. rr "
  "rr .. .. .. .. bb .. bb bb bb .. bb bb .. .. .. .. .. rr "
  "rr .. .. .. bb bb .. .. .. .. bb bb bb bb .. .. .. .. rr "
  "rr .. .. .. bb .. .. bb bb .. .. bb bb .. bb .. .. .. rr "
  "rr .. .. bb .. .. bb .. .. .. .. .. bb .. bb bb .. .. rr "
  "rr .. .. bb .. bb .. .. .. .. .. bb .. .. bb .. .. .. rr "
  "rr .. .. bb .. .. bb .. .. .. .. .. bb .. bb .. .. .. rr "
  "rr .. .. bb bb .. bb .. .. .. .. .. bb bb .. bb .. .. rr "
  "rr .. .. .. .. .. bb bb .. .. .. bb bb .. .. bb .. .. rr "
  "rr .. .. .. bb bb bb .. bb .. bb .. .. bb bb .. .. .. rr "
  "rr .. .. .. bb bb bb bb bb bb bb .. bb bb bb .. .. .. rr "
  "rr bb .. .. .. bb .. bb bb .. bb .. .. bb .. .. .. .. rr "
  "rr bb bb .. .. .. bb bb bb bb .. bb .. .. .. .. .. .. rr "
  "rr bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb rr "
  "rr bb bb .. bb .. .. .. .. .. .. .. .. .. .. .. bb bb rr "
  "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
  )
 
 (put (mk-angriss) 9 9)

 (put (kern-mk-obj F_web_perm 1) 9  6)
 (put (kern-mk-obj F_web_perm 1) 10 6)
 
 (put (kern-mk-obj F_web_perm 1) 7  7)
 (put (kern-mk-obj F_web_perm 1) 8  7)
 (put (kern-mk-obj F_web_perm 1) 9  7)
 (put (kern-mk-obj F_web_perm 1) 10 7)
 (put (kern-mk-obj F_web_perm 1) 11 7)
 
 (put (kern-mk-obj F_web_perm 1) 6  8)
 (put (kern-mk-obj F_web_perm 1) 7  8)
 (put (kern-mk-obj F_web_perm 1) 8  8)
 (put (kern-mk-obj F_web_perm 1) 9  8)
 (put (kern-mk-obj F_web_perm 1) 10 8)
 
 (put (kern-mk-obj F_web_perm 1) 7  9)
 (put (kern-mk-obj F_web_perm 1) 8  9)
 (put (kern-mk-obj F_web_perm 1) 9  9)
 (put (kern-mk-obj F_web_perm 1) 10 9)
 (put (kern-mk-obj F_web_perm 1) 11 9)
 
 (put (kern-mk-obj F_web_perm 1) 7  10)
 (put (kern-mk-obj F_web_perm 1) 8  10)
 (put (kern-mk-obj F_web_perm 1) 9  10)
 (put (kern-mk-obj F_web_perm 1) 10 10)
 (put (kern-mk-obj F_web_perm 1) 11 10)

 (put (kern-mk-obj F_web_perm 1) 8  11)
 (put (kern-mk-obj F_web_perm 1) 9  11)
 (put (kern-mk-obj F_web_perm 1) 10 11)

 (put (kern-mk-obj F_web_perm 1) 9  12)

 (put (kern-mk-obj F_web_perm 1) 11 12)
 (put (kern-mk-obj F_web_perm 1) 11 13)
 (put (kern-mk-obj F_web_perm 1) 11 14)
 (put (kern-mk-obj F_web_perm 1) 12 14)
 (put (kern-mk-obj F_web_perm 1) 5 6)
 
 )

;; corpse & treasure heaps
(put-random-stuff p_angriss_throne_room
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_grass))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  50)

;; spider eggs
(put-random-stuff p_angriss_throne_room
                  (mk-rect 6 6 7 7)
                  (lambda (loc) #t)
                  (lambda (loc)
                    (kern-obj-put-at (mk-spider-egg) loc))
                  20)



(mk-dungeon-room
 'p_spider_cave "Spider Cave"
 (list
  "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
  "rr bb .. .. .. .. .. .. .. .. .. .. rr rr bb .. bb bb rr "
  "rr rr .. .. .. .. .. .. .. .. .. rr rr bb .. .. .. bb rr "
  "rr rr rr .. rr .. .. .. .. tt .. rr .. .. .. .! .. bb rr "
  "rr rr rr rr rr rr .. .. tt tt tt rr .. .. .! .! .! .. rr "
  "rr rr rr bb rr tt tt tt tt tt tt rr bb .. .. .! .. .. rr "
  "rr rr .. .. tt tt tt tt tt tt .. rr rr bb bb .. .. bb rr "
  "rr .. .. tt tt tt tt tt tt tt .. .. rr rr rr rr rr rr rr "
  "rr .. tt tt tt .! .! .! tt tt tt .. .. .. bb .. .. .. rr "
  "rr .. tt tt tt .! .! .! tt tt tt tt .. .. .. .. .. rr rr "
  "rr .. tt tt tt .! .! .! tt tt tt .. .. .. bb .. .. .. rr "
  "rr .. tt tt tt tt tt tt tt tt rr .. .. .. .. bb .. .. rr "
  "rr .. .. tt tt tt bb tt tt rr rr bb .. rr rr .. .. rr rr "
  "rr .. .. .. tt rr rr rr rr rr rr rr rr rr bb rr .. .. rr "
  "rr bb .. rr .. .. rr rr .. .. bb .. .. bb .. .. .. .. rr "
  "rr rr bb .. .. .. .. .. .. .. .. bb .. .. .. .. .. .. rr "
  "rr rr .. .. .. bb .. .. .. .. bb .. .. .. .. bb .. bb rr "
  "rr .. .. .. .. .. .. .. rr rr rr rr rr .. .. .. .. rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-up 'p_angriss_lair 6 9) 6 9)
 (put (mk-ladder-up 'p_angriss_lair 15 4) 15 4)

 (put (spawn-pt 'queen-spider faction-spider) 9 0)
 (put (spawn-pt 'queen-spider faction-spider) 15 15)
 (put (spawn-pt 'queen-spider faction-spider) 1 17)

 ;; meat locker
 (put (mk-corpse) 15 1)
 (put (mk-corpse) 12 3)
 (put (mk-corpse) 12 4)
 (put (mk-corpse) 16 6)
 (put (mk-corpse) 17 4)
 (put (mk-spider-egg) 15 1)
 (put (mk-spider-egg) 12 3)
 (put (mk-spider-egg) 12 4)
 (put (mk-spider-egg) 16 6)
 (put (mk-spider-egg) 17 4)
 (put (kern-mk-obj web-type 1) 15 1)
 (put (kern-mk-obj web-type 1) 12 3)
 (put (kern-mk-obj web-type 1) 12 4)
 (put (kern-mk-obj web-type 1) 16 6)
 (put (kern-mk-obj web-type 1) 17 4)
 (put (kern-mk-obj web-type 1) 17 5)
 (put (kern-mk-obj web-type 1) 15 6)
 (put (kern-mk-obj web-type 1) 13 5)
 (put (kern-mk-obj t_gold_coins 52) 14 2)
 (put (kern-mk-obj t_gold_coins 34) 17 5)
 (put (kern-mk-obj t_bow 1) 15 6)
 (put (kern-mk-obj t_arrow 34) 13 5)
 (put (kern-mk-obj t_leather_helm 1) 16 2)
 (put (kern-mk-obj t_halberd 1) 12 3)
 (put (kern-mk-obj t_heal_potion 3) 15 1)
 (put (kern-mk-obj t_mana_potion 5) 16 6)
)

(mk-dungeon-level 
 (list p_angriss_throne_room)
 (list p_spider_cave)
 )
