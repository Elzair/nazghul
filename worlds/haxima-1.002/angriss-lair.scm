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
  (put (mk-ladder-down 'p_angriss_lair_l2 6 9) 6 9)
  (put (mk-ladder-down 'p_angriss_lair_l2 15 4) 15 4)

  (put (mk-edge-gen 995 2 'is-queen-spider? 'mk-at-level 
                    (list 'mk-queen-spider "1d3+4")) 6 9)
  (put (mk-edge-gen 995 2 'is-queen-spider? 'mk-at-level 
                    (list 'mk-queen-spider "1d3+4")) 15 4)
  (put (mk-edge-gen 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+4")) 7 4)
  (put (mk-mongen2 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+4")) 6 13)
  (put (mk-mongen2 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+4")) 14 12)

  )

 nil ; hooks
 nil ; edge entrances
 )

;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_angriss_lair_l3 19 19 pal_expanded
    (list
            "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
            "rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr "
            "rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr "
            "rr .. .. .. .. .. .. bb .. bb bb bb .. .. .. .. .. .. rr "
            "rr .. .. .. .. bb bb .. .. .. .. .. bb bb .. .. .. .. rr "
            "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
            "rr .. .. .. bb .. .. .. .. .. .. .. .. .. bb .. .. .. rr "
            "rr .. .. .. bb .. .. .. .. .. .. .. .. .. .. bb .. .. rr "
            "rr .. .. .. bb .. .. .. .. .. .. .. .. .. bb .. .. .. rr "
            "rr .. .. bb .. .. .. .. .. .. .. .. .. .. bb .. .. .. rr "
            "rr .. .. bb .. .. .. .. .. .. .. .. .. .. .. bb .. .. rr "
            "rr .. .. bb .. .. .. .. .. .. .. .. .. .. .. bb .. .. rr "
            "rr .. .. .. bb .. .. .. .. .. .. .. .. .. bb .. .. .. rr "
            "rr .. .. .. bb .. .. .. .. .. .. .. .. .. bb .. .. .. rr "
            "rr bb .. .. .. bb bb .. .. .. .. .. bb bb .. .. .. .. rr "
            "rr bb bb .. .. .. .. bb bb bb bb bb .. .. .. .. .. .. rr "
            "rr bb .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. bb rr "
            "rr bb bb .. bb .. .. .. .. .. .. .. .. .. .. .. bb bb rr "
            "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
    )
  )

(kern-load "angriss.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_angriss_lair_l3 ; tag
 "Angriss's Throne Room"   ; name
 nil              ; sprite
 m_angriss_lair_l3  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-angriss) 9 9)
  )

 nil ; hooks
 nil ; edge entrances
 )

;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map
 'm_angriss_lair_l2 19 19 pal_expanded
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
  )

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_angriss_lair_l2 ; tag
 "Angriss's Lair"   ; name
 nil              ; sprite
 m_angriss_lair_l2  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  (list p_angriss_lair_l3 north)
  )
 
 ;; objects
 (list
  
  (put (mk-ladder-up 'p_angriss_lair 6 9) 6 9)
  (put (mk-ladder-up 'p_angriss_lair 15 4) 15 4)

  (put (mk-edge-gen 995 2 'is-queen-spider? 'mk-at-level 
                    (list 'mk-queen-spider "1d3+5")) 9 0)
  (put (mk-edge-gen 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+5")) 15 4)
  (put (mk-mongen2 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+5")) 15 15)
  (put (mk-mongen2 995 5 'is-spider? 'mk-at-level 
                    (list 'mk-wood-spider "1d3+5")) 1 17)

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

 nil ; hooks
 nil ; edge entrances
 )
