;;----------------------------------------------------------------------------
;; Trigrave
;;
;; This town is at the heart of the starting region. It's a frontier town in
;; the province of a lord who rules from the south, so don't expect anything
;; too fancy. Here the player will find the basic shops and amenities needed to
;; get by.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Map
;;
;; This is the terrain map for the place. It may be altered at run-time so it
;; must be saved and loaded with every session.
;;
;; This is a "composite" map. The individual building maps are defined
;; separately and then blitted onto the terrain map.
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_lusty_juggs 14 10 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx x! @@ .L .U .S .T .Y x! xx cc cc cc xx "
  "ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx "
  "sT cc cc 00 cc cc 00 cc cc x! xx cc xx xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc && xx "
  "xx cc cc 00 cc cc 00 cc cc x! cc cc && xx "
  "ws cc cc 00 cc cc 00 cc cc xx cc cc cc xx "
  "xx x! @@ .J .U .G .S @@ x! xx cc cc cc xx "
  "xx xx xx xx xx xx xx xx xx xx xx ws xx xx "
  )
)

(kern-mk-map
 'm_iron_works 7 12 pal_expanded
 (list
  "xx xx xx xx xx xx xx "
  "xx .I .R .O .N @@ xx "
  "xx .W .O .R .K .S xx "
  "ws cc cc cc cc cc xx "
  "sA cc cc cc cc cc xx "
  "cc cc cc cc cc __ xx "
  "xx cc cc cc cc !! xx "
  "ws cc cc cc cc cc xx "
  "xx xx cc xx cc xx xx "
  "xx cc cc x! cc cc xx "
  "xx cc cc xx cc cc xx "
  "xx xx xx xx xx xx xx "
  ))

(kern-mk-map 
 'm_quiet_inn 13 11 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx cc cc x! .G .R .A .Y @@ x! cc cc xx "
  "xx cc cc x! @@ .D .O .V .E x! cc cc xx "
  "cc cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx x! @@ .I .N .N @@ x! xx xx xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx xx xx x! cc cc cc cc cc x! xx xx xx "
  "xx cc cc cc cc cc cc cc cc cc cc cc xx "
  "xx cc cc xx cc cc cc cc cc xx cc cc xx "
  "xx xx xx xx ws x! cc x! ws xx xx xx xx "
  )
 )

(kern-mk-map
 'm_dry_goods 7 10 pal_expanded
 (list
  " xx xx xx xx xx xx xx "
  " xx @@ .D .R .Y @@ xx "
  " xx .G .O .O .D .S xx "
  " cc cc cc cc cc cc ws "
  " xx @@ @@ @@ @@ @@ xx "
  " xx cc cc cc cc cc sE "
  " xx cc cc cc cc cc cc "
  " xx cc cc cc cc cc xx "
  " xx cc cc cc cc cc ws "
  " xx xx xx xx xx xx xx "
  )
 )


(kern-mk-map 
 'm_trigrave 32 32 pal_expanded
	(list
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt td .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt td .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb ta tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt td .. .. .. .. .. bb ta tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. tD tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt td .. .. .. .. .. bb t3 tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb t3 tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt td .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tc t& ta tt tt tt tt tt tt tt td .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tc bb .. bb tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tc sI .. .. .. ta tt tt tt tt tt tc .. .. .. .. .. ta tt tt tt tt tt tt tt tt tt tt "
		"bb te bb te bb .. .. .. .. .. bb te bb te bb .. .. .. .. .. .. .. bb te bb te bb te bb te bb ta "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"bb t7 bb t7 bb t7 bb t7 bb .. .. .. .. .. .. .. bb t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb t7 bb t3 "
		"tt tt tt tt tt tt tt tt tt t5 .. .. .. .. .. t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt td .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt bb .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt td .. .. .. .. ta tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt td .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt .. .. .. .. .. .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt bb .. .. .. .. .. .. t3 tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt td .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt bb .. .. .. tb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
		"tt tt tt tt tt tt tt tt tt tt t5 .. .. .. bb tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
	)
 )

;;----------------------------------------------------------------------------
;; NPCs
;;
;; NPC's are defined in two steps. Step 1 is to kern-load their read-only
;; definition file (this file includes their conversation, schedule,
;; constructor, etc). Step 2 is to call kern-mk-char to instantiate them.
;;----------------------------------------------------------------------------
(kern-load "jim.scm")
(bind 
 (kern-mk-char 'ch_jim ; tag
               "Jim"                ; name
               sp_human            ; species
               nil                 ; occ
               s_townsman          ; sprite
               faction-men         ; starting alignment
               4 1 2              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               max-health -1 max-health 0 4  ; hp/xp/mp/AP_per_turn/lvl
               #f                  ; dead
               'jim-conv          ; conv
               sch_jim           ; sched
               'townsman-ai                 ; special ai
               nil                 ; container
               (list t_mace   ; we need a 'hammer'
                 			t_armor_chain)
               )
 (jim-mk))

(kern-load "gwen.scm")
(bind 
 (kern-mk-char 'ch_gwen ; tag
               "Gwen"               ; name
               sp_human            ; species
               nil                 ; occ
               s_gwen ; sprite
               faction-men         ; starting alignment
               0 1 2              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
               #f                  ; dead
               'gwen-conv          ; conv
               sch_gwen           ; sched
               'townsman-ai                 ; special ai
               (mk-inventory (list (list 1 t_dagger)))                 ; container
               nil                 ; readied
               )
 (gwen-mk))

(kern-load "chanticleer.scm")
(bind 
 (kern-mk-char 'ch_chant ; tag
               "Chanticleer"          ; name
               sp_human            ; species
               nil                 ; occ
               s_chanticleer     ; sprite
               faction-men         ; starting alignment
               0 4 2              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               max-health -1 max-health 0 5  ; hp/xp/mp/AP_per_turn/lvl
               #f                  ; dead
               'chant-conv         ; conv
               sch_chant           ; sched
               'townsman-ai                 ; special ai
               (mk-inventory (list (list 1 t_sword)))                 ; container
               (list t_armor_leather)                 ; readied
               )
 (chant-mk))

(kern-load "earl.scm")
(bind 
 (kern-mk-char 'ch_earl     ;; tag
               "Earl"       ;; name
               sp_human     ;; species
               nil          ;; occ
               s_townsman   ;; sprite
               faction-men  ;; starting alignment
               0 5 3       ;; str/int/dex
               0 0          ;; hp mod/mult
               0 0          ;; mp mod/mult
               max-health -1 max-health 0 4  ;; hp/xp/mp/AP_per_turn/lvl
               #f                  ; dead
               'earl-conv   ;; conv
               sch_earl     ;; sched
               'townsman-ai          ;; special ai
               (mk-inventory (list
               	(list 1 t_staff)
               	(list 1 t_sling)))          ;; container
               nil          ;; readied
               )
 (earl-mk))

(kern-load "miggs.scm")
(mk-miggs 'ch_miggs)

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
(define jims-chest
  (mk-chest
   nil ;; trap
   '((1 t_scratched_shield)
                (1 t_armor_plate)
                (1 t_iron_helm))))

(define jims-other-chest
  (mk-chest
   'spike-trap ;; trap
   '((100 t_gold_coins)
                (3 t_dagger)
                (3 t_mace)
                (3 t_sword)
                )))

(define earls-chest
  (make-invisible
   (mk-chest
    nil ;; trap
    '((1 t_stun_wand)
                 (3 t_xen_corp_scroll)
                 (3 t_in_vas_por_ylem_scroll)
                 (5 t_vas_mani_scroll)))))
  
;;----------------------------------------------------------------------------
;; Place
;;
;; The place definition instantiates the place.
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_trigrave     ; tag
 "Trigrave"      ; name
 s_town          ; sprite

 ;; Blit the buildings over the basic terrain map.
 (if #f
     m_trigrave
     (blit-maps m_trigrave
                (list 17 21 m_lusty_juggs 0 0 14 10)
                (list 24 1  m_iron_works  0 0 7  12)
                (list 1  1  m_quiet_inn   0 0 13 11)
                (list 1 21  m_dry_goods   0 0 7  10)))
 
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list ; objects

  ;; characters
  (list ch_jim   15 15)
  (list ch_gwen  15 15)
  (list ch_chant 15 15)
  (list ch_earl  15 15)
  (put ch_miggs 15 15)

  ;; Inn
  (list (kern-tag 'trigrave-inn-room-1-door (mk-locked-door))  4 7)
  (list (kern-tag 'trigrave-inn-room-2-door (mk-locked-door))  4 9)
  (list (kern-tag 'trigrave-inn-room-3-door (mk-locked-door)) 10 9)
  (list (kern-tag 'trigrave-inn-room-4-door (mk-locked-door)) 10 7)
  (list (mk-locked-door) 1 4)
  (list (mk-door)  4  4)
  (list (mk-door) 10  4)
  (list (mk-door)  7 11)
  (list (mk-bed)   2  6)
  (list (mk-bed)  12  6)
  (list (mk-bed)  12  9)
  (list (mk-bed)  12  2)

  ;; Earl's room in the inn
  (put (mk-bed)   2  9)
  (put earls-chest 2 10)

  ;; General store
  (list (mk-locked-door)  1 24)
  (list (mk-door)         7 27)
 
  ;; Iron Works
  (list (mk-door)                                  24 6)
  (list (mk-locked-door)                           26 9)
  (list (kern-tag 'tiw-portcullis (mk-portcullis)) 28 9)
  (list (mk-lever 'tiw-portcullis)                 25 11)
  (list (mk-bed)                                   25 10)
  (put jims-chest                                  28 11)
  (put jims-other-chest                            29 11)

  ;; Lusty Juggs (tavern)
  (list (mk-locked-door) 28 24)
  (list (kern-tag 'tlj-d-1 (mk-connected-door 'tlj-d-2)) 17 25)
  (list (kern-tag 'tlj-d-2 (mk-connected-door 'tlj-d-1)) 17 26)
  (list (mk-clock) 25 23)
  (put (mk-bed) 27 22)

  ;; Chickens!
  (put (mk-npc 'chicken 5) 15 15)
  (put (mk-npc 'chicken 5) 16 16)
  

  )
 (list 'lock-inn-room-doors) ;; hooks
 (list  ;; edge entrances
  (list south 18 0)
  (list north  12 31)
  )
 )

(mk-place-music p_trigrave 'ml-small-town)
