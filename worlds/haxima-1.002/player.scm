;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-mk-char 
 'ch_wanderer
 "The Wanderer"        ; name
 sp_human              ; species
 oc_wanderer           ; occ
 s_companion_ranger    ; sprite
 faction-player        ; starting alignment
 0 10 2                ; str/int/dex
 0 1                   ; hp mod/mult
 10 5                  ; mp mod/mult
 29 0 3 1              ; hp/xp/mp/lvl
 #f                    ; dead
 nil                   ; conv
 nil                   ; sched
 nil                   ; special ai
 nil                   ; container
 nil)                  ; readied


;; For test
(kern-mk-char 
 'ch_thorald_greybeard ; tag
 "Thorald Greybeard"   ; name
 sp_human              ; species
 oc_wizard             ; occ
 s_companion_wizard    ; sprite
 faction-player          ; starting alignment
 0 10 2                ; str/int/dex
 0 1                   ; hp mod/mult
 10 5                  ; mp mod/mult
 240 0 8 8           ; hp/xp/mp/lvl
 #f                    ; dead
 nil                   ; conv
 nil                   ; sched
 nil                   ; special ai
 nil                   ; container
 (list t_doom_staff))         ; readied
 
;;----------------------------------------------------------------------------
;; Player Party
;;----------------------------------------------------------------------------
(kern-mk-player
 'player                     ; tag
 s_companion_fighter         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 10                          ; food
 0                           ; gold
 0                           ; turns to next meal
 nil                         ; formation
 m_campsite                  ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle
 ;; inventory
 (kern-mk-container
  nil ;; type
  nil ;; trap
  nil ;; contents
  )

 nil ;; party members (should be nil for initial load file)
 )

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)
;(kern-party-add-member player ch_thorald_greybeard)
