;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              align-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 240 8           ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              (list t_rpg))         ; readied

;;----------------------------------------------------------------------------
;; Player
;;----------------------------------------------------------------------------
(kern-mk-player
 'player                     ; tag
 s_companion_fighter         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 1000                        ; food
 500                         ; gold
 (+ align-player align-town) ; alignment
 nil                         ; formation
 nil                         ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle

 
 ;; inventory
 (kern-mk-container
  nil ;; type
  nil ;; trap
  nil ;; contents:
  )
 
 ;; party members
 (list ch_thorald_greybeard)
 )
