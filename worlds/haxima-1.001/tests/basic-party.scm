;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              faction-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              240 0 240 8           ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              (list t_rpg))         ; readied

;;----------------------------------------------------------------------------
;; Player
;;----------------------------------------------------------------------------
(load "tests/empty-party.scm")
(kern-party-add-member player ch_thorald_greybeard)
