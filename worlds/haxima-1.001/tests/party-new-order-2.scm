(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-char 'ch_olin ; tag
              "Olin the Ghast"      ; name
              sp_ghast              ; species
              nil                   ; occ
              s_ghost               ; sprite
              faction-men            ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              240 0 8 8             ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil)                  ; readied

(kern-mk-char
 'ch_thorald_greybeard
 "Thorald Greybeard"
 sp_human
 oc_wizard
 s_companion_wizard
 3
 20 30 22
 0 1
 10 5
 0 0
 0 0
 39 0
 240 8
 nil
 nil
 nil
 (list
  t_rpg
  ))

(load "tests/empty-party.scm")

(kern-party-add-member player ch_thorald_greybeard)
(kern-party-add-member player ch_olin)

(kern-mk-place 
 'p_test
 "Test Place"
 nil          ; sprite
 m_test_1
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place
 nil ;; subplaces
 nil ;; neighbors
 
 ;; objects:
 (list
  (list player 9 9)
  (list (kern-tag 'portcullis-1 (mk-portcullis)) 5 12)
  (list (mk-lever 'portcullis-1) 6 11)
   )
 nil ;; hooks
)

