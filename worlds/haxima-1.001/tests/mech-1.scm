(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-place 
 'p_test
 "Test Place"
 nil          ; sprite
 m_test_1
 #f  ;; wraps
 #f  ;; underground
 #f  ;; wilderness
 #f  ;; tmp combat place
 nil ;; subplaces
 nil ;; neighbors
 
 ;; objects:
 (list
  (list
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
   9 9)
  (list (kern-tag 'portcullis-1 (mk-portcullis)) 5 12)
  (list (mk-lever 'portcullis-1) 6 11)
   )
 nil ;; hooks
)

(load "tests/empty-party.scm")

(kern-party-add-member player ch_thorald_greybeard)
