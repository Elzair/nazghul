(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-place 'p_test
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
                (list (kern-mk-obj F_sleep_perm 1) 8 8)
                (list (kern-mk-char 'ch_shroom ; tag
                                    "Shroom"              ; name
                                    sp_human              ; species
                                    oc_druid              ; occ
                                    s_companion_druid     ; sprite
                                    faction-men            ; starting alignment
                                    0 10 0                ; str/int/dex
                                    0 0                   ; hp mod/mult
                                    0 0                   ; mp mod/mult
                                    30 0 9 9              ; hp/xp/mp/lvl
                                    nil          ; conv
                                    nil            ; sched
                                    nil                   ; special ai
                                    (list t_dagger))      ; readied
                      9 9)
                )
               nil ;; hooks
               )

(load "tests/empty-party.scm")

(kern-party-add-member player ch_shroom)
