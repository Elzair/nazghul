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
                (list (kern-tag 'portcullis-1 (mk-portcullis)) 5 12)
                (list (mk-lever 'portcullis-1) 6 11)
                (list (mk-door) 4 11)
                (list (kern-mk-container t_small_wooden_chest 
                                         nil
                                         (list 
                                          (list 1 t_iron_helm)
                                          (list 1 t_sm_shield)
                                          (list 4 t_spear))) 
                      4 12)
                (list (kern-mk-container t_small_wooden_chest 
                                         'lightning-trap
                                         (list 
                                          (list 1 t_iron_helm)
                                          (list 1 t_sm_shield)
                                          (list 4 t_spear))) 
                      3 10)
                (list (kern-mk-container t_small_wooden_chest 
                                         'lightning-trap
                                         nil)
                      3 9)
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

                (list (kern-mk-char 'ch_olin ; tag
                                    "Olin the Ghast"      ; name
                                    sp_ghast              ; species
                                    nil                   ; occ
                                    s_ghost               ; sprite
                                    faction-men            ; starting alignment
                                    0 10 2                ; str/int/dex
                                    0 1                   ; hp mod/mult
                                    10 5                  ; mp mod/mult
                                    240 0 8 8             ; hp/xp/mp/lvl
                                    nil          ; conv
                                    nil                   ; sched
                                    nil                   ; special ai
                                    nil)                  ; readied
                      8 9)
                )
               nil ;; hooks
               )

(load "tests/empty-party.scm")

(kern-party-add-member player ch_olin)
(kern-party-add-member player ch_shroom)
