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
                (list (kern-mk-char 'ch_shroom ; tag
                                    "Shroom"              ; name
                                    sp_human              ; species
                                    oc_druid              ; occ
                                    s_companion_druid     ; sprite
                                    align-town            ; starting alignment
                                    0 10 0                ; str/int/dex
                                    0 0                   ; hp mod/mult
                                    0 0                   ; mp mod/mult
                                    0 0                   ; hit mod def mod
                                    0 0                   ; dam mod arm mod
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
                                    align-town            ; starting alignment
                                    0 10 2                ; str/int/dex
                                    0 1                   ; hp mod/mult
                                    10 5                  ; mp mod/mult
                                    0 0                   ; hit mod def mod
                                    0 0                   ; dam mod arm mod
                                    240 0 8 8             ; hp/xp/mp/lvl
                                    nil          ; conv
                                    nil                   ; sched
                                    nil                   ; special ai
                                    nil)                  ; readied
                      8 9)
                )
               nil ;; hooks
               )

(kern-mk-player 'player                     ; tag
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
                (list
                 ch_olin
                 ch_shroom
                 )
                )
