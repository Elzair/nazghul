(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-map
 'm_grass_map 19 19 pal_expanded
 (list
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. tt .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. tt tt tt .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. tt .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx"
))


(kern-mk-char 'ch_thorald_greybeard
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

(kern-mk-char 'ch_slurmok ; tag
              "Slurmok"             ; name
              sp_yellow_slime       ; species
              oc_wizard             ; occ
              s_yellow_slime        ; sprite
              align-player          ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 5 7             ; hp/xp/mp/lvl
              'conv-b                 ; conv
              nil                   ; sched
              nil                   ; special ai
              (list t_dagger))      ; readied


(kern-mk-place 'p_test2
               "Underplace"
               nil          ; sprite
               m_grass_map
               #f  ;; wraps
               #t  ;; underground
               #f  ;; wilderness
               #f  ;; tmp combat place
               nil ;; subplaces
               nil ;; neighbors
               ;; objects
               (list 
                (list (mk-ladder-up 'p_test 4 12) 9 9)
                )
               nil ;; hooks
               )

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
                (list (mk-door) 4 11)
                (list (mk-door) 5 12)
                (list (mk-ladder-down 'p_test2 9 9) 4 12)
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
                nil ;; party members
                )

(kern-party-add-member player ch_olin)
(kern-party-add-member player ch_shroom)
