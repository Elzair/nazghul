(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(define conv-a
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Dunno.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Hi.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Bye.")))
       (method 'job (lambda (knpc kpc) (say knpc "Following you around.")))
       (method 'name (lambda (knpc kpc) (say knpc "Forget me already?")))
       ))

(define conv-b
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Huh?")))
       (method 'hail (lambda (knpc kpc) (say knpc "Howdy.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Sigh-yo-nara!")))
       (method 'job (lambda (knpc kpc) (say knpc "Whatever you like.")))
       (method 'name (lambda (knpc kpc) (say knpc "Call me Slim.")))
       ))

 (kern-mk-char 'ch_shroom ; tag
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
               'conv-a          ; conv
               sch_shroom            ; sched
               nil                   ; special ai
               (list t_dagger))      ; readied

(kern-mk-char 'ch_olin ; tag
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
              'conv-b          ; conv
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

(kern-mk-player
 'player                     ; tag
 s_companion_fighter         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 1000                        ; food
 500                         ; gold
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

(kern-party-add-member player ch_thorald_greybeard)
(kern-party-add-member player ch_shroom)
(kern-party-add-member player ch_slurmok)

(define invisible-chest
  (kern-mk-container t_small_wooden_chest 
                     nil
                     (list 
                      (list 1 t_iron_helm)
                      (list 1 t_sm_shield)
                      (list 4 t_spear))))
(kern-obj-set-visible invisible-chest #f)

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
  (list (mk-door) 4 11)
  (list invisible-chest 3 10)
  )
 nil ;; hooks
)

