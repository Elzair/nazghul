(define humanoid-slots (list slot-helm
                             slot-amulet
                             slot-weapon-or-shield
                             slot-weapon-or-shield
                             slot-armor
                             slot-boot
                             slot-ring
                             slot-ring))

(kern-mk-species 'sp_human         ; tag
                 "human"           ; name
                 20 20 20          ; str/int/dex
                 speed-human       ; speed
                 13                ; vision radius
                 mmode-walk        ; passability
                 20 2              ; hp mod/mult
                 10 10             ; mp mod/mult
                 0 0 0 0           ; hit/def/dam/arm mod
                 s_corpse          ; sleep sprite
                 t_hands           ; natural (unarmed) weapon
                 #t                ; visible
                 sound-damage      ; damage sound
                 sound-walking     ; walking sound
                 humanoid-slots    ; slots
                 nil               ; native spells
                 )

(kern-mk-species 'sp_ghast         ; tag
                 "ghast"           ; name
                 20 20 20          ; str/int/dex
                 speed-human       ; speed
                 8                 ; vision radius
                 mmode-spirit      ; passability
                 10 5              ; hp mod/mult
                 10 10             ; mp mod/mult
                 0 1 -1 -1         ; hit/def/dam/arm mod
                 s_corpse          ; sleep sprite
                 t_hands           ; natural (unarmed) weapon
                 #f                ; visible
                 sound-damage      ; damage sound
                 nil               ; walking sound
                 humanoid-slots    ; slots
                 nil               ; native spells FIXME!!!!
                 )

(kern-mk-species 'sp_goblin        ; tag
                 "goblin"          ; name
                 18 18 22          ; str/int/dex
                 speed-human       ; speed
                 14                ; vision radius
                 mmode-walk        ; passability
                 15 2              ; hp mod/mult
                 10 10             ; mp mod/mult
                 0 0 0 0           ; hit/def/dam/arm mod
                 s_corpse          ; sleep sprite
                 t_hands           ; natural (unarmed) weapon
                 #t                ; visible
                 sound-damage      ; damage sound
                 sound-walking     ; walking sound
                 humanoid-slots    ; slots
                 nil               ; native spells
                 )

(kern-mk-species 'sp_insect        ; tag
                 "insect swarm"    ; name
                 1  1  30          ; str/int/dex
                 speed-insect      ; speed
                 4                 ; vision radius
                 mmode-hover       ; passability
                 3 1               ; hp mod/mult
                 0 0               ; mp mod/mult
                 0 0 0 0           ; hit/def/dam/arm mod
                 nil               ; sleep sprite
                 t_stinger         ; natural (unarmed) weapon
                 #t                ; visible
                 sound-damage      ; damage sound
                 nil               ; walking sound
                 nil               ; slots
                 nil               ; native spells
                 )

(kern-mk-species 'sp_yellow_slime      ; tag
                 "yellow slime"        ; name
                 4 4 1                 ; str/int/dex
                 speed-yellow-slime    ; speed
                 6                     ; vision radius
                 mmode-walk            ; pmask
                 15 1                  ; hp mod/mult
                 20 20                 ; mp mod/mult
                 1 0 1 0               ; hit/def/dam/arm mod
                 s_yellow_slime_asleep ; sleep sprite
                 t_acid_spray          ; unarmed weapon
                 #t                    ; visible
                 sound-damage          ; damage sound
                 sound-squishing       ; walking sound
                 nil                   ; slots
                 ;; native spells
                 (list 
                  "KXN"
                  )
                 )

(kern-mk-species 'sp_green_slime       ; tag
                 "green slime"         ; name
                 2 2 1                 ; str/int/dex
                 speed-yellow-slime    ; speed
                 4                     ; vision radius
                 mmode-walk            ; pmask
                 10 1                  ; hp mod/mult
                 0 0                   ; mp mod/mult
                 2 0 2 0               ; hit/def/dam/arm mod
                 s_slime_asleep        ; sleep sprite
                 t_acid_spray          ; unarmed weapon
                 #t                    ; visible
                 sound-damage          ; damage sound
                 sound-squishing       ; walking sound
                 nil                   ; slots
                 nil                   ; native spells
                 )

(kern-mk-species 'sp_skeleton      ; tag
                 "skeleton"        ; name
                 18 10 24          ; str/int/dex
                 speed-human       ; speed
                 6                 ; vision radius
                 mmode-walk        ; passability
                 22 2              ; hp mod/mult
                 0  0              ; mp mod/mult
                 0 1 0 0           ; hit/def/dam/arm mod
                 s_corpse          ; sleep sprite
                 t_hands           ; natural (unarmed) weapon
                 #t                ; visible
                 sound-damage      ; damage sound
                 sound-walking     ; walking sound
                 humanoid-slots    ; slots
                 nil               ; native spells
                 )

(kern-mk-species 'sp_snake         ; tag
                 "snake"           ; name
                 5 1 5             ; str/int/dex
                 speed-human       ; speed
                 6                 ; vision radius
                 mmode-walk        ; passability
                 5 1               ; hp mod/mult
                 20 10             ; mp mod/mult
                 0 2 0 0           ; hit/def/dam/arm mod
                 nil               ; sleep sprite
                 t_fangs           ; natural (unarmed) weapon
                 #t                ; visible
                 sound-damage      ; damage sound
                 sound-walking     ; walking sound
                 nil               ; slots
                 nil               ; native spells FIXME!!!!
                 )

(define undead-species-tags
  (list sp_skeleton))

(define (species-is-undead? species)
  (foldr (lambda (x undead) (or x (eqv? species undead)))
         #f undead-species-tags))

;; A slime constructor which attaches the native effects:
(define (slime-init slime)
    (kern-obj-add-effect slime ef_slime_split nil)
    (kern-obj-add-effect slime ef_poison_immunity nil))
  
(define (mk-slime align)
  (let ((slime (kern-mk-stock-char sp_green_slime 
                                   nil
                                   s_slime
                                   " a slime" 
                                   align
                                   nil)))
    (slime-init slime)
    slime
    ))

(load "yellow-slime.scm")
