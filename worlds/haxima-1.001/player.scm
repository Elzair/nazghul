;;----------------------------------------------------------------------------
;; Occs
;;----------------------------------------------------------------------------
(kerm-mk-occ 'oc_wizard          ; tag
             "wizard"             ; name 
             2.0                  ; magic 
             -5                   ; hp_mod 
             +0                   ; hp_mult 
             +30                  ; mp_mod 
             +15                  ; mp_mult 
             -1                   ; hit_mod 
             -1                   ; def_mod 
             -1                   ; dam_mod 
             -1                   ; arm_mod 
             t_small_wooden_chest ; container 
             (list t_poison_trap) ; typical traps 
             (list 
              t_dagger
              t_sword
              t_sm_shield
              t_oil)
             (list                ; typical items
              (list t_dagger 100 1)
              (list t_sword 50 1)
              (list t_sm_shield 50 1)
              (list t_oil 50 10)
              (list t_light_potion 50 5)
              (list t_cure_potion 50 2)
              (list t_heal_potion 75 5)
              (list t_restore_potion 50 2))

(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod/mult
              0 0                   ; dam mod/mult
              240 0 "G" 240 8       ; hp/xp/cond/mp/lvl
              c_generic             ; conv
              (list t_magic_bow     ; readied
                    t_leather_cap
                    t_leather_boots
                    t_leather_armor))

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
;; left off here
(kern-mk-char 'ch_thorald_greybeard ; tag
              "Thorald Greybeard"   ; name
              sp_human              ; species
              oc_wizard             ; occ
              s_companion_wizard    ; sprite
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod/mult
              0 0                   ; dam mod/mult
              240 0 "G" 240 8       ; hp/xp/cond/mp/lvl
              nil                   ; conv
              (list t_dagger        ; readied
                    ))
;;----------------------------------------------------------------------------
;; Player
;;----------------------------------------------------------------------------
(kern-mk-player  p_dark_passage 1 1          ; location
                 s_companion_fighter         ; sprite
                 "Walk"                      ; movement description
                 "data/sounds/walk.wav"      ; movement sound
                 1000                        ; food
                 500                         ; gold
                 (+ align-player align-town) ; alignment
                 nil                         ; formation
                 m_campsite                  ; campsite map
                 nil                         ; campsite formation
                 nil                         ; inventory
                 (list ch_thorald_greybeard) ; party members
)

(kern-mk-palette 'std_palette
                 (list
                  (list "_" t_deep)
                  (list "-" t_shallow)
                  (list "~" t_shoals)
                  (list "%" t_bog)
                  (list "." t_grass)
                  (list "t" t_trees)
                  (list "|" t_forest)
                  (list "{" t_hills)
                  (list "^" t_mountains)
                  (list "," t_flagstones)
                  (list "c" t_cobblestone)
                  (list "e" t_deck)
                  (list "=" t_bridge)
                  (list "g" t_bridge_top)
                  (list "h" t_bridge_bottom)
                  (list "f" t_fire_terrain)
                  (list "!" t_lava)
                  (list "&" t_fireplace)
                  (list "x" t_wall)
                  (list "?" t_secret_door)
                  (list "p" t_pillar)
                  (list "b" t_boulder)
                  (list "r" t_wall_rock)
                  (list "A" t_A)
                  (list "B" t_B)
                  (list "C" t_C)
                  (list "D" t_D)
                  (list "E" t_E)
                  (list "F" t_F)
                  (list "G" t_G)
                  (list "H" t_H)
                  (list "I" t_I)
                  (list "J" t_J)
                  (list "K" t_K)
                  (list "L" t_L)
                  (list "M" t_M)
                  (list "N" t_N)
                  (list "O" t_O)
                  (list "P" t_P)
                  (list "Q" t_Q)
                  (list "R" t_R)
                  (list "S" t_S)
                  (list "T" t_T)
                  (list "U" t_U)
                  (list "V" t_V)
                  (list "W" t_W)
                  (list "X" t_X)
                  (list "Y" t_Y)
                  (list "Z" t_Z)
                  (list "q" t_rune_Q)
                  (list "@" t_counter_middle)
                  (list "[" t_counter_left)
                  (list "]" t_counter_right)
                  (list "0" t_counter_1x1)
                  (list "+" t_ankh)
                  (list "a" t_altar)
                  (list "<" t_leftwing)
                  (list ">" t_rightwing)
                  (list "d" t_red_roses)
                  (list "/" t_trail_f)))

;; An object can be fire-proof, which means ITS CONTENTS will not be damaged by
;; fire. Or an object can be immune to fire, which means IT will not be damaged
;; by fire. The two are independent.
(define (effect-fire obj damage-amount)
  (define (apply-damage obj)
    (kern-obj-apply-damage obj fire-damage damage-amount))
  (if (not (kern-obj-proof? fire-damage))
      (map apply-damage (kern-obj-contents obj)))
  (if (not (kern-obj-immunge? fire-damage))
      (apply-damage obj)))

(define (effect-lava obj)
  (effect-fire obj lava-damage-amount))

(kern-attach-effect t_lava effect-lava)
