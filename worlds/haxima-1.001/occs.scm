
;;----------------------------------------------------------------------------
;; Occs
;;----------------------------------------------------------------------------
(kern-mk-occ 'oc_wizard           ; tag
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
             nil                  ; container 
             6                    ; xpval
             nil                  ; typical traps 
             (list 
              t_dagger
              )
             (list                ; typical items
              (list t_dagger 100 1)              
              )
             )

(kern-mk-occ 'oc_wanderer         ; tag
             "wanderer"           ; name 
             2.0                  ; magic 
             +5                   ; base hp 
             +2                   ; hp per level 
             +5                   ; base mp
             +2                   ; mp per level 
             +1                   ; hit_mod 
             +1                   ; def_mod 
             +1                   ; dam_mod 
             +1                   ; arm_mod
             nil                  ; container 
             16                   ; xpval
             nil                  ; typical traps 
             nil                  ; typical arms
             nil                  ; typical items
             )

(kern-mk-occ 'oc_druid
             "wizard"             ; name 
             2.0                  ; magic 
             -1                   ; hp_mod 
             +0                   ; hp_mult 
             +20                  ; mp_mod 
             +10                  ; mp_mult 
             0                    ; hit_mod 
             0                    ; def_mod 
             0                    ; dam_mod 
             0                    ; arm_mod
             t_small_wooden_chest ; container 
             4                    ; xpval
             nil                  ; typical traps 
             (list 
              t_dagger
              )
             (list                ; typical items
              (list t_dagger 100 1)              
              )
             )

(kern-mk-occ 'oc_raider           ; tag
             "raider"             ; name 
             0.0                  ; magic 
             +2                   ; hp_mod 
             +2                   ; hp_mult 
             -10                  ; mp_mod 
             -5                   ; mp_mult 
             +2                   ; hit_mod 
             -1                   ; def_mod 
             +2                   ; dam_mod 
             -1                   ; arm_mod
             t_small_wooden_chest ; container 
             4                    ; xpval
             (list 'test-trap)    ; typical traps 
             ;; readied:
             nil
             ;; items:
             (list
              (list t_dagger 75 1)
              (list t_spear  50 3)
              (list t_mushroom 50 2)
              (list t_arrow 50 20)
              (list t_sm_shield 50 1)
              (list t_bow 50 1)
              (list t_iron_helm 50 1)
              )
             )

(kern-mk-occ 'oc_bandit           ; tag
             "bandit"             ; name 
             0.0                  ; magic 
             0                    ; hp_mod 
             0                    ; hp_mult 
             0                    ; mp_mod 
             0                    ; mp_mult 
             +1                   ; hit_mod 
             -1                   ; def_mod 
             0                    ; dam_mod 
             0                    ; arm_mod
             t_small_wooden_chest ; container 
             4                    ; xpval
             (list 'spike-trap)   ; typical traps 
             ;; readied:
             nil
             ;; items:
             (list
              (list t_dagger        75 1)
              (list short-sword     50 1)
              (list t_oil           25 10)
              (list t_mace          50 1)
              (list t_bolt          50 20)
              (list t_xbow          25 1)
              (list t_sm_shield     75 1)
              (list t_armor_leather 90 1)
              (list t_leather_helm  90 1)
              (list t_gold_coins    50 10)
              )
             )
