
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
             6                    ; xpval
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
             16                   ; xpval
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
             4                    ; xpval
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
             4                    ; xpval
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
             4                    ; xpval
             )

(kern-mk-occ 'oc_troll            ;; tag
             "troll"              ;; name 
             0.0                  ;; magic 
             2                    ;; base hp
             2                    ;; hp per level 
             0                    ;; base mp
             0                    ;; mp per level 
             2                    ;; hit_mod 
             -1                   ;; def_mod 
             2                    ;; dam_mod 
             -1                   ;; arm_mod
             4                    ;; xpval
             )
