;;----------------------------------------------------------------------------
;; Species declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-species 'sp_troll      ;; tag: script variable name
                 "troll"        ;; name: used to display name in the UI
                 20             ;; strength: limits armament weight
                 0              ;; intelligence: unused by kernel (just reported in stats)
                 0              ;; dexterity: used to avoid traps on chests
                 speed-human    ;; speed: action points per turn
                 6              ;; vision radius: in tiles
                 mmode-walk     ;; movement mode: determines passability and cost of travel
                 20             ;; base hp: hit points at level zero
                 2              ;; hp multiplier: extra hp per level
                 0              ;; base mp: mana points at level zero
                 0              ;; mp multiplier: extra mana points per level
                 s_corpse       ;; sleep sprite: shown when sleeping/dead/unconscious
                 t_hands        ;; natural weapon: used when unarmed
                 #t             ;; visible: can be seen
                 sound-damage   ;; damage sound
                 sound-walking  ;; walking sound
                 humanoid-slots ;; slots: hands
                 nil            ;; native spells: currently unused
                 )

;;----------------------------------------------------------------------------
;; Occupation declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-occ 'oc_troll            ;; tag
             "raider"             ;; name 
             0.0                  ;; magic 
             +2                   ;; hp_mod 
             +2                   ;; hp_mult 
             -10                  ;; mp_mod 
             -5                   ;; mp_mult 
             +2                   ;; hit_mod 
             -1                   ;; def_mod 
             +2                   ;; dam_mod 
             -1                   ;; arm_mod
             t_small_wooden_chest ;; container (needed for items)
             nil                  ;; typical traps on the container
             ;; readied:
             (list t_thrown_boulder)
             ;; items: typical equipment
             (list (list t_thrown_boulder 100 3)
                   )
             )

;;----------------------------------------------------------------------------
;; Constructor
;;----------------------------------------------------------------------------
(define (mk-troll faction)
  (let ((troll (kern-mk-stock-char sp_troll 
                                   oc_troll
                                   s_troll ;; no troll sprite yet
                                   "a troll" 
                                   nil)))
    (kern-being-set-base-faction troll faction)
    troll ;; return the kernel object
    ))

