;;----------------------------------------------------------------------------
;; Species declaration (used by the kernel)
;;----------------------------------------------------------------------------
(kern-mk-species 'sp_troll      ;; tag: script variable name
                 "troll"        ;; name: used to display name in the UI
                 20             ;; strength: limits armament weight
                 0              ;; intelligence: unused by kernel (just reported in stats)
                 0              ;; dexterity: used to avoid traps on chests
                 speed-human    ;; speed: action points per turn
                 10             ;; vision radius: in tiles
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
                                   'generic-ai)))
    (kern-being-set-base-faction troll faction)
    troll ;; return the kernel object
    ))

(define (mk-hill-troll)
  (mk-troll faction-hill-troll))

(define (char-is-troll? kchar)
  (eqv? (kern-char-get-species kchar) sp_troll))

;;----------------------------------------------------------------------------
;; Generic AI used by kernel, reproduced here as a starting point
;;----------------------------------------------------------------------------
(define (ai-select-target kchar)
  (display "ai-select-target")(newline)
  (nearest-obj kchar (all-visible-hostiles kchar)))

(define (ai-wander kchar)
  (display "ai-wander")(newline)
  (kern-obj-wander kchar))

(define (in-range? karms dist)
  (display "in-range?")(newline)
  (<= dist (kern-arms-type-get-range karms)))

(define (has-ammo? kchar karms)
  (display "has-ammo?")(newline)
  (or (not (arms-type-needs-ammo? karms))
      (let ((ammo-type (kern-arms-type-get-ammo-type karms)))
        (display "has-ammo?: ammo-type=")(display ammo-type)(newline)
        (or (null? ammo-type)
            (kern-obj-has? kchar ammo-type)))))

(define (weapon-blocked? karms dist)
  (display "weapon-blocked?")(newline)
  (and (< dist 2)
       (arms-type-is-blockable? karms)))

(define (ai-select-weapon katt kdef)  
  (let ((defdist (distance katt kdef)))
  (display "ai-select-weapon:defdist=")(display defdist)(newline)
    (define (weapon-ok? karms)
      (display "ai-select-weapon:weapon-ok?")(newline)
      (and (in-range? karms defdist)
           (has-ammo? katt karms)
           (not (weapon-blocked? karms defdist))))
    (define (scan-weapons wlist)
      (display "ai-select-weapon:scan-weapons")(newline)
      (if (null? wlist) nil
          (let ((karms (car wlist)))
            (if (weapon-ok? karms)
                karms
                (scan-weapons (cdr wlist))))))
    (scan-weapons (kern-char-get-weapons katt))))

(define (ai-attack-target kchar ktarg)
  (display "ai-attack-target")(newline)
  (define (do-attack-loop retval)
    (let ((kweap (ai-select-weapon kchar ktarg)))
      (display "ai-attack-target:kweap=")(display kweap)(newline)
      (if (null? kweap) retval
          (begin
            (kern-char-attack kchar kweap ktarg)
            (if (and (is-alive? ktarg)
                     (has-ap? kchar))
                (do-attack-loop #t)
                #t)))))
  (do-attack-loop #f))

(define (ai-pathfind-to-target kchar ktarg)
  (display "ai-pathfind-to-target")(newline)
  (pathfind kchar (kern-obj-get-location ktarg)))

(define (generic-ai kchar)
  (display "generic-ai")(newline)
  (let ((ktarg (ai-select-target kchar)))
    (display "generic-ai: ktarg=")(display ktarg)(newline)
    (if (null? ktarg)
        (ai-wander kchar)
        (or (ai-attack-target kchar ktarg)
            (ai-pathfind-to-target kchar ktarg)))))
