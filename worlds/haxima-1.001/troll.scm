(define troll-base-hp       20)
(define troll-critical-hp   10)
(define troll-melee-weapon  t_hands)
(define troll-ranged-weapon t_thrown_boulder)

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
                 troll-base-hp  ;; base hp: hit points at level zero
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
             (list troll-ranged-weapon)
             ;; items: typical equipment
             (list (list troll-ranged-weapon 100 3)
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
                                   'troll-ai)))
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

;;----------------------------------------------------------------------------
;; Troll AI
;;----------------------------------------------------------------------------
(define (troll-is-critical? ktroll)
  (< (kern-char-get-hp ktroll) troll-critical-hp))

(define (troll-wander ktroll) (wander ktroll))
(define (troll-flee ktroll) (flee ktroll))

(define (troll-foes-in-weapon-range ktroll karms kfoes)
  (display "troll-foes-in-weapon-range")(newline)
  (all-in-range (kern-obj-get-location ktroll) 
                (kern-arms-type-get-range karms)
                 kfoes))

(define (weaker? a b)
  (< (kern-char-get-hp a) (kern-char-get-hp b)))

(define (troll-pick-target ktroll foes)
  (foldr (lambda (a b) (if (weaker? a b) a b))
         (car foes) 
         (cdr foes)))

(define (troll-pathfind-foe ktroll foes)
  (let ((ktarg (troll-pick-target ktroll foes)))
    (if (notnull? ktarg)
        (pathfind ktroll (kern-obj-get-location (troll-pick-target ktroll foes))))))

(define (troll-attack ktroll karms foes)
  (kern-char-attack ktroll 
                    karms
                    (troll-pick-target ktroll 
                                       foes)))

(define (troll-find-nearest-ammo ktroll)
  (let* ((loc (kern-obj-get-location ktroll))
         (rad (kern-obj-get-vision-radius ktroll))
         (coords (find-terrain (loc-place loc)
                               (- (loc-x loc) rad)
                               (- (loc-y loc) rad)
                               (* 2 rad)
                               (* 2 rad)
                               t_boulder)))
    ;;(display "troll-find-nearest-ammo:")
    ;;(display " coords=")(display coords)
    ;;(newline)
    ;;(display " loc=")(display loc)
    ;;(newline)
    (define (closer? a b)
      (< (loc-distance a loc)
         (loc-distance b loc)))
    (if (null? coords)
        nil
        (foldr (lambda (a b) (if (closer? a b) a b))
               (car coords)
               (cdr coords)))))

(define (troll-stronger? ktroll foes)
  (display "troll-stronger?")(newline)
  (> (kern-char-get-strength ktroll)
     (foldr (lambda (a b) (+ a (kern-char-get-strength b))) 
            0 
            foes)))

(define (troll-has-ranged-weapon? ktroll)
  (in-inventory? ktroll troll-ranged-weapon))

;; Simple approach: each foe's coordinates forms a vector to the troll's
;; coordinates. Take the sum of these coordinates to get the evasion
;; vector. "Normalize" the vector components by rounding them to the nearest 0,
;; 1 or -1. This is the dx/dy to move. If the terrain is impassable in the
;; preferred direction then try zeroing out the non-zero components and
;; moving. This will give two backup vectors to try.
(define (troll-evade ktroll foes)
  (display "troll-evade")(newline)
  (let* ((tloc (kern-obj-get-location ktroll))
         (v (loc-norm (foldr (lambda (a b) (loc-sum a (loc-diff tloc (kern-obj-get-location b))))
                             (mk-loc (loc-place tloc) 0 0)
                             foes))))
    (display "troll-evade:v=")(display v)(newline)
    (or (kern-obj-move ktroll (loc-x v) (loc-y v))
        (and (!= 0 (loc-y v))
             (kern-obj-move ktroll (loc-x v) 0))
        (and (!= 0 (loc-x v))
             (kern-obj-move ktroll 0 (loc-y v))))))
  
;; troll-get-ammo -- give troll a boulder and convert terrain to grass
(define (troll-get-ammo ktroll coords)
  (display "troll-get-ammo")(newline)
  (kern-obj-add-to-inventory ktroll troll-ranged-weapon 1)
  (kern-place-set-terrain coords t_grass))

(define (troll-ai ktroll)
  (display "troll-ai")(newline)
  (let ((foes (all-visible-hostiles ktroll)))
    (if (null? foes)
        (troll-wander ktroll)
        (if (troll-is-critical? ktroll) 
            (troll-flee ktroll)
            (let ((melee-targs (troll-foes-in-weapon-range ktroll 
                                                           troll-melee-weapon 
                                                           foes)))
              (if (null? melee-targs)
                  (if (troll-has-ranged-weapon? ktroll)
                      (let ((ranged-foes (troll-foes-in-weapon-range ktroll
                                                                     troll-ranged-weapon
                                                                     foes)))
                        (if (null? ranged-foes)
                            (troll-pathfind-foe ktroll foes)
                            (troll-attack ktroll troll-ranged-weapon ranged-foes)))
                      (let ((ammo-coords (troll-find-nearest-ammo ktroll)))
                        (if (null? ammo-coords)
                            (troll-pathfind-foe ktroll foes)
                            (if (adjacent? (kern-obj-get-location ktroll) ammo-coords)
                                (troll-get-ammo ktroll ammo-coords)
                                (pathfind ktroll ammo-coords)))))
                  (if (troll-stronger? ktroll melee-targs)
                      (troll-attack ktroll troll-melee-weapon melee-targs)
                      (troll-evade ktroll melee-targs))))))))
