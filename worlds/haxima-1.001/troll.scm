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
                 0              ;; intelligence: (just reported in stats)
                 0              ;; dexterity: used to avoid traps on chests
                 speed-human    ;; speed: action points per turn
                 10             ;; vision radius: in tiles
                 mmode-walk     ;; movement mode
                 troll-base-hp  ;; base hp: hit points at level zero
                 2              ;; hp multiplier: extra hp per level
                 0              ;; base mp: mana points at level zero
                 0              ;; mp multiplier: extra mana points per level
                 s_corpse       ;; sleep sprite
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
        (pathfind ktroll (kern-obj-get-location (troll-pick-target ktroll 
                                                                   foes))))))

(define (troll-attack ktroll karms foes)
  (kern-char-attack ktroll 
                    karms
                    (troll-pick-target ktroll 
                                       foes)))

;; Simple approach: each foe's coordinates forms a vector to the troll's
;; coordinates. Take the sum of these coordinates to get the evasion
;; vector. "Normalize" the vector components by rounding them to the nearest 0,
;; 1 or -1. This is the dx/dy to move. If the terrain is impassable in the
;; preferred direction then try zeroing out the non-zero components and
;; moving. This will give two backup vectors to try.
;;
;; ADDENDUM: I don't want to allow diagonal evasion, so the "normalized" vector
;; must be skipped if it's a diagonal, thus causing us to try the fallbak
;; vector(s).
(define (troll-evade ktroll foes)
  (display "troll-evade")
  (display " foes=")(display foes)
  (newline)
  (let* ((tloc (kern-obj-get-location ktroll))
         (v (loc-norm (foldr (lambda (a b) 
                               (loc-sum a 
                                        (loc-diff tloc 
                                                  (kern-obj-get-location b))))
                             (mk-loc (loc-place tloc) 0 0)
                             foes))))
    (display "troll-evade:v=")(display v)(newline)
    (define (evade-on-normal)
      (and (or (eq? 0 (loc-x v))
              (eq? 0 (loc-y v)))
           (kern-obj-move ktroll (loc-x v) (loc-y v))))
    (or (evade-on-normal)
        (and (not (eq? 0 (loc-y v)))
             (kern-obj-move ktroll (loc-x v) 0))
        (and (not (eq? 0 (loc-x v)))
             (kern-obj-move ktroll 0 (loc-y v))))))
  
(define (loc-closer? a b c)
  (< (loc-distance a c)
     (loc-distance b c)))

;; Given an "origin" location and a list of locations, find the location in the
;; list closest to the coordinates.
(define (loc-closest origin lst)
  (if (null? lst) nil
      (foldr (lambda (a b) (if (loc-closer? a b origin) a b))
             (car lst)
             (cdr lst))))

(define (troll-find-nearest-terrain-ammo ktroll)
  (let* ((loc (kern-obj-get-location ktroll))
         (rad (kern-obj-get-vision-radius ktroll))
         (coords (find-terrain (loc-place loc)
                               (- (loc-x loc) rad)
                               (- (loc-y loc) rad)
                               (* 2 rad)
                               (* 2 rad)
                               t_boulder)))
    (loc-closest loc coords)))

;;
;; NOTE: find-objects not completely implemented yet
;;
(define (troll-find-nearest-loose-ammo ktroll)
  (let* ((loc (kern-obj-get-location ktroll))
         (rad (kern-obj-get-vision-radius ktroll))
         (coords (find-objects (loc-place loc)
                               (- (loc-x loc) rad)
                               (- (loc-y loc) rad)
                               (* 2 rad)
                               (* 2 rad)
                               t_thrown_boulder)))
    (loc-closest loc coords)))

(define (troll-stronger? ktroll foes)
  (> (kern-char-get-strength ktroll)
     (foldr (lambda (a b) (+ a (kern-char-get-strength b))) 
            0 
            foes)))

(define (troll-has-ranged-weapon? ktroll)
  (in-inventory? ktroll troll-ranged-weapon))

;; troll-get-ammo -- give troll a boulder and convert terrain to grass
(define (troll-get-terrain-ammo ktroll coords)
  (display "troll-get-terrain-ammo")(newline)
  (kern-obj-add-to-inventory ktroll troll-ranged-weapon 1)
  (kern-place-set-terrain coords t_grass)
  )

(define (troll-get-loose-ammo ktroll coords)
  (display "troll-get-loose-ammo")
  (newline)
  (let ((ammo (filter (lambda (a) (eqv? (kern-obj-get-type a) 
                                        troll-ranged-weapon))
                      (kern-place-get-objects-at coords))))
    (display "ammo=")(display ammo)(newline)
    (kern-obj-remove (car ammo))
    (kern-obj-add-to-inventory ktroll troll-ranged-weapon 1)))

(define (kchar-get-or-goto kchar coords getproc)
  (if (loc-adjacent? (kern-obj-get-location kchar) coords)
      (getproc kchar coords)
      (pathfind kchar coords)))

(define (troll-hunt-for-ammo ktroll)
  (let ((lac (troll-find-nearest-loose-ammo ktroll))
        (tac (troll-find-nearest-terrain-ammo ktroll))
        (kloc (kern-obj-get-location ktroll)))
    (display "lac=")(display lac)(newline)
    (display "tac=")(display tac)(newline)
    (if (null? lac)
        (if (null? tac)
            #f
            (begin
              (kchar-get-or-goto ktroll tac troll-get-terrain-ammo)
              #t))
        (if (null? tac)
            (kchar-get-or-goto ktroll lac troll-get-loose-ammo)
            (if (loc-closer? lac tac kloc)
                (kchar-get-or-goto ktroll lac troll-get-loose-ammo)
                (kchar-get-or-goto ktroll tac troll-get-terrain-ammo)))
        #t)))

(define (troll-ai ktroll)
  (newline)(display "troll-ai")(newline)
  (let ((foes (all-visible-hostiles ktroll)))
    (if (null? foes)
        (troll-wander ktroll)
        (if (troll-is-critical? ktroll) 
            (troll-flee ktroll)
            (let ((melee-targs (troll-foes-in-weapon-range ktroll 
                                                           troll-melee-weapon 
                                                           foes)))
              (display "troll-ai:melee-targs=")(display melee-targs)
              (newline)
              (if (null? melee-targs)
                  (if (troll-has-ranged-weapon? ktroll)
                      (let 
                          ((ranged-foes 
                            (troll-foes-in-weapon-range ktroll
                                                        troll-ranged-weapon
                                                        foes)))
                        (display "troll-ai:ranged-foes=")(display ranged-foes)
                        (newline)
                        (if (null? ranged-foes)
                            (troll-pathfind-foe ktroll foes)
                            (troll-attack ktroll troll-ranged-weapon 
                                          ranged-foes)))
                      (or (troll-hunt-for-ammo ktroll)
                          (troll-pathfind-foe ktroll foes)))
                  (if (troll-stronger? ktroll melee-targs)
                      (troll-attack ktroll troll-melee-weapon melee-targs)
                      (troll-evade ktroll melee-targs))))))))
