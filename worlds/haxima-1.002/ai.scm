;;----------------------------------------------------------------------------
;; Generic AI used by kernel, reproduced here as a starting point
;;----------------------------------------------------------------------------
(define (ai-display args) nil)
(define (ai-newline) nil)

(define (ai-select-target kchar)
  (ai-display "ai-select-target")(ai-newline)
  (nearest-obj kchar (all-visible-hostiles kchar)))

(define (ai-wander kchar)
  (ai-display "ai-wander")(ai-newline)
  (kern-obj-wander kchar))

(define (in-range-of-arms? karms dist)
  (ai-display "in-range-of-arms?")(ai-newline)
  (<= dist (kern-arms-type-get-range karms)))

(define (has-ammo? kchar karms)
  (ai-display "has-ammo?")(ai-newline)
  (or (not (arms-type-needs-ammo? karms))
      (let ((ammo-type (kern-arms-type-get-ammo-type karms)))
        (ai-display "has-ammo?: ammo-type=")(ai-display ammo-type)(ai-newline)
        (or (null? ammo-type)
            (kern-obj-has? kchar ammo-type)))))

(define (weapon-blocked? karms dist)
  (ai-display "weapon-blocked?")(ai-newline)
  (and (< dist 2)
       (arms-type-is-blockable? karms)))

(define (ai-select-weapon katt kdef)  
  (let ((defdist (distance katt kdef)))
  (ai-display "ai-select-weapon:defdist=")(ai-display defdist)(ai-newline)
    (define (weapon-ok? karms)
      (ai-display "ai-select-weapon:weapon-ok?")(ai-newline)
      (and (in-range-of-arms? karms defdist)
           (has-ammo? katt karms)
           (not (weapon-blocked? karms defdist))))
    (define (scan-weapons wlist)
      (ai-display "ai-select-weapon:scan-weapons")(ai-newline)
      (if (null? wlist) nil
          (let ((karms (car wlist)))
            (if (weapon-ok? karms)
                karms
                (scan-weapons (cdr wlist))))))
    (scan-weapons (kern-char-get-weapons katt))))

(define (ai-attack-target kchar ktarg)
  (ai-display "ai-attack-target")(ai-newline)
  (define (do-attack-loop retval)
    (let ((kweap (ai-select-weapon kchar ktarg)))
      (ai-display "ai-attack-target:kweap=")(ai-display kweap)(ai-newline)
      (if (null? kweap) retval
          (begin
            (kern-char-attack kchar kweap ktarg)
            (if (and (is-alive? ktarg)
                     (has-ap? kchar))
                (do-attack-loop #t)
                #t)))))
  (do-attack-loop #f))

(define (ai-pathfind-to-target kchar ktarg)
  (ai-display "ai-pathfind-to-target")(ai-newline)
  (pathfind kchar (kern-obj-get-location ktarg)))

(define (generic-ai kchar)
  (ai-display "generic-ai")(ai-newline)
  (let ((ktarg (ai-select-target kchar)))
    (ai-display "generic-ai: ktarg=")(ai-display ktarg)(ai-newline)
    (if (null? ktarg)
        (ai-wander kchar)
        (begin
          (taunt kchar ktarg)
          (or (ai-attack-target kchar ktarg)
              (ai-pathfind-to-target kchar ktarg))))))

;; Bandit AI --------------------------------------------------

(define bandit-taunts 
  (list 
   "Yer money or yer life!"
   "Have at 'cher!"
   "Yer a dead man, ye are!"
   "Oy!  You!  Gerrout!"
   "'Ave at 'im, boys!"
   "Circle round, we've got a dead one!"
   "Dibs on 'is boots!"
   "Stranger, meetcha couple my friends..."
   ))

(define (bandit-taunt kbandit ktarg)
  (taunt kbandit ktarg bandit-taunts)
  (npcg-set-taunted! (gob kbandit) #t))

(define (bandit-ai kchar)
  (let ((ktarg (ai-select-target kchar)))
    (if (null? ktarg)
        (ai-wander kchar)
        (begin
          (or (npcg-taunted? (gob kchar))
              (bandit-taunt kchar ktarg))
          (or (ai-attack-target kchar ktarg)
              (ai-pathfind-to-target kchar ktarg))))))

;;----------------------------------------------------------------------------
;; spell-sword-ai -- aggressive, selfish fighter that uses magic for combat.
(define (std-moves? kchar)
  (or (get-off-bad-tile? kchar)
      (use-potion? kchar)))

(define (spell-sword-ai kchar)
  ;;(display "spell-sword-ai")(newline)
  (or (std-moves? kchar)
      (use-spell-on-self? kchar)
      (use-melee-spell-on-foes? kchar)
      (use-ranged-spell-on-foes? kchar)))


(define (shaman-ai kchar)
  (display "shaman-ai ")(dump-char kchar)
  (or (std-moves? kchar)
      (use-heal-spell-on-ally? kchar)
      (move-toward-patient? kchar)
      (spell-sword-ai kchar)
      (move-away-from-foes? kchar)))

;;----------------------------------------------------------------------------
;; Forest goblin hunters prefer to attack at range while protected by forest
;; cover from enemy missiles. They avoid melee except in heavy forest where
;; they have the movement and visibility advantage (they have no movement
;; penalty in forest and can "hear" things in the forest beyond LOS).
;;
;; Their first priority is to move to a forested tile. Once there they will
;; engage foes at range with their bows until the enemy closes with them or
;; they run out of ammo. They always target the physically weakest enemy in
;; range. When ranged combat is impossible they will attempt to withdraw into
;; deeper forest and there wait in ambush. Once in the forest they will switch
;; to melee weapons. If an enemy enters the forest they will attack.
(define (take-cover-in-forest? kchar)
  (println "take-cover-in-forest?")
  (println " on-terrain? " (on-terrain? kchar t_forest))
  (if (on-terrain? kchar t_forest)
      #f
      (let ((loc (find-nearest-visible-terrain-of-type kchar t_forest)))
        (println "  loc=" loc)
        (if (null? loc)
            #f
            (pathfind kchar loc)))))

(define (nearest-foe-is-melee? kchar nearest)
  (println "nearest-foe-is-melee?")
  (<= (distance kchar nearest) 2))

(define (hide-in-forest? kchar nearest)
  (println "hide-in-forest?")
  (define (is-hiding-place? loc)
    (println "  is-hiding-place? " loc)
    (and (passable? loc kchar)
         (eqv? t_forest
               (kern-place-get-terrain loc))))
  (let* ((curloc (kern-obj-get-location kchar))
         (foeloc (kern-obj-get-location nearest))
         (vect (loc-norm (loc-diff curloc foeloc)))
         (locs (filter is-hiding-place?
                       (append (loc-opposite-x curloc (loc-x vect))
                               (loc-opposite-y curloc (loc-y vect))))))
    (if (null? locs)
        #f
        (let* ((newloc (car locs))
              (vect (loc-norm (loc-diff newloc curloc)))
              (dx (loc-x vect))
              (dy (loc-y vect)))
          (println "  locs=" locs)
          (println "  move to" newloc " dx=" dx " dy=" dy)
          (kern-obj-move kchar dx dy)))))

; (define (in-melee-range? kchar kfoe)
;   (<= (distance kchar kfoe) 2))

; (define (using-missile-weapon? kchar)
;   (foldr (lambda (x kweap) (or x (is-missile-weapon? kweap)))
;          #f
;          (kern-char-get-readied-weapons kchar)))

; (define (using-melee-weapon? kchar)
;   (foldr (lambda (x kweap) (or x (not (is-missile-weapon? kweap))))
;          #f
;          (kern-char-get-readied-weapons kchar)))

; (define (unready-missile-weapons kchar)
;   (define (unready-if-missile kweap)
;     (if (is-missile-weapon? kweap)
;         (kern-char-unready kchar kweap)))
;   (map unready-if-missile
;        (kern-char-get-readied-weapons kchar)))

; (define (ready-melee-weapons kchar)
;   ;; left off here...

; (define (switch-to-melee-weapon kchar)
;   (unready-missile-weapons kchar)
;   (ready-melee-weapons kchar))

; (define (switch-weapons kchar)
;   (let ((nearest (nearest-obj kchar (all-visible-hostiles kchar))))
;     (if (null? nearest)
;         #f
;         (if (in-melee-range? kchar nearest)
;             (if (using-missile-weapon? kchar)
;                 (switch-to-melee-weapon kchar)
;                 #f)
;             (if (using-melee-weapon? kchar)
;                 (switch-to-missile-weapon kchar)
;                 #f)))))

(define (forest-goblin-hunter-ai kchar)
  (display "fgh-ai ")(dump-char kchar)
  (std-moves? kchar))

;       (take-cover-in-forest? kchar)
;       (switch-weapons kchar)
;       ))

;       (let ((nearest (nearest-obj kchar (all-visible-hostiles kchar))))
;         (if (null? nearest)
;             #f
;             (and (nearest-foe-is-melee? kchar nearest)
;                  (not (hidden? kchar))
;                  (hide-in-forest? kchar nearest))))))
