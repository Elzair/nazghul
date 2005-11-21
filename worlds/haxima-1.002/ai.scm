;;----------------------------------------------------------------------------
;; procedures for using items or spells on self or others

;; use-potion? -- use potion on self if desired and available
(define (use-potion? kchar)
  (or (and (wants-healing? kchar)
           (has-heal-potion? kchar)
           (drink-heal-potion kchar))
      (and (wants-mana? kchar)
           (has-mana-potion? kchar)
           (drink-mana-potion kchar))))

(define (use-heal-spell-on-self? kchar)
  ;;;;(display "use-heal-spell-on-self?")(newline)
  (and (wants-healing? kchar)
       (can-use-ability? heal-ability kchar)
       (use-ability heal-ability kchar kchar)))

(define (use-great-heal-spell-on-self? kchar)
  ;;;;(display "use-great-heal-spell-on-self?")(newline)
  (and (wants-great-healing? kchar)
       (can-use-ability? great-heal-ability kchar)
       (use-ability great-heal-ability kchar kchar)))

(define (use-spell-on-self? kchar)
  ;;;;(display "use-spell-on-self?")(newline)
  (or (use-great-heal-spell-on-self? kchar)
      (use-heal-spell-on-self? kchar)))

;; use-melee-spell-on-foe? -- randomly select from a list of melee spells and
;; return #t iff the spell is used
(define (use-melee-spell-on-foe? kchar ktarg)
  (let ((spell (random-select (filter (lambda (spell)
                                        (can-use-ability? spell kchar))
                                      melee-spells))))
    (if (null? spell)
        #f
        (use-ability spell kchar ktarg))))

(define (use-melee-spell-on-foes? kchar)
  ;;(display "use-melee-spell-on-foes?")(newline)
  (foldr (lambda (val ktarg)
           (or val
               (use-melee-spell-on-foe? kchar ktarg)))
         #f 
         (get-hostiles-in-range kchar 1)))


(define (use-ranged-spell-on-foe? kchar ktarg)
  (let ((spell-range (random-select (filter (lambda (spell-range)
                                        (and (can-use-ability? (car spell-range)
                                                               kchar)
                                             (can-hit? kchar 
                                                       ktarg 
                                                       (cdr spell-range))))
                                      ranged-spells))))
    (if (null? spell-range)
        #f
        (use-ability (car spell-range) kchar ktarg))))

(define (use-ranged-spell-on-foes? kchar)
  ;;(display "use-ranged-spell-on-foes?")(newline)
  (foldr (lambda (val ktarg)
           ;;(display "ktarg=");;(display ktarg)(newline)
           (or val
               (use-ranged-spell-on-foe? kchar ktarg)))
         #f 
         (all-visible-hostiles kchar)))

(define (use-heal-spell-on? kchar ktarg)
  ;;(println "use-heal-spell-on?")
  (or (and (wants-great-healing? ktarg)
           (can-use-ability? great-heal-ability kchar)
           (use-ability great-heal-ability kchar ktarg)
           )
      (and (wants-healing? ktarg)
           (can-use-ability? heal-ability kchar)
           (use-ability heal-ability kchar ktarg)
           )))

(define (use-heal-spell-on-ally? kchar)
  ;;(println "use-heal-spell-on-ally?")
  (and (or (can-use-ability? heal-ability kchar)
           (can-use-ability? great-heal-ability kchar))
       (foldr (lambda (val ktarg)
                (or val
                    (use-heal-spell-on? kchar ktarg)))
              #f 
              (all-in-range (kern-obj-get-location kchar)
                            2
                            (all-visible-allies kchar)))))

;;----------------------------------------------------------------------------
;; procedures for searching for nearby things

(define (get-nearest-patient kchar)
  (let ((kloc (kern-obj-get-location kchar)))
    (foldr (lambda (kpatient ktarg)
             ;;(display "  checking ")(dump-char ktarg)
             (if (and (wants-healing? ktarg)
                      (or (null? kpatient)                      
                          (< (kern-get-distance kloc 
                                                (kern-obj-get-location ktarg))
                             (kern-get-distance kloc 
                                                (kern-obj-get-location kpatient)))))
                 ktarg
                 kpatient))
           nil
           (all-visible-allies kchar))))


;;----------------------------------------------------------------------------
;; procedures for pursuing or avoiding

(define (avoid-melee? kchar)
  ;;;;(display "avoid-melee? kchar")(newline)
  (let ((nearby-foes (get-hostiles-in-range kchar 1)))
    (if (null? nearby-foes)
        #f
        (evade kchar nearby-foes))))

;; This is for medics. A patient is an ally that needs healing. If a patient is
;; less than 2 tiles away then do nothing. If a patient is more than 2 tiles
;; away then pathfind toward it.
(define (move-toward-patient? kchar)
  (let ((patient (get-nearest-patient kchar)))
    (if (null? patient)
        #f
        (begin
          ;;(display "selected ")(dump-char patient)
          (if (in-range? (kern-obj-get-location kchar)
                         2
                         patient)
              #f
              (pathfind kchar (kern-obj-get-location patient)))))))

(define (get-off-bad-tile? kchar)
  ;;(display "get-off-bad-tile")(newline)
  
  (define (is-bad-loc? loc)
    ;;(display "is-bad-loc?")(newline)
    (or (is-bad-terrain-at? loc)
        (any-object-types-at? loc spider-bad-fields)))

  (define (choose-good-tile tiles)
    ;;(display "choose-good-tile")(newline)
    (define (is-good-tile? tile)
      ;;(display "is-good-tile?")(newline)
      (and (passable? tile kchar)
           (not (occupied? tile))
           (not (is-bad-loc? tile))))
    (if (null? tiles)
        nil
        (if (is-good-tile? (car tiles))
            (car tiles)
            (choose-good-tile (cdr tiles)))))

  (define (move-to-good-tile)
    ;;(display "move-to-good-tile")(newline)
    (let* ((curloc (kern-obj-get-location kchar))
           (tiles (get-4-neighboring-tiles curloc))
           (newloc (choose-good-tile tiles)))
      (if (null? newloc)
          #f
          (begin
            ;;(display "moving")(newline)
            (kern-obj-move kchar 
                           (- (loc-x newloc) (loc-x curloc))
                           (- (loc-y newloc) (loc-y curloc)))
            #t))))

  (and (is-bad-loc? (kern-obj-get-location kchar))
       (move-to-good-tile)))

(define (move-away-from-foes? kchar)
  ;;(println "move-away-from-foes?")
  (evade kchar (all-visible-hostiles kchar)))

(define (in-melee-range-of-foes? kchar)
  (> (length (get-hostiles-in-range kchar 1))
     0))

(define (blink-offset kchar)
  (let ((origin (kern-obj-get-location kchar)))
    (loc-add origin
             (loc-norm (apply loc-add 
                              (map (lambda (kfoe)
                                     (loc-diff origin
                                               (kern-obj-get-location kfoe)))
                                   (all-visible-hostiles kchar)))))))

(define (choose-blink-loc kchar)
  (let ((loc (blink-offset kchar)))
    (println "choose-blink-loc " loc)
    (if (and (not (null? loc))
             (not (loc-equal? loc
                              (kern-obj-get-location kchar)))
             (passable? loc kchar)
             (not (is-bad-terrain-at? loc))
             (not (any-object-types-at? loc all-field-types))
             (not (occupied? loc)))
        loc
        nil)))

(define (blink-away-from-foes kchar)
  (if (not (can-use-ability? teleport kchar))
      #f
      (let ((loc (choose-blink-loc kchar)))
        (if (null? loc)
            #f
            (use-ability teleport kchar loc)))))

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


(define (animal-ai kchar)
  (get-off-bad-tile? kchar))

(define (std-ai kchar)
  (or (get-off-bad-tile? kchar)
      (use-potion? kchar)))

;;----------------------------------------------------------------------------
;; spell-sword-ai -- aggressive, selfish fighter that uses magic for combat.
(define (spell-sword-ai kchar)
  (println "spell-sword-ai")
  (or (std-ai kchar)
      (use-spell-on-self? kchar)
      (use-melee-spell-on-foes? kchar)
      (use-ranged-spell-on-foes? kchar)))

(define (shaman-ai kchar)
  ;;(display "shaman-ai ")(dump-char kchar)
  (or (std-ai kchar)
      (use-heal-spell-on-ally? kchar)
      (move-toward-patient? kchar)
      (spell-sword-ai kchar)
      (move-away-from-foes? kchar)))

(define (priest-ai kchar)
  (or (std-ai kchar)
      (and (in-melee-range-of-foes? kchar)
           (blink-away-from-foes kchar))
      (spell-sword-ai kchar)))

(define (generic-ai kchar)
  (std-ai kchar))

;; Death knights can use Vampiric Touch at L3 and Disease at L6
(define (death-knight-ai kchar)
  (or (use-potion? kchar)
      (let ((vt (can-use-ability? vampiric-touch kchar))
            (dis (can-use-ability? disease-touch kchar)))
        (if (not (or vt dis))
            #f
            (let ((victims (get-hostiles-in-range kchar 1)))
              (if (null? victims)
                  #f
                  (if (wants-healing? kchar)
                      (use-ability vampiric-touch kchar (car victims))
                      (if (and dis
                               (>= (kern-dice-roll "1d20") 16))
                          (use-ability disease-touch kchar (car victims))
                          #f))))))))

;; guard-ai
(define (guard-ai kchar)
  (define (try-to-use-ability)
    ;;(display "try-to-use-ability")(newline)
    (if (can-use-ability? disarm kchar)
        (let ((victims (get-hostiles-in-range kchar 1)))
          (and (not (null? victims))
               (>= (kern-dice-roll "1d20") 16)
               (or (use-ability disarm kchar (car victims))
                   #t)))
        #f))
  (define (goto-post)
    (println "goto-post")
    (let ((guard (gob kchar)))
      (if (npcg-has-post? guard)
          (let ((post (cons (loc-place (kern-obj-get-location kchar))
                            (npcg-get-post guard))))
            (println "post:" post)
            (pathfind kchar post)))))
  (or (use-potion? kchar)
      (if (any-visible-hostiles? kchar)
          (try-to-use-ability)
          (goto-post))))

;;-------------------> old stuff for reference:
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
; (define (take-cover-in-forest? kchar)
;   (println "take-cover-in-forest?")
;   (println " on-terrain? " (on-terrain? kchar t_forest))
;   (if (on-terrain? kchar t_forest)
;       #f
;       (let ((loc (find-nearest-visible-terrain-of-type kchar t_forest)))
;         (println "  loc=" loc)
;         (if (null? loc)
;             #f
;             (pathfind kchar loc)))))

; (define (nearest-foe-is-melee? kchar nearest)
;   (println "nearest-foe-is-melee?")
;   (<= (distance kchar nearest) 2))

; (define (hide-in-forest? kchar nearest)
;   (println "hide-in-forest?")
;   (define (is-hiding-place? loc)
;     (println "  is-hiding-place? " loc)
;     (and (passable? loc kchar)
;          (eqv? t_forest
;                (kern-place-get-terrain loc))))
;   (let* ((curloc (kern-obj-get-location kchar))
;          (foeloc (kern-obj-get-location nearest))
;          (vect (loc-canonical (loc-diff curloc foeloc)))
;          (locs (filter is-hiding-place?
;                        (append (loc-opposite-x curloc (loc-x vect))
;                                (loc-opposite-y curloc (loc-y vect))))))
;     (if (null? locs)
;         #f
;         (let* ((newloc (car locs))
;               (vect (loc-canonical (loc-diff newloc curloc)))
;               (dx (loc-x vect))
;               (dy (loc-y vect)))
;           (println "  locs=" locs)
;           (println "  move to" newloc " dx=" dx " dy=" dy)
;           (kern-obj-move kchar dx dy)))))



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

;       (take-cover-in-forest? kchar)
;       (switch-weapons kchar)
;       ))

;       (let ((nearest (nearest-obj kchar (all-visible-hostiles kchar))))
;         (if (null? nearest)
;             #f
;             (and (nearest-foe-is-melee? kchar nearest)
;                  (not (hidden? kchar))
;                  (hide-in-forest? kchar nearest))))))

;;----------------------------------------------------------------------------
;; Generic AI used by kernel, reproduced here as a starting point
;;----------------------------------------------------------------------------
; (define (ai-display args) nil)
; (define (ai-newline) nil)

; (define (ai-select-target kchar)
;   (ai-display "ai-select-target")(ai-newline)
;   (nearest-obj kchar (all-visible-hostiles kchar)))

; (define (ai-wander kchar)
;   (ai-display "ai-wander")(ai-newline)
;   (kern-obj-wander kchar))

; (define (in-range-of-arms? karms dist)
;   (ai-display "in-range-of-arms?")(ai-newline)
;   (<= dist (kern-arms-type-get-range karms)))

; (define (has-ammo? kchar karms)
;   (ai-display "has-ammo?")(ai-newline)
;   (or (not (arms-type-needs-ammo? karms))
;       (let ((ammo-type (kern-arms-type-get-ammo-type karms)))
;         (ai-display "has-ammo?: ammo-type=")(ai-display ammo-type)(ai-newline)
;         (or (null? ammo-type)
;             (kern-obj-has? kchar ammo-type)))))

; (define (weapon-blocked? karms dist)
;   (ai-display "weapon-blocked?")(ai-newline)
;   (and (< dist 2)
;        (arms-type-is-blockable? karms)))

; (define (ai-select-weapon katt kdef)  
;   (let ((defdist (distance katt kdef)))
;   (ai-display "ai-select-weapon:defdist=")(ai-display defdist)(ai-newline)
;     (define (weapon-ok? karms)
;       (ai-display "ai-select-weapon:weapon-ok?")(ai-newline)
;       (and (in-range-of-arms? karms defdist)
;            (has-ammo? katt karms)
;            (not (weapon-blocked? karms defdist))))
;     (define (scan-weapons wlist)
;       (ai-display "ai-select-weapon:scan-weapons")(ai-newline)
;       (if (null? wlist) nil
;           (let ((karms (car wlist)))
;             (if (weapon-ok? karms)
;                 karms
;                 (scan-weapons (cdr wlist))))))
;     (scan-weapons (kern-char-get-weapons katt))))

; (define (ai-attack-target kchar ktarg)
;   (ai-display "ai-attack-target")(ai-newline)
;   (define (do-attack-loop retval)
;     (let ((kweap (ai-select-weapon kchar ktarg)))
;       (ai-display "ai-attack-target:kweap=")(ai-display kweap)(ai-newline)
;       (if (null? kweap) retval
;           (begin
;             (kern-char-attack kchar kweap ktarg)
;             (if (and (is-alive? ktarg)
;                      (has-ap? kchar))
;                 (do-attack-loop #t)
;                 #t)))))
;   (do-attack-loop #f))
; (define (ai-pathfind-to-target kchar ktarg)
;   (ai-display "ai-pathfind-to-target")(ai-newline)
;   (pathfind kchar (kern-obj-get-location ktarg)))

; (define (old-ai kchar)
;   (ai-display "generic-ai")(ai-newline)
;   (let ((ktarg (ai-select-target kchar)))
;     (ai-display "generic-ai: ktarg=")(ai-display ktarg)(ai-newline)
;     (if (null? ktarg)
;         (ai-wander kchar)
;         (begin
;           (taunt kchar ktarg)
;           (or (ai-attack-target kchar ktarg)
;               (ai-pathfind-to-target kchar ktarg))))))
