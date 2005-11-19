(define humanoid-slots (list slot-helm
                             slot-amulet
                             slot-weapon-or-shield
                             slot-weapon-or-shield
                             slot-armor
                             slot-boot
                             slot-ring
                             slot-ring))

(define gint-slots (list slot-helm
                         slot-helm
                         slot-amulet
                         slot-amulet
                         slot-weapon-or-shield
                         slot-weapon-or-shield
                         slot-weapon-or-shield
                         slot-weapon-or-shield
                         slot-armor
                         slot-boot
                         slot-ring
                         slot-ring))

(define humanoid humanoid-slots)
(define giant gint-slots)

(define troll-speed         speed-human)
(define gint-speed          speed-human)
(define balron-speed        (* 2 speed-human))
(define troll-base-hp       20)
(define troll-critical-hp   10)
(define troll-melee-weapon  t_hands)
(define troll-ranged-weapon t_thrown_boulder)
(define troll-ripup-boulder-ap (* 2 troll-speed))

(define (mk-species tag name str int dex spd con mag vr mmode weap morph xp sspr mvsnd)
  (kern-mk-species tag name str int dex spd vr mmode 
                   con 
                   (max (round (/ con 10)) 1)
                   mag 
                   (max (round (/ mag 10)) 1)
                   sspr
                   weap #t sound-damage 
                   mvsnd
                   nil xp morph nil)) 

;;          tag              name           st in dx sp hp mp vr mmode       weap         morph    xp sspr           mvsnd           die
(mk-species 'sp_human        "human"        10 10 10 1  20 10 13 mmode-walk  t_hands      humanoid 2  s_asleep       sound-walking   nil)
(mk-species 'sp_nixie        "nyad"         10 10 10 1  20 10 13 mmode-fish  t_hands      humanoid 2  s_shoals       sound-splashing nil)
(mk-species 'sp_ghast        "ghast"        10 10 10 1  10  3  8 mmode-phase t_hands      humanoid 2  s_asleep       nil             nil)
(mk-species 'sp_cave_goblin  "cave goblin"  12  8 12 1  15  8 14 mmode-walk  t_hands      humanoid 2  s_asleep       sound-walking   nil)
(mk-species 'sp_forest_goblin "forest goblin" 8 10 14 1 14 14 16 mmode-walk  t_hands      humanoid 2  s_asleep       sound-walking   nil)
(mk-species 'sp_insect       "insects"       1  1 18 2   3  0  4 mmode-hover t_stinger    nil      1  nil            nil             nil)
(mk-species 'sp_yellow_slime "yellow slime"  4  4  4 1  15  8  6 mmode-walk  t_acid_spray nil      2  nil            sound-squishing nil)
(mk-species 'sp_green_slime  "green slime"   2  2  2 1  10  0  5 mmode-walk  t_acid_spray nil      2  s_slime_asleep sound-squishing nil)
(mk-species 'sp_skeleton     "skeleton"     12  8 12 1  22 10 10 mmode-walk  t_hands      humanoid  2  s_asleep       sound-walking   nil)
(mk-species 'sp_snake        "snake"         2  2 14 1   5  0  6 mmode-walk  t_fangs      nil      1  s_asleep       sound-walking   nil)
(mk-species 'sp_troll        "troll"        14  6 12 1  20  5 10 mmode-walk  t_hands      humanoid 3  s_asleep       sound-walking   nil)
(mk-species 'sp_gint         "gint"         50  3  8 1 100  8 20 mmode-hover t_hands      giant    10 s_asleep       sound-walking   nil)
(mk-species 'sp_balron       "balron"       50 50 50 1 100 100 13 mmode-fly  t_horns      giant    50 s_asleep       sound-walking   nil)
(mk-species 'sp_bull         "bull"         20  1  5 1  40  0  6 mmode-walk  t_horns      nil      2  s_bull         sound-walking   nil)
(mk-species 'sp_statue       "statue"        1  1  1 1 100000000 0 1 mmode-walk nil       nil      0  nil            nil             nil)
(mk-species 'sp_spider       "spider"       12  6 14 2  10  1 10 mmode-crawl t_fangs      nil      2  s_asleep       sound-walking   'spider-killed)
(mk-species 'sp_queen_spider "queen spider" 18  6 12 1  30 10 10 mmode-crawl t_fangs      nil      4  s_asleep       sound-walking   'queen-spider-killed)


;;----------------------------------------------------------------------------
;; This list of the undead species is used by spells which affect the undead.
;;----------------------------------------------------------------------------
(define undead-species-tags
  (list sp_skeleton))

(define (species-is-undead? species)
  (foldr (lambda (x undead) (or x (eqv? species undead)))
         #f undead-species-tags))

(define (is-undead? kchar)
  (species-is-undead? (kern-char-get-species kchar)))

;; ----------------------------------------------------------------------------
;; Species immunities
;; ----------------------------------------------------------------------------
(define (species-is-immune-to-ensnare? species)
  (or (eqv? species sp_spider)
      (eqv? species sp_queen_spider)))

(define (species-is-immune-to-paralyze? species)
  #f)

;;----------------------------------------------------------------------------
;; Trigger to generate slimes
;;----------------------------------------------------------------------------
(define (slime-gen-target-loc kgen)
  (let* ((kplace (loc-place (kern-obj-get-location kgen)))
        (gob (gob-data (kobj-gob kgen)))
        (x (car gob))
        (y (cadr gob)))
    (display "gob:")(display gob)(newline)
    (mk-loc kplace x y)))

(define (slime-generator-step kgen kstepper)
  (define (mkslime)
    (kern-log-msg "A slime emerges from the ooze!")
    (mk-green-slime))
  (let* ((kplace (loc-place (kern-obj-get-location kstepper)))
         (slimes (filter is-green-slime? (kern-place-get-beings kplace))))
    (if (< (length slimes) 1)
        (psummon (slime-gen-target-loc kgen)
                 mkslime
                 (kern-dice-roll "1d2")))))

(define slime-generator-ifc
  (ifc '()
       (method 'step slime-generator-step)))

(mk-obj-type 't_slime_generator nil nil layer-mechanism slime-generator-ifc)

(define (mk-slime-generator x y)
  (kern-obj-set-visible (bind (kern-mk-obj t_slime_generator 1)
                              (list x y))
                        #f))
