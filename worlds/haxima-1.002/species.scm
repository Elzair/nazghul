;;----------------------------------------------------------------------------
;; Morphologies
;;
;; Slots should be listed in desired paper-doll rendering order, bottom layer
;; to top.
(define humanoid
  (list 
   slot-armor
   slot-boot
   slot-helm
   slot-amulet
   slot-ring
   slot-ring
   slot-weapon-or-shield
   slot-weapon-or-shield
   ))

(define giant
  (list
   slot-armor
   slot-boot
   slot-helm
   slot-helm
   slot-amulet
   slot-amulet
   slot-ring
   slot-ring
   slot-weapon-or-shield
   slot-weapon-or-shield
   slot-weapon-or-shield
   slot-weapon-or-shield
   ))

;;----------------------------------------------------------------------------
;; species

;; mk-species -- register a species type with the kernel
(define (mk-species tag name str int dex spd con mag vr mmode weap morph xp sspr mvsnd)
  (kern-mk-species tag name str int dex spd vr mmode 
                   con 
                   (max (round (/ con 10)) 1)
                   mag 
                   (max (round (/ mag 2)) 1)
                   sspr
                   weap #t sound-damage 
                   mvsnd
                   xp
                   #f ;; stationary?
                   morph nil)) 

(define (mk-stationary-species tag name str int dex spd con mag vr mmode weap morph xp sspr)
  (kern-mk-species tag name str int dex spd vr mmode 
                   con 
                   (max (round (/ con 10)) 1)
                   mag 
                   (max (round (/ mag 2)) 1)
                   sspr
                   weap #t sound-damage 
                   nil
                   xp 
                   #t ;; stationary?
                   morph nil)) 

;; Note: SF Bug# [ 1523228 ] "AI not moving to attack" was almost certainly
;; caused by the limited vision radius of most species not reaching across the
;; 19x19 combat map. This is intentional, I think the player should have the
;; opportunity to escape some NPC's or sneak up on others because the foe's
;; vision is handicapped. However, this is not the situation for sea battles,
;; where most player character's won't be able to close with distant sea
;; critters. So make all sea creatures have at least 19 vision radius, but
;; leave the rest as-is.

;;          tag              name             st in dx sp hp mp vr mmode       weap         morph   xp sspr           mvsnd
(mk-species 'sp_balron       "balron"         50 50 50 1 50 10 13 mmode-fly   t_horns      giant    50 s_asleep       sound-walking   )
(mk-species 'sp_bull         "bull"           20  1  5 1 20  0  6 mmode-walk  t_horns      nil       2 s_bull         sound-walking   )
(mk-species 'sp_cave_goblin  "cave goblin"    12  8 12 1 12  2 14 mmode-walk  t_hands      humanoid  2 s_asleep       sound-walking   )
(mk-species 'sp_forest_goblin "forest goblin"  8 10 14 1  8  4 16 mmode-walk  t_hands      humanoid  2 s_asleep       sound-walking   )
(mk-species 'sp_ghast        "ghast"          10 10 10 1 10  2  8 mmode-phase t_hands      humanoid  2 s_asleep       nil             )
(mk-species 'sp_gint         "gint"           50  3  8 1 50  2 20 mmode-hover t_hands      giant    10 s_asleep       sound-walking   )
(mk-species 'sp_green_slime  "green slime"     2  2  2 1  6  0  5 mmode-walk  t_acid_spray nil       2 s_slime_asleep sound-squishing )
(mk-species 'sp_fire_slime   "fire slime"      2  2  2 1  6  0  5 mmode-walk  t_fire_glob  nil       2 s_red_slime_asleep sound-squishing )
(mk-species 'sp_human        "human"          10 10 10 1 10  2 13 mmode-walk  t_hands      humanoid  2 s_asleep       sound-walking   )
(mk-species 'sp_insect       "insects"         1  1 18 2  3  0  4 mmode-hover t_stinger    nil       1 nil            nil             )
(mk-species 'sp_nixie        "nyad"           10 10 10 1 10  2 19 mmode-fish  t_hands      humanoid  2 s_shoals       sound-splashing )
(mk-species 'sp_queen_spider "queen spider"   18  6 12 1 20  4 10 mmode-crawl t_fangs      nil       4 s_asleep       sound-walking   )
(mk-species 'sp_skeleton     "skeleton"       12  8 12 1 12  2 10 mmode-walk  t_hands      humanoid  3 s_asleep       sound-walking   )
(mk-species 'sp_snake        "snake"           2  2 14 1  4  0  6 mmode-walk  t_fangs      nil       1 s_asleep       sound-walking   )
(mk-species 'sp_bat          "bat"             2  2 14 2  2  0  8 mmode-fly   t_fangs      nil       1 s_asleep       nil             )
(mk-species 'sp_rat          "rat"             4  2 12 1  6  0  6 mmode-crawl t_fangs      nil       1 s_asleep       nil             )
(mk-species 'sp_spider       "spider"         12  6 14 2  8  2 10 mmode-crawl t_fangs      nil       2 s_asleep       sound-walking   )
(mk-species 'sp_statue       "statue"          1  1  1 1 99  0  1 mmode-none  nil          nil       0 nil            nil             )
(mk-species 'sp_troll        "troll"          14  6 12 1 20  2 10 mmode-walk  t_horns      humanoid  3 s_asleep       sound-walking   )
(mk-species 'sp_yellow_slime "yellow slime"    4  4  4 1 12  2  6 mmode-walk  t_acid_spray nil       2 nil            sound-squishing )
(mk-species 'sp_kraken       "kraken"         30  3 20 1 30  4 19 mmode-fish  t_tentacles  nil      10 s_shoals       sound-splashing ) 
(mk-species 'sp_sea_serpent  "sea serpent"    20  2 14 1 20  4 19 mmode-fish  t_fangs      nil      8  s_asleep       sound-walking   ) 
(mk-species 'sp_wolf         "wolf"            8  2 12 2  8  0 13 mmode-walk  t_fangs      nil      2  s_asleep       sound-walking   ) 
(mk-species 'sp_gazer        "gazer"           6 20  6 1 10  8 16 mmode-hover t_fangs      nil      8  s_asleep       nil             ) 
(mk-species 'sp_headless     "headless"       12  0 10 1 14  0  6 mmode-walk  t_hands      humanoid 2  s_asleep       sound-walking   ) 
(mk-species 'sp_wisp         "wisp"            2 20 16 2  8  8  9 mmode-hover nil          nil      4  nil            nil             ) 
(mk-species 'sp_dragon       "dragon"         20 10 10 1 50  8  9 mmode-fly   t_fangs      nil      20 nil            sound-walking   )
(mk-species 'sp_zorn         "zorn"           10 10 10 1 10  2  9 mmode-phase t_beak       nil      4  s_asleep       sound-walking   ) 
(mk-species 'sp_demon        "demon"          14 14 14 1 14  8 12 mmode-phase t_hands      humanoid 8  s_asleep       sound-walking   ) 
(mk-species 'sp_lich         "lich"           12 14 14 1 20 10  9 mmode-walk  t_hands      humanoid 8  s_asleep       sound-walking   )
(mk-species 'sp_carabid      "carabid"        30  1 10 1 40  0  3 mmode-walk  t_pincers    nil      4  s_carabid_asleep sound-walking )

;; species that don't move around
(mk-stationary-species 'sp_dryad  "dryad"     12 12  4 1 12  6  6 mmode-walk  nil          nil      8  s_forest)
(mk-stationary-species 'sp_hydra  "hydra"     20  2 10 1 30  8  6 mmode-walk  t_tentacles  nil      10 nil     )

;;----------------------------------------------------------------------------
;; This list of the undead species is used by spells which affect the undead.
;;----------------------------------------------------------------------------
(define undead-species-tags
  (list sp_skeleton sp_lich))

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
  (eqv? species sp_balron)
  )

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

;;----------------------------------------------------------------------------
;; On-death closures
(define (drop kchar . stuff)
  (map (lambda (ktype) (kern-obj-put-at (kern-mk-obj ktype 1) 
                                        (kern-obj-get-location kchar))) 
       stuff))
