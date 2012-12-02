;; The species data file. Every character has a species type, which determines
;; some natural characteristics.

;; The concept of a species is built into the engine. Register a new species
;; with kern-mk-species.
;;
;; Args for kern-mk-species:
;;
;;                           tag : qstr, to reference the occupation elsewhere in the script
;;                          name : str, name for UI display
;;                 strength (str): additional strength
;;             intelligence (int): additional intelligence
;;                dexterity (dex): additional dexterity
;;                    speed (spd): action points per turn (shown here as a percentage of standard human)
;;             vision radius (vr): site range in tiles
;;          movement mode (mmode): sym, determines passability over different terrain
;;     hit point modified (hpmod): int, additional base hit points
;;  hit point multiplier (hpmult): int, additional hit points per level
;;    mana point modified (mpmod): int, additional base mana points
;; mana point multiplier (mpmult): int, additional mana points per level
;;            sleep sprite (sspr): special sprite used to show when the char is sleeping
;;          natural weapon (weap): sym or nil, the default weapon when one is not readied.
;;                  visible (vis): bool, false if species is normally invisible
;;          damage sound (dmgsnd): sym or nil, special sound to play when creature is damaged
;;         movement sound (mvsnd): sym or nil, sound to play when the creature moves
;;          experience value (xp): int, additional xp rewarded to opponent
;;     natural armor dice (armdc): string or nil, armor bonus
;;             morphology (morph): list of equipment and armor slots
;;                  spells (spls): list of natural spells (unused by engine)

;; Function to normalize all speeds relative to the standard human speed.
(define (rel-spd speed)
	(/ (* speed-human speed) 100))

;; Curried convenience proc to register a new species type with the kernel,
;; assuming certain fields are their normal values.
(define (mk-species tag name str int dex spd con mag vr mmode weap morph xp sspr armordice mvsnd)
  (kern-mk-species tag name str int dex (rel-spd spd) vr mmode 
                   con 
                   (max (round (/ con 10)) 1)
                   mag 
                   (max (round (/ mag 2)) 1)
                   sspr
                   weap #t 
		   nil ;
                   mvsnd
                   xp
                   #f ;; stationary?
                   armordice
                   morph nil)) 

;; Define standard humanoid weapon and armor slots.
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

;; The Species Table. 'bHP' will be used for both hpmod and hpmult, 'bMP'
;; simlarly for mpmod and mpmult.
;;          tag        name    st in dx  spd bHP bMP vr mmode    weap  morph    xp sspr armrdc  mvsnd
(mk-species 'sp_human  "human" 10 10 10  100 10  2   13 mm_walk  nil   humanoid  2 nil  nil     nil)
