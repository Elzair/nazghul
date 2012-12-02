;; Define game system constants.

;; Slots for weapons and armor (must be powers of 2)
(define slot-nil              0)
(define slot-amulet           1)
(define slot-ring             2)
(define slot-gazer-helm       4)
(define slot-weapon           8)
(define slot-shield           8)
(define slot-weapon-or-shield 8)
(define slot-armor            16)
(define slot-boot             32)
(define slot-helm             64)

;; Layers for object types (must match object.h in the engine source)
(define layer-none       0)
(define layer-tfeat      1)
(define layer-mechanism  2)
(define layer-portal     3)
(define layer-vehicle    4)
(define layer-bed        5)
(define layer-container  6)
(define layer-item       7)
(define layer-field      8)
(define layer-being      9)
(define layer-projectile 10)
(define layer-crosshair  11)

;; Speeds (expressed as action points per round).
(define speed-human 50)

;; The "normal" cost to move one tile (see passability.scm).
(define base-move-ap 50)

;; Player character bonuses
(define pc-hp-off  25)
(define pc-hp-gain 5)
(define pc-mp-off  1)
(define pc-mp-gain 1)

;; Factions (for diplomacy rules).
(define faction-none   0)
(define faction-player 1)
