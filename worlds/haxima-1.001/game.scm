;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;; Alignments
(define align-none    0)
(define align-player  1)
(define align-town    2)
(define align-monster 4)

;; Sounds
(define sound-damage         "sounds/damage.wav")
(define sound-walking        "sounds/walk.wav")
(define sound-splashing      "sounds/rowing.wav")
(define sound-squishing      "sounds/walk.wav") ;; fixme
(define sound-moongate-enter "sounds/enter_moongate.wav")
(define sound-cannon-fire    "sounds/cannon.wav")
(define sound-ship-move       sound-splashing)

;; Slots
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

;; Speeds
(define speed-insect       3)
(define speed-yellow-slime 1)
(define speed-human        2)
(define speed-ship         4)

;; Pmasks
(define pmask-none   0)
(define pmask-solid  1)
(define pmask-land   2)
(define pmask-water  4)
(define pmask-shoals 8)
(define pmask-bridge (+ pmask-land pmask-water pmask-shoals))
(define pmask-all    (+ pmask-solid pmask-land pmask-water pmask-shoals))

;; Layers
(define layer-none       0)
(define layer-mechanism  1)
(define layer-portal     2)
(define layer-vehicle    3)
(define layer-bed        4)
(define layer-container  5)
(define layer-item       6)
(define layer-field      7)
(define layer-being      8)
(define layer-projectile 9)
(define layer-crosshair  10)

;; Contexts
(define context-wilderness 1)
(define context-town       2)
(define context-any        (+ context-town context-wilderness))

;; Damage/Immunity types (ordinal, not bitmasks)
(define damage-none   0)
(define damage-fire   1)
(define damage-poison 2)
(define damage-sleep  3)

;; Damage amounts
(define lava-damage 10)
(define tremor-damage 10)

;; Directions (as returned by kern-ui-get-direction)
(define northwest 0)
(define north     1)
(define northeast 2)
(define west      3)
(define here      4)
(define east      5)
(define southwest 6)
(define south     7)
(define southeast 8)
(define up        9)
(define down      10)


;; Some of the following are order-dependent
(load "loc.scm")
(load "kobj.scm")
(load "ifc.scm")
(load "sprite-sets.scm")
(load "sprites.scm")
(load "effects.scm")
(load "terrains.scm")
(load "palette.scm")
(load "fields.scm")

;; Object types
(load "objs.scm")
(load "reagents.scm")
(load "food.scm")
(load "arms.scm")
(load "items.scm")
(load "vehicles.scm")

(load "species.scm")
(load "occs.scm")
(load "parties.scm")

;; Mechanism-like things
(load "bim.scm")
(load "monster-generator.scm")
(load "door.scm")
(load "portcullis.scm")
(load "lever.scm")
(load "tblit.scm")
(load "portals.scm")
(load "moongate.scm")

(load "conv.scm") ;; basic conversation

;; Read-only data for specific characters
(load "shroom.scm")

;; Astronomy
(load "moon.scm")

(load "spells.scm")

;; Miscellaneous crap
(mk-obj-type 't_crosshair "crosshair" s_crosshair layer-crosshair nil)
(kern-set-crosshair t_crosshair)
(kern-set-frame s_frame_ulc
                s_frame_urc
                s_frame_llc
                s_frame_lrc
                s_frame_td
                s_frame_tu
                s_frame_tl
                s_frame_tr
                s_null
                s_frame_horz
                s_frame_vert
                s_frame_endl
                s_frame_endr)
(kern-set-ascii ss_little_sprites 32)
(kern-set-cursor ls_whirlpool)
