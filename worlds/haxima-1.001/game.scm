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
(define speed-yellow-slime 1)
(define speed-human        2)
(define speed-insect       3)
(define speed-ship         4)

;; Pmasks (keep them around until mechs are converted to use passability
;; classes (see below))
(define pmask-none   0)
(define pmask-solid  1)
(define pmask-land   2)
(define pmask-water  4)
(define pmask-shoals 8)
(define pmask-bridge (+ pmask-land pmask-water pmask-shoals))
(define pmask-all    (+ pmask-solid pmask-land pmask-water pmask-shoals))

;; Difficulty levels (Note: 255 is well-known to the kernel to mean
;; "impassible" in the case of movement costs)
(define veasy      1)
(define easy       2)
(define normal     3)
(define hard       4)
(define vhard      5)
(define cant       255)

;; Passability classes
(define pclass-none      0)
(define pclass-grass     1)
(define pclass-deep      2)
(define pclass-shoals    3)
(define pclass-mountains 4) ;; no ceiling
(define pclass-wall      5) ;; has a ceiling
(define pclass-trees     6)
(define pclass-forest    7)
(define pclass-repel     8) ;; energy shield blocks all
(define pclass-bridge    pclass-grass)
(define pclass-road      pclass-grass)

;; Movement modes
(define mmodes
  (list
   (list 'mmode-walk  "walking"  0)
   (list 'mmode-hover "hovering" 1)
   (list 'mmode-ship  "sailing"  2)
   (list 'mmode-phase "phasing"  3)
   (list 'mmode-fly   "flying"   4)
   (list 'mmode-skiff "rowing"   5)
   ))
(map (lambda (mmode) (apply kern-mk-mmode mmode)) mmodes)

;; Movement cost table
(kern-mk-ptable
  ;;   walk   hover ship   phase  fly    skiff
  ;;   ====== ===== ====== =====  ====== ======
 (list 0      0     0      0      0      0     ) ;; none
 (list normal easy  cant   normal easy   cant  ) ;; grass/paving
 (list cant   cant  normal cant   easy   vhard ) ;; deep
 (list cant   hard  cant   cant   easy   normal) ;; shoals
 (list cant   cant  cant   cant   vhard  cant  ) ;; mountains
 (list cant   cant  cant   normal cant   cant  ) ;; wall (w/ ceiling)
 (list hard   hard  cant   normal easy   cant  ) ;; trees  
 (list vhard  vhard cant   normal easy   cant  ) ;; forest/hills/bog
 (list cant   cant  cant   cant   cant   cant  ) ;; energy fields
 )

;; Factions. The diplomacy table (which defines the relationship between
;; factions) must be defined in the session file, because it changes over time.
(define faction-none     0)
(define faction-player   1)
(define faction-men      2)
(define faction-orks     3)
(define faction-accursed 4)

;; Layers (must match object.h)
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
(load "scenery.scm")

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
(load "bridge.scm")
(load "stone-lantern.scm")
(load "road.scm")
(load "drawbridge.scm")

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

;; Note: this procedure cannot be defined in the session.scm file or it will
;; not be saved and therefore will not be available when reloading the game. In
;; general, nothing should be defined (using Scheme 'define') in the session
;; file, only kernel calls should be made. The exception is things that are
;; truly only needed once on initial startup (for example, I currently define
;; some time constants there that fit this category). Since in general
;; procedures do not have modifiable state they can be kept in files (like this
;; one) which are loaded by the session file.
(define (green-tower-pre-entry-hook kpp)
  (kern-print "Enter GreenTower?\n")
  (kern-conv-get-yes-no? kpp))

(define (pit-of-death-pre-entry-hook kpp)
  (kern-print "BWA-HA-HA-HA-HA!")
  #t)
