;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

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
(define speed-human        1)
(define speed-insect       2)
(define speed-ship         2)

;; Action Points
(define ap-to-use-scroll speed-human)

;; Difficulty Classes
(define dc-escape-ensnare  26)
(define dc-escape-paralyze 10)

;; Pmasks (keep them around until mechs are converted to use passability
;; classes (see below))
(define pmask-none   0)
(define pmask-solid  1)
(define pmask-land   2)
(define pmask-water  4)
(define pmask-shoals 8)
(define pmask-bridge (+ pmask-land pmask-water pmask-shoals))
(define pmask-all    (+ pmask-solid pmask-land pmask-water pmask-shoals))

;; Passability Difficulty Levels (Note: 255 is well-known to the kernel to mean
;; "impassible" in the case of movement costs)
(define norm       1)
(define hard       2)
(define vhard      3)
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
(define pclass-boulder   pclass-forest)

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
 (list norm   norm  cant   norm   norm   cant  ) ;; grass/paving
 (list cant   cant  norm   cant   norm   vhard ) ;; deep
 (list cant   hard  cant   cant   norm   norm  ) ;; shoals
 (list cant   cant  cant   cant   vhard  cant  ) ;; mountains
 (list cant   cant  cant   norm   cant   cant  ) ;; wall (w/ ceiling)
 (list hard   hard  cant   norm   norm   cant  ) ;; trees  
 (list vhard  vhard cant   norm   norm   cant  ) ;; forest/hills/bog
 (list cant   cant  cant   cant   cant   cant  ) ;; energy fields
 )

;; Factions. The diplomacy table (which defines the relationship between
;; factions) must be defined in the session file, because it changes over time.
(define faction-none        0)
(define faction-player      1)
(define faction-men         2)
(define faction-orks        3)
(define faction-accursed    4)
(define faction-monster     5)
(define faction-hill-troll  6)
(define faction-wood-spider 7)
(define faction-outlaw      8)

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

;; NPC activities
(define (isdrunk? knpc)
  (string=? "drunk" (kern-obj-get-activity knpc)))
(define (isworking? knpc)
  (string=? "working" (kern-obj-get-activity knpc)))

;; Some of the following are order-dependent
(load "loc.scm")
(load "kobj.scm")
(load "ifc.scm")
(load "sprite-sets.scm")
(load "sprites.scm")
(load "sounds.scm")
(load "effects.scm")
(load "terrains.scm")
(load "palette.scm")
(load "fields.scm")
(load "scenery.scm")
(load "combat-maps.scm")

;; Object types
(load "objs.scm")
(load "containers.scm")
(load "reagents.scm")
(load "food.scm")
(load "arms.scm")
(load "items.scm")
(load "vehicles.scm")
(load "beds.scm")
(load "money.scm")
(load "ai.scm")
(load "species.scm")
(load "occs.scm")
(load "npc-types.scm")
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
(load "drawbridge.scm")

(load "conv.scm") ;; basic conversation

;; Astronomy
(load "moon.scm")

(load "spells.scm")
(load "scrolls.scm")

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
