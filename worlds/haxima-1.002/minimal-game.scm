;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;; This file loads 58 files


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

;; Speeds  ;; TODO: move most of these into kern-intvars ?

(define speed-human             50)  ;; typical AP/round for humans

(define base-move-ap		50)  ;; this may not bear a neat relationship to speed-human
(define default-weapon-rap      50)  ;; this may not bear a neat relationship to speed-human
(define default-armour-apmod    2)  ;; this may not bear a neat relationship to speed-human

(define base-skill-ap			base-move-ap)  ;; this may not bear a neat relationship to speed-human
(define base-spell-ap			base-move-ap)  ;; this may not bear a neat relationship to speed-human

;; AP costs of various actions which the kernal needs to know about:
(kern-set-kern-intvar "AP_TOTAL:normal_human"    speed-human)

(kern-set-kern-intvar "AP_COST:default"           speed-human)
(kern-set-kern-intvar "AP_COST:search"            (* 3 speed-human))
(kern-set-kern-intvar "AP_COST:get_item"          (* 0.34 speed-human))
(kern-set-kern-intvar "AP_COST:drop_item"         (* 0.34 speed-human))
(kern-set-kern-intvar "AP_COST:open_mechanism"    speed-human)
(kern-set-kern-intvar "AP_COST:open_container"    speed-human)
(kern-set-kern-intvar "AP_COST:handle_mechanism"  speed-human)
(kern-set-kern-intvar "AP_COST:use_item"          speed-human)  ;; may be unused, per comment in cmd.c cmdUse()

;; Normal mixing: 18 + (num_mixed * 12) + (spell_level * 12) AP
(kern-set-kern-intvar "AP_COST:mix_reagents_base"         (* 3 speed-human))
(kern-set-kern-intvar "AP_COST:mix_reagents_per_mix"      (* 2 speed-human))
(kern-set-kern-intvar "AP_COST:mix_reagents_per_level"    (* 2 speed-human))
;; Attempt at non-existent spell: 3d18+6 AP
(kern-set-kern-intvar "AP_COST:mix_reagents_nospell_num"   3)
(kern-set-kern-intvar "AP_COST:mix_reagents_nospell_dice" (* 3 speed-human))
(kern-set-kern-intvar "AP_COST:mix_reagents_nospell_plus"  speed-human)
;; Missing or additional ingredients: (2 * spell_level)d18+18 AP
(kern-set-kern-intvar "AP_COST:mix_reagents_badmix_num"    2)  ;; times spell Level
(kern-set-kern-intvar "AP_COST:mix_reagents_badmix_dice"  (* 3 speed-human))
(kern-set-kern-intvar "AP_COST:mix_reagents_badmix_plus"  (* 3 speed-human))

;; These values are used by ctrl.c ctrl_attack_target() 
;; to adjust weapon AP costs in the event of dual wielding.
;; The dual weapon rules can thus be tweaked here...
(kern-set-kern-intvar "AP_MULT12:second_wpn_attack"       6)  ;; AP cost * 6/12 for 2nd weapon attack if dual wpns used
(kern-set-kern-intvar "AP_MULT12:third_plus_wpn_attack"   6)  ;; AP cost * 6/12 for 3rd+ weapon attacks, if 3+ weapons used
(kern-set-kern-intvar "AP_THRESHOLD:multi_attack_overage" 0)  ;; attack sequence can continue if AP overage is not > 0

(kern-set-kern-intvar "submerged_def_bonus" 10) ;; defense bonus for submerged critters

;; ship speeds are better handled using mmodes/pclasses-
;; it should only affect actual movement
(define speed-ship            speed-human)  

;; Action Point costs for various basic actions:
;; are these used anywhere?
;;(define ap-for-1H-melee-attack   9)
;;(define ap-for-2H-melee-attack  12)

;;(define ap-for-1H-thrown-attack 12)
;;(define ap-for-2H-thrown-attack 18)

;;(define ap-for-shooting-attack  12)

;;(define ap-for-combat-spell      9)
;;(define ap-to-use-scroll        12)


;; Difficulty Classes
(define dc-escape-ensnare  26)
(define dc-escape-paralyze 16)

;; Pmasks (keep them around until mechs are converted to use passability
;; classes (see below))
(define pmask-none   0)
(define pmask-solid  1)
(define pmask-land   2)
(define pmask-water  4)
(define pmask-shoals 8)
(define pmask-bridge (+ pmask-land pmask-water pmask-shoals))
(define pmask-all    (+ pmask-solid pmask-land pmask-water pmask-shoals))

;; Passability Difficulty Levels 
;;   (Note: 255 is well-known to the kernel to mean
;;   "impassible" in the case of movement costs)
(define fast        (* 0.66 base-move-ap))  ;; 0.66 (2/3)
(define s-fast      (* 0.8 base-move-ap))  ;; 'slightly fast' 0.8
(define norm        base-move-ap)  ;; 1.0
(define s-hard      (* 1.5 base-move-ap))  ;; 1.5
(define hard       (* 2 base-move-ap))  ;; 2.0
(define v-hard     (* 3 base-move-ap))  ;; 3.0

(define no-drop    100)  ;; special, used for dropability (not related to speed-human)
(define cant      255)  ;; special

;(define norm       50)  ;; 1.0
;(define s-hard     75)  ;; 1.5
;(define hard      100)  ;; 2.0
;(define v-hard    150)  ;; 3.0
;(define fast       30)  ;; 0.6
;(define s-fast     40)  ;; 0.4
;(define no-drop   100)  ;; 2.0
;(define cant      255)  ;; 


;; Passability classes
(define pclass-none          0)
(define pclass-grass         1)
(define pclass-deep          2)
(define pclass-shoals        3)
(define pclass-mountains     4) ;; no ceiling
(define pclass-wall          5) ;; has a ceiling
(define pclass-trees         6)
(define pclass-forest        7)
(define pclass-hills         8)
(define pclass-repel         9) ;; energy shield blocks all
(define pclass-space         10)
(define pclass-bridge        pclass-grass)
(define pclass-road          pclass-grass)
(define pclass-boulder       11) ;; no ceiling, smaller than mountain
(define pclass-waterboulder  12) ;; worst case of boulder and water
(define pclass-sludge        13)
(define pclass-shallows      14)
(define pclass-bars          15) ;; portcullis
(define pclass-window        16) ;; separating from bars for shoot-but-not-crawl-through passability
(define pclass-vmountains    17)
(define pclass-canfloat      18) ;; avoids drowning
(define pclass-canfly        19) ;; avoids ground based issues

;; Movement modes
(define mmodes
  (list
   (list 'mmode-walk      "walking"     0)
   (list 'mmode-hover     "hovering"    1)
   (list 'mmode-ship      "sailing"     2)
   (list 'mmode-phase     "phasing"     3)
   (list 'mmode-fly       "flying"      4)
   (list 'mmode-skiff     "rowing"      5)
   (list 'mmode-fish      "swimming"    6)
   (list 'mmode-crawl     "crawling"    7) ;; spiders, can cross boulders
   (list 'mmode-voidship  "sailing"     8)
   (list 'mmode-ranger    "stalking"    9)
   (list 'mmode-none      "stationary" 10)
   (list 'mmode-wriggle   "wriggle"    11) ;; rogue special move
   (list 'mmode-missile   "missile"    12)
   (list 'mmode-fastfly   "flying"     13)
   (list 'mmode-fastrun   "running"    14)
   (list 'mmode-fastcrawl "crawling"   15)
   (list 'mmode-smallobj  "smallobj"   16) ;; for determining dropability of small objects
   (list 'mmode-largeobj  "largeobj"   17) ;; for determining dropability of big objects- basically, stuff that wont fit through bars/windows
   (list 'mmode-field     "field"      18) ;; for determining dropability of fields
   (list 'mmode-return    "return"     19) ;; return path for magic axe (for now assume it always returns)
   (list 'mmode-cannon    "cannon"     20) ;; enhanced missile passibility for cannon shells
   (list 'mmode-large     "striding"   21) ;; big critters
))
(map (lambda (mmode) (apply kern-mk-mmode mmode)) mmodes)

(define mmode-jump mmode-fly)

;; Movement cost table (optimized for cut to/paste from spreadsheet!)
(kern-mk-ptable																									
	;;	walk	hover	ship	phase	fly	skiff	fish	crawl	vship	rangr	none	wrigl	missl	f_fly	f_run	f_crawl	sml_obj	lrg_obj	fields	return	cannon	striding		
	(list	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	)	;; none
	(list	norm	norm	cant	norm	norm	cant	cant	norm	cant	norm	cant	norm	0	fast	fast	fast	norm	norm	norm	0	0	norm	)	;; grass/paving
	(list	cant	cant	s-fast	cant	norm	v-hard	norm	cant	cant	cant	cant	cant	0	fast	cant	cant	cant	cant	no-drop	0	0	cant	)	;; deep
	(list	cant	s-hard	cant	cant	norm	norm	norm	cant	cant	cant	cant	cant	0	fast	cant	cant	cant	cant	no-drop	0	0	s-hard	)	;; shoals
	(list	cant	cant	cant	cant	s-hard	cant	cant	cant	cant	cant	cant	cant	95	s-fast	cant	cant	no-drop	no-drop	cant	0	90	cant	)	;; mountains
	(list	cant	cant	cant	s-hard	cant	cant	cant	cant	cant	cant	cant	cant	100	cant	cant	cant	cant	cant	cant	0	100	cant	)	;; wall (w/ ceiling)
	(list	hard	hard	cant	norm	norm	cant	cant	hard	cant	norm	cant	hard	10	fast	norm	norm	norm	norm	norm	0	7	hard	)	;; trees
	(list	v-hard	v-hard	cant	norm	norm	cant	cant	v-hard	cant	s-hard	cant	v-hard	30	fast	hard	hard	norm	norm	norm	0	20	v-hard	)	;; forest
	(list	v-hard	hard	cant	norm	norm	cant	cant	v-hard	cant	s-hard	cant	v-hard	7	fast	hard	hard	norm	norm	norm	0	5	hard	)	;; hills/bog
	(list	cant	cant	cant	cant	cant	cant	cant	cant	cant	cant	cant	cant	100	cant	cant	cant	no-drop	no-drop	norm	0	100	cant	)	;; energy fields
	(list	cant	cant	cant	cant	norm	cant	cant	cant	norm	cant	cant	cant	0	fast	cant	cant	cant	cant	no-drop	0	0	cant	)	;; space
	(list	cant	norm	cant	norm	norm	cant	cant	hard	cant	cant	cant	hard	10	fast	cant	norm	norm	norm	norm	0	4	norm	)	;; boulder
	(list	cant	hard	cant	cant	norm	cant	cant	hard	cant	cant	cant	hard	10	fast	cant	norm	norm	norm	no-drop	0	4	hard	)	;; waterboulder
	(list	cant	norm	hard	cant	norm	v-hard	v-hard	cant	cant	cant	cant	cant	0	fast	cant	cant	cant	cant	no-drop	0	0	norm	)	;; sludge
	(list	s-hard	norm	cant	norm	norm	norm	norm	s-hard	cant	norm	cant	cant	0	fast	norm	norm	cant	cant	no-drop	0	0	norm	)	;; shallow sludge
	(list	cant	cant	cant	s-hard	cant	cant	cant	cant	cant	cant	cant	v-hard	7	cant	cant	cant	norm	no-drop	norm	0	7	cant	)	;; bars (eg portcullis)
	(list	cant	cant	cant	s-hard	cant	cant	cant	cant	cant	cant	cant	cant	30	cant	cant	cant	no-drop	no-drop	no-drop	0	25	cant	)	;; window
	(list	cant	cant	cant	cant	s-hard	cant	cant	cant	cant	cant	cant	cant	30	s-fast	cant	cant	no-drop	no-drop	no-drop	0	10	cant	)	;; passlos mountains
	(list	cant	v-hard	s-fast	cant	norm	cant	norm	cant	cant	cant	cant	cant	norm	norm	cant	cant	cant	cant	norm	norm	norm	v-hard	)	;; float
	(list	cant	hard	cant	cant	norm	hard	cant	cant	norm	cant	cant	cant	norm	norm	cant	cant	cant	cant	norm	norm	norm	cant	)	;; fly
)																									
;; Note that pclass 'missl' is using the value as a percentage chance 
;; for a missile to be blocked by obscuring terrain, not as an AP cost


;; Factions. The diplomacy table (which defines the relationship between
;; factions) must be defined in the session file, because it changes over time.
(define faction-none          0)
(define faction-player        1)
(define faction-men           2)
(define faction-cave-goblin   3)
(define faction-accursed      4)
(define faction-monster       5)
(define faction-troll         6)
(define faction-spider        7)
(define faction-outlaw        8)
(define faction-gint          9)
(define faction-demon         10)
(define faction-forest-goblin 11)
(define faction-green-tower   faction-men)
(define faction-oparine       faction-men)
(define faction-trigrave      faction-men)
(define faction-nixie         faction-monster)
(define faction-prisoner      12)
(define faction-glasdrin      13)
(define faction-num           14)

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
(define context-world 1)
(define context-town  2)
(define context-any      (+ context-town context-world))

;; Damage/Immunity types (ordinal, not bitmasks)
(define damage-none   0)
(define damage-fire   1)
(define damage-poison 2)
(define damage-sleep  3)

;; Damage amounts
(define lava-damage 10)

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

;; Scroll directions (for UI keyhandling, must match kernel's StatusScrollDir)
(define scroll-up       0)
(define scroll-down     1)
(define scroll-right    2)
(define scroll-left     3)
(define scroll-pageup   4)
(define scroll-pagedown 5)
(define scroll-top      6)
(define scroll-bottom   7)

(define opposite-dir (vector southeast south southwest
                             east here west
                             northeast north northwest
                             down up))

;; Player character bonuses
(define pc-hp-off  25)
(define pc-hp-gain 5)
(define pc-mp-off  1)
(define pc-mp-gain 1)

;; NPC activities
(define (isdrunk? knpc)
  (string=? "drunk" (kern-obj-get-activity knpc)))
(define (isworking? knpc)
  (string=? "working" (kern-obj-get-activity knpc)))

;; Prices
(define base-scroll-cost 20) ;; gold pieces per level of scroll's spell
(define reagent-price-mult 1) ;; global reagent price multiplier

;; rather than trying to calculate appropriate hp/mp for
;; characters, stick in a big number and let Character::new
;; trim it as needed
(define max-health 999999999)

;; Some of the following are order-dependent
(load "loc.scm")
(load "kobj.scm")
(load "ifc.scm")

;; sprite sets
;;
;; 'ss_sprite_set_name tile_pix_w tile_pix_h tiles_h tiles_w x_offset y_offset "path/filename"
;;
;;                  'ss_sprite_set_name 
;;                  |                    tile_pixels_wide tile_pixels_high
;;                  |                    |       sheet_tiles_high sheet_tiles_wide
;;                  |                    |       |       sheet_pixels_x_offset sheet_pixels_y_offset
;;                  |                    |       |       |     "path/filename"
;;                  |                    |       |       |     |
;;                  v                    v       v       v     v
;;----------------------------------------------------------------------------
(kern-mk-sprite-set 'ss_u4_shapes        32 32   16 16   0 0  "shapes.png")
(kern-mk-sprite-set 'ss_u4_charset       8  16    8 16   0 0  "charset.png")
(kern-mk-sprite-set 'ss_frame            16 16    4  4   0 0  "frame.png")
; (kern-mk-sprite-set 'ss_rune             32 32    4  8   0 0  "rune.png")
(kern-mk-sprite-set 'ss_addon            32 32   16  8   0 0  "addons.png")
(kern-mk-sprite-set 'ss_moons            16 16    4  8   0 0  "moons.png")
; (kern-mk-sprite-set 'ss_signs            32 32    1  8   0 0  "signs.png")
; (kern-mk-sprite-set 'ss_runestones       32 32    4  8   0 0  "runestones.png")
; (kern-mk-sprite-set 'ss_newmonst         32 32   16  8   0 0  "newmonst.png") 
; (kern-mk-sprite-set 'ss_newfolks         32 32   16  8   0 0  "newfolks.png")
; (kern-mk-sprite-set 'ss_buildings        32 32    1  2   0 0  "tower.png")
; (kern-mk-sprite-set 'ss_overlays         32 32   13  8   0 0  "newterrain.png")
; (kern-mk-sprite-set 'ss_effects          8  16    3  16  0 0  "effects.png")
; (kern-mk-sprite-set 'ss_bigobjects       40 40    8  8   0 0  "bigobjects.png")
; (kern-mk-sprite-set 'ss_humanoids        32 32   16  8   0 0  "humanoids.png")
; (kern-mk-sprite-set 'ss_people           32 32   16  8   0 0  "hirespeople.png")
; (kern-mk-sprite-set 'ss_monsters         32 32   16  8   0 0  "monsters.png")
; (kern-mk-sprite-set 'ss_ship             32 32    8  8   0 0  "ship.png")
; (kern-mk-sprite-set 'ss_sfx              32 32    8  8   0 0  "sfx.png")
; (kern-mk-sprite-set 'ss_creatures        32 32   16  8   0 0  "creatures.png")
; (kern-mk-sprite-set 'ss_tools            32 32    8  8   0 0  "tools.png")

;; New paper-doll sprite sets
; (kern-mk-sprite-set 'ss_bodies     32 32 4 4 0 0 "bodies.png")
; (kern-mk-sprite-set 'ss_adornments 32 32 4 4 0 0 "adornments.png")
; (kern-mk-sprite-set 'ss_clothes    32 32 5 4 0 0 "clothes.png")


;; sprites
(kern-mk-sprite 's_sun                      ss_moons 1 24 #f 0 )
(kern-mk-sprite 's_grass         ss_u4_shapes 1  4 #f 0 )
(kern-mk-sprite 's_wanderer         ss_u4_shapes 4 224 #f 0 )
(kern-mk-sprite 's_crosshair            ss_addon 1  0 #f   0 )
; (kern-mk-sprite 's_bog           ss_u4_shapes 1  3 #f 0 )
; (kern-mk-sprite 's_town          ss_u4_shapes 1 10 #f 0 )
(kern-mk-sprite 's_keep          ss_u4_shapes 1 11 #f 0 )
; (kern-mk-sprite 's_hamlet        ss_u4_shapes 1 12 #f 0 )
; (kern-mk-sprite 's_leftwing      ss_u4_shapes 1 13 #f 0 )
; (kern-mk-sprite 's_castle        ss_u4_shapes 1 14 #f 0 )
; (kern-mk-sprite 's_rightwing     ss_u4_shapes 1 15 #f 0 )
; (kern-mk-sprite 's_cobblestone   ss_u4_shapes 1 22 #f 0 )
; (kern-mk-sprite 's_ew_bridge     ss_u4_shapes 1 23 #f 0 )
; (kern-mk-sprite 's_ballon        ss_u4_shapes 1 24 #f 0 )
; (kern-mk-sprite 's_bridge_top    ss_u4_shapes 1 25 #f 0 )
; (kern-mk-sprite 's_bridge_bottom ss_u4_shapes 1 26 #f 0 )
; (kern-mk-sprite 's_ladder_up     ss_u4_shapes 1 27 #f 0 )
; (kern-mk-sprite 's_ladder_down   ss_u4_shapes 1 28 #f 0 )
; (kern-mk-sprite 's_ruin          ss_u4_shapes 1 29 #f 0 )
; (kern-mk-sprite 's_shrine        ss_u4_shapes 1 30 #f 0 )
; (kern-mk-sprite 's_pillar      ss_u4_shapes 1 48 #f 0 )
; (kern-mk-sprite 's_wall_b      ss_u4_shapes 1 49 #f 0 )
; (kern-mk-sprite 's_wall_a      ss_u4_shapes 1 50 #f 0 )
; (kern-mk-sprite 's_wall_c      ss_u4_shapes 1 51 #f 0 )
; (kern-mk-sprite 's_wall_d      ss_u4_shapes 1 52 #f 0 )
; (kern-mk-sprite 's_mast        ss_u4_shapes 1 53 #f 0 )
; (kern-mk-sprite 's_ships_wheel ss_u4_shapes 1 54 #f 0 )
; (kern-mk-sprite 's_boulder     ss_u4_shapes 1 55 #f 0 )
(kern-mk-sprite 's_asleep      ss_u4_shapes 1 56 #f 0 )
; (kern-mk-sprite 's_wall_rock   ss_u4_shapes 1 57 #f 0 )
; (kern-mk-sprite 's_door_locked ss_u4_shapes 1 58 #f 0 )
; (kern-mk-sprite 's_door        ss_u4_shapes 1 59 #f 0 )
; (kern-mk-sprite 's_chest       ss_u4_shapes 1 60 #f 0 )
; (kern-mk-sprite 's_ankh        ss_u4_shapes 1 61 #f 0 )
; (kern-mk-sprite 's_flagstone   ss_u4_shapes 1 62 #f 0 )
; (kern-mk-sprite 's_deck        ss_u4_shapes 1 63 #f 0 )

; (kern-mk-sprite 's_moongate_quarter        ss_u4_shapes 1 64 #f 0 )
; (kern-mk-sprite 's_moongate_half           ss_u4_shapes 1 65 #f 0 )
; (kern-mk-sprite 's_moongate_three_quarters ss_u4_shapes 1 66 #f 0 )
; (kern-mk-sprite 's_moongate_full           ss_u4_shapes 1 67 #f 0 )

; (kern-mk-sprite 's_field_poison ss_u4_shapes 1 68 #t 0 )
; (kern-mk-sprite 's_field_energy ss_u4_shapes 1 69 #t 0 )
; (kern-mk-sprite 's_field_fire   ss_u4_shapes 1 70 #t 0 )
; (kern-mk-sprite 's_field_sleep  ss_u4_shapes 1 71 #t 0 )
; (kern-mk-sprite 's_wall         ss_u4_shapes 1 72 #f 0 )
; (kern-mk-sprite 's_secret_door  ss_u4_shapes 1 73 #f 0 )
; (kern-mk-sprite 's_altar_obj        ss_u4_shapes 1 74 #f 0 )
; (kern-mk-sprite 's_lava         ss_u4_shapes 1 76 #t 0 )
; (kern-mk-sprite 's_projectile   ss_u4_shapes 1 77 #f 0 )
; (kern-mk-sprite 's_magic        ss_u4_shapes 1 78 #f 0 )
(kern-mk-sprite 's_hit          ss_u4_shapes 1 79 #f 0 )
; (kern-mk-sprite 's_guard        ss_u4_shapes 2 80 #f 0 )
; (kern-mk-sprite 's_townsman     ss_u4_shapes 2 82 #f 0 )
; (kern-mk-sprite 's_bard         ss_u4_shapes 2 84 #f 0 )
; (kern-mk-sprite 's_jester       ss_u4_shapes 2 86 #f 0 )
; (kern-mk-sprite 's_beggar       ss_u4_shapes 2 88 #f 0 )
; (kern-mk-sprite 's_child        ss_u4_shapes 2 90 #f 0 )
; (kern-mk-sprite 's_bull         ss_u4_shapes 2 92 #f 0 )
; (kern-mk-sprite 's_lord         ss_u4_shapes 2 94 #f 0 )

; (kern-mk-sprite 's_A ss_u4_shapes 1  96 #f 0 )
; (kern-mk-sprite 's_B ss_u4_shapes 1  97 #f 0 )
; (kern-mk-sprite 's_C ss_u4_shapes 1  98 #f 0 )
; (kern-mk-sprite 's_D ss_u4_shapes 1  99 #f 0 )
; (kern-mk-sprite 's_E ss_u4_shapes 1 100 #f 0 )
; (kern-mk-sprite 's_F ss_u4_shapes 1 101 #f 0 )
; (kern-mk-sprite 's_G ss_u4_shapes 1 102 #f 0 )
; (kern-mk-sprite 's_H ss_u4_shapes 1 103 #f 0 )
; (kern-mk-sprite 's_I ss_u4_shapes 1 104 #f 0 )
; (kern-mk-sprite 's_J ss_u4_shapes 1 105 #f 0 )
; (kern-mk-sprite 's_K ss_u4_shapes 1 106 #f 0 )
; (kern-mk-sprite 's_L ss_u4_shapes 1 107 #f 0 )
; (kern-mk-sprite 's_M ss_u4_shapes 1 108 #f 0 )
; (kern-mk-sprite 's_N ss_u4_shapes 1 109 #f 0 )
; (kern-mk-sprite 's_O ss_u4_shapes 1 110 #f 0 )
; (kern-mk-sprite 's_P ss_u4_shapes 1 111 #f 0 )
; (kern-mk-sprite 's_Q ss_u4_shapes 1 112 #f 0 )
; (kern-mk-sprite 's_R ss_u4_shapes 1 113 #f 0 )
; (kern-mk-sprite 's_S ss_u4_shapes 1 114 #f 0 )
; (kern-mk-sprite 's_T ss_u4_shapes 1 115 #f 0 )
; (kern-mk-sprite 's_U ss_u4_shapes 1 116 #f 0 )
; (kern-mk-sprite 's_V ss_u4_shapes 1 117 #f 0 )
; (kern-mk-sprite 's_W ss_u4_shapes 1 118 #f 0 )
; (kern-mk-sprite 's_X ss_u4_shapes 1 119 #f 0 )
; (kern-mk-sprite 's_Y ss_u4_shapes 1 120 #f 0 )
; (kern-mk-sprite 's_Z ss_u4_shapes 1 121 #f 0 )

; (kern-mk-sprite 's_counter_2x1_c ss_u4_shapes 1 122 #f 0 )
; (kern-mk-sprite 's_counter_2x1_e ss_u4_shapes 1 123 #f 0 )
; (kern-mk-sprite 's_counter_2x1_w ss_u4_shapes 1 124 #f 0 )
; (kern-mk-sprite 's_counter_1x1   ss_u4_shapes 1 125 #f 0 )

; (kern-mk-sprite 's_blank          ss_u4_shapes 1 126 #f 0 )
(kern-mk-sprite 's_null           ss_u4_shapes 1 126 #f 0 )
; (kern-mk-sprite 's_wall_stone     ss_u4_shapes 1 127 #f 0 )

; (kern-mk-sprite 's_pirate_left    ss_u4_shapes 1 128 #f 0 )
; (kern-mk-sprite 's_pirate_front   ss_u4_shapes 1 129 #f 0 )
; (kern-mk-sprite 's_pirate_right   ss_u4_shapes 1 130 #f 0 )
; (kern-mk-sprite 's_pirate_back    ss_u4_shapes 1 131 #f 0 )

; (kern-mk-sprite 's_nixie          ss_u4_shapes 2 132 #f 0 )
; (kern-mk-sprite 's_kraken         ss_u4_shapes 2 134 #f 0 )
; (kern-mk-sprite 's_sea_serpent    ss_u4_shapes 2 136 #f 0 )
; (kern-mk-sprite 's_sea_horse      ss_u4_shapes 2 138 #f 0 )
; (kern-mk-sprite 's_whirlpool      ss_u4_shapes 2 140 #f 0 )
; (kern-mk-sprite 's_tornado        ss_u4_shapes 2 142 #f 0 )

; (kern-mk-sprite 's_rat            ss_u4_shapes 4 144 #f 0 )
; (kern-mk-sprite 's_bat            ss_u4_shapes 4 148 #f 0 )
; (kern-mk-sprite 's_spider         ss_u4_shapes 4 152 #f 0 )
; (kern-mk-sprite 's_ghost          ss_u4_shapes 4 156 #f 0 )
; (kern-mk-sprite 's_slime          ss_u4_shapes 4 160 #f 0 )
; (kern-mk-sprite 's_slime_asleep   ss_u4_shapes 1 160 #f 0 )
; (kern-mk-sprite 's_troll          ss_u4_shapes 4 164 #f 0 )
; (kern-mk-sprite 's_gremlin        ss_u4_shapes 4 168 #f 0 )
; (kern-mk-sprite 's_mimic          ss_u4_shapes 4 172 #f 0 )
; (kern-mk-sprite 's_reaper         ss_u4_shapes 4 176 #f 0 )
; (kern-mk-sprite 's_insects        ss_u4_shapes 4 180 #f 0 )
; (kern-mk-sprite 's_gazer          ss_u4_shapes 4 184 #f 0 )
; (kern-mk-sprite 's_deathknight         ss_u4_shapes 4 188 #f 0 )
; (kern-mk-sprite 's_orc            ss_u4_shapes 4 192 #f 0 )
; (kern-mk-sprite 's_skeleton       ss_u4_shapes 4 196 #f 0 )
; (kern-mk-sprite 's_brigand        ss_u4_shapes 4 200 #f 0 )
; (kern-mk-sprite 's_snake          ss_u4_shapes 4 204 #f 0 )
; (kern-mk-sprite 's_ettin          ss_u4_shapes 4 208 #f 0 )
; (kern-mk-sprite 's_headless       ss_u4_shapes 4 212 #f 0 )
; (kern-mk-sprite 's_cyclops        ss_u4_shapes 4 216 #f 0 )
; (kern-mk-sprite 's_wisp           ss_u4_shapes 4 220 #f 0 )
; (kern-mk-sprite 's_lich           ss_u4_shapes 4 228 #f 0 )
; (kern-mk-sprite 's_drake          ss_u4_shapes 4 232 #f 0 )
; (kern-mk-sprite 's_zorn           ss_u4_shapes 4 236 #f 0 )
; (kern-mk-sprite 's_demon          ss_u4_shapes 4 240 #f 0 )
; (kern-mk-sprite 's_hydra          ss_u4_shapes 4 244 #f 0 )
; (kern-mk-sprite 's_dragon         ss_u4_shapes 4 248 #f 0 )
; (kern-mk-sprite 's_balron         ss_u4_shapes 4 252 #f 0 )

(kern-mk-sprite 's_frame_ulc   ss_frame 1  0 #f 0 )
(kern-mk-sprite 's_frame_td    ss_frame 1  1 #f 0 )
(kern-mk-sprite 's_frame_urc   ss_frame 1  2 #f 0 )
(kern-mk-sprite 's_frame_endu  ss_frame 1  3 #f 0 )  ; top of vertical bar, currently unused
(kern-mk-sprite 's_frame_tr    ss_frame 1  4 #f 0 )
(kern-mk-sprite 's_frame_plus  ss_frame 1  5 #f 0 )  ; center crosspiece, currently unused
(kern-mk-sprite 's_frame_tl    ss_frame 1  6 #f 0 )
(kern-mk-sprite 's_frame_vert  ss_frame 1  7 #f 0 )
(kern-mk-sprite 's_frame_llc   ss_frame 1  8 #f 0 )
(kern-mk-sprite 's_frame_tu    ss_frame 1  9 #f 0 )
(kern-mk-sprite 's_frame_lrc   ss_frame 1 10 #f 0 )
(kern-mk-sprite 's_frame_endb  ss_frame 1 11 #f 0 )  ; bottom of vertical bar, currently unused
(kern-mk-sprite 's_frame_endl  ss_frame 1 12 #f 0 )
(kern-mk-sprite 's_frame_horz  ss_frame 1 13 #f 0 )
(kern-mk-sprite 's_frame_endr  ss_frame 1 14 #f 0 )
(kern-mk-sprite 's_frame_dot   ss_frame 1 15 #f 0 )  ; disconnected disk, currently unused

; (kern-mk-sprite 'ls_ankh          ss_u4_charset 1  0 #f 0 )
; (kern-mk-sprite 'ls_shield        ss_u4_charset 1  1 #f 0 )
; (kern-mk-sprite 'ls_holey_wall    ss_u4_charset 1  2 #f 0 )
; (kern-mk-sprite 'ls_wall          ss_u4_charset 1  3 #f 0 )
; (kern-mk-sprite 'ls_updown_arrow  ss_u4_charset 1  4 #f 0 )
; (kern-mk-sprite 'ls_down_arrow    ss_u4_charset 1  5 #f 0 )
; (kern-mk-sprite 'ls_up_arrow      ss_u4_charset 1  6 #f 0 )
; (kern-mk-sprite 'ls_holey_ankh    ss_u4_charset 1  7 #f 0 )
; (kern-mk-sprite 'ls_white_ball    ss_u4_charset 1  8 #f 0 )
; (kern-mk-sprite 'ls_copyright     ss_u4_charset 1  9 #f 0 )
; (kern-mk-sprite 'ls_trademark     ss_u4_charset 1 10 #f 0 )
; (kern-mk-sprite 'ls_male          ss_u4_charset 1 11 #f 0 )
; (kern-mk-sprite 'ls_female        ss_u4_charset 1 12 #f 0 )
; (kern-mk-sprite 'ls_hbar          ss_u4_charset 1 13 #f 0 )
; (kern-mk-sprite 'ls_vbar          ss_u4_charset 1 13 #f 0 )
; (kern-mk-sprite 'ls_square        ss_u4_charset 1 14 #f 0 )
; (kern-mk-sprite 'ls_blue_ball     ss_u4_charset 1 15 #f 0 )
; (kern-mk-sprite 'ls_hbar_right    ss_u4_charset 1 16 #f 0 )
; (kern-mk-sprite 'ls_hbar_left     ss_u4_charset 1 17 #f 0 )
; (kern-mk-sprite 'ls_vbar_top      ss_u4_charset 1 16 #f 0 )
; (kern-mk-sprite 'ls_vbar_bottom   ss_u4_charset 1 17 #f 0 )
; (kern-mk-sprite 'ls_blank_one     ss_u4_charset 1 18 #f 0 )
; (kern-mk-sprite 'ls_dot_dot_dot   ss_u4_charset 1 19 #f 0 )
(kern-mk-sprite 'ls_whirlpool     ss_u4_charset 4 28 #f 0 )
; (kern-mk-sprite 'ls_blank_three   ss_u4_charset 1 32 #f 0 )


;; sounds
(load "sounds.scm")

;(load "effects.scm")

;; terrain
(define opq 12) ;; opaque
(define hvy 5)  ;; heavy
(define dns 3)  ;; dense
(define lgt 2)  ;; light (density)
(define trn 0)  ;; transparent
(kern-mk-terrain 't_grass "grass" pclass-grass s_grass trn 0 nil)

;; palette
(kern-mk-palette 'pal_expanded
  (list
    (list  ".."   t_grass)              ;; "grass"
  )
) ;; palette pal_expanded




;(load "fields.scm")
;;(load "combat-maps.scm")

;; Object types
(load "objs.scm")
;(load "traps.scm")
;(load "pitfalls.scm")
;(load "landslide.scm")
;(load "containers.scm")
;(load "reagents.scm")
;(load "food.scm")

;; arms
(define (weap-ap mult)
	(floor (* mult default-weapon-rap)))
	
(define (armour-ap mult)
	(floor (* mult default-armour-apmod)))

(define obj-ifc-cap (ifc-cap obj-ifc))    

(define (mk-melee-arms-type tag name sprite to-hit-bonus damage deflect AP_cost AP_mod slots 
                            num-hands range weight
                            stratt_mod dexatt_mod
                            damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range AP_cost AP_mod nil nil #f #f weight nil
					 obj-ifc-cap obj-ifc stratt_mod dexatt_mod damage_mod avoid_mod mmode-smallobj))

(mk-melee-arms-type  't_hands          "bare hands"     nil              "1d2"    "1d2"    "1d2"    (weap-ap 0.67) 0 slot-nil      1      1     0        50      20       10      1.0  )

;(load "powers.scm")
;(load "ability.scm")
;(load "cast-ui.scm")
;(load "spells.scm")
;(load "items.scm")
;(load "vehicles.scm")
;(load "beds.scm")
;(load "money.scm")
;(load "skills.scm")

;; occs
(define (mk-occ tag name hit def dam arm xp skset)
  (kern-mk-occ tag name 1.0 0 0 0 0 hit def dam arm xp skset)
  (kern-occ-set-gob (eval tag) (list nil nil nil nil nil nil nil nil)))

;;            /           /         / t  / f / e /   /
;;           /           /         / i  / e / g / r /
;;          /           / e       / h  / d / a / o /
;;         / g         / m       / -  / - / m / m /
;;        / a         / a       / o  / o / a / r / p
;;       / t         / n       / t  / t / d / a / x
(mk-occ 'oc_wanderer "wanderer"  5   5   5   5   8 nil)


;(load "ai.scm")

;; species
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

(define (rel-spd speed)
	(/ (* speed-human speed) 100))

;; mk-species -- register a species type with the kernel
(define (mk-species tag name str int dex spd con mag vr mmode weap morph xp sspr armordice mvsnd)
  (kern-mk-species tag name str int dex (rel-spd spd) vr mmode 
                   con 
                   (max (round (/ con 10)) 1)
                   mag 
                   (max (round (/ mag 2)) 1)
                   sspr
                   weap #t sound-damage 
                   mvsnd
                   xp
                   #f ;; stationary?
                   armordice
                   morph nil)) 
(mk-species 'sp_human "human" 10 10 10 100 10 2 13 mmode-walk t_hands humanoid  2 s_asleep nil sound-walking)


(load "conv.scm") ;; basic conversation
;(load "yellow-slime.scm")
;(load "troll.scm")
;(load "spider.scm")
;(load "npc-types.scm")
;(load "mimic.scm")
;(load "parties.scm")
;(load "jewelry.scm")
;(load "gate-guard.scm")

;; Mechanism-like things
;(load "bim.scm")
;(load "step.scm")
;(load "monster-generator.scm")
;;(load "wilderness-manager.scm")
; (load "terrain-to-ptype.scm")
; (load "edge-spawn.scm")
; (load "door.scm")
; (load "portcullis.scm")
; (load "hidden.scm")
; (load "lever.scm")
; (load "timer.scm")
; (load "tblit.scm")
; (load "portals.scm")
; (load "moongate.scm")
; (load "bridge.scm")
; (load "drawbridge.scm")
; (load "weather-vane.scm")
;(load "wind-bridge.scm")

;; Astronomy
;(load "moon.scm")

;; Quest system
(load "tbl.scm")
(load "ztats-quest-ui.scm")
(load "quest-sys.scm")
(load "quest-talk-to.scm")

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
(kern-set-ascii ss_u4_charset 32)
(kern-set-cursor ls_whirlpool)
(kern-set-damage-sprite s_hit)
(kern-add-hook 'combat_change_hook 'music-on-combat-change)
(kern-add-hook 'session_start_hook 'music-on-combat-change)

;; Setup the global effect sprites
; (kern-set-quicken-sprite s_quicken)
; (kern-set-time-stop-sprite s_time_stop)
; (kern-set-magic-negated-sprite s_magic_negated)
; (kern-set-reveal-sprite s_reveal)
; (kern-set-xray-vision-sprite s_xray_vision)

(kern-init-random)

;;----------------------------------------------------------------------------
;; this needs to be in a kern-loaded file so it's redefined on reload
(define gregors-conv
 (ifc nil
      (method 'default (lambda (knpc kpc) (say knpc "Can't help you there.")))
      (method 'hail (lambda (knpc kpc) (say knpc "Hullo.")))
      (method 'heal (lambda (knpc kpc) (say knpc "[cough] Well enough, my granddaughter helps take care of me.")))
      (method 'bye (lambda (knpc kpc) (say knpc "So long.")))
      (method 'job (lambda (knpc kpc) (say knpc "I'm a charcoal burner. I also care for this ^c+rshrine^c-.")))
      (method 'join (lambda (knpc kpc) (say knpc "Nope. Already got a job.")))
      (method 'name (lambda (knpc kpc) (say knpc "Gregor's my name.")))
      ))

