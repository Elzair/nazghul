;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;; This file loads 58 files

;; Toggle handling for quests
(define use-quest-pane #t)

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


;; Difficulty Classes. "Normal" means an L5 professional with key attribute at
;; 16 will succeed 50% of the time. For example, an L5 wrogue with 16 dexterity
;; will have a thiefly ability of 10. This person should be able to pick a
;; normal lock 50% of the time (remember, he gets to retry with no penalty
;; except mana loss and maybe breaking a pick on critical failure).
;;
;; 1d10 + 1d20 = 5.5 + 10.5 = 16
;;
;; For the most difficult things, an L9 professional wrogue with an unusually
;; strong key attribute of 30 will have an ability rating of 19, and should
;; only succeed on two perfect rolls:
;;
;; 19 + 20 = 39
;;
;; Caveat: for abilities other than wrogues (eg, magic or strength-based
;; abilities), the relationship between ability level, key attribute and
;; advancement level is different (in fact, it's different in each case, see
;; occs.scm).
(define dc-trivial        3) ;; >98%
(define dc-easy           6) ;; >90%
(define dc-nontrivial     10) ;; >75%
(define dc-normal         15) ;; >50%
(define dc-challenging    26) ;; 5%
(define dc-hard           28) ;; <2%
(define dc-masterful      30) ;; just impossible at L5,DEX=16
(define dc-supremely-hard 38) ;; perfect roll at high levels

(define dc-escape-ensnare  dc-challenging)
(define dc-escape-paralyze dc-normal)
(define dc-avoid-stuck     dc-hard)
(define dc-escape-stuck    dc-hard)
(define dc-reach           dc-hard)

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
(define faction-glasdrin      13) ;; must be separate from men, or exile by statue will affect player with all towns
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
(load "sprite-sets.scm")
(load "sprites.scm")
(load "sounds.scm")
(load "effects.scm")
(load "terrains.scm")
(load "palette.scm")
(load "fields.scm")
(load "combat-maps.scm")

;; Object types
(load "objs.scm")
(load "traps.scm")
(load "pitfalls.scm")
(load "landslide.scm")
(load "containers.scm")
(load "reagents.scm")
(load "food.scm")
(load "arms.scm")
(load "powers.scm")
(load "ability.scm")
(load "cast-ui.scm")
(load "spells.scm")
(load "items.scm")
(load "vehicles.scm")
(load "beds.scm")
(load "money.scm")
(load "skills.scm")
(load "occs.scm")
(load "ai.scm")
(load "species.scm")
(load "conv.scm") ;; basic conversation
(load "yellow-slime.scm")
(load "troll.scm")
(load "spider.scm")
(load "npc-types.scm")
(load "mimic.scm")
(load "parties.scm")
(load "jewelry.scm")
(load "gate-guard.scm")

;; Mechanism-like things
(load "bim.scm")
(load "step.scm")
(load "monster-generator.scm")
;;(load "wilderness-manager.scm")
(load "terrain-to-ptype.scm")
(load "edge-spawn.scm")
(load "door.scm")
(load "portcullis.scm")
(load "hidden.scm")
(load "lever.scm")
(load "timer.scm")
(load "tblit.scm")
(load "portals.scm")
(load "moongate.scm")
(load "bridge.scm")
(load "drawbridge.scm")
(load "weather-vane.scm")
(load "wind-bridge.scm")

;; Astronomy
(load "moon.scm")

;; Quest system
(load "tbl.scm")
(load "ztats-quest-ui.scm")
(load "quest-sys.scm")

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
(kern-add-query 'str_based_attack_query proc-stratt)
(kern-add-query 'dex_based_attack_query proc-dexatt)
(kern-add-query 'damage_bonus_query proc-stratt)
(kern-add-query 'defense_bonus_query proc-dexdef)
(kern-add-hook 'combat_change_hook 'music-on-combat-change)
(kern-add-hook 'session_start_hook 'music-on-combat-change)

;; Setup the global effect sprites
(kern-set-quicken-sprite s_quicken)
(kern-set-time-stop-sprite s_time_stop)
(kern-set-magic-negated-sprite s_magic_negated)
(kern-set-reveal-sprite s_reveal)
(kern-set-xray-vision-sprite s_xray_vision)

(kern-init-random)

