;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")

;; clone of game.scm ---------------------------------------------------------

;; Setup progress bar stuff. The number 44 should be the total number of files
;; we're going to load.
(kern-progress-bar-start "Loading" 44)

;; Wrap the original definition of (load ...) with one that advances the
;; progress bar.
(define original-load load)  
(define (load file)
  (kern-progress-bar-advance 1)
  (original-load file)
  )

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
   (list 'mmode-cannon    "crawling"   20) ;; enhanced missile passibility for cannon shells
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
(define faction-num           12)
(define faction-green-tower   faction-men)
(define faction-glasdrin      faction-men)
(define faction-oparine       faction-men)
(define faction-trigrave      faction-men)
(define faction-nixie         faction-monster)
(define faction-prisoner      13)

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

(define s_altar (mk-composite-sprite (list s_grass s_altar_obj)))
(define s_active_altar (mk-composite-sprite (list s_grass s_active_altar_obj)))
(define s_overgrown_altar (mk-composite-sprite (list s_trees s_altar_obj)))

(load "sounds.scm")
(load "effects.scm")
(load "terrains.scm")
(load "fields.scm")
;;(load "combat-maps.scm")

;; Object types
(load "objs.scm")
;;(load "traps.scm")
;;(load "pitfalls.scm")
;;(load "landslide.scm")
(load "containers.scm")
(load "reagents.scm")
(load "food.scm")
(load "arms.scm")
(load "powers.scm")
(load "ability.scm")
(load "cast-ui.scm")
(load "spells.scm")
(load "items.scm")
;;(load "vehicles.scm")
;;(load "beds.scm")
(load "money.scm")
(load "skills.scm")
(load "occs.scm")
(load "ai.scm")
(load "species.scm")
(load "conv.scm") ;; basic conversation
(load "npc-types.scm")
(load "spider.scm")
;;(load "mimic.scm")
;;(load "parties.scm")
;;(load "jewelry.scm")
(load "gate-guard.scm")

;; Mechanism-like things
(load "bim.scm")
(load "step.scm")
(load "monster-generator.scm")
;;(load "wilderness-manager.scm")
(load "terrain-to-ptype.scm")
;;(load "edge-spawn.scm")
;;(load "door.scm")
;;(load "portcullis.scm")
(load "hidden.scm")
;;(load "lever.scm")
(load "timer.scm")
;;(load "tblit.scm")
(load "portals.scm")
(load "moongate.scm")
;;(load "bridge.scm")
;;(load "drawbridge.scm")
;;(load "weather-vane.scm")
;;(load "wind-bridge.scm")

;; Astronomy
(load "moon.scm")

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

;; Setup the global effect sprites
(kern-set-quicken-sprite s_quicken)
(kern-set-time-stop-sprite s_time_stop)
(kern-set-magic-negated-sprite s_magic_negated)
(kern-set-reveal-sprite s_reveal)
(kern-set-xray-vision-sprite s_xray_vision)

(kern-init-random)

(kern-progress-bar-finish)

;; end clone of game.scm------------------------------------------------------



(kern-load "runes.scm")

(define logo-image (kern-image-load "haximatext.png"))
(define yoff 7)
(define xoff -3)

;;----------------------------------------------------------------------------
;; Time -- this needs to be set before loading any dungeon rooms
;;----------------------------------------------------------------------------
(define hour 12)
(define minutes 00)
(define time-in-minutes (+ (* hour 60) minutes))
(define game-start-time (time-mk 0 0 0 0 hour minutes))

(kern-set-clock 
 0 ; year
 0 ; month
 0 ; week
 0 ; day
 hour  ; hour
 minutes ; minutes
 )

;;----------------------------------------------------------------------------
;; Gate Traveler
;;----------------------------------------------------------------------------

(define flee-gate (list #f))

(define (traveler-goto-dest kchar dest)
  (let (
         (loc (kern-obj-get-location kchar))
         )
    (if (equal? loc dest)
        (begin
          (kern-obj-remove kchar)
          (kern-map-repaint)
          #t)
        (begin
          (pathfind kchar dest)))))
          
(define (traveler-dest kchar)
	(let ((loc (kern-obj-get-location kchar)))
		(if (car flee-gate)
			(loc-mk (loc-place loc) (+ xoff 20) (+ yoff  5))
			(cons 
				(loc-place loc) 
				(npcg-get-post (gob kchar))
			))
	))

(define (wizard-traveler-ai kchar)
  (or (spell-sword-ai kchar)
      (traveler-goto-dest kchar (traveler-dest kchar))))

(define (normal-traveler-ai kchar)
  (or (std-ai kchar)
      (if (any-visible-hostiles? kchar)
          #f
          (traveler-goto-dest kchar (traveler-dest kchar)))))
       
;; these detect who kchar is friendly/hostile to, not vice-versa
(define (all-demons-near kchar)
  (filter (lambda (kobj) (eqv? (kern-char-get-species kobj) sp_demon))
          (kern-place-get-beings (loc-place (kern-obj-get-location kchar)))))
    
(define (all-allies-near kchar)
  (filter (lambda (kobj) (eqv? (kern-char-get-species kobj) sp_human))
          (kern-place-get-beings (loc-place (kern-obj-get-location kchar)))))      
             
(define (wizard-defender-ai kchar)
  (or (spell-sword-ai kchar)
  		(if (and (null? (all-demons-near kchar)) (> (kern-dice-roll "1d3") 2))
  			(if (> (kern-dice-roll "1d3") 2)
     			(traveler-goto-dest kchar (traveler-dest kchar))
     			#t
     		)
     		#f)))

(define (normal-defender-ai kchar)
  (or (std-ai kchar)
      (if (and (null? (all-demons-near kchar)) (> (kern-dice-roll "1d3") 2))
      	(if (> (kern-dice-roll "1d3") 2)
          	(traveler-goto-dest kchar (traveler-dest kchar))
          	(begin 
          		(kern-obj-dec-ap kchar base-move-ap)
          		#t
          	)
          )
          #f)))

(define (traveler-exit kchar)
  (let* ((loc (kern-obj-get-location kchar))
         (dest (loc-mk (loc-place loc) (+ xoff 20) (+ yoff  5)))
       )
    (traveler-goto-dest kchar dest)
  ))

(define (seek-loot kchar)
  (let ((loot-list 
         (filter (mk-ifc-query 'get)
                 (kern-place-get-objects (loc-place (kern-obj-get-location kchar))))
         ))
    (if (not (null? loot-list))
        (let* ((targetloot (nearest-obj kchar loot-list)))
          (if (< (kern-get-distance (kern-obj-get-location kchar) (kern-obj-get-location targetloot)) 2)
              (begin 
                (kern-obj-remove targetloot)
                (kern-map-repaint)
                #t
                )
              (pathfind kchar (kern-obj-get-location targetloot))
              ))
        #f
        )
    ))
    
(define (beggar-exit kchar)
  (or (seek-loot kchar)
      (traveler-exit kchar))
    ) 
    
(define (erratic-traveler-ai kchar)
  (or (std-ai kchar)
     (if (> (kern-dice-roll "1d5") 1)
	   	(if (> (kern-dice-roll "1d3") 1)
	       	(traveler-goto-dest kchar (traveler-dest kchar))
	       	(begin
          		(kern-obj-dec-ap kchar base-move-ap)
		       	#t
	       	)
       )
       #f)))
          
(define (traveler-mk kplace)
  (let* ((wizard-sprites (list s_black_mage s_old_mage s_plain_mage s_cloaked_female s_companion_wizard s_red_wizard s_lady s_silas s_companion_druid))
         (warrior-sprites (list s_avatar s_wanderer s_companion_fighter s_companion_paladin s_brigand))
         (wrogue-sprites (list s_companion_bard s_companion_tinker s_companion_ranger s_ranger s_brigandess s_old_ranger))
         (info (random-select (list (list 'wizard 'wizard-traveler-ai wizard-sprites) 
                                    (list 'paladin 'normal-traveler-ai warrior-sprites)
                                    (list 'tinker 'normal-traveler-ai wrogue-sprites)
                                    (list 'ranger 'normal-traveler-ai wrogue-sprites)
                                    )))
         (path (random-select (list 
                               (list (loc-mk kplace (+ xoff 20) (+ yoff  4)) (list  (+ xoff 9) (+ yoff  5)) #f)
                               (list (loc-mk kplace (+ xoff 20) (+ yoff  5)) (list  (+ xoff 9) (+ yoff  5)) #f)
                               (list (loc-mk kplace (+ xoff 20) (+ yoff  6)) (list  (+ xoff 9) (+ yoff  5)) #f)

                               (list (loc-mk kplace (+ xoff 10) (+ yoff  4)) (list  (+ xoff 20)(+ yoff  4)) #t)
                               (list (loc-mk kplace (+ xoff 10) (+ yoff  5)) (list  (+ xoff 20)(+ yoff  5)) #t)
                               (list (loc-mk kplace (+ xoff 10) (+ yoff  6)) (list  (+ xoff 20)(+ yoff  6)) #t)
                               )))
         (kchar (mk-npc (car info) 9))
         )
    (npcg-set-post! (gob kchar) (cadr path))
    (kern-char-set-ai kchar (cadr info) kchar)
    (kern-obj-set-sprite kchar (random-select (caddr info)))
    (kern-obj-put-at kchar (car path))
    (kern-map-repaint)
    (kern-sleep 20)
    kchar))

(define (defender-mk ctype aitype kplace postx posty)
	(let ((kchar (mk-npc ctype 9)))
		(npcg-set-post! (gob kchar) (list postx posty))
	   (kern-char-set-ai kchar aitype)
   	(kern-obj-put-at kchar (loc-mk kplace postx posty))
   	(kern-map-repaint)
      (kern-sleep 20)
   	kchar		
	))
    
;;----------------------------------------------------------------------------
;; Wise
;;----------------------------------------------------------------------------
(define (wait-ai kchar)
  #t)

(define (wise-enter-ai kchar)
  (let* ((wise (gob kchar))
         (loc (kern-obj-get-location kchar))
         (dest (cons (loc-place loc) (npcg-get-post wise)))
         )
    (if (equal? loc dest)
        (let ((kmgr (car (npcg-get-subgob wise))))
          ;; Tell the mgr this one is in position and switch it over to waiting
          ;; mode.
          (scene-mgr-incr-num-wise-in-pos! (gob kmgr))
          (kern-char-set-ai kchar 'wait-ai)
          #t)
        (pathfind kchar dest))))

(define (wise-exit-ai kchar)
  (let* ((wise (gob kchar))
         (loc (kern-obj-get-location kchar))
         (dest (cons (loc-place loc) 
                     (npcg-get-post wise)))
         )
    (if (equal? loc dest)
        (let ((kmgr (car (npcg-get-subgob wise))))
          (kern-obj-remove kchar)
          (kern-map-repaint)
          (scene-mgr-incr-num-wise-in-pos! (gob kmgr))
          #t)
        (begin
          (pathfind kchar dest)))))

(define (wise-mk kplace n kmgr)
  (let* ((kchar (mk-npc 'wizard 9))
         (info (list-ref (list 
                         (list (list (+ xoff 20) (+ yoff 4)) (list  3 11) s_black_mage)
                         (list (list (+ xoff 20) (+ yoff 5)) (list  3 13) s_old_mage)
                         (list (list (+ xoff 20) (+ yoff 6)) (list  5  9) s_plain_mage)
                         (list (list (+ xoff 20) (+ yoff 4)) (list  5 15) s_cloaked_female)
                         (list (list (+ xoff 20) (+ yoff 5)) (list  7 15) s_companion_wizard)
                         (list (list (+ xoff 20) (+ yoff 6)) (list  7  9) s_red_wizard)
                         (list (list (+ xoff 20) (+ yoff 4)) (list  9 11) s_lady)
                         (list (list (+ xoff 20) (+ yoff 5)) (list  9 13) s_silas)
                              )
                        n))
         (enter-pos (cons kplace (car info)))
         (altar-pos (cadr info))
        )
    (kern-char-set-ai kchar 'wise-enter-ai)
    (npcg-set-post! (gob kchar) altar-pos)    

    ;; Save the kmgr object in the gob so the wise can notify it when they are
    ;; in position. Also remember the entrance position so they can pathfind
    ;; back.
    (npcg-set-subgob! (gob kchar) (cons kmgr (car info)))
    (kern-obj-set-sprite kchar (caddr info))
    (kern-obj-put-at kchar enter-pos)
    (kern-map-repaint)
    (kern-sleep 20)
    kchar))

;;----------------------------------------------------------------------------
;; Special Object Types
;;----------------------------------------------------------------------------
(define portal-ifc
  (ifc '()
       (method 'step (lambda (kportal kobj) 
                       ;;(println "portal-step")
                       ;;(kern-map-flash 100)
                       (kern-obj-remove kobj)
                       ))
       ))
(mk-obj-type 't_portal "Portal" s_blackgate_full layer-mechanism portal-ifc)

;;----------------------------------------------------------------------------
;; Scene Manager
;;----------------------------------------------------------------------------
(define (scene-mgr-mk) (list 'scene-mgr 0 0 0 '() 0 0))
(define (scene-mgr-state gob) (list-ref gob 1))
(define (scene-mgr-set-state! gob val) (set-car! (list-tail gob 1) val))
(define (scene-mgr-advance-state! gob) 
  (set-car! (list-tail gob 1) (+ 1 (scene-mgr-state gob)))
  (scene-mgr-set-pause! gob 0)
  )
(define (scene-mgr-get-num-demons gob) (list-ref gob 2))
(define (scene-mgr-set-num-demons! gob val) (set-car! (list-tail gob 2) val))
(define (scene-mgr-incr-num-demons! gob) (set-car! (list-tail gob 2) (+ 1 (scene-mgr-get-num-demons gob))))
(define (scene-mgr-get-num-travelers gob) (list-ref gob 3))
(define (scene-mgr-set-num-travelers! gob val) (set-car! (list-tail gob 3) val))
(define (scene-mgr-incr-num-travelers! gob) (set-car! (list-tail gob 3) (+ 1 (scene-mgr-get-num-travelers gob))))

(define (scene-mgr-get-wise gob) (list-ref gob 4))
(define (scene-mgr-get-num-wise gob) (length (scene-mgr-get-wise gob)))
(define (scene-mgr-add-wise! gob kwise) (set-car! (list-tail gob 4) (cons kwise (scene-mgr-get-wise gob))))

(define (scene-mgr-get-num-wise-in-pos gob) (list-ref gob 5))
(define (scene-mgr-set-num-wise-in-pos! gob val) (set-car! (list-tail gob 5) val))
(define (scene-mgr-incr-num-wise-in-pos! gob) (set-car! (list-tail gob 5) (+ 1 (scene-mgr-get-num-wise-in-pos gob))))
(define (scene-mgr-get-pause gob) (list-ref gob 6))
(define (scene-mgr-set-pause! gob val) (set-car! (list-tail gob 6) val))
(define (scene-mgr-incr-pause! gob) (set-car! (list-tail gob 6) (+ 1 (scene-mgr-get-pause gob))))

(define (scene-mgr-intro-travelers-phase kobj)
  ;;(println "scene-mgr-intro-travelers-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-travelers smgr))
         )
    (define (put-traveler)
      ;;(println "put-traveler")
      (traveler-mk (loc-place (kern-obj-get-location kobj)))
      (scene-mgr-incr-num-travelers! smgr)
      )
    (cond ((< n 10) 
           (if (> (kern-dice-roll "1d10") 9)
               (put-traveler)))
          (else
           (if (> (kern-dice-roll "1d5") 4)
           (scene-mgr-advance-state! smgr))))
    ))

(define (scene-mgr-intro-demons-phase kobj)
  ;;(println "scene-mgr-intro-demons-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-demons smgr))
         )
    (define (put-demon dir)
      (let ((kdemon (mk-npc 'demon 9)))
        (kern-char-set-ai kdemon 'std-ai)
        ;;(kern-map-flash 1000)
        (kern-obj-put-at kdemon 
                         (loc-offset (mk-loc (loc-place (kern-obj-get-location kobj)) (+ xoff 9)  (+ yoff 5))
                                     dir)))
    
      (scene-mgr-incr-num-demons! smgr)
              (kern-map-repaint)
  		  (kern-sleep 20)
      )
    (cond ((= n 0) 
    			(set-car! flee-gate #t)
           (kern-add-reveal 1000)
           (put-demon west))
          ((= n 1) (put-demon east))
          ((= n 2) (put-demon south))
          ((= n 3) (put-demon north))
          ((= n 4) ;; second wave
          	(if (> (kern-dice-roll "1d7") 6)
          		(put-demon west)
          	)
          )
          ((= n 5) (put-demon east))
          ((= n 6) (put-demon south))
          ((= n 7) (put-demon north))
          (else
           (scene-mgr-advance-state! smgr)))
    ))
          
(define (scene-mgr-wait-for-no-demons-phase kobj)
  (if (null? (all-demons-near (kern-get-player)))
  			(if (> (kern-dice-roll "1d7") 6)
      		(scene-mgr-advance-state! (gob kobj)))
			(if (< (length (all-allies-near (kern-get-player))) 3)
				(let ((kplace (loc-place (kern-obj-get-location kobj))))
					(defender-mk 'ranger 'normal-defender-ai kplace (+ xoff 20) (+ yoff  5))
					(defender-mk 'paladin 'normal-defender-ai kplace (+ xoff 20) (+ yoff  6))
					(if (> (kern-dice-roll "1d4") 3)
						(defender-mk 'wizard 'wizard-defender-ai kplace (+ xoff 20) (+ yoff  4))
						(defender-mk 'paladin 'normal-defender-ai kplace (+ xoff 20) (+ yoff  4))
					)
				))
      ))

(define (scene-mgr-intro-wise kobj)
  ;;(println "scene-mgr-intro-wise")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-wise smgr))
         )
    (cond ((< n 8) 
    			(if (or (> n 6) (> (kern-dice-roll "1d4") 3))
           (scene-mgr-add-wise! smgr (wise-mk (loc-place (kern-obj-get-location kobj)) n kobj))
           ))
          (else
           (scene-mgr-advance-state! smgr)))
    )
  )

(define (scene-mgr-wait-for-wise kobj)
  ;;(println "scene-mgr-wait-for-wise")
  (let ((smgr (gob kobj)))
    (if (= 8 (scene-mgr-get-num-wise-in-pos smgr))
      (scene-mgr-advance-state! smgr))))

(define (scene-mgr-close-gate kobj)
  ;;(println "scene-mgr-close-gate")
  
  (define (drop-rune krune loc)
    (kern-obj-put-at krune loc)
    (kern-place-set-terrain loc t_active_altar)
    )

  (define (drop-all-runes)
    (let ((kplace (loc-place (kern-obj-get-location kobj))))
      (drop-rune (kern-tag 'rune_k (kern-mk-obj t_rune_k 1)) (loc-mk kplace  4   8))
      (drop-rune (kern-tag 'rune_p (kern-mk-obj t_rune_p 1)) (loc-mk kplace  8   8))
      (drop-rune (kern-tag 'rune_s (kern-mk-obj t_rune_s 1)) (loc-mk kplace 10  10))
      (drop-rune (kern-tag 'rune_c (kern-mk-obj t_rune_c 1)) (loc-mk kplace 10  14))
      (drop-rune (kern-tag 'rune_f (kern-mk-obj t_rune_f 1)) (loc-mk kplace  8  16))
      (drop-rune (kern-tag 'rune_w (kern-mk-obj t_rune_w 1)) (loc-mk kplace  4  16))
      (drop-rune (kern-tag 'rune_d (kern-mk-obj t_rune_d 1)) (loc-mk kplace  2  14))
      (drop-rune (kern-tag 'rune_l (kern-mk-obj t_rune_l 1)) (loc-mk kplace  2  10))
      )
    (kern-map-repaint)
    )

  (define (close-portal)
    (map (lambda (stage)
           ;; The last sprite in the sequence is null, and if we set the portal's
           ;; sprite to null it will fall back on the sprite of its object type,
           ;; which is a fully-open gate, so catch that as a special case.
           (cond ((not (null? (stage-sprite stage)))
                  (kern-obj-set-sprite portal (stage-sprite stage))
                  (kern-obj-set-light portal (stage-light stage))
                  (kern-map-repaint)
                  (kern-sleep 125))))
         (reverse blackgate-stages))
    (kern-obj-remove portal)
    (kern-map-repaint)
    )
  
  (kern-log-msg "VAS AN EX REL POR!")
  ;; we need a better rumbling thunder/ earthquake type noise
  (kern-sound-play sound-lightning)
  (shake-map 15)
  (kern-map-flash 1000)
  (drop-all-runes)
  (close-portal)

  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-pause kobj delay)
  ;; Keep the sprites animating during pauses.
  (kern-map-repaint)
  (let ((smgr (gob kobj)))
    (if (>= (scene-mgr-get-pause smgr) delay)
        (scene-mgr-advance-state! (gob kobj))
        (scene-mgr-incr-pause! smgr))))

(define (scene-mgr-pickup-runes kobj)
  ;;(println "scene-mgr-pickup-runes")
  (for-each (lambda (krune)
              (kern-obj-remove krune)
              (kern-map-repaint)
              (kern-sleep 100)
              ;; Delay changing back the altar so the player can see that it is
              ;; the altar glowing, not the rune.
              (kern-place-set-terrain (kern-obj-get-location krune) t_altar)
              )
            (list rune_s rune_w rune_p rune_d rune_f rune_k rune_c rune_l))
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-exit-wise kobj)
  ;;(println "scene-mgr-exit-wise")
  (map (lambda (kwise)
         (let ((wise (gob kwise)))
           ;;(println "post:" (cdr (npcg-get-subgob wise)))
           (npcg-set-post! wise (cdr (npcg-get-subgob wise)))
           (kern-char-set-ai kwise 'wise-exit-ai)))
       (scene-mgr-get-wise (gob kobj)))
  (scene-mgr-set-num-wise-in-pos! (gob kobj) 0)
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-exit-guards kobj)
  ;;(println "scene-mgr-exit-guards") 
  (map (lambda (kchar)
		(if (not (is-player-party-member? kchar))
           (kern-char-set-ai kchar 'traveler-exit)))
       (kern-place-get-beings (loc-place (kern-obj-get-location kobj))))
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-wait-for-exits kobj)
  ;;(println "scene-mgr-wait-for-guards")
  (if (< (length (all-allies-near (kern-get-player))) 2)
	(scene-mgr-advance-state! (gob kobj)))
  )
  
(define (scene-mgr-exit-beggar kobj)
  ;; This is a hack to keep the camera centered on the beggar's location (and
  ;; thus the whole scene within the vision radius) while allowing the beggar
  ;; to go pick up the loot and then exit. The party member beggar's sprite is
  ;; changed to nil, making him invisible, then a new npc beggar is cloned.
  (let ((kcharn (mk-npc 'ranger 9)))
    (kern-obj-set-sprite kcharn s_beggar)
    (kern-char-set-ai kcharn 'beggar-exit)
    (kern-obj-put-at kcharn (kern-obj-get-location (kern-get-player)))
   )
  (map (lambda (kchar)
		(if (is-player-party-member? kchar)
           (kern-obj-set-sprite kchar nil)))
       (kern-place-get-beings (loc-place (kern-obj-get-location kobj))))
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-start-days-pass kobj)
  (kern-log-msg "Days pass...")
  (set-car! flee-gate #f)
  (let ((kplace (loc-place (kern-obj-get-location kobj))))
    (define (wolf-mk from-loc to-xy)
      (let ((kchar (mk-npc 'wolf 9)))
        (npcg-set-post! (gob kchar) to-xy)
        (kern-char-set-ai kchar 'erratic-traveler-ai)
        (kern-obj-put-at kchar from-loc)
        (kern-map-repaint)
  		  (kern-sleep 20)        
        ))
    (wolf-mk (loc-mk kplace 1 14) (list 17 7))
    )
  (kern-map-repaint)
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-end-days-pass kobj)
  (if (null? (filter (lambda (kobj)
                       (not (is-player-party-member? kobj)))
                     (kern-place-get-beings (loc-place (kern-obj-get-location kobj)))))
      (scene-mgr-advance-state! (gob kobj))
      ))

(define (scene-mgr-years-pass kobj)
  (kern-log-msg "Then years...")
  (let ((kplace (loc-place (kern-obj-get-location kobj))))
    (define (mk-troll loc)
      (let ((kchar (mk-npc 'troll 9)))
        (kern-char-set-ai kchar 'wait-ai)
        (kern-obj-put-at kchar loc)
        ))
    (kern-blit-map (kern-place-map kplace) 0 0
                   (kern-mk-map
                    nil     19 19 pal_expanded
                    (list
                     "000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 "
                     "019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 "
                     "038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 "
                     "057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 "
                     "076 077 078 079 080 081 082 083 084 085 086 087 088 089 090 091 092 093 094 "
                     "095 096 097 098 099 100 101 102 103 104 105 106 107 108 109 110 111 112 113 "
                     "fg fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fi "
							"fj t. |. t. t. tc .. .. ta t. t. |. |. |. tL __ t. |. fl "
							"fj |. t. tc ar .. .. .. ar t% t. |. t. t. _3 _c t. t. fl "
							"fj t. tc t# .. .. .. .. .. .. ta t. t. t. _2 tG t. t. fl "
							"fj tc ar .. .. .. bb .. .. .. ar |. t. tc _2 ta t. t. fl "
							"fj |. .. .. bb dd dd dd .. .. .. ta tc t# ee ee dd .. fl "
							"fj t. .. .. dd dd && dd dd .. .. dd dd ee ee dd dd dd fl "
							"fj t. .. .. .. dd dd dd .. .. .. t7 tA ee __ ee .. dd fl "
							"fj t. ar .. .. .. .. bb .. .. ar t. t. t5 __ t3 t. t. fl "
							"fj t. t5 tA .. .. .. .. .. .. t3 |. t. t5 _2 tJ |. t. fl "
							"fj t. t. t5 ar .. .. .. ar tC t. t. t. t. _a _5 t. |. fl "
							"fj t. t. |. t5 .. t7 .. t3 |. |. t. t. t. tH __ t. |. fl "
                     "fm fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fo " 
                     ))
                   0 0 19 19)
    (mk-troll (loc-mk kplace 5 12))
    (mk-troll (loc-mk kplace 7 12))
    )
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-end-years-pass kobj)
  (println "end-years-pass")
  (let ((trolls (filter 
                 (lambda (kobj)
                   (not (is-player-party-member? kobj)))
                 (kern-place-get-beings (loc-place (kern-obj-get-location kobj))))))
    (for-each kern-obj-remove trolls))
  (kern-map-repaint)
  (scene-mgr-advance-state! (gob kobj))
  )


(define (scene-mgr-ages-pass kobj)
  (kern-log-msg "Then ages...")
  (let ((kplace (loc-place (kern-obj-get-location kobj))))
    (kern-blit-map (kern-place-map kplace) 0 0
                   (kern-mk-map
                    nil     19 19 pal_expanded
                    (list
                     "000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 "
                     "019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 "
                     "038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 "
                     "057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 "
                     "076 077 078 079 080 081 082 083 084 085 086 087 088 089 090 091 092 093 094 "
                     "095 096 097 098 099 100 101 102 103 104 105 106 107 108 109 110 111 112 113 "
                     "fg fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fi "
							"fj |. |. |. t. |. |. |. t. |. |. |. |. tc %% __ _c %% fl "
							"fj |. t. t. ar t. |. t. at t. t. |. |. %3 _3 _c %c .. fl "
							"fj |. t. t. t. t. t. |. t. |. t. t. t. %a _e %c tC t3 fl "
							"fj t. at t. tc t# bb t% ta t. at t. t. .. %% .. t3 |. fl "
							"fj |. t. t. bb .. .. .. t% ta t. |. tc tA %% .. ta |. fl "
							"fj |. |. t. .. .. {f .. .. tD t. tB tD ta gg .. tD t. fl "
							"fj |. t. t. tA .. .. .. tC t3 t. t. t5 t# %% .. t3 |. fl "
							"fj t. at t. t5 tA .. bb t3 t. at t. t. .. %% .. ta |. fl "
							"fj |. t. |. t. t. t. t. t. |. t. |. t. tA %e .. t% t. fl "
							"fj |. |. t. at t. t. t. ar t. |. |. |. t5 .. %7 .. |. fl "
							"fj |. |. |. t. |. |. |. t. |. |. t. |. t. .. %% .. |. fl "                     
                     "fm fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fo "
                     ))
                   0 0 19 19)
    (define (deer-mk from-loc to-xy)
      (let ((kchar (mk-npc 'wolf 9)))
        (npcg-set-post! (gob kchar) to-xy)
        (kern-obj-set-sprite kchar s_deer)
        (kern-char-set-ai kchar 'erratic-traveler-ai)
        (kern-obj-put-at kchar from-loc)
        (kern-map-repaint)
        (kern-sleep 20)
      )
      )
    (deer-mk (loc-mk kplace 5 7) (list 12 17))
    (deer-mk (loc-mk kplace 3 7) (list 13 17))
    )
  (kern-map-repaint)
  (scene-mgr-advance-state! (gob kobj))
  )

(define (scene-mgr-ages-passed kobj)
  (let ((kplace (loc-place (kern-obj-get-location kobj))))
  )
  (kern-map-repaint)
  (scene-mgr-advance-state! (gob kobj))
  )



(define (scene-mgr-conclude kobj)
  (kern-log-msg "Until what was closed and locked by magic has been forgotten.")
  (let ((kplace (loc-place (kern-obj-get-location kobj))))
    (kern-blit-map (kern-place-map kplace) 0 0
                   (kern-mk-map
                    nil     19 19 pal_expanded
                    (list
                     "000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 "
                     "019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 "
                     "038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 "
                     "057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 "
                     "076 077 078 079 080 081 082 083 084 085 086 087 088 089 090 091 092 093 094 "
                     "095 096 097 098 099 100 101 102 103 104 105 106 107 108 109 110 111 112 113 "
                     "fg fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fi "
							"fj |. |. |. t. |. |. |. t. |. t. .. %% %% %% %% %c .. fl "
							"fj |. |. t. at t. |. t. at t. t. tA %a %% %% %% .. t3 fl "
							"fj |. t. t. t. |. |. |. t. |. t. t5 tA %a %% %c tC t. fl "
							"fj t. at t. |. |. bb |. |. t. at t. t5 .. %% .. t3 |. fl "
							"fj |. t. |. bb te .. ta |. |. t. |. t. .. %% .. ta |. fl "
							"fj |. |. |. t5 tB {f tD t. |. |. |. ta %b gg %d tD t. fl "
							"fj |. t. |. |. t5 tE tb |. |. t. |. t. .. %% .. t3 |. fl "
							"fj t. at t. |. |. t5 bb |. t. at t. t. .. gg .. ta |. fl "
							"fj |. t. |. t. |. |. |. t. |. t. |. t. tA gg %5 t% t. fl "
							"fj |. |. t. at t. |. t. at t. |. |. |. t5 .. gg .. t. fl "
							"fj |. |. |. t. |. |. |. t. |. |. |. |. t. .. gg .. t. fl "
                     "fm fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fo "
                     ))
                   0 0 19 19)
    (let ((kchar (mk-npc 'giant-spider 9)))
      (kern-obj-put-at  kchar (loc-mk kplace 6 12))
      (kern-char-set-ai kchar 'wait-ai)
      )
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 5 12))
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 6 10))
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 6 11))
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 6 12))
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 6 13))
    (kern-obj-put-at (kern-mk-obj F_web_perm 1) (loc-mk kplace 7 12))
    (kern-obj-put-at (kern-mk-obj t_corpse 1) (loc-mk kplace 7 12))
    )
  (kern-map-repaint)
  (scene-mgr-advance-state! (gob kobj))
  )
  
(define (scene-mgr-exec kobj) 
  (let* ((smgr (kobj-gob-data kobj))
         (state (scene-mgr-state smgr)))
    (cond ((= 0 state) (scene-mgr-intro-travelers-phase kobj))
          ((= 1 state) (scene-mgr-intro-demons-phase kobj))
          ((= 2 state) (scene-mgr-wait-for-no-demons-phase kobj)) 
          ((= 3 state) (scene-mgr-pause kobj 10))
          ((= 4 state) (scene-mgr-intro-wise kobj))
          ((= 5 state) (scene-mgr-wait-for-wise kobj))
          ((= 6 state) (scene-mgr-pause kobj 10))
          ((= 7 state) (scene-mgr-close-gate kobj))
          ((= 8 state) (scene-mgr-pause kobj 10))
          ((= 9 state) (scene-mgr-pickup-runes kobj))
          ((= 10 state) (scene-mgr-pause kobj 10))
          ((= 11 state) (scene-mgr-exit-wise kobj))
          ((= 12 state) (scene-mgr-wait-for-wise kobj))
          ((= 13 state) (scene-mgr-exit-guards kobj))
          ((= 14 state) (scene-mgr-wait-for-exits kobj))
          ((= 15 state) (scene-mgr-exit-beggar kobj))
          ((= 16 state) (scene-mgr-wait-for-exits kobj))
          ((= 17 state) (scene-mgr-pause kobj 10))
          ((= 18 state) (scene-mgr-start-days-pass kobj))
          ((= 19 state) (scene-mgr-pause kobj 10))
          ((= 20 state) (scene-mgr-end-days-pass kobj))
          ((= 21 state) (scene-mgr-years-pass kobj))
          ((= 22 state) (scene-mgr-pause kobj 20))
          ((= 23 state) (scene-mgr-end-years-pass kobj))
          ((= 24 state) (scene-mgr-ages-pass kobj))
          ((= 25 state) (scene-mgr-pause kobj 20))
          ((= 26 state) (scene-mgr-end-years-pass kobj))
          ((= 27 state) (scene-mgr-conclude kobj))
          ((= 28 state) (scene-mgr-pause kobj 20))
          (else
           (kern-end-game)
           )
          (else
           ))
    ))

(define scene-mgr-ifc
  (ifc nil
       (method 'exec scene-mgr-exec)))

(mk-obj-type 't_scene_mgr nil nil layer-none scene-mgr-ifc)

(define (mk-scene-mgr)
  (bind (kern-obj-set-visible (kern-mk-obj t_scene_mgr 1) #f)
        (scene-mgr-mk)
        ))

;;----------------------------------------------------------------------------
;; Haxima Logo & Frame Terrain
;;----------------------------------------------------------------------------
(kern-mk-sprite-set 'ss_hl 32 32 6 19 0 0  "haximatext.png")
(kern-mk-sprite-set 'ss_gf 32 32 3  3 0 0  "gold_frame.png")

(kern-mk-sprite 's_hl_0 ss_hl 1 0 #f 0)
(kern-mk-sprite 's_hl_1 ss_hl 1 1 #f 0)
(kern-mk-sprite 's_hl_2 ss_hl 1 2 #f 0)
(kern-mk-sprite 's_hl_3 ss_hl 1 3 #f 0)
(kern-mk-sprite 's_hl_4 ss_hl 1 4 #f 0)
(kern-mk-sprite 's_hl_5 ss_hl 1 5 #f 0)
(kern-mk-sprite 's_hl_6 ss_hl 1 6 #f 0)
(kern-mk-sprite 's_hl_7 ss_hl 1 7 #f 0)
(kern-mk-sprite 's_hl_8 ss_hl 1 8 #f 0)
(kern-mk-sprite 's_hl_9 ss_hl 1 9 #f 0)
(kern-mk-sprite 's_hl_10 ss_hl 1 10 #f 0)
(kern-mk-sprite 's_hl_11 ss_hl 1 11 #f 0)
(kern-mk-sprite 's_hl_12 ss_hl 1 12 #f 0)
(kern-mk-sprite 's_hl_13 ss_hl 1 13 #f 0)
(kern-mk-sprite 's_hl_14 ss_hl 1 14 #f 0)
(kern-mk-sprite 's_hl_15 ss_hl 1 15 #f 0)
(kern-mk-sprite 's_hl_16 ss_hl 1 16 #f 0)
(kern-mk-sprite 's_hl_17 ss_hl 1 17 #f 0)
(kern-mk-sprite 's_hl_18 ss_hl 1 18 #f 0)
(kern-mk-sprite 's_hl_19 ss_hl 1 19 #f 0)
(kern-mk-sprite 's_hl_20 ss_hl 1 20 #f 0)
(kern-mk-sprite 's_hl_21 ss_hl 1 21 #f 0)
(kern-mk-sprite 's_hl_22 ss_hl 1 22 #f 0)
(kern-mk-sprite 's_hl_23 ss_hl 1 23 #f 0)
(kern-mk-sprite 's_hl_24 ss_hl 1 24 #f 0)
(kern-mk-sprite 's_hl_25 ss_hl 1 25 #f 0)
(kern-mk-sprite 's_hl_26 ss_hl 1 26 #f 0)
(kern-mk-sprite 's_hl_27 ss_hl 1 27 #f 0)
(kern-mk-sprite 's_hl_28 ss_hl 1 28 #f 0)
(kern-mk-sprite 's_hl_29 ss_hl 1 29 #f 0)
(kern-mk-sprite 's_hl_30 ss_hl 1 30 #f 0)
(kern-mk-sprite 's_hl_31 ss_hl 1 31 #f 0)
(kern-mk-sprite 's_hl_32 ss_hl 1 32 #f 0)
(kern-mk-sprite 's_hl_33 ss_hl 1 33 #f 0)
(kern-mk-sprite 's_hl_34 ss_hl 1 34 #f 0)
(kern-mk-sprite 's_hl_35 ss_hl 1 35 #f 0)
(kern-mk-sprite 's_hl_36 ss_hl 1 36 #f 0)
(kern-mk-sprite 's_hl_37 ss_hl 1 37 #f 0)
(kern-mk-sprite 's_hl_38 ss_hl 1 38 #f 0)
(kern-mk-sprite 's_hl_39 ss_hl 1 39 #f 0)
(kern-mk-sprite 's_hl_40 ss_hl 1 40 #f 0)
(kern-mk-sprite 's_hl_41 ss_hl 1 41 #f 0)
(kern-mk-sprite 's_hl_42 ss_hl 1 42 #f 0)
(kern-mk-sprite 's_hl_43 ss_hl 1 43 #f 0)
(kern-mk-sprite 's_hl_44 ss_hl 1 44 #f 0)
(kern-mk-sprite 's_hl_45 ss_hl 1 45 #f 0)
(kern-mk-sprite 's_hl_46 ss_hl 1 46 #f 0)
(kern-mk-sprite 's_hl_47 ss_hl 1 47 #f 0)
(kern-mk-sprite 's_hl_48 ss_hl 1 48 #f 0)
(kern-mk-sprite 's_hl_49 ss_hl 1 49 #f 0)
(kern-mk-sprite 's_hl_50 ss_hl 1 50 #f 0)
(kern-mk-sprite 's_hl_51 ss_hl 1 51 #f 0)
(kern-mk-sprite 's_hl_52 ss_hl 1 52 #f 0)
(kern-mk-sprite 's_hl_53 ss_hl 1 53 #f 0)
(kern-mk-sprite 's_hl_54 ss_hl 1 54 #f 0)
(kern-mk-sprite 's_hl_55 ss_hl 1 55 #f 0)
(kern-mk-sprite 's_hl_56 ss_hl 1 56 #f 0)
(kern-mk-sprite 's_hl_57 ss_hl 1 57 #f 0)
(kern-mk-sprite 's_hl_58 ss_hl 1 58 #f 0)
(kern-mk-sprite 's_hl_59 ss_hl 1 59 #f 0)
(kern-mk-sprite 's_hl_60 ss_hl 1 60 #f 0)
(kern-mk-sprite 's_hl_61 ss_hl 1 61 #f 0)
(kern-mk-sprite 's_hl_62 ss_hl 1 62 #f 0)
(kern-mk-sprite 's_hl_63 ss_hl 1 63 #f 0)
(kern-mk-sprite 's_hl_64 ss_hl 1 64 #f 0)
(kern-mk-sprite 's_hl_65 ss_hl 1 65 #f 0)
(kern-mk-sprite 's_hl_66 ss_hl 1 66 #f 0)
(kern-mk-sprite 's_hl_67 ss_hl 1 67 #f 0)
(kern-mk-sprite 's_hl_68 ss_hl 1 68 #f 0)
(kern-mk-sprite 's_hl_69 ss_hl 1 69 #f 0)
(kern-mk-sprite 's_hl_70 ss_hl 1 70 #f 0)
(kern-mk-sprite 's_hl_71 ss_hl 1 71 #f 0)
(kern-mk-sprite 's_hl_72 ss_hl 1 72 #f 0)
(kern-mk-sprite 's_hl_73 ss_hl 1 73 #f 0)
(kern-mk-sprite 's_hl_74 ss_hl 1 74 #f 0)
(kern-mk-sprite 's_hl_75 ss_hl 1 75 #f 0)
(kern-mk-sprite 's_hl_76 ss_hl 1 76 #f 0)
(kern-mk-sprite 's_hl_77 ss_hl 1 77 #f 0)
(kern-mk-sprite 's_hl_78 ss_hl 1 78 #f 0)
(kern-mk-sprite 's_hl_79 ss_hl 1 79 #f 0)
(kern-mk-sprite 's_hl_80 ss_hl 1 80 #f 0)
(kern-mk-sprite 's_hl_81 ss_hl 1 81 #f 0)
(kern-mk-sprite 's_hl_82 ss_hl 1 82 #f 0)
(kern-mk-sprite 's_hl_83 ss_hl 1 83 #f 0)
(kern-mk-sprite 's_hl_84 ss_hl 1 84 #f 0)
(kern-mk-sprite 's_hl_85 ss_hl 1 85 #f 0)
(kern-mk-sprite 's_hl_86 ss_hl 1 86 #f 0)
(kern-mk-sprite 's_hl_87 ss_hl 1 87 #f 0)
(kern-mk-sprite 's_hl_88 ss_hl 1 88 #f 0)
(kern-mk-sprite 's_hl_89 ss_hl 1 89 #f 0)
(kern-mk-sprite 's_hl_90 ss_hl 1 90 #f 0)
(kern-mk-sprite 's_hl_91 ss_hl 1 91 #f 0)
(kern-mk-sprite 's_hl_92 ss_hl 1 92 #f 0)
(kern-mk-sprite 's_hl_93 ss_hl 1 93 #f 0)
(kern-mk-sprite 's_hl_94 ss_hl 1 94 #f 0)
(kern-mk-sprite 's_hl_95 ss_hl 1 95 #f 0)
(kern-mk-sprite 's_hl_96 ss_hl 1 96 #f 0)
(kern-mk-sprite 's_hl_97 ss_hl 1 97 #f 0)
(kern-mk-sprite 's_hl_98 ss_hl 1 98 #f 0)
(kern-mk-sprite 's_hl_99 ss_hl 1 99 #f 0)
(kern-mk-sprite 's_hl_100 ss_hl 1 100 #f 0)
(kern-mk-sprite 's_hl_101 ss_hl 1 101 #f 0)
(kern-mk-sprite 's_hl_102 ss_hl 1 102 #f 0)
(kern-mk-sprite 's_hl_103 ss_hl 1 103 #f 0)
(kern-mk-sprite 's_hl_104 ss_hl 1 104 #f 0)
(kern-mk-sprite 's_hl_105 ss_hl 1 105 #f 0)
(kern-mk-sprite 's_hl_106 ss_hl 1 106 #f 0)
(kern-mk-sprite 's_hl_107 ss_hl 1 107 #f 0)
(kern-mk-sprite 's_hl_108 ss_hl 1 108 #f 0)
(kern-mk-sprite 's_hl_109 ss_hl 1 109 #f 0)
(kern-mk-sprite 's_hl_110 ss_hl 1 110 #f 0)
(kern-mk-sprite 's_hl_111 ss_hl 1 111 #f 0)
(kern-mk-sprite 's_hl_112 ss_hl 1 112 #f 0)
(kern-mk-sprite 's_hl_113 ss_hl 1 113 #f 0)

(kern-mk-sprite 's_gf_nw ss_gf 1 0 #f 0)
(kern-mk-sprite 's_gf_n  ss_gf 1 1 #f 0)
(kern-mk-sprite 's_gf_ne ss_gf 1 2 #f 0)
(kern-mk-sprite 's_gf_w  ss_gf 1 3 #f 0)
(kern-mk-sprite 's_gf_c  ss_gf 1 4 #f 0)
(kern-mk-sprite 's_gf_e  ss_gf 1 5 #f 0)
(kern-mk-sprite 's_gf_sw ss_gf 1 6 #f 0)
(kern-mk-sprite 's_gf_s  ss_gf 1 7 #f 0)
(kern-mk-sprite 's_gf_se ss_gf 1 8 #f 0)

(kern-mk-terrain 't_hl_0 "logo" pclass-wall s_hl_0 trn 0 nil)
(kern-mk-terrain 't_hl_1 "logo" pclass-wall s_hl_1 trn 0 nil)
(kern-mk-terrain 't_hl_2 "logo" pclass-wall s_hl_2 trn 0 nil)
(kern-mk-terrain 't_hl_3 "logo" pclass-wall s_hl_3 trn 0 nil)
(kern-mk-terrain 't_hl_4 "logo" pclass-wall s_hl_4 trn 0 nil)
(kern-mk-terrain 't_hl_5 "logo" pclass-wall s_hl_5 trn 0 nil)
(kern-mk-terrain 't_hl_6 "logo" pclass-wall s_hl_6 trn 0 nil)
(kern-mk-terrain 't_hl_7 "logo" pclass-wall s_hl_7 trn 0 nil)
(kern-mk-terrain 't_hl_8 "logo" pclass-wall s_hl_8 trn 0 nil)
(kern-mk-terrain 't_hl_9 "logo" pclass-wall s_hl_9 trn 0 nil)
(kern-mk-terrain 't_hl_10 "logo" pclass-wall s_hl_10 trn 0 nil)
(kern-mk-terrain 't_hl_11 "logo" pclass-wall s_hl_11 trn 0 nil)
(kern-mk-terrain 't_hl_12 "logo" pclass-wall s_hl_12 trn 0 nil)
(kern-mk-terrain 't_hl_13 "logo" pclass-wall s_hl_13 trn 0 nil)
(kern-mk-terrain 't_hl_14 "logo" pclass-wall s_hl_14 trn 0 nil)
(kern-mk-terrain 't_hl_15 "logo" pclass-wall s_hl_15 trn 0 nil)
(kern-mk-terrain 't_hl_16 "logo" pclass-wall s_hl_16 trn 0 nil)
(kern-mk-terrain 't_hl_17 "logo" pclass-wall s_hl_17 trn 0 nil)
(kern-mk-terrain 't_hl_18 "logo" pclass-wall s_hl_18 trn 0 nil)
(kern-mk-terrain 't_hl_19 "logo" pclass-wall s_hl_19 trn 0 nil)
(kern-mk-terrain 't_hl_20 "logo" pclass-wall s_hl_20 trn 0 nil)
(kern-mk-terrain 't_hl_21 "logo" pclass-wall s_hl_21 trn 0 nil)
(kern-mk-terrain 't_hl_22 "logo" pclass-wall s_hl_22 trn 0 nil)
(kern-mk-terrain 't_hl_23 "logo" pclass-wall s_hl_23 trn 0 nil)
(kern-mk-terrain 't_hl_24 "logo" pclass-wall s_hl_24 trn 0 nil)
(kern-mk-terrain 't_hl_25 "logo" pclass-wall s_hl_25 trn 0 nil)
(kern-mk-terrain 't_hl_26 "logo" pclass-wall s_hl_26 trn 0 nil)
(kern-mk-terrain 't_hl_27 "logo" pclass-wall s_hl_27 trn 0 nil)
(kern-mk-terrain 't_hl_28 "logo" pclass-wall s_hl_28 trn 0 nil)
(kern-mk-terrain 't_hl_29 "logo" pclass-wall s_hl_29 trn 0 nil)
(kern-mk-terrain 't_hl_30 "logo" pclass-wall s_hl_30 trn 0 nil)
(kern-mk-terrain 't_hl_31 "logo" pclass-wall s_hl_31 trn 0 nil)
(kern-mk-terrain 't_hl_32 "logo" pclass-wall s_hl_32 trn 0 nil)
(kern-mk-terrain 't_hl_33 "logo" pclass-wall s_hl_33 trn 0 nil)
(kern-mk-terrain 't_hl_34 "logo" pclass-wall s_hl_34 trn 0 nil)
(kern-mk-terrain 't_hl_35 "logo" pclass-wall s_hl_35 trn 0 nil)
(kern-mk-terrain 't_hl_36 "logo" pclass-wall s_hl_36 trn 0 nil)
(kern-mk-terrain 't_hl_37 "logo" pclass-wall s_hl_37 trn 0 nil)
(kern-mk-terrain 't_hl_38 "logo" pclass-wall s_hl_38 trn 0 nil)
(kern-mk-terrain 't_hl_39 "logo" pclass-wall s_hl_39 trn 0 nil)
(kern-mk-terrain 't_hl_40 "logo" pclass-wall s_hl_40 trn 0 nil)
(kern-mk-terrain 't_hl_41 "logo" pclass-wall s_hl_41 trn 0 nil)
(kern-mk-terrain 't_hl_42 "logo" pclass-wall s_hl_42 trn 0 nil)
(kern-mk-terrain 't_hl_43 "logo" pclass-wall s_hl_43 trn 0 nil)
(kern-mk-terrain 't_hl_44 "logo" pclass-wall s_hl_44 trn 0 nil)
(kern-mk-terrain 't_hl_45 "logo" pclass-wall s_hl_45 trn 0 nil)
(kern-mk-terrain 't_hl_46 "logo" pclass-wall s_hl_46 trn 0 nil)
(kern-mk-terrain 't_hl_47 "logo" pclass-wall s_hl_47 trn 0 nil)
(kern-mk-terrain 't_hl_48 "logo" pclass-wall s_hl_48 trn 0 nil)
(kern-mk-terrain 't_hl_49 "logo" pclass-wall s_hl_49 trn 0 nil)
(kern-mk-terrain 't_hl_50 "logo" pclass-wall s_hl_50 trn 0 nil)
(kern-mk-terrain 't_hl_51 "logo" pclass-wall s_hl_51 trn 0 nil)
(kern-mk-terrain 't_hl_52 "logo" pclass-wall s_hl_52 trn 0 nil)
(kern-mk-terrain 't_hl_53 "logo" pclass-wall s_hl_53 trn 0 nil)
(kern-mk-terrain 't_hl_54 "logo" pclass-wall s_hl_54 trn 0 nil)
(kern-mk-terrain 't_hl_55 "logo" pclass-wall s_hl_55 trn 0 nil)
(kern-mk-terrain 't_hl_56 "logo" pclass-wall s_hl_56 trn 0 nil)
(kern-mk-terrain 't_hl_57 "logo" pclass-wall s_hl_57 trn 0 nil)
(kern-mk-terrain 't_hl_58 "logo" pclass-wall s_hl_58 trn 0 nil)
(kern-mk-terrain 't_hl_59 "logo" pclass-wall s_hl_59 trn 0 nil)
(kern-mk-terrain 't_hl_60 "logo" pclass-wall s_hl_60 trn 0 nil)
(kern-mk-terrain 't_hl_61 "logo" pclass-wall s_hl_61 trn 0 nil)
(kern-mk-terrain 't_hl_62 "logo" pclass-wall s_hl_62 trn 0 nil)
(kern-mk-terrain 't_hl_63 "logo" pclass-wall s_hl_63 trn 0 nil)
(kern-mk-terrain 't_hl_64 "logo" pclass-wall s_hl_64 trn 0 nil)
(kern-mk-terrain 't_hl_65 "logo" pclass-wall s_hl_65 trn 0 nil)
(kern-mk-terrain 't_hl_66 "logo" pclass-wall s_hl_66 trn 0 nil)
(kern-mk-terrain 't_hl_67 "logo" pclass-wall s_hl_67 trn 0 nil)
(kern-mk-terrain 't_hl_68 "logo" pclass-wall s_hl_68 trn 0 nil)
(kern-mk-terrain 't_hl_69 "logo" pclass-wall s_hl_69 trn 0 nil)
(kern-mk-terrain 't_hl_70 "logo" pclass-wall s_hl_70 trn 0 nil)
(kern-mk-terrain 't_hl_71 "logo" pclass-wall s_hl_71 trn 0 nil)
(kern-mk-terrain 't_hl_72 "logo" pclass-wall s_hl_72 trn 0 nil)
(kern-mk-terrain 't_hl_73 "logo" pclass-wall s_hl_73 trn 0 nil)
(kern-mk-terrain 't_hl_74 "logo" pclass-wall s_hl_74 trn 0 nil)
(kern-mk-terrain 't_hl_75 "logo" pclass-wall s_hl_75 trn 0 nil)
(kern-mk-terrain 't_hl_76 "logo" pclass-wall s_hl_76 trn 0 nil)
(kern-mk-terrain 't_hl_77 "logo" pclass-wall s_hl_77 trn 0 nil)
(kern-mk-terrain 't_hl_78 "logo" pclass-wall s_hl_78 trn 0 nil)
(kern-mk-terrain 't_hl_79 "logo" pclass-wall s_hl_79 trn 0 nil)
(kern-mk-terrain 't_hl_80 "logo" pclass-wall s_hl_80 trn 0 nil)
(kern-mk-terrain 't_hl_81 "logo" pclass-wall s_hl_81 trn 0 nil)
(kern-mk-terrain 't_hl_82 "logo" pclass-wall s_hl_82 trn 0 nil)
(kern-mk-terrain 't_hl_83 "logo" pclass-wall s_hl_83 trn 0 nil)
(kern-mk-terrain 't_hl_84 "logo" pclass-wall s_hl_84 trn 0 nil)
(kern-mk-terrain 't_hl_85 "logo" pclass-wall s_hl_85 trn 0 nil)
(kern-mk-terrain 't_hl_86 "logo" pclass-wall s_hl_86 trn 0 nil)
(kern-mk-terrain 't_hl_87 "logo" pclass-wall s_hl_87 trn 0 nil)
(kern-mk-terrain 't_hl_88 "logo" pclass-wall s_hl_88 trn 0 nil)
(kern-mk-terrain 't_hl_89 "logo" pclass-wall s_hl_89 trn 0 nil)
(kern-mk-terrain 't_hl_90 "logo" pclass-wall s_hl_90 trn 0 nil)
(kern-mk-terrain 't_hl_91 "logo" pclass-wall s_hl_91 trn 0 nil)
(kern-mk-terrain 't_hl_92 "logo" pclass-wall s_hl_92 trn 0 nil)
(kern-mk-terrain 't_hl_93 "logo" pclass-wall s_hl_93 trn 0 nil)
(kern-mk-terrain 't_hl_94 "logo" pclass-wall s_hl_94 trn 0 nil)
(kern-mk-terrain 't_hl_95 "logo" pclass-wall s_hl_95 trn 0 nil)
(kern-mk-terrain 't_hl_96 "logo" pclass-wall s_hl_96 trn 0 nil)
(kern-mk-terrain 't_hl_97 "logo" pclass-wall s_hl_97 trn 0 nil)
(kern-mk-terrain 't_hl_98 "logo" pclass-wall s_hl_98 trn 0 nil)
(kern-mk-terrain 't_hl_99 "logo" pclass-wall s_hl_99 trn 0 nil)
(kern-mk-terrain 't_hl_100 "logo" pclass-wall s_hl_100 trn 0 nil)
(kern-mk-terrain 't_hl_101 "logo" pclass-wall s_hl_101 trn 0 nil)
(kern-mk-terrain 't_hl_102 "logo" pclass-wall s_hl_102 trn 0 nil)
(kern-mk-terrain 't_hl_103 "logo" pclass-wall s_hl_103 trn 0 nil)
(kern-mk-terrain 't_hl_104 "logo" pclass-wall s_hl_104 trn 0 nil)
(kern-mk-terrain 't_hl_105 "logo" pclass-wall s_hl_105 trn 0 nil)
(kern-mk-terrain 't_hl_106 "logo" pclass-wall s_hl_106 trn 0 nil)
(kern-mk-terrain 't_hl_107 "logo" pclass-wall s_hl_107 trn 0 nil)
(kern-mk-terrain 't_hl_108 "logo" pclass-wall s_hl_108 trn 0 nil)
(kern-mk-terrain 't_hl_109 "logo" pclass-wall s_hl_109 trn 0 nil)
(kern-mk-terrain 't_hl_110 "logo" pclass-wall s_hl_110 trn 0 nil)
(kern-mk-terrain 't_hl_111 "logo" pclass-wall s_hl_111 trn 0 nil)
(kern-mk-terrain 't_hl_112 "logo" pclass-wall s_hl_112 trn 0 nil)
(kern-mk-terrain 't_hl_113 "logo" pclass-wall s_hl_113 trn 0 nil)

(kern-mk-terrain 't_gf_nw "frame" pclass-wall s_gf_nw trn 0 nil)
(kern-mk-terrain 't_gf_n  "frame" pclass-wall s_gf_n  trn 0 nil)
(kern-mk-terrain 't_gf_ne "frame" pclass-wall s_gf_ne trn 0 nil)
(kern-mk-terrain 't_gf_w  "frame" pclass-wall s_gf_w  trn 0 nil)
(kern-mk-terrain 't_gf_c  "frame" pclass-wall s_gf_c  trn 0 nil)
(kern-mk-terrain 't_gf_e  "frame" pclass-wall s_gf_e  trn 0 nil)
(kern-mk-terrain 't_gf_sw "frame" pclass-wall s_gf_sw trn 0 nil)
(kern-mk-terrain 't_gf_s  "frame" pclass-wall s_gf_s  trn 0 nil)
(kern-mk-terrain 't_gf_se "frame" pclass-wall s_gf_se trn 0 nil)

(kern-mk-terrain 't_overgrown_altar "altar" pclass-boulder s_overgrown_altar trn 0 nil)

;; define our own palette to include the logo terrain
(kern-mk-palette 'pal_expanded
  (list
    ;; NOTE: "x#" is reserved for blocking mechanisms, see block-teleporting in
    ;; naz.scm
    (list  "xx"   t_wall)               ;; "wall"
    (list  "__"   t_deep)               ;; "deep water"
    (list  "_!"   t_sunlit_deep)               ;; "deep water"
    (list  "~*"   t_blendable_shoals)            ;; "shallow water"
    (list  "_s"   t_sludge)
    (list  "~s"   t_shallow_sludge)
    (list  "dd"   t_dirt)
    (list  "gg"   t_gravel)
	
    (list  "%%"   t_bog)                ;; "bog"
    (list  ".."   t_grass)              ;; "grass"
    (list  ".!"   t_sunlit_grass)              ;; "grass"
    (list  "t."   t_trees_v)            ;; "trees (transparent)"
    (list  "tt"   t_trees)              ;; "trees"
    (list  "t|"   t_trees_d)            ;; "trees denser"

    (list  "||"   t_forest)             ;; "forest"
    (list  "|X"   t_forest_d)           ;; "forest (denser)"
    (list  "|t"   t_forest_l)           ;; "forest (lighter)"
    (list  "|."   t_forest_v)           ;; "forest (non-LOS-blocking)"
    (list  "|v"   t_forest_b)           ;; "forest (totally LOS-blocking)"

    (list  "{{"   t_hills)              ;; "hills"

    (list  "^^"   t_mountains)          ;; "mountains"
    (list  "^."   t_mountains_v)        ;; "mountains" (non-LOS-blocking)
    (list  "^v"   t_mountains_b)        ;; "mountains" (below player)
    (list  "^~"   t_fake_mountains)

    (list  ",,"   t_flagstones)         ;; "flagstones"
    (list  "~,"   t_inv_wall)
    (list  "d,"   t_doorway)
    (list  "cc"   t_cobblestone)        ;; "cobblestone"
    (list  "ee"   t_deck)               ;; "deck"
    (list  "oo"   t_mast)               ;; "mast"
    (list  "ff"   t_fire_terrain)       ;; "fire"
    (list  "!!"   t_lava)               ;; "lava"
    (list  "~!"   t_fake_lava)
    (list  "!_"   t_deep_lava)
    (list  "&&"   t_fireplace)          ;; "fireplace"

    (list  "x."   t_wall_v)             ;; "wall"  (non-LOS-blocking)
    (list  "~x"   t_fake_wall)

    (list  "**"   t_stars)              ;; "stars"
	(list  "*."   t_void)
    (list  "??"   t_secret_door)        ;; "secret door"
    (list  "pp"   t_pillar)             ;; "pillar"
    (list  "~p"   t_false_pillar)
    (list  "bb"   t_boulder)            ;; "boulder"
    (list  "b~"   t_water_rocks)        ;; "boulder" in water
	
    (list  "rr"   t_wall_rock)          ;; "rock wall"
    (list  "r."   t_wall_rock_v)        ;; "rock wall"  (non-LOS-blocking)
    (list  "~r"   t_fake_wall_rock)     ;; "rock wall"  (fake)

    (list  "WW"   t_ships_wheel)        ;; "ship's wheel"
    (list  "x!"   t_wall_torch)         ;; "wall torch"
    (list  "##"   t_ship_hull)          ;; "ship's hull"
    (list  "#>"   t_ship_hull2)          ;; "ship's hull (LOS-blocking)"

    (list  ".A"   t_a)                  ;; "an A"
    (list  ".B"   t_b)                  ;; "a B"
    (list  "?B"   t_fake_b)                  ;; "a B"
    (list  ".C"   t_c)                  ;; "a C"
    (list  ".D"   t_d)                  ;; "a D"
    (list  ".E"   t_e)                  ;; "an E"
    (list  ".F"   t_f)                  ;; "an F"
    (list  ".G"   t_g)                  ;; "a G"
    (list  ".H"   t_h)                  ;; "an H"
    (list  ".I"   t_i)                  ;; "an I"
    (list  ".J"   t_j)                  ;; "a J"
    (list  ".K"   t_k)                  ;; "a K"
    (list  ".L"   t_l)                  ;; "an L"
    (list  ".M"   t_m)                  ;; "an M"
    (list  ".N"   t_n)                  ;; "an N"
    (list  ".O"   t_o)                  ;; "an O"
    (list  "~O"   t_fake_o)
    (list  ".P"   t_p)                  ;; "a P"
    (list  ".Q"   t_q)                  ;; "a Q"
    (list  ".R"   t_r)                  ;; "an R"
    (list  ".S"   t_s)                  ;; "an S"
    (list  ".T"   t_t)                  ;; "a T"
    (list  ".U"   t_u)                  ;; "a U"
    (list  ".V"   t_v)                  ;; "a V"
    (list  ".W"   t_w)                  ;; "a W"
    (list  ".X"   t_x)                  ;; "an X"
    (list  ".Y"   t_y)                  ;; "a Y"
    (list  ".Z"   t_z)                  ;; "a Z"

    (list  ",A"   t_rune_a)             ;; "a rune"
    (list  ",B"   t_rune_b)             ;; "a rune"
    (list  ",C"   t_rune_c)             ;; "a rune"
    (list  ",D"   t_rune_d)             ;; "a rune"
    (list  ",E"   t_rune_e)             ;; "a rune"
    (list  ",F"   t_rune_f)             ;; "a rune"
    (list  ",G"   t_rune_g)             ;; "a rune"
    (list  ",H"   t_rune_h)             ;; "a rune"
    (list  ",I"   t_rune_i)             ;; "a rune"
    (list  ",J"   t_rune_j)             ;; "a rune"
    (list  ",K"   t_rune_k)             ;; "a rune"
    (list  ",L"   t_rune_l)             ;; "a rune"
    (list  ",M"   t_rune_m)             ;; "a rune"
    (list  ",N"   t_rune_n)             ;; "a rune"
    (list  ",O"   t_rune_o)             ;; "a rune"
    (list  ",P"   t_rune_p)             ;; "a rune"
    (list  ",Q"   t_rune_q)             ;; "a rune"
    (list  ",R"   t_rune_r)             ;; "a rune"
    (list  ",S"   t_rune_s)             ;; "a rune"
    (list  ",T"   t_rune_t)             ;; "a rune"
    (list  ",U"   t_rune_u)             ;; "a rune"
    (list  ",V"   t_rune_v)             ;; "a rune"
    (list  ",W"   t_rune_w)             ;; "a rune"
    (list  ",X"   t_rune_x)             ;; "a rune"
    (list  ",Y"   t_rune_y)             ;; "a rune"
    (list  ",Z"   t_rune_z)             ;; "a rune"
    (list  ";T"   t_rune_th)            ;; "a rune"
    (list  ";E"   t_rune_ee)            ;; "a rune"
    (list  ";N"   t_rune_ng)            ;; "a rune"
    (list  ";A"   t_rune_ea)            ;; "a rune"
    (list  ";S"   t_rune_st)            ;; "a rune"
    (list  ";D"   t_rune_dot)           ;; "a rune"

    (list  "@@"   t_counter_2x1_c)      ;; "counter"
    (list  "[["   t_counter_2x1_w)      ;; "counter"
    (list  "]]"   t_counter_2x1_e)      ;; "counter"

    (list  "++"   t_ankh)               ;; "ankh"
    (list  "+s"   t_statue)               ;; "ankh"
    (list  "aa"   t_altar)              ;; "altar"
    (list  "ar"   t_rune_altar)              ;; "altar"
    (list  "a!"   t_active_altar)              ;; "altar"
    (list  "at"   t_overgrown_altar)              ;; "altar"
    (list  "<<"   t_leftwing)           ;; "castle wall"
    (list  ">>"   t_rightwing)          ;; "castle wall"
    (list  "w+"   t_arrow_slit)         ;; "arrow slit"
    (list  "ws"   t_window_in_stone)    ;; "window"
    (list  "wr"   t_window_in_rock)     ;; "window"
    
    (list  "=="   t_bridge_WE)          ;; "east-west bridge"
    (list  "=|"   t_bridge_NS)          ;; "east-west bridge"
    (list  "=!"   t_lava_bridge_NS)
    (list  "vv"   t_chasm)              ;; "chasm"

    (list "sE" t_equip_sign)
    (list "sA" t_weapon_sign)
    (list "sH" t_healer_sign)
    (list "sT" t_tavern_sign)
    (list "sI" t_inn_sign)
    (list "sP" t_alchemy_sign)
    (list "sR" t_magic_sign)
    (list "sS" t_str_sign)
    (list "sD" t_dex_sign)
    (list "sW" t_wis_sign)
	
	;; blended terrains (mostly terrain + corner of something else)
	
	(list  "/0"   t_trail_0)            ;; "trail"
    (list  "/1"   t_trail_1)            ;; "trail"
    (list  "/2"   t_trail_2)            ;; "trail"
    (list  "/3"   t_trail_3)            ;; "trail"
    (list  "/4"   t_trail_4)            ;; "trail"
    (list  "/5"   t_trail_5)            ;; "trail"
    (list  "/6"   t_trail_6)            ;; "trail"
    (list  "/7"   t_trail_7)            ;; "trail"
    (list  "/8"   t_trail_8)            ;; "trail"
    (list  "/9"   t_trail_9)            ;; "trail"
    (list  "/a"   t_trail_a)            ;; "trail"
    (list  "/b"   t_trail_b)            ;; "trail"
    (list  "/c"   t_trail_c)            ;; "trail"
    (list  "/d"   t_trail_d)            ;; "trail"
    (list  "/e"   t_trail_e)            ;; "trail"
    (list  "/f"   t_trail_f)            ;; "trail"
	
	(list  "~~" t_shoals)     ;; shallow + land
    (list  "~1" t_shore_n)
    (list  "~2" t_shore_w)
    (list  "~3" t_shore_nw)
    (list  "~4" t_shore_e)
    (list  "~5" t_shore_ne)
    (list  "~6" t_shore_we)
    (list  "~7" t_shore_nwe)
    (list  "~8" t_shore_s)
    (list  "~9" t_shore_ns)
    (list  "~a" t_shore_ws)
    (list  "~b" t_shore_nws)
    (list  "~c" t_shore_es)
    (list  "~d" t_shore_nes)
    (list  "~e" t_shore_wes)
    (list  "~f" t_shore_c)
	
    (list  "--" t_shallow)            ;; water + land
    (list  "-1" t_wshore_n)
    (list  "-2" t_wshore_w)
    (list  "-3" t_wshore_nw)
    (list  "-4" t_wshore_e)
    (list  "-5" t_wshore_ne)
    (list  "-6" t_wshore_we)
    (list  "-7" t_wshore_nwe)
    (list  "-8" t_wshore_s)
    (list  "-9" t_wshore_ns)
    (list  "-a" t_wshore_ws)
    (list  "-b" t_wshore_nws)
    (list  "-c" t_wshore_es)
    (list  "-d" t_wshore_nes)
    (list  "-e" t_wshore_wes)
    (list  "-f" t_wshore_c)
	
	(list  "_1" t_dshore_n)        ;; deep water + land
    (list  "_2" t_dshore_w)
    (list  "_3" t_dshore_nw)
    (list  "_4" t_dshore_e)
    (list  "_5" t_dshore_ne)
    (list  "_6" t_dshore_we)
    (list  "_7" t_dshore_nwe)
    (list  "_8" t_dshore_s)
    (list  "_9" t_dshore_ns)
    (list  "_a" t_dshore_ws)
    (list  "_b" t_dshore_nws)
    (list  "_c" t_dshore_es)
    (list  "_d" t_dshore_nes)
    (list  "_e" t_dshore_wes)
    (list  "_f" t_dshore_c)
	
	(list  "*1" t_voids_n)             ;; void + land
    (list  "*2" t_voids_w)
    (list  "*3" t_voids_nw)
    (list  "*4" t_voids_e)
    (list  "*5" t_voids_ne)
    (list  "*6" t_voids_we)
    (list  "*7" t_voids_nwe)
    (list  "*8" t_voids_s)
    (list  "*9" t_voids_ns)
    (list  "*a" t_voids_ws)
    (list  "*b" t_voids_nws)
    (list  "*c" t_voids_es)
    (list  "*d" t_voids_nes)
    (list  "*e" t_voids_wes)
    (list  "*f" t_voids_c)
	
	(list  "{1" t_hilledge_n)          ;; grass + hills
    (list  "{2" t_hilledge_w)
    (list  "{3" t_hilledge_nw)
    (list  "{4" t_hilledge_e)
    (list  "{5" t_hilledge_ne)
    (list  "{6" t_hilledge_we)
    (list  "{7" t_hilledge_nwe)
    (list  "{8" t_hilledge_s)
    (list  "{9" t_hilledge_ns)
    (list  "{a" t_hilledge_ws)
    (list  "{b" t_hilledge_nws)
    (list  "{c" t_hilledge_es)
    (list  "{d" t_hilledge_nes)
    (list  "{e" t_hilledge_wes)
    (list  "{f" t_hilledge_c)
	
    (list  "%3" t_bog_nw)              ;; bog + land
    (list  "%5" t_bog_ne)
    (list  "%7" t_bog_nwe)
    (list  "%a" t_bog_ws)
    (list  "%b" t_bog_nws)
    (list  "%c" t_bog_es)
    (list  "%d" t_bog_nes)
    (list  "%e" t_bog_wes)
    (list  "%f" t_bog_c)

    (list  "t3" t_trees_nw)               ;; trees + grass
    (list  "t5" t_trees_ne)
    (list  "t7" t_trees_nwe)
    (list  "ta" t_trees_ws)
    (list  "tb" t_trees_nws)
    (list  "tc" t_trees_es)
    (list  "td" t_trees_nes)
    (list  "te" t_trees_wes)
    (list  "tf" t_trees_c)

	(list  "t#" t_grasst_nw)             ;; grass + trees
    (list  "t%" t_grasst_ne)
    (list  "t&" t_grasst_nwe)
    (list  "tA" t_grasst_ws)
    (list  "tB" t_grasst_nws)
    (list  "tC" t_grasst_es)
    (list  "tD" t_grasst_nes)
    (list  "tE" t_grasst_wes)
    (list  "tF" t_grasst_c)
	
	(list  "~#" t_grassw_nw)           ;; grass + water
    (list  "~%" t_grassw_ne)
    (list  "~&" t_grassw_nwe)
    (list  "~A" t_grassw_ws)
    (list  "~B" t_grassw_nws)
    (list  "~C" t_grassw_es)
    (list  "~D" t_grassw_nes)
    (list  "~E" t_grassw_wes)
    (list  "~F" t_grassw_c)
	
	(list  "{#" t_hilli_nw)             ;; hills + grass
    (list  "{%" t_hilli_ne)
    (list  "{&" t_hilli_nwe)
    (list  "{A" t_hilli_ws)
    (list  "{B" t_hilli_nws)
    (list  "{C" t_hilli_es)
    (list  "{D" t_hilli_nes)
    (list  "{E" t_hilli_wes)
    (list  "{F" t_hilli_c)
	
	(list  "|#" t_forestg_nw)          ;; forest + grass
    (list  "|%" t_forestg_ne)
    (list  "|&" t_forestg_nwe)
    (list  "|A" t_forestg_ws)
    (list  "|B" t_forestg_nws)
    (list  "|C" t_forestg_es)
    (list  "|D" t_forestg_nes)
    (list  "|E" t_forestg_wes)

	(list  "tG" t_treew_nw)            ;; trees + water
    (list  "tH" t_treew_ne)
    (list  "tI" t_treew_nwe)
    (list  "tJ" t_treew_ws)
    (list  "tK" t_treew_nws)
    (list  "tL" t_treew_es)
    (list  "tM" t_treew_nes)
    (list  "tN" t_treew_wes)
    (list  "tO" t_treew_c)
	
	(list  "{G" t_hillw_nw)            ;; hills + water
    (list  "{H" t_hillw_ne)
    (list  "{I" t_hillw_nwe)
    (list  "{J" t_hillw_ws)
    (list  "{K" t_hillw_nws)
    (list  "{L" t_hillw_es)
    (list  "{M" t_hillw_nes)
    (list  "{N" t_hillw_wes)
    (list  "{O" t_hillw_c)	
	
	(list  "{g" t_hillv_nw)           ;; hills + void
    (list  "{h" t_hillv_ne)
    (list  "{i" t_hillv_nwe)
    (list  "{j" t_hillv_ws)
    (list  "{k" t_hillv_nws)
    (list  "{l" t_hillv_es)
    (list  "{m" t_hillv_nes)
    (list  "{n" t_hillv_wes)	
	
	(list  ".g" t_grassv_nw)          ;; grass + void
    (list  ".h" t_grassv_ne)
    (list  ".i" t_grassv_nwe)
    (list  ".j" t_grassv_ws)
    (list  ".k" t_grassv_nws)
    (list  ".l" t_grassv_es)
    (list  ".m" t_grassv_nes)
    (list  ".n" t_grassv_wes)
	
	(list  "^g" t_mountv_nw)        ;; mounts + void
    (list  "^h" t_mountv_ne)
    (list  "^i" t_mountv_nwe)
    (list  "^j" t_mountv_ws)
    (list  "^k" t_mountv_nws)
    (list  "^l" t_mountv_es)
    (list  "^m" t_mountv_nes)
    (list  "^n" t_mountv_wes)	
	
	(list  "^3" t_mountg_nw)     ;; mounts + grass
    (list  "^5" t_mountg_ne)
    (list  "^7" t_mountg_nwe)
    (list  "^a" t_mountg_ws)
    (list  "^b" t_mountg_nws)
    (list  "^c" t_mountg_es)
    (list  "^d" t_mountg_nes)
    (list  "^e" t_mountg_wes)
    (list  "^f" t_mountg_c)	
	
	(list  "^G" t_mountw_nw)        ;; mounts + water
    (list  "^H" t_mountw_ne) 
    (list  "^I" t_mountw_nwe)
    (list  "^J" t_mountw_ws)
    (list  "^K" t_mountw_nws)
    (list  "^L" t_mountw_es)
    (list  "^M" t_mountw_nes)
    (list  "^N" t_mountw_wes)
    (list  "^O" t_mountw_c)	
	
    (list  "!3" t_lava_nw)        ;; lava + land
    (list  "!5" t_lava_ne)
    (list  "!6" t_lava_we)
    (list  "!7" t_lava_nwe)
    (list  "!a" t_lava_ws)
    (list  "!b" t_lava_nws)
    (list  "!c" t_lava_es)
    (list  "!d" t_lava_nes)
    (list  "!e" t_lava_wes)
    (list  "!f" t_lava_c)
    
    (list	"#=" t_rail_ew)
    (list	"#|" t_rail_ns)
    (list	"#a" t_bulwark_n)
    (list	"#b" t_bulwark_w)
    (list	"#c" t_bulwark_e)
    (list	"#d" t_bulwark_s)
    (list	"#A" t_bulwark_v_n)
    (list	"#B" t_bulwark_v_w)
    (list	"#C" t_bulwark_v_e)
    (list	"#D" t_bulwark_v_s)
    (list	"#e" t_bulwark_w_nw)
    (list	"#f" t_bulwark_w_ne)
    (list	"#g" t_bulwark_w_sw)
    (list	"#h" t_bulwark_w_se)
    (list	"#E" t_bulwark_d_nw)
    (list	"#F" t_bulwark_d_ne)
    (list	"#G" t_bulwark_d_sw)
    (list	"#H" t_bulwark_d_se)  
    (list	"#i" t_bulwark_v_nw)
    (list	"#j" t_bulwark_v_ne)
    (list	"#k" t_bulwark_v_sw)
    (list	"#l" t_bulwark_v_se)
    (list	"#I" t_bulwark_x_nw)
    (list	"#J" t_bulwark_x_ne)
    (list	"#K" t_bulwark_x_sw)
    (list	"#L" t_bulwark_x_se)
    (list	"#s" t_bulwark_x_ns)
    (list	"#r" t_bulwark_x_ew)    
  
    (list	"#m" t_tank_l)
    (list	"#M" t_tank_d)
    (list	"#n" t_tank_nw)
    (list	"#o" t_tank_ne)
    (list	"#p" t_tank_sw)
    (list	"#q" t_tank_se)  
    (list	"#N" t_tank_d_nw)
    (list	"#O" t_tank_d_ne)
    (list	"#P" t_tank_d_sw)
    (list	"#Q" t_tank_d_se) 
      
    (list	"<n" t_stair_un)
    (list	"<s" t_stair_us)
    (list	"<w" t_stair_uw)
    (list	"<e" t_stair_ue)
    
    (list	"rn" t_nat_rock)
    (list	"r1" t_nat_rock_n)
    (list	"r2" t_nat_rock_w)
    (list	"r3" t_nat_rock_nw)
    (list	"r4" t_nat_rock_e)
    (list	"r5" t_nat_rock_ne)
    (list	"r6" t_nat_rock_we)
    (list	"r7" t_nat_rock_nwe)
    (list	"r8" t_nat_rock_s)
    (list	"r9" t_nat_rock_ns)
    (list	"ra" t_nat_rock_ws)
    (list	"rb" t_nat_rock_nws)
    (list	"rc" t_nat_rock_es)
    (list	"rd" t_nat_rock_nes)
    (list	"re" t_nat_rock_wes)
    (list	"rf" t_nat_rock_nwes)
    (list	"r~" t_fake_wall_nrock)   

    (list "000" t_hl_0)
    (list "001" t_hl_1)
    (list "002" t_hl_2)
    (list "003" t_hl_3)
    (list "004" t_hl_4)
    (list "005" t_hl_5)
    (list "006" t_hl_6)
    (list "007" t_hl_7)
    (list "008" t_hl_8)
    (list "009" t_hl_9)
    (list "010" t_hl_10)
    (list "011" t_hl_11)
    (list "012" t_hl_12)
    (list "013" t_hl_13)
    (list "014" t_hl_14)
    (list "015" t_hl_15)
    (list "016" t_hl_16)
    (list "017" t_hl_17)
    (list "018" t_hl_18)
    (list "019" t_hl_19)
    (list "020" t_hl_20)
    (list "021" t_hl_21)
    (list "022" t_hl_22)
    (list "023" t_hl_23)
    (list "024" t_hl_24)
    (list "025" t_hl_25)
    (list "026" t_hl_26)
    (list "027" t_hl_27)
    (list "028" t_hl_28)
    (list "029" t_hl_29)
    (list "030" t_hl_30)
    (list "031" t_hl_31)
    (list "032" t_hl_32)
    (list "033" t_hl_33)
    (list "034" t_hl_34)
    (list "035" t_hl_35)
    (list "036" t_hl_36)
    (list "037" t_hl_37)
    (list "038" t_hl_38)
    (list "039" t_hl_39)
    (list "040" t_hl_40)
    (list "041" t_hl_41)
    (list "042" t_hl_42)
    (list "043" t_hl_43)
    (list "044" t_hl_44)
    (list "045" t_hl_45)
    (list "046" t_hl_46)
    (list "047" t_hl_47)
    (list "048" t_hl_48)
    (list "049" t_hl_49)
    (list "050" t_hl_50)
    (list "051" t_hl_51)
    (list "052" t_hl_52)
    (list "053" t_hl_53)
    (list "054" t_hl_54)
    (list "055" t_hl_55)
    (list "056" t_hl_56)
    (list "057" t_hl_57)
    (list "058" t_hl_58)
    (list "059" t_hl_59)
    (list "060" t_hl_60)
    (list "061" t_hl_61)
    (list "062" t_hl_62)
    (list "063" t_hl_63)
    (list "064" t_hl_64)
    (list "065" t_hl_65)
    (list "066" t_hl_66)
    (list "067" t_hl_67)
    (list "068" t_hl_68)
    (list "069" t_hl_69)
    (list "070" t_hl_70)
    (list "071" t_hl_71)
    (list "072" t_hl_72)
    (list "073" t_hl_73)
    (list "074" t_hl_74)
    (list "075" t_hl_75)
    (list "076" t_hl_76)
    (list "077" t_hl_77)
    (list "078" t_hl_78)
    (list "079" t_hl_79)
    (list "080" t_hl_80)
    (list "081" t_hl_81)
    (list "082" t_hl_82)
    (list "083" t_hl_83)
    (list "084" t_hl_84)
    (list "085" t_hl_85)
    (list "086" t_hl_86)
    (list "087" t_hl_87)
    (list "088" t_hl_88)
    (list "089" t_hl_89)
    (list "090" t_hl_90)
    (list "091" t_hl_91)
    (list "092" t_hl_92)
    (list "093" t_hl_93)
    (list "094" t_hl_94)
    (list "095" t_hl_95)
    (list "096" t_hl_96)
    (list "097" t_hl_97)
    (list "098" t_hl_98)
    (list "099" t_hl_99)
    (list "100" t_hl_100)
    (list "101" t_hl_101)
    (list "102" t_hl_102)
    (list "103" t_hl_103)
    (list "104" t_hl_104)
    (list "105" t_hl_105)
    (list "106" t_hl_106)
    (list "107" t_hl_107)
    (list "108" t_hl_108)
    (list "109" t_hl_109)
    (list "110" t_hl_110)
    (list "111" t_hl_111)
    (list "112" t_hl_112)
    (list "113" t_hl_113)

    (list "fg" t_gf_nw)
    (list "fh" t_gf_n )
    (list "fi" t_gf_ne)
    (list "fj" t_gf_w )
    (list "fk" t_gf_c )
    (list "fl" t_gf_e )
    (list "fm" t_gf_sw)
    (list "fn" t_gf_s )
    (list "fo" t_gf_se)
  )
) ;; palette pal_expanded

;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_demo_scene 19 19 pal_expanded

	(list
		"000 001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 "
		"019 020 021 022 023 024 025 026 027 028 029 030 031 032 033 034 035 036 037 "
		"038 039 040 041 042 043 044 045 046 047 048 049 050 051 052 053 054 055 056 "
		"057 058 059 060 061 062 063 064 065 066 067 068 069 070 071 072 073 074 075 "
		"076 077 078 079 080 081 082 083 084 085 086 087 088 089 090 091 092 093 094 "
		"095 096 097 098 099 100 101 102 103 104 105 106 107 108 109 110 111 112 113 "
		"fg fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fh fi "
		"fj t. t. t# .. .. .. .. .. .. t% ta t. t. tc __ t. t. fl "
		"fj t. t# .. ar .. .. .. ar .. .. t% t. t. _3 _c t. t. fl "
		"fj t# .. .. .. .. .. .. .. .. .. .. ta tc _2 tG t. t. fl "
		"fj .. ar .. .. .. .. .. .. .. ar .. .. .. __ .. .. .. fl "
		"fj .. .. .. .. dd dd dd .. .. .. .. dd ee ee ee dd dd fl "
		"fj .. .. .. dd dd {f dd dd dd dd dd dd ee ee ee dd dd fl "
		"fj .. .. .. .. dd dd dd .. .. .. .. dd ee ee ee dd dd fl "
		"fj .. ar .. .. .. .. .. .. .. ar .. .. .. __ .. .. .. fl "
		"fj tA .. .. .. .. .. .. .. .. .. .. t3 t5 _2 tJ t. t. fl "
		"fj t. tA .. ar .. .. .. ar .. .. tC t. t. _a _5 t. t. fl "
		"fj t. t. tA .. .. .. .. .. .. tC t3 t. t. t5 __ t. t. fl "
		"fm fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fn fo "
	)
 )

(kern-mk-place
 'p_demo_scene   ; tag
 "Demo Scene"    ; name
 nil             ; sprite
 m_demo_scene    ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil             ; subplaces
 nil             ; neighbors

 (list ; objects
 (put (guard-pt 'halberdier)   15 10)
 (put (guard-pt 'halberdier)   15 14)
 (put (guard-pt 'crossbowman) 13 10)
 (put (guard-pt 'crossbowman) 13 14)
 (put (mk-monman) 0 0)
 (put (mk-scene-mgr) 0 0)
 (put (kern-tag 'portal (kern-mk-obj t_portal 1)) 6  12)
 )
 (list 'on-entry-to-dungeon-room) ; hooks
 nil ;; edge entrances
)

;;----------------------------------------------------------------------------
;; Characters
;;
;; Make an invisible, passive character for the player party, because there has
;; to be a player party to get the camera on the scene, and the player party
;; has to have at least one living member. Yes, this is a blatant hack.
;;----------------------------------------------------------------------------

(define (passive-ai kchar)
	(kern-char-set-hp kchar max-health) ; functionally immortal, in case of stray fireballs
	(kern-map-repaint)
	(kern-sleep 75)
      #t)
  
(let ((kchar (kern-mk-char 
              'ch_wanderer
              "John the Mute"       ; name
              sp_human              ; species
              oc_wanderer           ; occ
              s_beggar    ; sprite
              faction-player        ; starting alignment
              5 5 5                ; str/int/dex
              9999							; functionally immortal, in case of stray fireballs
              pc-hp-gain
              pc-mp-off
              pc-mp-gain
              max-health 0 max-health 0 1  ; hp/xp/mp/AP_per_turn/lvl
              #f                    ; dead
              nil                   ; conv
              nil                   ; sched
              'passive-ai           ; special ai
              nil                   ; container
              nil                   ; readied
              )))
  (kern-char-set-ai kchar 'passive-ai)
  )
 
;;----------------------------------------------------------------------------
;; Player Party
;;----------------------------------------------------------------------------
(kern-mk-player
 'player                     ; tag
 s_wanderer         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 1                           ; food
 0                           ; gold
 (* 60 60 5)                 ; turns to next meal (5 hours)
 nil                         ; formation
 nil                         ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle
 ;; inventory
 (kern-mk-inventory nil)
 nil ;; party members (should be nil for initial load file)
 )

(kern-party-add-member player ch_wanderer)

;;----------------------------------------------------------------------------
;; Astronomy
;;----------------------------------------------------------------------------
(kern-mk-astral-body
 'sun              ; tag
 "Fyer (the sun)"  ; name
 1                 ; relative astronomical distance 
 1                 ; minutes per phase (n/a for sun)
 (/ (* 24 60) 360) ; minutes per degree
 0                 ; initial arc
 0                 ; initial phase
 '()               ; script interface
 ;; phases:
 (list 
  (list s_sun 255 "full")
  )
 )

;;----------------------------------------------------------------------------
;; Lumis is the source gate, which means it opens the source moongates on its
;; phases. We designate this by using the source-moon-ifc as its ifc.
;;
;; Note: the arc and phase are calculated to give the moon the right orientation
;; with respect to phase vs sun position
;;----------------------------------------------------------------------------
(mk-moon 'lumis  ; tag
         "Lumis" ; name
         5       ; hours per phase
         60      ; hours per revolution
         22      ; initial arc
         0       ; initial phase
         'source-moon-ifc ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-5 'mg-6 'mg-7 'mg-8
               )
         "yellow")

;;----------------------------------------------------------------------------
;; Ord is the destination gate, which means its phase decides the destination
;; when the player steps through a moongate. We designate this by giving it a
;; nil ifc. Note that its gates do not need to be listed in the same order as
;; Lumis. In fact, they don't even need to be the same set of gates.
;;
;; Note: the arc and phase are calculated to give the moon the right orientation
;; with respect to phase vs sun position
;;----------------------------------------------------------------------------
(mk-moon 'ord    ; tag
         "Ord"   ; name
         9       ; hours per phase
         36      ; hours per revolution
         67     ; initial arc
         7       ; initial phase
         nil     ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-5 'mg-6 'mg-7 'mg-8
               )
         "blue")

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;;
;; Note: factions should always be allied with themselves in order for
;; summoning AI to work properly.
;;       
;; Formatted for spreadsheet
;; ----------------------------------------------------------------------------
(kern-mk-dtable																	
	;;	non	pla	men	cgb	acc	mon	tro	spd	out	gnt	dem	fgb	prs			
	(list	2	0	0	0	-1	-2	-2	-2	0	-2	-2	0	0	)	;;	none
	(list	0	2	2	-1	-2	-2	-2	-2	-2	2	-2	-2	2	)	;;	player
	(list	-1	2	2	-1	-2	-2	-2	-2	-2	-2	-2	-2	2	)	;;	men
	(list	-1	2	-2	2	-1	-2	0	-2	-2	-1	-2	-2	0	)	;;	cave goblin
	(list	-1	2	-1	-1	2	-2	-1	-1	-2	-1	-2	-2	0	)	;;	accursed
	(list	-2	2	-2	-2	-2	2	-2	0	-2	0	-2	0	0	)	;;	monsters
	(list	-2	2	-2	0	-1	-2	2	-2	-2	-1	-2	-1	0	)	;;	hill trolls
	(list	-2	2	-2	-2	-1	0	-2	2	-2	-1	-2	0	0	)	;;	wood spiders
	(list	0	2	-2	-2	-2	-2	-2	-2	2	-2	-2	-1	0	)	;;	outlaws
	(list	-2	2	-2	-1	-1	0	-1	-1	-2	2	-2	-1	0	)	;;	gint
	(list	-2	2	-2	-2	-2	-2	-2	-2	-2	-2	2	-2	0	)	;;	demon
	(list	0	2	-2	-2	-2	0	-2	0	-1	-1	-2	2	0	)	;;	forest goblin
	(list	0	2	2	0	0	0	0	0	0	0	0	0	2	)	;;	prisoners
)																	

(define (simple-start kplayer)
  (kern-obj-put-at kplayer (list p_demo_scene (+ xoff 14)  (+ yoff 3)))
	(kern-map-center-camera (mk-loc p_demo_scene (+ xoff 10)  (+ yoff 5)))

        (kern-char-set-control-mode ch_wanderer "auto")

  ;; Do this to initialize the map viewer's active view, and to replace the
  ;; splash screen with the scene.
  (kern-map-repaint)
  )

(kern-add-hook 'new_game_start_hook 'simple-start)
