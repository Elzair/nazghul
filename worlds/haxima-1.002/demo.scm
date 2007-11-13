;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")

;; clone of game.scm ---------------------------------------------------------
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
(load "sounds.scm")
(load "effects.scm")
(load "terrains.scm")
(load "palette.scm")
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
(kern-set-combat-procs proc-stratt proc-dexatt
	proc-stratt proc-dexdef)

;; Setup the global effect sprites
(kern-set-quicken-sprite s_quicken)
(kern-set-time-stop-sprite s_time_stop)
(kern-set-magic-negated-sprite s_magic_negated)
(kern-set-reveal-sprite s_reveal)
(kern-set-xray-vision-sprite s_xray_vision)

(kern-init-random)

;; end clone of game.scm------------------------------------------------------



(kern-load "runes.scm")

;;----------------------------------------------------------------------------
;; Time -- this needs to be set before loading any dungeon rooms
;;----------------------------------------------------------------------------
(define hour 07)
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
(define (traveler-goto-dest kchar)
  (let* ((trvl (gob kchar))
         (loc (kern-obj-get-location kchar))
         (dest (cons (loc-place loc) 
                     (npcg-get-post trvl)))
         )
    (if (equal? loc dest)
        (begin
          (kern-obj-remove kchar)
          #t)
        (begin
          (pathfind kchar dest)))))

(define (wizard-traveler-ai kchar)
  (or (spell-sword-ai kchar)
      (traveler-goto-dest kchar)))

(define (normal-traveler-ai kchar)
  (or (std-ai kchar)
      (if (any-visible-hostiles? kchar)
          #f
          (traveler-goto-dest kchar))))

(define (traveler-mk kplace)
  (let* ((type-ai (random-select (list (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'paladin 'normal-traveler-ai)
                                       (cons 'tinker 'normal-traveler-ai)
                                       (cons 'ranger 'normal-traveler-ai)
                                       )))
         (path (random-select (list (list (loc-mk kplace 9 0) (list 9 9) #f)
                                    (list (loc-mk kplace 0 9) (list 9 9) #f)
                                    (list (loc-mk kplace 18 9) (list 9 9) #f)
                                    (list (loc-mk kplace 9 18) (list 9 9) #f)
                                    (list (loc-mk kplace 9 8) (list 9 0) #t)
                                    (list (loc-mk kplace 9 10) (list 9 18) #t)
                                    (list (loc-mk kplace 8 9) (list 0 9) #t)
                                    (list (loc-mk kplace 10 9) (list 18 9) #t)
                                    )))
         (kchar (mk-npc (car type-ai) 9))
         )
    (npcg-set-post! (gob kchar) (cadr path))
    (kern-char-set-ai kchar (cdr type-ai) kchar)
    (kern-obj-put-at kchar (car path))
    kchar))

;;----------------------------------------------------------------------------
;; Special Object Types
;;----------------------------------------------------------------------------
(define portal-ifc
  (ifc '()
       (method 'step (lambda (kportal kobj) 
                       (println "portal-step")
                       ;;(kern-map-flash 100)
                       (kern-obj-remove kobj)
                       ))
       ))
(mk-obj-type 't_portal "Portal" s_blackgate_full layer-mechanism portal-ifc)

;;----------------------------------------------------------------------------
;; Scene Manager
;;----------------------------------------------------------------------------
(define (scene-mgr-mk) (list 'scene-mgr 0 0 0))
(define (scene-mgr-state gob) (list-ref gob 1))
(define (scene-mgr-set-state! gob val) (set-car! (list-tail gob 1) val))
(define (scene-mgr-advance-state! gob) (set-car! (list-tail gob 1) (+ 1 (scene-mgr-state gob))))
(define (scene-mgr-get-num-demons gob) (list-ref gob 2))
(define (scene-mgr-set-num-demons! gob val) (set-car! (list-tail gob 2) val))
(define (scene-mgr-incr-num-demons! gob) (set-car! (list-tail gob 2) (+ 1 (scene-mgr-get-num-demons gob))))
(define (scene-mgr-get-num-travelers gob) (list-ref gob 3))
(define (scene-mgr-set-num-travelers! gob val) (set-car! (list-tail gob 3) val))
(define (scene-mgr-incr-num-travelers! gob) (set-car! (list-tail gob 3) (+ 1 (scene-mgr-get-num-travelers gob))))

(define (scene-mgr-intro-travelers-phase kobj)
  (println "scene-mgr-intro-travelers-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-travelers smgr))
         )
    (define (put-traveler)
      (println "put-traveler")
      (traveler-mk (loc-place (kern-obj-get-location kobj)))
      (scene-mgr-incr-num-travelers! smgr)
      )
    (cond ((< n 10) 
           (if (> (kern-dice-roll "1d3") 2)
               (put-traveler)))
          (else
           (scene-mgr-advance-state! smgr)))
    ))

(define (scene-mgr-intro-demons-phase kobj)
  (println "scene-mgr-intro-demons-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-demons smgr))
         )
    (define (put-demon dir)
      (let ((kdemon (mk-npc 'demon 9)))
        (kern-char-set-ai kdemon 'std-ai)
        ;;(kern-map-flash 1000)
        (kern-obj-put-at kdemon 
                         (loc-offset (mk-loc (loc-place (kern-obj-get-location kobj)) 9 9)
                                     dir)))
      (scene-mgr-incr-num-demons! smgr)
      )
    (cond ((= n 0) 
           (kern-add-reveal 1000)
           (put-demon west))
          ((= n 1) (put-demon east))
          ((= n 2) (put-demon south))
          ((= n 3) (put-demon north))
          (else
           (scene-mgr-advance-state! smgr)))
    ))

(define (scene-mgr-exec kobj) 
  (let* ((smgr (kobj-gob-data kobj))
         (state (scene-mgr-state smgr)))
    (println "scene-mgr-exec: state=" state)
    (cond ((= 0 state) (scene-mgr-intro-travelers-phase kobj))
          ((= 1 state) (scene-mgr-intro-demons-phase kobj))
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
;; Places
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_demo_scene 19 19 pal_expanded
 (list
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx x! ,, cc cc cc ,, x! xx xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx ,, cc cc cc cc ar cc cc cc cc ,, xx xx xx xx "
      "xx xx xx x! ,, cc ar cc cc cc cc cc ar cc ,, x! xx xx xx "
      ",, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc ,, ,, ,, ,, ,, "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
      "cc cc cc cc cc ar cc cc cc .. cc cc cc ar cc cc cc cc cc "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
      ",, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc ,, ,, ,, ,, ,, "
      "xx xx xx x! ,, cc ar cc cc cc cc cc ar cc ,, x! xx xx xx "
      "xx xx xx xx ,, cc cc cc cc ar cc cc cc cc ,, xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx x! ,, cc cc cc ,, x! xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
  )
 )

(kern-mk-place
 'p_demo_scene   ; tag
 "Gate Portal"   ; name
 nil             ; sprite
 m_demo_scene    ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil             ; subplaces
 nil             ; neighbors

 (list ; objects
  (put (guard-pt 'halberdier) 12 4)
  (put (guard-pt 'halberdier) 4 6)
  (put (guard-pt 'halberdier) 6 14)
  (put (guard-pt 'halberdier) 14 12)
   (put (guard-pt 'crossbowman) 4 12)
   (put (guard-pt 'crossbowman) 6 4)
   (put (guard-pt 'crossbowman) 14 6)
   (put (guard-pt 'crossbowman) 12 14)
  (put (mk-monman) 0 0)
  (put (mk-scene-mgr) 0 0)
  (put (kern-mk-obj t_portal 1) 9 9)
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
  (or (std-ai kchar)
      #t))
  
(let ((kchar (kern-mk-char 
              'ch_wanderer
              "Wanderer"       ; name
              sp_human              ; species
              oc_wanderer           ; occ
              s_wanderer    ; sprite
              faction-player        ; starting alignment
              5 5 5                ; str/int/dex
              pc-hp-off
              pc-hp-gain
              pc-mp-off
              pc-mp-gain
              max-health 0 max-health 0 1  ; hp/xp/mp/AP_per_turn/lvl
              #f                    ; dead
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
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
	(list	0	2	2	-2	-2	-2	-2	-2	-2	-2	-2	-2	2	)	;;	player
	(list	-1	2	2	-1	-2	-2	-2	-2	-2	-2	-2	-2	2	)	;;	men
	(list	-1	-2	-2	2	-1	-2	0	-2	-2	-1	-2	-2	0	)	;;	cave goblin
	(list	-1	-2	-1	-1	2	-2	-1	-1	-2	-1	-2	-2	0	)	;;	accursed
	(list	-2	-2	-2	-2	-2	2	-2	0	-2	0	-2	0	0	)	;;	monsters
	(list	-2	-2	-2	0	-1	-2	2	-2	-2	-1	-2	-1	0	)	;;	hill trolls
	(list	-2	-2	-2	-2	-1	0	-2	2	-2	-1	-2	0	0	)	;;	wood spiders
	(list	0	-2	-2	-2	-2	-2	-2	-2	2	-2	-2	-1	0	)	;;	outlaws
	(list	-2	-2	-2	-1	-1	0	-1	-1	-2	2	-2	-1	0	)	;;	gint
	(list	-2	-2	-2	-2	-2	-2	-2	-2	-2	-2	2	-2	0	)	;;	demon
	(list	0	-2	-2	-2	-2	0	-2	0	-1	-1	-2	2	0	)	;;	forest goblin
	(list	0	2	2	0	0	0	0	0	0	0	0	0	2	)	;;	prisoners
)																	

(define (simple-start kplayer)
  (kern-obj-put-at kplayer (list p_demo_scene 9 16))

  (kern-char-set-control-mode ch_wanderer "auto")

  ;; Do this to initialize the map viewer's active view, and to replace the
  ;; splash screen with the scene.
  (kern-map-repaint)
  )

(kern-set-start-proc simple-start)
