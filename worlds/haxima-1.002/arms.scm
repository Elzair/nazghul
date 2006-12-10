;;----------------------------------------------------------------------------
;; arms.scm - armament types
;;
;; The basic primitive for creating an armament type is the kern-mk-arms-type
;; procedure. This procedure takes a lot of parameters (listed below in
;; order). A lot of the parameters are boiler-plate for whole classes of
;; weapons, so I added some "curried" wrapper calls below.
;;
;;          tag : the symbol for the type in the script (variable-name)
;;         name : the string name used by the game
;;       sprite : sprite for the type
;;       to-hit : to-hit attack bonus (dice expr)
;;       damage : attack damage (dice expr)
;;        armor : damage reduced when hit (dice expr)
;;      deflect : bonus to deflect attack (dice expr)
;;        slots : slots it will fit in (e.g., hands)
;;        hands : number of slots required to ready it
;;        range : range it will fire
;;          rap : required action points to attack with it
;;      missile : nil or the armament type it fires
;;       thrown : true or false
;;         ubiq : true if it needs ammo in inventory, false otherwise
;;       weight : weight of arms
;;   fire-sound : string name of sound file to play when it's fired
;;     gifc-cap :
;;         gifc :
;;   stratt_mod : percentage of str based attack bonus used
;;   dexatt_mod : percentage of dex based attack bonus used
;;   damage_mod : percentage of damage bonus used
;;    avoid_mod : multiplier for avoidance bonus (1.0 = no effect)
;;
;;
;;----------------------------------------------------------------------------

;; Make the default number of required action points the same as the number of
;; turns it takes a human to cross normal terrain
(define default-rap norm) 

(kern-mk-sprite-set 'ss_arms 32 32 9 8 0 0 "arms.png")

;;-------------------------------------------------------------------------
;; Temp ifc for mutable attack types
;;-------------------------------------------------------------------------

(define temp-ifc-state (list 0))

(define (temp-ifc-set tempifc)
	(set-car! temp-ifc-state tempifc))
	
(define temp-ifc
  (ifc '()
       (method 'hit-loc
               (lambda (kmissile kplace x y)
				 ((car temp-ifc-state) kmissile kplace x y)
                 ))))

;;--------------------------------------------------------------------------
;; Curried constructors
;;
;; These are for creating the standard classes of armaments. They simplify
;; things by filling in the blanks for all the boilerplate parameters of
;; the primitive kern-mk-arms-type procedure.
;;--------------------------------------------------------------------------

(define obj-ifc-cap (ifc-cap obj-ifc))    

(define (mk-melee-arms-type tag name sprite to-hit-bonus damage deflect slots 
                            num-hands range weight
							stratt_mod dexatt_mod
							damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range default-rap nil #f #f weight nil
					 obj-ifc-cap obj-ifc stratt_mod dexatt_mod damage_mod avoid_mod))

;; Curried constructor: missile weapon (add missile, ubiq flag to melee)
(define (mk-projectile-arms-type tag name sprite to-hit-bonus damage deflect 
                                 slots num-hands range missile ubiq weight
								 stratt_mod dexatt_mod damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range default-rap missile #f ubiq weight nil obj-ifc-cap obj-ifc stratt_mod dexatt_mod damage_mod avoid_mod))

;; Curried constructor: thrown weapon (add field to melee)
(define (mk-thrown-arms-type tag name sprite to-hit-bonus damage deflect slots 
                             num-hands range ifc weight
							 stratt_mod dexatt_mod damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range default-rap nil #t #f weight nil (ifc-cap ifc) ifc stratt_mod dexatt_mod damage_mod avoid_mod))

(define (mk-missile-arms-type tag name sprite ifc)
  (kern-mk-arms-type tag name sprite "0" "0" "0" "0" slot-nil 0 0 0 nil #f #f 
                     0 nil (ifc-cap ifc) ifc 20 60 20 1.0))

(define (mk-armor-type tag name sprite to-hit armor slots weight avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit "0" armor "0" slots 1 0 0 nil #f #f 
                     weight nil obj-ifc-cap obj-ifc 20 60 20 avoid_mod))

(define (mk-shield-type tag name sprite to-hit deflect slots weight avoid_mod) 
  (kern-mk-arms-type tag name sprite to-hit "0" "0" deflect slots 1 0 0 nil #f #f 
                     weight nil obj-ifc-cap obj-ifc 20 60 20 avoid_mod))

;; ============================================================================
;; Missiles for Projectile Weapons & Spells
;; ============================================================================

(kern-mk-sprite 's_sling_stone ss_arms 1 0  #f 0)
(kern-mk-sprite 's_warhead     ss_arms 1 1  #f 0)
(kern-mk-sprite 's_cannonball  ss_arms 1 2  #f 0)
(kern-mk-sprite 's_fireball    ss_arms 1 3  #f 0)
(kern-mk-sprite 's_deathball   ss_arms 1 4  #f 0)
(kern-mk-sprite 's_arrow       ss_arms 1 8  #f 170)
(kern-mk-sprite 's_bolt        ss_arms 1 12 #f 170)
(kern-mk-sprite 's_poison_bolt ss_arms 1 16 #f 170)
(kern-mk-sprite 's_acid_bolt   ss_arms 1 20 #f 170)
(kern-mk-sprite 's_thrownweb   ss_arms 1 31 #f 0)
(kern-mk-sprite 's_prismatic_bolt ss_arms 1 60 #f 0)
(kern-mk-sprite 's_squat_bubbly_green_potion ss_arms 1 30 #f 170)

;; ----------------------------------------------------------------------------
;; mk-missile-ifc -- automate missile ifc creation. 'pred?' takes an object as
;; a parameter and returns true iff the 'hit' proc should be applied to it.
;; ----------------------------------------------------------------------------
(define (mk-missile-ifc hit)
  (ifc '()
       (method 'hit-loc (lambda (kmissile kplace x y)
                          (let ((targets (filter obj-is-char? 
                                                 (kern-get-objects-at (mk-loc kplace 
                                                                              x 
                                                                              y)))))
                            (if (notnull? targets)
                                (hit (car targets))))))))


(define poison-bolt-ifc (mk-missile-ifc apply-poison))
(define deathball-ifc   (mk-missile-ifc magical-kill))
(define stunball-ifc (mk-missile-ifc paralyze))
(define acid-bolt-ifc (mk-missile-ifc apply-acid))
(define prismatic-bolt-ifc (mk-missile-ifc apply-prismatic))

;; fireball-hit -- when a fireball hits it burns all characters and leaves a
;; fire 
(define fireball-ifc
  (ifc '()
       (method 'hit-loc
               (lambda (kmissile kplace x y)
                 ;;(println "fireball-hit")
                 (for-each burn
                           (filter obj-is-char? 
                                   (kern-get-objects-at (mk-loc kplace x y))))
                 ))))
               

(define warhead-ifc
  (ifc nil
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (println "warhead-hit")
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))

(define missile-arms-types
  (list
   ;;    ===================================================================
   ;;    tag           | name        | sprite        | gifc
   ;;    ===================================================================
   (list 't_slingstone     "sling stone"    s_sling_stone    nil)
   (list 't_arrow          "arrow"          s_arrow          obj-ifc)
   (list 't_bolt           "bolt"           s_bolt           obj-ifc)
   (list 't_warhead        "warhead"        s_warhead        warhead-ifc)
   (list 't_cannonball     "cannonball"     s_cannonball     obj-ifc)
   (list 't_poison_bolt    "poison bolt"    s_poison_bolt    poison-bolt-ifc)
   (list 't_acid_bolt      "acid bolt"      s_acid_bolt      acid-bolt-ifc)
   (list 't_fireball       "fireball"       s_fireball       fireball-ifc)
   (list 't_deathball      "deathball"      s_deathball      deathball-ifc)
   (list 't_stunball       "stunball"       s_projectile     stunball-ifc)
   (list 't_slimeglob      "slime glob"     s_acid_bolt      nil)
   (list 't_mfireball      "fireball"       s_fireball       temp-ifc)
   (list 't_mweb           "web"            s_thrownweb      temp-ifc)
   (list 't_mpoison_bolt   "poison bolt"    s_poison_bolt    temp-ifc)
   (list 't_prismatic_bolt "prismatic bolt" s_prismatic_bolt prismatic-bolt-ifc)
   ))

;; If we don't create these missile types now, we won't be able to refer to
;; them below in the projectile-arms-types table. For example, t_bow needs to
;; refer to t_arrow. But the interpreter won't recognize t_arrow as a variable
;; name until we call this procedure to create the t_arrow type.
(map (lambda (type) (apply mk-missile-arms-type type)) missile-arms-types)

;; ============================================================================
;; Projectile Weapons
;; ============================================================================

(kern-mk-sprite 's_sling      ss_arms 1 24 #f 0)
(kern-mk-sprite 's_bow        ss_arms 1 25 #f 0)
(kern-mk-sprite 's_crossbow   ss_arms 1 26 #f 0)
(kern-mk-sprite 's_doom_staff ss_arms 1 27 #f 0)
(kern-mk-sprite 's_stun_wand  ss_arms 1 28 #f 0)

(define projectile-arms-types
  (list
   ;;     =============================================================================================================================================================
   ;;     tag         | name       | sprite     | to-hit | damage | to-def | slots       | hnds | rng | missile    | ubiq | weight | stratt | dexatt | dammod | avoid
   ;;     =============================================================================================================================================================
   (list 't_sling      "sling"      s_sling      "1d2-2"  "1d4"    "-1"     slot-weapon   1      4     t_slingstone #t      0        10       60       30       0.9  )
   (list 't_sling_4    "+4 sling"   s_sling      "3"      "1d4+4"  "0"      slot-weapon   1      6     t_slingstone #t      0        10       60       30       0.9  )
   (list 't_bow        "bow"        s_bow        "1d3-2"  "2d4"    "-2"     slot-weapon   2      6     t_arrow      #f      2        10       70       20       0.9  )
   (list 't_crossbow   "crossbow"   s_crossbow   "1d4-2"  "4d4"    "-1"     slot-weapon   2      4     t_bolt       #f      3        0        80       0        0.95 )
   (list 't_doom_staff "doom staff" s_doom_staff "1d4"    "1"      "+2"     slot-weapon   2      5     t_fireball   #t      2        0        50       0        1.0  )
   (list 't_acid_spray "acid spray" nil          "-7"     "1d6"    "+0"     slot-nil      2      2     t_slimeglob  #t      0        10       50       20       1.0  )
   (list 't_fire_glob  "fire glob"  nil          "-8"     "1d6"    "+0"     slot-nil      2      2     t_fireball   #t      0        10       50       20       1.0  )
   (list 't_stun_wand  "stun wand"  s_stun_wand  "-2"     "1d4"    "-1"     slot-weapon   1      6     t_stunball   #t      2        0        80       0        1.0  )
   (list 't_prismatic_gaze "prismatic gaze" nil  "1d4"    "0"      "0"      slot-nil      1      3     t_prismatic_bolt #t  0        0        0        0        0.85 )
   ))

;; ============================================================================
;; Thrown Weapons
;; ============================================================================

(kern-mk-sprite 's_flaming_oil    ss_arms 1 5 #f 0)
(kern-mk-sprite 's_spear          ss_arms 1 6 #f 0)
(kern-mk-sprite 's_thrown_boulder ss_arms 1 7 #f 0)

(define flaming-oil-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))

(define vial-of-slime-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (let* ((lvl (kern-dice-roll "1d3+5"))
                        (knpc (spawn-npc 'green-slime lvl))
                        (loc (pick-loc (mk-loc kplace x y) knpc)))
                   (println " loc=" loc)
                   (cond ((null? loc) 
                          (kern-obj-dec-ref knpc)
                          0)
                         (else
                          (kern-being-set-base-faction knpc faction-none)
                          (kern-obj-set-temporary knpc #t)
                          (kern-obj-put-at knpc loc))))))))

(define thrown-arms-types
  (list
   ;;     ============================================================================================================================================================================
   ;;     tag              | name          | sprite              | to-hit | damage | to-def | slots       | hnds | rng | ifc             | weight | stratt | dexatt | dammod | avoid
   ;;     ============================================================================================================================================================================
   (list  't_oil            "flaming oil"   s_flaming_oil          "-1"     "1d6"    "-2"     slot-weapon   1      4     flaming-oil-ifc    1       20       30       0        0.9     )
   (list  't_slime_vial     "vial of slime" s_squat_bubbly_green_potion "-1" "1d2"   "-2"     slot-weapon   1      4     vial-of-slime-ifc  1       20       30       0        1.0     )
   (list  't_spear          "spear"         s_spear                "0"      "1d8"    "+1"     slot-weapon   1      4     obj-ifc            2       20       40       40       1.0     )
   (list  't_thrown_boulder "loose boulder" s_thrown_boulder       "-2"     "3d4+1"  "-2"     slot-weapon   2      5     obj-ifc            10      40       20       60       0.9     )
   ))

;; Inventory sprites
(kern-mk-sprite 's_axe            ss_arms 1 29 #f 0)
(kern-mk-sprite 's_dagger         ss_arms 1 32 #f 0)
(kern-mk-sprite 's_mace           ss_arms 1 33 #f 0)
(kern-mk-sprite 's_sword          ss_arms 1 34 #f 0)
(kern-mk-sprite 's_2h_axe         ss_arms 1 35 #f 0)
(kern-mk-sprite 's_2h_sword       ss_arms 1 36 #f 0)
(kern-mk-sprite 's_morning_star   ss_arms 1 37 #f 0)
(kern-mk-sprite 's_halberd        ss_arms 1 38 #f 0)
(kern-mk-sprite 's_staff          ss_arms 1 39 #f 0)
(kern-mk-sprite 's_eldritch_blade ss_arms 1 40 #f 0)
(kern-mk-sprite 's_mystic_sword   ss_arms 1 42 #f 0)
(kern-mk-sprite 's_flaming_sword  ss_arms 1 44 #f 0)

;; Paper-doll sprites
(kern-mk-sprite 's_hum_staff_gold     ss_arms 4 56 #f 0)
(kern-mk-sprite 's_hum_staffglo_blue  ss_arms 4 60 #f 0)
(kern-sprite-apply-matrix (kern-sprite-clone s_hum_staffglo_blue 
                                             's_hum_staffglo_green) 
                          mat_blue_to_green)
(kern-mk-sprite 's_hum_halberd ss_arms 4 64 #f 0)

(define melee-arms-types
  (list
   ;;     ===================================================================================================================================================
   ;;     tag          |    name          | sprite           | to-hit | damage | to-def | slots       | hnds | rng | weight | dxmod | stmod | dammod | avoid
   ;;     ===================================================================================================================================================
   (list  't_hands          "bare hands"     nil              "1d2"    "1d2"    "1d2"    slot-nil      1      1      0			50		20		10		1.0	)
   (list  't_fangs          "fangs"          nil              "1d2"    "1d6"    "+0"     slot-nil      1      1      0			50		20		30		1.0	)
   (list  't_horns          "horns"          nil              "1d2"    "1d8"    "1d2"    slot-nil      1      1      0			30		40		60		1.0	)
   (list  't_stinger        "stinger"        nil              "1d2"    "1d2"    "+0"     slot-nil      1      1      0			50		20		10		1.0	)
   (list  't_tentacles      "tentacles"      nil              "1d3"    "4d4"    "4d2"    slot-nil      1      1      0			70		20		60		1.0	)
   (list  't_beak           "beak"           nil              "0"      "2d4"    "0"      slot-nil      1      1      0			50		30		30		1.0	)
   (list  't_pincers        "pincers"        nil              "-1"     "4d4"    "4d2"    slot-nil      1      1      0			50		30		30		1.0	)
   (list  't_dagger         "dagger"         s_dagger         "1d4"    "1d4"    "1d2"    slot-weapon   1      1      0			80		10		10		1.0	)
   (list  't_dagger_4       "+4 dagger"      s_dagger         "1d4+4"  "1d4+4"  "1d2+4"  slot-weapon   1      1      0			80		10		10		1.0	)
   (list  't_mace           "mace"           s_mace           "1d4"    "1d6+2"  "+0"     slot-weapon   1      1      3			20		60		80		0.95	)
   (list  't_axe            "axe"            s_axe            "1d2"    "2d3+2"  "+0"     slot-weapon   1      1      3			30		50		90		0.95	)
   (list  't_sword          "sword"          s_sword          "1d2"    "1d8+1"  "1d2"    slot-weapon   1      1      2			50		20		70		1.0	)
   (list  't_sword_2        "+2 sword"       s_sword          "1d2+2"  "1d8+3"  "1d2+2"  slot-weapon   1      1      2			50		20		70		1.0	)
   (list  't_sword_4        "+4 sword"       s_sword          "1d2+4"  "1d8+5"  "1d2+4"  slot-weapon   1      1      2			50		20		70		1.0	)
   (list  't_2H_axe         "2H axe"         s_2h_axe         "0"      "4d3+2"  "-2"     slot-weapon   2      1      4			20		60		100		0.9	)
   (list  't_2H_sword       "2H sword"       s_2h_sword       "0"      "2d8+2"   "1"     slot-weapon   2      1      4			40		40		90		0.95	)
   (list  't_morning_star   "morning star"   s_morning_star   "1d2+2"  "1d6+1"  "-1"     slot-weapon   1      2      3			20		40		70		0.9	)
   (list  't_morning_star_2 "+2 morning star" s_morning_star  "1d2+4"  "1d6+3"  "2"      slot-weapon   1      2      3			20		40		70		0.9	)
   (list  't_halberd        "halberd"        s_halberd        "1d3+1"  "2d8-2"  "1d2"    slot-weapon   2      2      4			30		30		100		0.9	)
   (list  't_staff          "staff"          s_staff          "1d3"    "1d4"    "1d3"    slot-weapon   2      2      2			60		30		40		1.0	)
   (list  't_eldritch_blade "eldritch blade" s_eldritch_blade "2"      "2d8+5"  "+0"     slot-weapon   2      1      2			50		20		70		1.0	)
   (list  't_mystic_sword   "mystic sword"   s_mystic_sword   "+3"     "1d10+5" "+2"     slot-weapon   1      1      1			60		20		70		1.0	)
   (list  't_flaming_sword  "flaming sword"  s_flaming_sword  "1d2"    "1d10+3" "1d2"    slot-weapon   1      1      2			50		20		70		1.0	)
   ))

(kern-mk-sprite 's_leather_helm  ss_arms 1 48 #f 0)
(kern-mk-sprite 's_chain_coif    ss_arms 1 49 #f 0)
(kern-mk-sprite 's_iron_helm     ss_arms 1 50 #f 0)
(kern-mk-sprite 's_leather_armor ss_arms 1 51 #f 0)
(kern-mk-sprite 's_chain_armor   ss_arms 1 52 #f 0)
(kern-mk-sprite 's_plate_armor   ss_arms 1 53 #f 0)

(define armor-types
  (list
   ;;     ===========================================================================================================
   ;;     tag             | name          |  sprite           |  to-hit | armor  | slots      | weight | avoid
   ;;     ===========================================================================================================
   (list   't_leather_helm   "leather helm"    s_leather_helm      "-1"     "1d2"    slot-helm     0		1.0		)
   (list   't_leather_helm_2 "+2 leather helm" s_leather_helm      "0"      "1d2+2"  slot-helm     0		1.0		)
   (list   't_leather_helm_4 "+4 leather helm" s_leather_helm      "0"      "1d2+4"  slot-helm     0		1.0		)
   (list   't_chain_coif     "chain coif"      s_chain_coif        "-1"     "1d3"    slot-helm     1		0.9		)
   (list   't_chain_coif_4   "+4 chain coif"   s_chain_coif        "0"      "1d3+4"  slot-helm     1		0.9		)
   (list   't_iron_helm      "iron helm"       s_iron_helm         "-1"     "1d4"    slot-helm     2		0.9		)
   (list   't_iron_helm_4    "+4 iron helm"    s_iron_helm         "0"      "1d4+4"  slot-helm     2		0.9		)
   (list   't_armor_leather  "leather armor"   s_leather_armor     "-1"     "1d4"    slot-armor    2		0.85	)
   (list   't_armor_leather_2 "+2 leather armor" s_leather_armor   "0"      "1d4+2"  slot-armor    2		0.85	)
   (list   't_armor_leather_4 "+4 leather armor" s_leather_armor   "0"      "1d4+4"  slot-armor    2		0.9		)
   (list   't_armor_chain    "chain armor"     s_chain_armor       "-2"     "2d4"    slot-armor    4		0.7		)
   (list   't_armor_chain_4  "+4 chain armor"  s_chain_armor       "0"      "2d4+4"  slot-armor    4		0.8		)
   (list   't_armor_plate    "plate armor"     s_plate_armor       "-4"     "4d4"    slot-armor    8		0.6		)
   (list   't_armor_plate_4  "+4 plate armor"  s_plate_armor       "0"      "4d4+4"  slot-armor    8		0.7		)
   ))	

(kern-mk-sprite 's_shield            ss_arms 1 54 #f 0)
(kern-mk-sprite 's_scratched_shield  ss_arms 1 55 #f 0)

(define shield-types
  (list
   ;;     =================================================================================================================
   ;;     tag                 | name             | sprite                | to-hit | deflect  | slots    | weight | avoid
   ;;     =================================================================================================================
   (list   't_shield           "small shield"     s_shield                "-1"     "2"      slot-shield   2			0.9		)
   (list   't_shield_4         "+4 small shield"  s_shield                "0"      "6"      slot-shield   2			0.95	)
   (list   't_scratched_shield "scratched shield" s_scratched_shield      "0"      "3"      slot-shield   2			0.9		)
   ))


(map (lambda (type) (apply mk-thrown-arms-type     type)) thrown-arms-types)
(map (lambda (type) (apply mk-projectile-arms-type type)) projectile-arms-types)
(map (lambda (type) (apply mk-melee-arms-type      type)) melee-arms-types)
(map (lambda (type) (apply mk-armor-type           type)) armor-types)
(map (lambda (type) (apply mk-shield-type          type)) shield-types)

;;----------------------------------------------------------------------------
;; Spiked Armor
;;----------------------------------------------------------------------------
(kern-mk-sprite 's_spiked_helm    ss_arms 1 46 #f 0)
(kern-mk-sprite 's_spiked_shield  ss_arms 1 47 #f 0)

(kern-mk-arms-type 't_spiked_helm "spiked helm" s_spiked_helm
                   "0" "1d4" "3" "0"
                   slot-helm 1 1 default-rap
                   nil #f #f
                   2 ;; weight
                   nil obj-ifc-cap obj-ifc
				   30 10 20 0.9)

(kern-mk-arms-type 't_spiked_shield "spiked shield" s_spiked_shield
                   "0" "1d5" "0" "1"
                   slot-shield 1 1 default-rap
                   nil #f #f
                   3 ;; weight
                   nil obj-ifc-cap obj-ifc
				   40 20 20 0.8)

;;--------------------------------------------------------------------------
;; Special arms types
;;
;; These don't fit into the mold for any standard arms type.
;;--------------------------------------------------------------------------

(kern-mk-arms-type 't_cannon         ; tag
                   "cannon"          ; name
                   nil               ; sprite
                   "+1"              ;;       to-hit : to-hit attack bonus (dice expr)
                   "1d10+4"          ;;       damage : attack damage (dice expr)
                   "0"               ;;        armor : added to armor class (dice expr)
                   "0"               ;;      deflect : damage deflected when hit (dice expr)
                   slot-nil          ;;        slots : slots it will fit in (e.g., hands)
                   0                 ;;        hands : number of slots required to ready it
                   6                 ;;        range : range it will fire
                   2                 ;;          rap : required action points to attack with it
                   t_cannonball      ;;      missile : nil or the armament type it fires
                   #f                ;;       thrown : true or false
                   #t                ;;         ubiq : true if it needs ammo in inventory, false otherwise
                   0                 ;;       weight : unused
                   sound-cannon-fire ;;   fire-sound : string name of sound file to play when it's fired
                   0                 ;;      ifc-cap : integer bitmap describing interface slots
                   nil               ;;  get-handler : script ifc
				   0 0 0 1.0
                   )

;;----------------------------------------------------------------------------
;; This list of "blockable" arms types is used by combat ai. An arms type is
;; "blockable" if an adjacent enemy can interfere with its usage.
;;----------------------------------------------------------------------------
(define blockable-arms-types
  (list t_bow
        t_spear
        t_thrown_boulder))

(define arms-types-needing-ammo
  (list t_bow t_crossbow))

(define (arms-type-is-blockable? karms)
  (display "arms-type-is-bloackable?")(newline)
  (in-list? karms blockable-arms-types))

(define (arms-type-needs-ammo? karms)
  (in-list? karms arms-types-needing-ammo))

;;----------------------------------------------------------------------------
;; Test paper doll sprites: add a gob to the staff arms type with a "readied"
;; sprite.
;;----------------------------------------------------------------------------
(kern-type-set-gob t_staff 
                   (kern-sprite-blit-over s_hum_staff_gold 
                                          s_hum_staffglo_blue))

(kern-type-set-gob t_halberd s_hum_halberd)
