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
;;        armor : added to armor class (dice expr)
;;      deflect : damage deflected when hit (dice expr)
;;        slots : slots it will fit in (e.g., hands)
;;        hands : number of slots required to ready it
;;        range : range it will fire
;;          rap : required action points to attack with it
;;      missile : nil or the armament type it fires
;;       thrown : true or false
;;         ubiq : true if it needs ammo in inventory, false otherwise
;;       weight : unused
;;   fire-sound : string name of sound file to play when it's fired
;;
;;----------------------------------------------------------------------------

;; Make the default number of required action points the same as the number of
;; turns it takes a human to cross normal terrain
(define default-rap norm) 

;;--------------------------------------------------------------------------
;; Curried constructors
;;
;; These are for creating the standard classes of armaments. They simplify
;; things by filling in the blanks for all the boilerplate parameters of
;; the primitive kern-mk-arms-type procedure.
;;--------------------------------------------------------------------------

(define obj-ifc-cap (ifc-cap obj-ifc))

(define (mk-melee-arms-type tag name sprite to-hit-bonus damage deflect slots 
                            num-hands range)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0"  slots 
                     num-hands range default-rap nil #f #f 0 nil obj-ifc-cap obj-ifc))

;; Curried constructor: missile weapon (add missile, ubiq flag to melee)
(define (mk-projectile-arms-type tag name sprite to-hit-bonus damage deflect 
                                 slots num-hands range missile ubiq)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0"  slots 
                     num-hands range default-rap missile #f ubiq 0 nil obj-ifc-cap obj-ifc))

;; Curried constructor: thrown weapon (add field to melee)
(define (mk-thrown-arms-type tag name sprite to-hit-bonus damage deflect slots 
                             num-hands range ifc)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0" slots 
                     num-hands range default-rap nil #t #f 0 nil (ifc-cap ifc) ifc))

(define (mk-missile-arms-type tag name sprite damage ifc)
  (kern-mk-arms-type tag name sprite "0" damage "0" "0" slot-nil 0 0 0 nil #f #f 
                     0 nil (ifc-cap ifc) ifc))

(define (mk-armor-type tag name sprite to-hit armor slots)
  (kern-mk-arms-type tag name sprite to-hit "0" armor "0" slots 1 0 0 nil #f #f 
                     0 nil obj-ifc-cap obj-ifc))

(define (mk-shield-type tag name sprite to-hit armor slots) 
  (kern-mk-arms-type tag name sprite to-hit "0" armor "0" slots 1 0 0 nil #f #f 
                     0 nil obj-ifc-cap obj-ifc))

;; ============================================================================
;; Missiles for Projectile Weapons
;; ============================================================================

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
(define deathball-ifc   (mk-missile-ifc kern-char-kill))
(define warhead-ifc
  (ifc nil
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))

;;--------------------------------------------------------------------------
;; Standard arms type tables
;;
;; These are run-of-the-mill arms types that vary only in the values assigned
;; to different fields.
;;--------------------------------------------------------------------------

(define missile-arms-types
  (list
   ;;    ============================================================================================================
   ;;    tag           | name        | sprite                     | damage | gifc
   ;;    ============================================================================================================
   (list 't_slingstone   "sling stone" s_sling_stones               "1d2"    nil)
   (list 't_arrow        "arrow"       s_arrow_wooden               "1d6"    obj-ifc)
   (list 't_bolt         "bolt"        s_bolt                       "2d4"    obj-ifc)
   (list 't_warhead      "warhead"     s_magic_ball_core_red        "0"      warhead-ifc)
   (list 't_cannonball   "cannonball"  s_sling_bullet               "0"      obj-ifc)
   (list 't_poison_bolt  "poison bolt" s_lightning_bolt_green       "1d6"    poison-bolt-ifc)
   (list 't_acid_bolt    "acid bolt"   s_magic_sphere_green         "1d2-1"  nil)
   (list 't_fireball     "fireball"    s_lightning_bolt_red         "2d6"    nil)
   (list 'deathball      "deathball"   s_magic_ball_core_dark_grey  "0"      deathball-ifc)
   ))

;; If we don't create these missile types now, we won't be able to refer to
;; them below in the projectile-arms-types table. For example, t_bow needs to
;; refer to t_arrow. But the interpreter won't recognize t_arrow as a variable
;; name until we call this procedure to create the t_arrow type.
(map (lambda (type) (apply mk-missile-arms-type type)) missile-arms-types)

;; ============================================================================
;; Projectile Weapons
;; ============================================================================

(define projectile-arms-types
  (list
   ;;     ===========================================================================================================
   ;;     tag   | name       | sprite     | to-hit | damage | to-def | slots       | hnds | rng | missile | ubiq
   ;;     ===========================================================================================================
   (list 't_sling "sling"      s_sling      "-1"     "1d2"    "-1"     slot-weapon   1      4     t_slingstone #t)
   (list 't_bow   "short bow"  s_shortbow   "1"      "1d4"    "-2"     slot-weapon   2      6     t_arrow   #f)
   (list 't_xbow  "crossbow"   s_crossbow_1 "2"      "2d3"    "-3"     slot-weapon   2      5     t_bolt    #f)
   (list 't_rpg   "doom staff" s_staff_7    "1d4"    "2d20"   "-5"     slot-weapon   2      12    t_warhead #t)
   (list 't_acid_spray "acid spray" nil     "0"      "1d2-1"  "+0"     slot-nil      2      2     t_acid_bolt #t)   
   ))

;; ============================================================================
;; Thrown Weapons
;; ============================================================================

(define flaming-oil-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))

(define thrown-boulder-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (kern-place-set-terrain (mk-loc kplace x y) t_boulder)))))

(define thrown-arms-types
  (list
   ;;     ====================================================================================================================
   ;;     tag              | name          | sprite              | to-hit | damage | to-def | slots       | hnds | rng | ifc
   ;;     ====================================================================================================================
   (list  't_oil            "flaming oil"   s_kg_potion_red_f33_2  "-1"     "1d6"    "-2"     slot-weapon   1      4     flaming-oil-ifc)
   (list  't_spear          "spear"         s_spear_1              "-1"     "1d8"    "+1"     slot-weapon   2      4     obj-ifc)
   (list  't_thrown_boulder "loose boulder" s_thrown_boulder       "-2"     "2d6"    "-2"     slot-weapon   2      5     thrown-boulder-ifc)
   ))

(define melee-arms-types
  (list
   ;;     ===============================================================================================================
   ;;     tag          |    name          | sprite           |  to-hit | damage | to-def | slots       | hnds | rng
   ;;     ===============================================================================================================
   (list  't_hands          "bare hands"     nil                "1d2"    "1d2"    "1d2"    slot-nil      1      1)

   (list  't_fangs          "fangs"          nil                "1d2"    "1d6"    "+0"     slot-nil      1      1)
   (list  't_stinger        "stinger"        nil                "1d8"    "1d2-1"  "+0"     slot-nil      1      1)

   (list  't_dagger         "dagger"         s_dagger_1         "1d4"    "1d4"    "1d2"    slot-weapon   1      1)
   (list  'short-sword      "short sword"    s_sword_short_1    "1d3"    "1d6"    "1d2"    slot-weapon   1      1)
   (list  't_longsword      "long sword"     s_sword_long_1     "1d2"    "1d8"    "1d2"    slot-weapon   1      2)
   (list  't_2H_sword       "2H sword"       s_sword_zweihander "0"      "2d8-1"    "+0"     slot-weapon   2      2)

   (list  't_eldritch_blade "eldritch blade" s_sword_eldritch   "2"      "2d8+5"  "+0"     slot-weapon   2      3)
   (list  't_mystic_sword   "mystic sword"   s_sword_mystic     "+3"     "1d10+1" "+2"     slot-weapon   1      2)
   (list  't_flaming_sword  "flaming sword"  s_sword_flaming    "1d2"    "1d10+3" "1d2"    slot-weapon   1      2)

   (list  't_mace           "mace"           s_mace_1           "1d3"    "1d6+1"   "+0"    slot-weapon   1      1)
   (list  't_mace_and_chain "mace and chain" s_flail_1          "1d3+3"  "1d6+1"   "-1"    slot-weapon   2      2)
   (list  't_nunchaku       "nunchaku"       s_nunchaku_1       "1d6+3"  "1d4"     "1d4"   slot-weapon   2      1)

   (list  't_hatchet        "hatchet"        s_axe_2            "+0"     "1d4+1"   "-1"    slot-weapon   1      1)
   (list  't_war_axe        "war axe"        s_axe_4            "+0"     "2d4+2"   "-2"    slot-weapon   2      2)

   (list  't_spear          "short spear"    s_spear_1          "1d4+2"  "1d6"     "+0"    slot-weapon   1      1)
   (list  't_halberd        "halberd"        s_halberd_2        "1d4+2"  "2d8-2"     "+1"    slot-weapon   2      3)

   (list  't_quarterstaff   "quarterstaff"   s_quarterstaff_1   "1d4"    "1d6"    "1d4"    slot-weapon   2      2)
   (list  't_bo_staff       "bo staff"       s_quarterstaff_2   "1d6"    "1d4"    "1d6"    slot-weapon   2      2)

   (list  't_bullwhip       "bullwhip"       s_whip             "1d8"    "1d3"    "0"      slot-weapon   1      4)

   ))

(define armor-types
  (list
   ;;     =============================================================================================================
   ;;     tag             | name          |  sprite           |  to-hit | armor  | slots
   ;;     =============================================================================================================
   (list   't_ranger_cap    "feathered cap"  s_hat_green_feather "0"      "0"      slot-helm)
   (list   't_leather_helm  "leather helm"   s_helm_leather_1    "0"      "1"      slot-helm)
   (list   't_iron_helm     "iron helm"      s_helm_metal_1      "0"      "2"      slot-helm)
   (list   't_ancient_helm  "ancient helmet" s_helm_gold_horned  "0"      "3"      slot-helm)
   (list   't_silver_crown  "silver crown"   s_crown_1           "0"      "1"      slot-helm)

   (list   't_amulet_green  "green amulet"   s_amulet_3          "0"      "1"      slot-amulet)
   (list   't_amulet_red    "red amulet"     s_amulet_8          "1"      "0"      slot-amulet)
   (list   't_amulet_blue   "blue amulet"    s_amulet_9          "0"      "0"      slot-amulet)

   (list   't_ring_wooden   "wooden ring"    s_ring_1            "0"      "0"      slot-ring)
   (list   't_ring_silver   "silver ring"    s_ring_4            "0"      "0"      slot-ring)
   (list   't_ring_golden   "golden ring"    s_ring_5            "0"      "0"      slot-ring)
   (list   't_ring_opal     "opal ring"      s_ring_9            "0"      "0"      slot-ring)
   (list   't_ring_ruby     "ruby ring"      s_ring_11           "0"      "0"      slot-ring)
   (list   't_ring_sapphire "sapphire ring"  s_ring_12           "0"      "0"      slot-ring)

   (list   't_clothing_rags "torn rags"      s_robe_rags         "0"      "0"      slot-armor)
   (list   't_robe_red      "red robe"       s_robe_1            "0"      "0"      slot-armor)
   (list   't_robe_green    "green robe"     s_robe_2            "0"      "0"      slot-armor)
   (list   't_robe_purple   "purple robe"    s_robe_3            "0"      "0"      slot-armor)
   (list   't_robe_blue     "blue robe"      s_robe_4            "0"      "0"      slot-armor)

   (list   't_armor_leather "leather armor"  s_armor_leather_3   "+0"     "1"      slot-armor)
   (list   't_armor_l_stud  "heavy leather"  s_armor_leather_4   "-1"     "2"      slot-armor)
   (list   't_armor_chain   "chain shirt"    s_armor_chain_3     "-1"     "3"      slot-armor)
   (list   't_armor_plate   "plate mail"     s_armor_plate_2     "-2"     "4"      slot-armor)

   (list   't_armor_viridian "viridian leather"   s_armor_chain_8  "+1"   "3"      slot-armor)
   (list   't_armor_opaline  "opaline chain"      s_armor_chain_5  "+0"   "1d4+1"  slot-armor)
   (list   't_armor_ancient  "ancient plate"      s_armor_plate_3  "-3"   "8"      slot-armor)
   (list   't_armor_warlord  "warlords suit"      s_armor_dragon_3 "+2"   "5"      slot-armor)
   (list   't_armor_golden   "golden breastplate" s_breastplate_dragon_6 "+1"  "2d3+1" slot-armor)

   ))

(define shield-types
  (list
   ;;     ===========================================================================================================
   ;;     tag                     | name           | sprite                | to-hit | armor  | slots
   ;;     ===========================================================================================================
   (list   't_sm_shield             "small shield"   s_shield_blank_round_1  "-1"     "1"      slot-shield)
   (list   't_shield_wooden_buckler "wooden buckler" s_shield_blank_round_2  "-1"     "1"      slot-shield)
   ))


(map (lambda (type) (apply mk-thrown-arms-type     type)) thrown-arms-types)
(map (lambda (type) (apply mk-projectile-arms-type type)) projectile-arms-types)
(map (lambda (type) (apply mk-melee-arms-type      type)) melee-arms-types)
(map (lambda (type) (apply mk-armor-type           type)) armor-types)
(map (lambda (type) (apply mk-shield-type          type)) shield-types)

;;--------------------------------------------------------------------------
;; Special arms types
;;
;; These don't fit into the mold for any standard arms type.
;;--------------------------------------------------------------------------

(kern-mk-arms-type 't_cannon         ; tag
                   "cannon"          ; name
                   nil               ; sprite
                   "+1"              ;;       to-hit : to-hit attack bonus (dice expr)
                   "2d20+5"          ;;       damage : attack damage (dice expr)
                   "0"               ;;        armor : added to armor class (dice expr)
                   "0"               ;;      deflect : damage deflected when hit (dice expr)
                   slot-nil          ;;        slots : slots it will fit in (e.g., hands)
                   0                 ;;        hands : number of slots required to ready it
                   6                 ;;        range : range it will fire
                   8                 ;;          rap : required action points to attack with it
                   t_cannonball      ;;      missile : nil or the armament type it fires
                   #f                ;;       thrown : true or false
                   #t                ;;         ubiq : true if it needs ammo in inventory, false otherwise
                   0                 ;;       weight : unused
                   sound-cannon-fire ;;   fire-sound : string name of sound file to play when it's fired
                   0                 ;;      ifc-cap : integer bitmap describing interface slots
                   nil               ;;  get-handler : script ifc
                   )

;;----------------------------------------------------------------------------
;; This list of "blockable" arms types is used by combat ai. An arms type is
;; "blockable" is an adjacent enemy can interfere with its usage.
;;----------------------------------------------------------------------------
(define blockable-arms-types
  (list t_bow
        t_spear
        t_thrown_boulder))

(define arms-types-needing-ammo
  (list t_bow))

(define (arms-type-is-blockable? karms)
  (display "arms-type-is-bloackable?")(newline)
  (in-list? karms blockable-arms-types))

(define (arms-type-needs-ammo? karms)
  (in-list? karms arms-types-needing-ammo))
