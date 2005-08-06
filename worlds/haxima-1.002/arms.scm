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
;;
;;----------------------------------------------------------------------------

;; Make the default number of required action points the same as the number of
;; turns it takes a human to cross normal terrain
(define default-rap norm) 

(kern-mk-sprite-set 'ss_arms 32 32 7 8 0 0 "arms.png")

;;--------------------------------------------------------------------------
;; Curried constructors
;;
;; These are for creating the standard classes of armaments. They simplify
;; things by filling in the blanks for all the boilerplate parameters of
;; the primitive kern-mk-arms-type procedure.
;;--------------------------------------------------------------------------

(define obj-ifc-cap (ifc-cap obj-ifc))

(define (mk-melee-arms-type tag name sprite to-hit-bonus damage deflect slots 
                            num-hands range weight)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0"  slots 
                     num-hands range default-rap nil #f #f weight nil obj-ifc-cap obj-ifc))

;; Curried constructor: missile weapon (add missile, ubiq flag to melee)
(define (mk-projectile-arms-type tag name sprite to-hit-bonus damage deflect 
                                 slots num-hands range missile ubiq weight)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0"  slots 
                     num-hands range default-rap missile #f ubiq weight nil obj-ifc-cap obj-ifc))

;; Curried constructor: thrown weapon (add field to melee)
(define (mk-thrown-arms-type tag name sprite to-hit-bonus damage deflect slots 
                             num-hands range ifc weight)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage deflect "0" slots 
                     num-hands range default-rap nil #t #f weight nil (ifc-cap ifc) ifc))

(define (mk-missile-arms-type tag name sprite damage ifc)
  (kern-mk-arms-type tag name sprite "0" damage "0" "0" slot-nil 0 0 0 nil #f #f 
                     0 nil (ifc-cap ifc) ifc))

(define (mk-armor-type tag name sprite to-hit armor slots weight)
  (kern-mk-arms-type tag name sprite to-hit "0" armor "0" slots 1 0 0 nil #f #f 
                     weight nil obj-ifc-cap obj-ifc))

(define (mk-shield-type tag name sprite to-hit deflect slots weight) 
  (kern-mk-arms-type tag name sprite to-hit "0" "0" deflect slots 1 0 0 nil #f #f 
                     weight nil obj-ifc-cap obj-ifc))

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

(define missile-arms-types
  (list
   ;;    ============================================================================================================
   ;;    tag           | name        | sprite        | damage | gifc
   ;;    ============================================================================================================
   (list 't_slingstone   "sling stone" s_sling_stone  "1d2"    nil)
   (list 't_arrow        "arrow"       s_arrow        "1d6"    obj-ifc)
   (list 't_bolt         "bolt"        s_bolt         "2d4"    obj-ifc)
   (list 't_warhead      "warhead"     s_warhead      "0"      warhead-ifc)
   (list 't_cannonball   "cannonball"  s_cannonball   "0"      obj-ifc)
   (list 't_poison_bolt  "poison bolt" s_poison_bolt  "1d6"    poison-bolt-ifc)
   (list 't_acid_bolt    "acid bolt"   s_acid_bolt    "1d2-1"  nil)
   (list 't_fireball     "fireball"    s_fireball     "2d6"    nil)
   (list 'deathball      "deathball"   s_deathball    "0"      deathball-ifc)
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

(define projectile-arms-types
  (list
   ;;     ========================================================================================================================
   ;;     tag         | name       | sprite     | to-hit | damage | to-def | slots       | hnds | rng | missile    | ubiq | weight
   ;;     ========================================================================================================================
   (list 't_sling      "sling"      s_sling      "-1"     "1d2"    "-1"     slot-weapon   1      4     t_slingstone #t      0)
   (list 't_bow        "bow"        s_bow        "1"      "1d4"    "-2"     slot-weapon   2      6     t_arrow      #f      2)
   (list 't_crossbow   "crossbow"   s_crossbow   "2"      "2d3"    "-3"     slot-weapon   2      5     t_bolt       #f      3)
   (list 't_doom_staff "doom staff" s_doom_staff "1d4"    "2d20"   "-5"     slot-weapon   2      12    t_warhead    #t      2)
   (list 't_acid_spray "acid spray" nil          "0"      "1d2-1"  "+0"     slot-nil      2      2     t_acid_bolt  #t      0)
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

(define thrown-boulder-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kplace x y)
                 (kern-place-set-terrain (mk-loc kplace x y) t_boulder)))))

(define thrown-arms-types
  (list
   ;;     ==========================================================================================================================================
   ;;     tag              | name          | sprite              | to-hit | damage | to-def | slots       | hnds | rng | ifc                | weight
   ;;     ==========================================================================================================================================
   (list  't_oil            "flaming oil"   s_flaming_oil          "-1"     "1d6"    "-2"     slot-weapon   1      4     flaming-oil-ifc      1)
   (list  't_spear          "spear"         s_spear                "-1"     "1d8"    "+1"     slot-weapon   2      4     obj-ifc              2)
   (list  't_thrown_boulder "loose boulder" s_thrown_boulder       "-2"     "2d6"    "-2"     slot-weapon   2      5     thrown-boulder-ifc   10)
   ))

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

(define melee-arms-types
  (list
   ;;     ===============================================================================================================
   ;;     tag          |    name          | sprite           | to-hit | damage | to-def | slots       | hnds | rng | weight
   ;;     ===============================================================================================================
   (list  't_hands          "bare hands"     nil              "1d2"    "1d2"    "1d2"    slot-nil      1      1      0)
   (list  't_fangs          "fangs"          nil              "1d2"    "1d6"    "+0"     slot-nil      1      1      0)
   (list  't_stinger        "stinger"        nil              "1d8"    "1d2-1"  "+0"     slot-nil      1      1      0)
   (list  't_dagger         "dagger"         s_dagger         "1d4"    "1d4"    "1d2"    slot-weapon   1      1      0)
   (list  't_mace           "mace"           s_mace           "1d3"    "1d6+1"  "+0"     slot-weapon   1      1      3)
   (list  't_sword          "sword"          s_sword          "1d2"    "1d8"    "1d2"    slot-weapon   1      2      2)
   (list  't_2H_axe         "2H axe"         s_2h_axe         "+0"     "2d4+2"  "-2"     slot-weapon   2      2      4)
   (list  't_2H_sword       "2H sword"       s_2h_sword       "0"      "2d8-1"  "+0"     slot-weapon   2      2      4)
   (list  't_morning_star   "morning star"   s_morning_star   "1d3+3"  "1d6+1"  "-1"     slot-weapon   2      2      3)
   (list  't_halberd        "halberd"        s_halberd        "1d4+2"  "2d8-2"  "+1"     slot-weapon   2      3      4)
   (list  't_staff          "staff"          s_staff          "1d6"    "1d4"    "1d6"    slot-weapon   2      2      2)
   (list  't_eldritch_blade "eldritch blade" s_eldritch_blade "2"      "2d8+5"  "+0"     slot-weapon   2      3      2)
   (list  't_mystic_sword   "mystic sword"   s_mystic_sword   "+3"     "1d10+1" "+2"     slot-weapon   1      2      1)
   (list  't_flaming_sword  "flaming sword"  s_flaming_sword  "1d2"    "1d10+3" "1d2"    slot-weapon   1      2      2)
   ))

(kern-mk-sprite 's_leather_helm  ss_arms 1 48 #f 0)
(kern-mk-sprite 's_chain_coif    ss_arms 1 49 #f 0)
(kern-mk-sprite 's_iron_helm     ss_arms 1 50 #f 0)
(kern-mk-sprite 's_leather_armor ss_arms 1 51 #f 0)
(kern-mk-sprite 's_chain_armor   ss_arms 1 52 #f 0)
(kern-mk-sprite 's_plate_armor   ss_arms 1 53 #f 0)

(define armor-types
  (list
   ;;     =============================================================================================================
   ;;     tag             | name          |  sprite           |  to-hit | armor  | slots      | weight
   ;;     =============================================================================================================
   (list   't_leather_helm  "leather helm"   s_leather_helm      "0"      "1"      slot-helm    0)
   (list   't_chain_coif    "chain coif"     s_chain_coif        "0"      "2"      slot-helm    1)
   (list   't_iron_helm     "iron helm"      s_iron_helm         "0"      "3"      slot-helm    2)
   (list   't_armor_leather "leather armor"  s_leather_armor     "+0"     "2"      slot-armor   2)
   (list   't_armor_chain   "chain armor"    s_chain_armor       "-1"     "4"      slot-armor   4)
   (list   't_armor_plate   "plate armor"    s_plate_armor       "-2"     "8"      slot-armor   8)
   ))

(kern-mk-sprite 's_shield            ss_arms 1 54 #f 0)
(kern-mk-sprite 's_scratched_shield  ss_arms 1 55 #f 0)

(define shield-types
  (list
   ;;     ===========================================================================================================
   ;;     tag                 | name             | sprite                | to-hit | deflect  | slots    | weight
   ;;     ===========================================================================================================
   (list   't_shield           "small shield"     s_shield                "-1"     "1"      slot-shield   2)
   (list   't_scratched_shield "scratched shield" s_scratched_shield      "0"      "3"      slot-shield   2)
   ))


(map (lambda (type) (apply mk-thrown-arms-type     type)) thrown-arms-types)
(map (lambda (type) (apply mk-projectile-arms-type type)) projectile-arms-types)
(map (lambda (type) (apply mk-melee-arms-type      type)) melee-arms-types)
(map (lambda (type) (apply mk-armor-type           type)) armor-types)
(map (lambda (type) (apply mk-shield-type          type)) shield-types)

;;----------------------------------------------------------------------------
;; Spiked Armor
;;----------------------------------------------------------------------------
(kern-mk-sprite 's_spiked_helm    ss_arms 1 45 #f 0)
(kern-mk-sprite 's_spiked_shield  ss_arms 1 46 #f 0)

(kern-mk-arms-type 't_spiked_helm "spiked helm" s_spiked_helm
                   "0" "1d4" "3" "0"
                   slot-helm 1 1 default-rap
                   nil #f #f
                   2 ;; weight
                   nil obj-ifc-cap obj-ifc)

(kern-mk-arms-type 't_spiked_shield "spiked shield" s_spiked_shield
                   "0" "1d5" "0" "1"
                   slot-shield 1 1 default-rap
                   nil #f #f
                   3 ;; weight
                   nil obj-ifc-cap obj-ifc)

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