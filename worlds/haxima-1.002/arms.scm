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
;;       AP_mod : modifier to max AP per round for the wielder
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

;; This keeps weapons proportional to the default cost, for a one line change between turn systems
(define (weap-ap mult)
	(floor (* mult default-weapon-rap)))
	
(define (armour-ap mult)
	(floor (* mult default-armour-apmod)))

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
               (lambda (kmissile kuser ktarget kplace x y dam)
				 ((car temp-ifc-state) kmissile kuser ktarget kplace x y dam)
                 ))))

(define temp-cannonball-state (list -1 -1))

(define (temp-cannonball-init x y)
	(set-car! temp-cannonball-state x)
	(set-car! (cdr temp-cannonball-state) y)
		)
                 
;;--------------------------------------------------------------------------
;; Curried constructors
;;
;; These are for creating the standard classes of armaments. They simplify
;; things by filling in the blanks for all the boilerplate parameters of
;; the primitive kern-mk-arms-type procedure.
;;--------------------------------------------------------------------------

(define obj-ifc-cap (ifc-cap obj-ifc))    

(define (mk-melee-arms-type tag name sprite to-hit-bonus damage deflect AP_cost AP_mod slots 
                            num-hands range weight
							stratt_mod dexatt_mod
							damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range AP_cost AP_mod nil nil #f #f weight nil
					 obj-ifc-cap obj-ifc stratt_mod dexatt_mod damage_mod avoid_mod mmode-smallobj))

;; Curried constructor: missile weapon (add missile, ubiq flag to melee)
(define (mk-projectile-arms-type tag name sprite to-hit-bonus damage deflect AP_cost AP_mod
                                 slots num-hands range projectile ammo ubiq weight
								 stratt_mod dexatt_mod damage_mod avoid_mod ifc)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range AP_cost AP_mod projectile ammo #f ubiq weight nil (ifc-cap ifc) ifc stratt_mod dexatt_mod damage_mod avoid_mod mmode-smallobj))

;; Curried constructor: thrown weapon (add field to melee)
(define (mk-thrown-arms-type tag name sprite to-hit-bonus damage deflect AP_cost AP_mod slots 
                             num-hands range missile ubiq ifc weight
							 stratt_mod dexatt_mod damage_mod avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit-bonus damage "0" deflect slots 
                     num-hands range AP_cost AP_mod missile nil #t ubiq weight nil (ifc-cap ifc) ifc stratt_mod dexatt_mod damage_mod avoid_mod mmode-smallobj))

(define (mk-ammo-arms-type tag name sprite ifc mmode)
  (kern-mk-arms-type tag name sprite "0" "0" "0" "0" slot-nil 0 0 0 0 nil nil #f #f 
                     0 nil (ifc-cap ifc) ifc 20 60 20 1.0 mmode))

(define (mk-missile-arms-type tag name sprite ifc mmode beam)
  (kern-mk-missile-type tag name sprite (ifc-cap ifc) ifc mmode beam beam))
                     
(define (mk-armor-type tag name sprite to-hit armor slots equiptime AP_mod weight avoid_mod)
  (kern-mk-arms-type tag name sprite to-hit "0" armor "0" slots 1 0 equiptime AP_mod nil nil #f #f 
                     weight nil obj-ifc-cap obj-ifc 20 60 20 avoid_mod mmode-largeobj))

(define (mk-shield-type tag name sprite to-hit deflect AP_mod slots weight avoid_mod) 
  (kern-mk-arms-type tag name sprite to-hit "0" "0" deflect slots 1 0 default-weapon-rap AP_mod nil nil #f #f 
                     weight nil obj-ifc-cap obj-ifc 20 60 20 avoid_mod mmode-largeobj))

;; ============================================================================
;; Missiles for Projectile Weapons & Spells
;; ============================================================================

(kern-mk-sprite 's_sling_stone               ss_arms 1  0  #f   0 )
(kern-mk-sprite 's_warhead                   ss_arms 1  1  #f   0 )
(kern-mk-sprite 's_cannonball                ss_arms 1  2  #f   0 )
(kern-mk-sprite 's_fireball                  ss_arms 1  3  #f   0 )
(kern-mk-sprite 's_deathball                 ss_arms 1  4  #f   0 )
(kern-mk-sprite 's_arrow                     ss_arms 1  8  #f 495 )
(kern-mk-sprite 's_bolt                      ss_arms 1 80  #f 495 )
(kern-mk-sprite 's_arrowobj                  ss_arms 1 68  #f   0 )
(kern-mk-sprite 's_arrowstack                ss_arms 1 69  #f   0 )
(kern-mk-sprite 's_boltobj                   ss_arms 1 70  #f   0 )
(kern-mk-sprite 's_boltstack                 ss_arms 1 71  #f   0 )
(kern-mk-sprite 's_poison_bolt               ss_arms 1 16  #f 170 )
(kern-mk-sprite 's_acid_bolt                 ss_arms 1 20  #f 170 )
(kern-mk-sprite 's_thrownweb                 ss_arms 1 31  #f   0 )
(kern-mk-sprite 's_prismatic_bolt            ss_arms 4 100  #f   0 )
(kern-mk-sprite 's_squat_bubbly_green_potion ss_arms 1 30  #f   0 )
(kern-mk-sprite 's_thrown_green_potion       ss_arms 4 104  #f   0 )

;; ----------------------------------------------------------------------------
;; mk-missile-ifc -- automate missile ifc creation. 'pred?' takes an object as
;; a parameter and returns true iff the 'hit' proc should be applied to it.
;; ----------------------------------------------------------------------------
(define (mk-missile-ifc hit)
  (ifc '()
       (method 'hit-loc (lambda (kmissile kuser ktarget kplace x y dam)
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

(define lightningbolt-ifc 
	(ifc '()
		(method 'enter
			(lambda (kmissile kplace x y)
				((car temp-ifc-state) kmissile nil nil kplace x y 0)
				))
		))

(define (on-hit-nontarget ktarget loc dam proc)
	(for-each proc
		(if (> dam -1)
   		(filter (lambda (obj) (not (equal? obj ktarget)))
           	(kern-get-objects-at loc))
      	(kern-get-objects-at loc)
           	))
)

(define (on-hit-target ktarget dam proc)
	(if (> dam -1)
		(proc ktarget)
	))

;; fireball-hit -- when a fireball hits it burns all characters and leaves a
;; fire 
(define fireball-ifc
  (ifc '()
       (method 'hit-loc
               (lambda (kmissile kuser ktarget kplace x y dam)
               	(let* (
               			(havemana (> (kern-char-get-mana kuser) 0))
               			(usedmana (if (and havemana (equal? (kern-dice-roll "1d15") 1))
               						(
               							begin
               							(kern-char-set-mana kuser (- (kern-char-get-mana kuser) 1))
               							#t
               						)
               						#f))
               			(setfire (and usedmana (equal? (kern-dice-roll "1d3") 1)))
               			(loc (mk-loc kplace x y))
               			(hurt (> dam 0))
               			(targdamage (cond
               								(usedmana (if hurt "2d5+3" "2d4+2"))
               								(havemana (if hurt "2d4+2" "2d3+2"))
               								(else (if hurt "1d4-1" "1d2-1"))
               								))
               			(othdamage (cond
               								(usedmana "2d3+2")
               								(havemana "1d4")
               								(else "0")
               								))
               			)
               		(if (and setfire (terrain-ok-for-field? loc))
              				(kern-obj-put-at (kern-mk-field F_fire (kern-dice-roll "1d5")) loc))
              			(if (not havemana)
              					(kern-log-msg "Attack fizzles!"))
              			(on-hit-target ktarget dam 
              				(lambda (obj) (generic-burn obj targdamage))
              			)
               		(if havemana
               			(on-hit-nontarget ktarget loc dam 
               				(lambda (obj) (generic-burn obj othdamage)))
               			)                         
               ))
			)))
			
(define (prismatic-acid ktarget power)
	(if (and (kern-obj-is-char? ktarget)
			(contest-of-skill power
				(occ-ability-dexdefend ktarget)))
		(apply-acid ktarget)))
		
(define (prismatic-slip ktarget power)
	(if (and (kern-obj-is-char? ktarget)
			(contest-of-skill power
			(occ-ability-dexdefend ktarget)))
		(slip ktarget)))
		
(define prismatic-bolt-ifc
	(ifc '()
       (method 'hit-loc
               (lambda (kmissile kuser ktarget kplace x y dam)
               	(let* (
               			(havemana (> (kern-char-get-mana kuser) 0))
               			(usedmana (if (and havemana (equal? (kern-dice-roll "1d15") 1))
               						(
               							begin
               							(kern-char-set-mana kuser (- (kern-char-get-mana kuser) 1))
               							#t
               						)
               						#f))
               			(magpower (if havemana
               					(if usedmana (max 7 (occ-ability-blackmagic kuser)) 5)
               					0))
               			(loc (mk-loc kplace x y))
               			(hit (> dam -1))
               			(hurt (> dam 0))
               			(havetarget (not (eqv? ktarget '())))
               			(pristype (kern-dice-roll "1d100"))
               			(proclist
               			
              					(cond ((< pristype 10)
              							(list nil
              						(lambda (obj) (powers-paralyse kuser obj magpower))
              						(lambda (obj) (powers-paralyse kuser obj (- magpower 3)))))
              										
              						((< pristype 20)
              							(list nil
              						(lambda (obj) (prismatic-acid obj magpower))
              						(lambda (obj) (prismatic-acid obj (- magpower 3)))))
              						          						
           							((< pristype 30)
           								(list nil
            						(lambda (obj) (powers-poison-effect kuser obj (+ magpower 3)))
              						(lambda (obj) (powers-poison-effect kuser obj (- magpower 2)))))
              						
              						((< pristype 40)
           								(list nil
            						(lambda (obj) (generic-burn obj "2d3+2"))
            						(lambda (obj) (generic-burn obj "1d5"))))
              							
              						((< pristype 50)
           								(list nil
            						(lambda (obj) (apply-lightning obj))
            						(lambda (obj) (apply-lightning obj))))
            					
										((< pristype 60)
           								(list nil
            						(lambda (obj) (prismatic-slip obj (+ magpower 5)))
              						(lambda (obj) (prismatic-slip obj (+ magpower 2)))))
              							
    									((< pristype 70)
           								(list
           							(lambda (loc) (powers-field-energy-weak kuser loc magpower))
           							nil nil))

    									((< pristype 80)
           								(list
           							(lambda (loc) (powers-field-fire-weak kuser loc magpower))
           							nil nil))
           							
    									((< pristype 90)
           								(list
           							(lambda (loc) (powers-field-poison-weak kuser loc magpower))
           							nil nil))
           							
    									((< pristype 101) 
           								(list
           							(lambda (loc) (powers-field-sleep-weak kuser loc magpower))
           							nil nil))
               			)))
              			(if (not havemana)
              					(kern-log-msg "Attack fizzles!")
              					(begin
              						(if (not (null? (car proclist)))
              							((car proclist) loc))
              						(if (not (null? (cadr proclist)))
              							(on-hit-target ktarget dam (cadr proclist)))
              						(if (not (null? (caddr proclist)))
              							(on-hit-nontarget ktarget loc dam (caddr proclist)))
              					)
              				)
              		))
			)))
			
(define warhead-ifc
  (ifc nil
       (method 'hit-loc 
               (lambda (kmissile kuser ktarget kplace x y dam)
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))
                                  
(kern-mk-sprite 's_flaming_oil    ss_arms 4 96 #f 0)
(kern-mk-sprite 's_oil_potion     ss_arms 1 5 #f 0)
(kern-mk-sprite 's_spear          ss_arms 1 88 #f 495 )
(kern-mk-sprite 's_spearobj       ss_arms 1 6 #f 0)
(kern-mk-sprite 's_throwing_axe   ss_arms 1 29 #f 0)
(kern-mk-sprite 's_thrown_axe     ss_arms 8 72 #f 0)
(kern-mk-sprite 's_thrown_boulder ss_arms 1 7 #f 0)
(kern-mk-sprite 's_smoke_bomb     ss_arms 4 112 #f 0)
(kern-mk-sprite 's_smoke_potion   ss_arms 1 108 #f 0)

(define flaming-oil-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kuser ktarget kplace x y dam)
                 (kern-obj-put-at (kern-mk-obj F_fire 1) 
                                  (mk-loc kplace x y))))))

(define vial-of-slime-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kuser ktarget kplace x y dam)
                 (let* ((lvl (kern-dice-roll "1d3+5"))
                        (knpc (spawn-npc 'green-slime lvl))
                        (loc (pick-loc (mk-loc kplace x y) knpc)))
                   (cond ((null? loc) 
                          (kern-obj-dec-ref knpc)
                          0)
                         (else
                          (kern-being-set-base-faction knpc (kern-being-get-base-faction kuser))
                          (kern-obj-set-temporary knpc #t)
                          (kern-obj-put-at knpc loc))))))))

(define smoke-bomb-ifc
	(ifc obj-ifc
		(method 'hit-loc
			(lambda (kmissile kuser ktarget kplace x y dam)
				(fields-smoke-apply kplace x y 10)
		))))
                          
(define (mk-drop-proj-ifc type-tag prob)
	(ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kuser ktarget kplace x y dam)
               	(if (< (kern-dice-roll "1d100") prob)
               		(let ((dropobj (kern-mk-obj (eval type-tag) 1))
               				(loc (mk-loc kplace x y)))
               			(if (can-be-dropped? dropobj loc cant)
               				(kern-obj-put-at dropobj loc)
               	))))
      )))
            
;; todo: handle possibility that magicaxe doesnt have a wielder?
(define magicaxe-ifc
	(ifc obj-ifc
		(method 'hit-loc 
			(lambda (kmissile kuser ktarget kplace x y dam)
				(kern-fire-missile (eval 't_returning_axe_p) (mk-loc kplace x y) (kern-obj-get-location kuser))
				(kern-log-msg "Magic axe returns!")
			)
 	))
)
                   
                          
(define missile-arms-types
  (list
   ;;    ==================================================================================================
   ;;    tag                 | name          | sprite          | gifc              | movement_mode | beam
   ;;    ====================================================================================================
   (list 't_slingstone        "sling stone"    s_sling_stone     obj-ifc             mmode-missile  	#f)
   (list 't_arrow_p           "arrow"          s_arrow           (mk-drop-proj-ifc 't_arrow 5)
                                                                                     mmode-missile  	#f  )
   (list 't_bolt_p            "bolt"           s_bolt            (mk-drop-proj-ifc 't_bolt 5)             
                                                                                     mmode-missile  	#f  )
   (list 't_warhead_p         "warhead"        s_warhead         warhead-ifc         mmode-missile  	#f  )
   (list 't_cannonball_p      "cannonball"     s_cannonball      obj-ifc             mmode-missile  	#f  )

   
   (list 't_poison_bolt       "poison bolt"    s_poison_bolt     poison-bolt-ifc     mmode-missile  	#f  )
   (list 't_acid_bolt         "acid bolt"      s_acid_bolt       acid-bolt-ifc       mmode-missile  	#f  )
   (list 't_fireball          "fireball"       s_fireball        fireball-ifc        mmode-missile  	#f  )
   (list 't_deathball         "deathball"      s_deathball       deathball-ifc       mmode-missile  	#f  )
   (list 't_slimeglob         "slime glob"     s_acid_bolt       obj-ifc             mmode-missile  	#f  )
   (list 't_mfireball         "fireball"       s_fireball        temp-ifc            mmode-missile  	#f  )
   (list 't_mpoison_bolt      "poison bolt"    s_poison_bolt     temp-ifc            mmode-missile  	#f  )
   (list 't_prismatic_bolt    "prismatic bolt" s_prismatic_bolt  prismatic-bolt-ifc  mmode-missile  	#f  )
   (list 't_stunball   			"stun ball" 	  s_lightning    stunball-ifc		 mmode-missile  	#f  )
   (list 't_lightning_bolt  	"lightning bolt"	s_lightning      lightningbolt-ifc   mmode-missile  	#t  )  
   (list 't_magicarrow_p      "arrow"          s_arrow           obj-ifc             mmode-missile  	#f  )
 
   
   
   (list 't_mweb              "web"            s_thrownweb       temp-ifc            mmode-missile  	#f  )
   (list 't_oil_p             "flaming oil"    s_flaming_oil     flaming-oil-ifc     mmode-missile  	#f  )
   (list 't_smoke_bomb_p      "smoke bomb"     s_smoke_bomb      smoke-bomb-ifc      mmode-missile  	#f  )
   (list 't_spear_p           "spear"          s_spear           (mk-drop-proj-ifc 't_spear 25)             
                                                                                     mmode-missile  	#f  )
   (list 't_thrown_axe_p      "thrown axe"     s_thrown_axe      magicaxe-ifc        mmode-missile  	#f  )
   (list 't_returning_axe_p   "thrown axe"     s_thrown_axe      obj-ifc              mmode-return  	#f  )
   (list 't_thrown_rock_p     "thrown rock"    s_cannonball      (mk-drop-proj-ifc 't_thrown_rock 80)             
                                                                                     mmode-missile  	#f  )
   (list 't_thrown_boulder_p  "hurled boulder" s_thrown_boulder  (mk-drop-proj-ifc 't_thrown_boulder 80)             
                                                                                     mmode-missile  	#f  )

   (list 't_slime_vial_p      "vial of slime"  s_thrown_green_potion vial-of-slime-ifc  mmode-missile  	#f  )

   ))

(map (lambda (type) (apply mk-missile-arms-type type)) missile-arms-types)  
         
                          
(define ammo-arms-types
  (list
   ;;    ===========================================================================================
   ;;    tag                 | name          | sprite          | gifc              | movement_mode 
   ;;    ===========================================================================================

   (list 't_arrow             "arrow"          s_arrowobj        obj-ifc             mmode-smallobj )
   (list 't_bolt              "bolt"           s_boltobj         obj-ifc             mmode-smallobj )
   (list 't_warhead           "warhead"        s_warhead         warhead-ifc         mmode-smallobj )
   (list 't_cannonball        "cannonball"     s_cannonball      obj-ifc             mmode-smallobj )
   ))
   
   
;; If we don't create these missile types now, we won't be able to refer to
;; them below in the projectile-arms-types table. For example, t_bow needs to
;; refer to t_arrow. But the interpreter won't recognize t_arrow as a variable
;; name until we call this procedure to create the t_arrow type.
(map (lambda (type) (apply mk-ammo-arms-type type)) ammo-arms-types)

;; ============================================================================
;; Projectile Weapons
;; ============================================================================

(kern-mk-sprite 's_sling      ss_arms 1 24 #f 0)
(kern-mk-sprite 's_bow        ss_arms 1 25 #f 0)
(kern-mk-sprite 's_crossbow   ss_arms 1 26 #f 0)
(kern-mk-sprite 's_doom_staff ss_arms 1 27 #f 0)
(kern-mk-sprite 's_stun_wand  ss_arms 1 28 #f 0)

(define proj-ifc
	(ifc obj-ifc
		(method 'on-attack
			(lambda (kuser)
				(println "oa")
				(kern-sound-play-at sound-missile (kern-obj-get-location kuser))
			)
 	))
)

(define projectile-arms-types
  (list
   ;;     =========================================================================================================================================================================================
   ;;     tag            | name           |  sprite     | to-hit | damage | to-def | AP_cost | AP_mod       | slots       | hnds | rng | missile        | ammo  | ubiq | weight | stratt | dexatt | dammod | avoid | ifc
   ;;     =========================================================================================================================================================================================
   (list 't_sling          "sling"           s_sling      "1d2-2"  "1d4"    "-1"      (weap-ap 1) 0   slot-weapon   1      4     t_slingstone     nil     #t     0        10       60       30       0.9	proj-ifc)
   (list 't_sling_4        "+4 sling"        s_sling      "+3"     "1d4+4"  "+0"      (weap-ap 1) 0   slot-weapon   1      6     t_slingstone     nil     #t     0        10       60       30       0.9	proj-ifc)

   (list 't_self_bow       "self bow"        s_bow        "+1"     "1d6"    "-2"      (weap-ap 0.8) 0   slot-weapon   2      4     t_arrow_p        t_arrow #f     2        10       70       20       0.9  proj-ifc)
   (list 't_bow            "bow"             s_bow        "1d3-2"  "2d4"    "-2"     (weap-ap 1) 0   slot-weapon   2      5     t_arrow_p        t_arrow #f     2        10       70       20       0.9  proj-ifc)
   (list 't_long_bow       "longbow"         s_bow        "1d3-2"  "2d6+1"  "-2"     (weap-ap 1.2) 0   slot-weapon   2      6     t_arrow_p        t_arrow #f     2        10       70       20       0.9  proj-ifc)
   (list 't_great_bow      "great bow"       s_bow        "1d3-2"  "2d6+3"  "-2"     (weap-ap 1.34) 0   slot-weapon   2      7     t_arrow_p        t_arrow #f     2        10       70       20       0.9  proj-ifc)

   (list 't_lt_crossbow    "light crossbow"  s_crossbow   "1d4-2"  "2d5"    "-1"     (weap-ap 1) 0   slot-weapon   2      5     t_bolt_p         t_bolt  #f     3         0       80        0       0.95 proj-ifc)
   (list 't_crossbow       "crossbow"        s_crossbow   "1d4-2"  "4d4"    "-1"     (weap-ap 1) 0   slot-weapon   2      6     t_bolt_p         t_bolt  #f     3         0       80        0       0.95 proj-ifc)
   (list 't_hvy_crossbow   "heavy crossbow"  s_crossbow   "1d4-2"  "4d6+2"  "-1"     (weap-ap 2) 0   slot-weapon   2      7     t_bolt_p         t_bolt  #f     3         0       80        0       0.95 proj-ifc)
   (list 't_trpl_crossbow  "triple crossbow" s_crossbow   "1d4-2"  "2d5"    "-1"      (weap-ap 0.67) 0   slot-weapon   2      5     t_bolt_p         t_bolt  #f     3         0       80        0       0.95 proj-ifc)

   (list 't_doom_staff     "doom staff"      s_doom_staff "1d4"    "1d2"    "+2"     (weap-ap 1) 0    slot-weapon   2      5     t_fireball       nil     #t     2         0       50        0       1.0  proj-ifc)
   (list 't_acid_spray     "acid spray"      nil          "-7"     "1d6"    "+0"     (weap-ap 1) 0    slot-nil      2      2     t_slimeglob      nil     #t     0        10       50       20       1.0  proj-ifc)
   (list 't_fire_glob      "fire glob"       nil          "-8"     "1d6"    "+0"     (weap-ap 1) 0    slot-nil      2      2     t_fireball       nil     #t     0        10       50       20       1.0  proj-ifc)
   (list 't_stun_wand      "stun wand"       s_stun_wand  "-2"     "1d4"    "-1"     (weap-ap 1) 0    slot-weapon   1      6     t_stunball       nil     #t     2         0       80        0       1.0  proj-ifc)
   (list 't_acid_wand      "acid wand"       s_stun_wand  "-2"     "1d4"    "-1"     (weap-ap 1) 0    slot-weapon   1      6     t_acid_bolt      nil     #t     2         0       80        0       1.0  proj-ifc)
   (list 't_prismatic_gaze "prismatic gaze"  nil          "1d4"    "0"      "+0"     (weap-ap 1) 0    slot-nil      1      3     t_prismatic_bolt nil     #t     0         0        0        0       0.85 proj-ifc)
   ))

;; ============================================================================
;; Thrown Weapons
;; ============================================================================

   
(define thrown-arms-types
  (list
   ;;     =================================================================================================================================================================================================================
   ;;     tag              | name          | sprite                   | to-hit | dmg | to-def | AP_cost | AP_mod        | slots       | hnds | rng | missile          | ubiq | ifc              | weight | stratt | dexatt | dammod | avoid
   ;;     =================================================================================================================================================================================================================
   (list  't_thrown_rock    "small rock"    s_cannonball                "-2"     "1d2"    "-2"   (weap-ap 1.33) 0  slot-weapon   1      4     t_thrown_rock_p    #t     obj-ifc             1       20       20         0      0.9 )
   (list  't_thrown_boulder "loose boulder" s_thrown_boulder            "-2"     "3d4+1"  "-2"  (weap-ap 2) 0  slot-weapon   2      5     t_thrown_boulder_p #f     obj-ifc            10       40       20        60      0.9 )

   (list  't_spear          "spear"         s_spearobj                  "+1"     "1d8+1"  "+1"  (weap-ap 1) 0  slot-weapon   1      4     t_spear_p          #f     obj-ifc             2       30       60        40      1.0 )
   (list  't_magic_axe      "magical axe"   s_throwing_axe              "+2"     "2d4+2"  "+0"  (weap-ap 1) 0  slot-weapon   1      4     t_thrown_axe_p     #t     obj-ifc             2       30       60        40      1.0 )

   (list  't_oil            "flaming oil"   s_oil_potion                "-1"     "1d6"    "-2"  (weap-ap 1.2) 0  slot-weapon   1      4     t_oil_p            #f     flaming-oil-ifc     1       20       30         0      0.9 )
   (list  't_slime_vial     "vial of slime" s_squat_bubbly_green_potion "-1"     "1d2"    "-2"  (weap-ap 1.2) 0  slot-weapon   1      4     t_slime_vial_p     #f     vial-of-slime-ifc   1       20       30         0      1.0 )
   (list  't_smoke_bomb     "smoke bomb"    s_smoke_potion                "-1"     "1"      "-2"  (weap-ap 1.2) 0  slot-weapon   1      6     t_smoke_bomb_p     #f     smoke-bomb-ifc      1       20       30         0      0.9 )
   ))

(map (lambda (type) (apply mk-thrown-arms-type type)) thrown-arms-types)  
   
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
(kern-mk-sprite 's_eldritch_blade ss_arms 2 40 #f 0)
(kern-mk-sprite 's_mystic_sword   ss_arms 2 42 #f 0)
(kern-mk-sprite 's_flaming_sword  ss_arms 2 44 #f 0)

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
   ;;     tag          |    name           | sprite         | to-hit | damage | to-def | AP_cost | AP_mod | slots | hnds | rng | weight | dxmod | stmod | dammod | avoid
   ;;     ===================================================================================================================================================
   (list  't_hands          "bare hands"     nil              "1d2"    "1d2"    "1d2"    (weap-ap 0.67) 0 slot-nil      1      1     0        50      20       10      1.0  )
   (list  't_F_fangs        "fangs"          nil              "1d2"    "1d4"    "+0"     (weap-ap 0.67) 0 slot-nil      1      1     0        50      20       30      1.0  )
   (list  't_fangs          "fangs"          nil              "1d2"    "1d6"    "+0"      (weap-ap 1)   0 slot-nil      1      1     0        50      20       30      1.0  )
   (list  't_G_fangs        "great fangs"    nil              "1d2"    "1d10"   "+0"     (weap-ap 1.34) 0 slot-nil      1      1     0        50      20       30      1.0  )
   (list  't_horns          "horns"          nil              "1d2"    "1d8"    "1d2"    (weap-ap 0.67) 0 slot-nil      1      1     0        30      40       60      1.0  )
   (list  't_stinger        "stinger"        nil              "1d2"    "1d2"    "+0"     (weap-ap 0.67) 0 slot-nil      1      1     0        50      20       10      1.0  )
   (list  't_tentacles      "tentacles"      nil              "1d3"    "4d4"    "4d2"     (weap-ap 1)   0 slot-nil      1      1     0        70      20       60      1.0  )
   (list  't_beak           "beak"           nil              "+0"     "2d4"    "+0"      (weap-ap 1)   0 slot-nil      1      1     0        50      30       30      1.0  )
   (list  't_pincers        "pincers"        nil              "-1"     "4d4"    "4d2"     (weap-ap 1.2) 0 slot-nil      1      1     0        50      30       30      1.0  )

   (list  't_dagger         "dagger"         s_dagger         "1d4"    "1d4"    "1d2"     (weap-ap 0.8) 0 slot-weapon   1      1     0        80      10       10      1.0  )
   (list  't_dagger_4       "+4 dagger"      s_dagger         "1d4+4"  "1d4+4"  "1d2+4"   (weap-ap 0.8) 0 slot-weapon   1      1     0        80      10       10      1.0  )
   (list  't_mace           "mace"           s_mace           "1d4"    "1d6+2"  "+0"      (weap-ap 1)   0 slot-weapon   1      1     3        20      60       80      0.95 )
   (list  't_axe            "axe"            s_axe            "1d2"    "2d4+2"  "+0"      (weap-ap 1.2) 0 slot-weapon   1      1     3        30      50       90      0.95 )
   (list  't_sword          "sword"          s_sword          "1d2"    "1d8+1"  "1d2"     (weap-ap 1)   0 slot-weapon   1      1     2        50      20       70      1.0  )
   (list  't_sword_2        "+2 sword"       s_sword          "1d2+2"  "1d8+3"  "1d2+2"   (weap-ap 1)   0 slot-weapon   1      1     2        50      20       70      1.0  )
   (list  't_sword_4        "+4 sword"       s_sword          "1d2+4"  "1d8+5"  "1d2+4"   (weap-ap 1)   0 slot-weapon   1      1     2        50      20       70      1.0  )
   (list  't_2H_axe         "2H axe"         s_2h_axe         "+0"     "4d4+4"  "-2"     (weap-ap 1.34) 0 slot-weapon   2      1     4        20      60      100      0.9  )
   (list  't_2H_sword       "2H sword"       s_2h_sword       "+0"     "2d8+2"  "+1"      (weap-ap 1.2) 0 slot-weapon   2      1     4        40      40       90      0.95 )
   (list  't_morning_star   "morning star"   s_morning_star   "1d2+2"  "1d6+1"  "-1"      (weap-ap 1)   0 slot-weapon   1      2     3        20      40       70      0.9  )
   (list  't_morning_star_2 "+2 morning star" s_morning_star  "1d2+4"  "1d6+3"  "+2"      (weap-ap 1)   0 slot-weapon   1      2     3        20      40       70      0.9  )
   (list  't_halberd        "halberd"        s_halberd        "1d3+1"  "2d8-2"  "1d2"     (weap-ap 1)   0 slot-weapon   2      2     4        30      30      100      0.9  )
   (list  't_staff          "staff"          s_staff          "1d3"    "1d4"    "1d3"     (weap-ap 0.8) 0 slot-weapon   2      2     2        60      30       40      1.0  )
   (list  't_eldritch_blade "eldritch blade" s_eldritch_blade "+2"     "3d7+5"  "+0"     (weap-ap 1.34) 0 slot-weapon   2      1     2        50      20       70      1.0  )
   (list  't_mystic_sword   "mystic sword"   s_mystic_sword   "+3"     "1d10+5" "+2"      (weap-ap 1)   0 slot-weapon   1      1     1        60      20       70      1.0  )
   ))
   
(kern-mk-sprite 's_leather_helm  ss_arms 1 48 #f 0)
(kern-mk-sprite 's_chain_coif    ss_arms 1 49 #f 0)
(kern-mk-sprite 's_iron_helm     ss_arms 1 50 #f 0)
(kern-mk-sprite 's_leather_armor ss_arms 1 51 #f 0)
(kern-mk-sprite 's_chain_armor   ss_arms 1 52 #f 0)
(kern-mk-sprite 's_plate_armor   ss_arms 1 53 #f 0)

(define armor-types
  (list
   ;;     ===============================================================================================================
   ;;     tag               | name            |  sprite        |  to-hit | armor  | slots     | equip_AP | AP_mod | weight | avoid 
   ;;     ===============================================================================================================
   (list   't_leather_helm    "leather helm"     s_leather_helm   "-1"     "1d2"    slot-helm    (weap-ap 1) -0  0  1.0  )
   (list   't_leather_helm_2  "+2 leather helm"  s_leather_helm   "+0"     "1d2+2"  slot-helm    (weap-ap 1) -0  0  1.0  )
   (list   't_leather_helm_4  "+4 leather helm"  s_leather_helm   "+0"     "1d2+4"  slot-helm    (weap-ap 1) -0  0  1.0  )

   (list   't_chain_coif      "chain coif"       s_chain_coif     "-1"     "1d3"    slot-helm    (weap-ap 1) (armour-ap -1)  1  0.9  )
   (list   't_chain_coif_4    "+4 chain coif"    s_chain_coif     "+0"     "1d3+4"  slot-helm    (weap-ap 1) (armour-ap -1)  1  0.9  )

   (list   't_iron_helm       "iron helm"        s_iron_helm      "-1"     "1d4"    slot-helm    (weap-ap 1) (armour-ap -2)  2  0.9  )
   (list   't_iron_helm_4     "+4 iron helm"     s_iron_helm      "+0"     "1d4+4"  slot-helm    (weap-ap 1) (armour-ap -2)  2  0.9  )

   (list   't_armor_leather   "leather armor"    s_leather_armor  "-1"     "1d4"    slot-armor   (weap-ap 2) (armour-ap -1)  2  0.85 )
   (list   't_armor_leather_2 "+2 leather armor" s_leather_armor  "+0"     "1d4+2"  slot-armor   (weap-ap 2) (armour-ap -1)  2  0.85 )
   (list   't_armor_leather_4 "+4 leather armor" s_leather_armor  "+0"     "1d4+4"  slot-armor   (weap-ap 2) (armour-ap -1)  2  0.9  )

   (list   't_armor_chain     "chain armor"      s_chain_armor    "-2"     "2d4"    slot-armor   (weap-ap 2) (armour-ap -5)  4  0.7  )
   (list   't_armor_chain_4   "+4 chain armor"   s_chain_armor    "+0"     "2d4+4"  slot-armor   (weap-ap 2) (armour-ap -5)  4  0.8  )

   (list   't_armor_plate     "plate armor"      s_plate_armor    "-4"     "4d4"    slot-armor   (weap-ap 5) (armour-ap -10) 8  0.6  )
   (list   't_armor_plate_4   "+4 plate armor"   s_plate_armor    "+0"     "4d4+4"  slot-armor   (weap-ap 5) (armour-ap -10) 8  0.7  )
   ))	

(kern-mk-sprite 's_shield            ss_arms 1 54 #f 0)
(kern-mk-sprite 's_scratched_shield  ss_arms 1 55 #f 0)

(define shield-types
  (list
   ;;     ============================================================================================================
   ;;     tag                 | name             | sprite           | to-hit | deflect | AP_mod | slots      | weight | avoid  
   ;;     ============================================================================================================
   (list   't_shield           "small shield"     s_shield            "-1"     "5"    -0  slot-shield  2         0.9  )
   (list   't_shield_4         "+4 small shield"  s_shield            "+0"     "9"    -0  slot-shield  2         0.95 )
   (list   't_scratched_shield "scratched shield" s_scratched_shield  "+0"     "7"    -0  slot-shield  2         0.9  )
   ))


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
                   slot-helm 1 1 (weap-ap 2) -0
                   nil nil #f #f
                   2 ;; weight
                   nil obj-ifc-cap obj-ifc
				   30 10 20 0.9 mmode-smallobj)

(kern-mk-arms-type 't_spiked_shield "spiked shield" s_spiked_shield
                   "0" "1d5" "0" "5"
                   slot-shield 1 1 (weap-ap 2) -0
                   nil nil #f #f
                   3 ;; weight
                   nil obj-ifc-cap obj-ifc
				   40 20 20 0.8 mmode-largeobj)

;;--------------------------------------------------------------------------
;; Special arms types
;;
;; These don't fit into the mold for any standard arms type.
;;--------------------------------------------------------------------------

(define flaming-sword-ifc
  (ifc obj-ifc
       (method 'hit-loc 
               (lambda (kmissile kuser ktarget kplace x y dam)
               	(cond ((equal? dam 0)
               				(generic-burn ktarget "1d5-2"))
               			((> dam 0)
               				(generic-burn ktarget "2d4"))
               	))
         )))          
							
(kern-mk-arms-type 't_flaming_sword "flaming sword" s_flaming_sword "1d2" "1d8+2" "0" "1d2" slot-weapon 1 1 (weap-ap 1) 0 nil nil #f #f 2 nil
					 (ifc-cap flaming-sword-ifc) flaming-sword-ifc 50 20 70 1.0 mmode-smallobj)


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
                   (weap-ap 2.0)     ;;          rap : required action points to attack with it
                   0                 ;;       AP_mod : modifier to max AP per round for the wielder
                   t_cannonball_p    ;;
                   nil		     ;;      missile : nil or the armament type it fires
                   #f                ;;       thrown : true or false
                   #t                ;;         ubiq : true if it needs ammo in inventory, false otherwise
                   0                 ;;       weight : unused
                   sound-cannon-fire ;;   fire-sound : string name of sound file to play when it's fired
                   0                 ;;      ifc-cap : integer bitmap describing interface slots
                   nil               ;;  get-handler : script ifc
				   0 0 0 1.0
				   mmode-largeobj
                   )

;;----------------------------------------------------------------------------
;; This list of "blockable" arms types is used by combat ai. An arms type is
;; "blockable" if an adjacent enemy can interfere with its usage.
;;----------------------------------------------------------------------------
(define blockable-arms-types
  (list t_sling t_sling_4
        t_self_bow t_bow t_long_bow t_great_bow
        t_hvy_crossbow t_trpl_crossbow
        t_spear
        t_thrown_rock t_thrown_boulder ))
; t_lt_crossbow is quick to load, and can be used in melee

(define arms-types-needing-ammo
  (list t_self_bow t_bow t_long_bow t_great_bow
        t_lt_crossbow t_crossbow t_hvy_crossbow t_trpl_crossbow ))

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

;;--------------------------------------------------------------------------
;; Cannon mounting for shipboard combat (and maybe anywhere else we can think of later)
;;--------------------------------------------------------------------------

;; uglyhack find target location or set up 'safe' location to simulate cannonball leaving play area
(define (arms-searchline place x y dx dy)
	(let* ((wid (kern-place-get-width place))
		(hgt (kern-place-get-height place)))
		(define (arms-searchline-iter ix iy)
			(cond ((< ix 0) (list 0 iy #f))
				((< iy 0) (list ix 0 #f))
				((>= ix wid) (list (- wid 1) iy #f))
				((>= iy hgt) (list ix (- wid 1) #f))
				((not (null? (get-being-at (mk-loc place ix iy))))
					(list ix iy #t))
				(else (arms-searchline-iter (+ ix dx) (+ iy dy)))
			))
		(let* ((target (arms-searchline-iter (+ x dx) (+ y dy)))
				(tx (car target))
				(ty (cadr target))
				(havet (caddr target))
				)
			(if havet
				(temp-cannonball-init -1 -1)
				(temp-cannonball-init tx ty)
			)
			(list tx ty)
			)))
			
(define localcannonball-ifc
	(ifc '()
		(method 'hit-loc 
			(lambda (kmissile kuser ktarget kplace x y dam)
				(let ((ktarget (get-being-at (mk-loc kplace x y))))
					(if (not (null? ktarget))
						(
							begin
							(kern-log-msg (kern-obj-get-name ktarget) " hit by cannonball!")
							(kern-obj-apply-damage ktarget "cannon" (kern-dice-roll "1d10+4"))
						)
					)
				))
		)))
				
(mk-missile-arms-type 't_localcannonball "cannonball" s_cannonball localcannonball-ifc mmode-cannon #f)
		
(define cannon-ifc
	(ifc '()
		(method 'xamine 
			(lambda (kcannon kuser)
				(let ((ready (cadr (gob kcannon))))
					(kern-log-msg "The cannon is "
						(cond ((equal? ready 2) 
							 "ready to fire")
							 ((equal? ready 1) 
							 "loaded but unready")
							 (else "unloaded")))
					result-ok
			))
		)
		(method 'handle
			(lambda (kcannon kuser)
				(let ((ready (cadr (gob kcannon)))
						(facing (car (gob kcannon))))
					(kern-obj-dec-ap kuser speed-human)
					(cond
						((equal? ready 2)
							(let* ((loc (kern-obj-get-location kcannon))
								(aimdir (direction-to-lvect facing))
								(targetloc (arms-searchline (car loc)
									(cadr loc) (caddr loc)
									(car aimdir) (cadr aimdir))))
								(kern-sound-play sound-cannon-fire)
								(kern-log-msg "BOOOM")
								(kern-fire-missile t_localcannonball loc (mk-loc (car loc) (car targetloc) (cadr targetloc)))
								)
							(bind kcannon (list facing 0)))
						((equal? ready 1)
							(kern-log-msg "Cannon ready to fire")
							(bind kcannon (list facing 2)))
						(else
							(kern-log-msg "Cannon loaded")
							(bind kcannon (list facing 1)))
					)
			))
		)
		(method 'init
			(lambda (kcannon)
				(kern-obj-set-facing kcannon (car (gob kcannon)))
				(kern-obj-set-pclass kcannon pclass-boulder)
		))	
	))

(mk-obj-type 't_cannonobj "cannon" s_cannon layer-mechanism cannon-ifc)     
         
(define  (arms-mk-cannon facing)
	(let ((kcannon (kern-mk-obj t_cannonobj 1)))
          (kern-obj-set-facing kcannon facing) 
          (bind kcannon (list facing 0))
          kcannon))

;; Weapons that aren't affected by acid       
(define arms-immune-to-acid
  (list t_flaming_sword
        t_shield_4
        t_armor_plate_4
        t_armor_chain_4
        t_armor_leather_2
        t_armor_leather_4
        t_iron_helm_4
        t_chain_coif_4
        t_leather_helm_2
        t_leather_helm_4
        t_sword_2
        t_sword_4
        t_eldritch_blade
        t_mystic_sword
        t_magic_axe
        t_doom_staff
        t_stun_wand))
