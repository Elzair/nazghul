;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")
(kern-script-version "0.7.0")

;; Setup progress bar for loading. I arrived at the number by printing the
;; current number of steps in src/foogod.c:foogod_progress_bar_finish().
(kern-progress-bar-start "Loading" 205)

;; Wrap the original definition of (load ...) with one that advances the
;; progress bar.
(define original-load load)  
(define (load file)
  (kern-progress-bar-advance 1)
  (original-load file)
  )

;;----------------------------------------------------------------------------
;; Load the read-only game data. See the note on 'kern-load' vs 'load' above.
;;----------------------------------------------------------------------------
(kern-load "game.scm")
(kern-load "quests-mech.scm")
(kern-load "camping-map.scm")

(kern-load "quests-data-static.scm")
(kern-load "zones.scm")
(kern-load "runes.scm")
(kern-load "prices.scm")
(kern-load "special.scm")
(kern-load "town-entry.scm")
(kern-load "pseudorandom-map.scm")
(kern-load "endless-deeps-mech.scm")
(kern-load "lost-halls-mech.scm")
(kern-load "voidgap-mech.scm")
(kern-load "player.scm")

;;----------------------------------------------------------------------------
;; Time -- this needs to be set before loading any dungeon rooms
;;----------------------------------------------------------------------------
(define hour 07)
(define minutes 00)
(define time-in-minutes (+ (* hour 60) minutes))
(define game-start-time (time-mk 1611 0 0 0 hour minutes))

(kern-set-clock 
 1611 ; year
 0 ; month
 0 ; week
 0 ; day
 hour  ; hour
 minutes ; minutes
 )

;; NPC's who inhabit multiple places
(kern-load "gregor.scm")
(kern-load "kalcifax.scm")

;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(load "gregors-hut.scm")
(load "moongate-clearing.scm")
(load "abandoned-farm.scm")
(load "abandoned-cellar.scm")
(load "slimy-cavern.scm")
(load "trigrave.scm")
(load "lost-halls.scm")
(load "enchanters-tower.scm")
(load "green-tower.scm")
(load "green-tower-lower.scm")
(load "mushroom-cave.scm")
(load "goblin-kingdoms.scm")
(load "treasury.scm")
(load "bole.scm")
(load "glasdrin.scm")
(load "oparine.scm")
(load "traps_1.scm")
(load "traps_2.scm")
(load "traps_3.scm")
(load "traps_4.scm")
(load "thiefs_den.scm")
(load "keep.scm")
(load "absalot.scm")
(load "old-absalot.scm")
(load "engineers-hut.scm")
(load "mans-hideout.scm")
(load "necromancers-lair.scm")
(load "fire_sea.scm")
(load "void-temple.scm")
(load "merciful-death.scm")
(load "angriss-lair.scm")
(load "poor-house.scm")
(load "prison.scm")
(load "ankh-shrine.scm")
(load "kraken-lakes.scm")
(load "endless-deeps.scm")
(load "forsaken-prison.scm")
(load "old-mine.scm")
(load "lich-tomb.scm")
(load "altar-room.scm")
(load "dank-cave.scm")
(load "eastpass.scm")
(load "westpass.scm")
(load "crypt.scm")
(load "ancient-derelict.scm")
(load "road_to_absalot.scm")
(load "kun.scm")
(load "gamestart.scm")
(load "bandit-hideout.scm")
(load "brundegardt.scm")
(load "voidgap-passage.scm")

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
 (kern-mk-char 
  'ch_wanderer
  "The Wanderer"        ; name
  sp_human              ; species
  oc_wanderer           ; occ
  s_wanderer    ; sprite
  faction-player        ; starting alignment
  6 6 6                ; str/int/dex
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
  )



; ;; For test
; (kern-mk-char 
;  'ch_thorald_greybeard ; tag
;  "Thorald Greybeard"   ; name
;  sp_human              ; species
;  oc_wizard             ; occ
;  s_companion_wizard    ; sprite
;  faction-player        ; starting alignment
;  0 10 2                ; str/int/dex
;  0 1                   ; hp mod/mult
;  10 5                  ; mp mod/mult
;  240 0 8 speed-human-med-armor 8             ; hp/xp/mp/AP_per_turn/lvl
;  #f                    ; dead
;  nil                   ; conv
;  nil                   ; sched
;  nil                   ; special ai
;  nil                   ; container
;  ;;(list t_doom_staff)         ; readied
;  nil
;  )
 
;;----------------------------------------------------------------------------
;; Player Party
;;----------------------------------------------------------------------------
(bind 
 (kern-mk-player
  'player                     ; tag
  s_wanderer         ; sprite
  "Walk"                      ; movement description
  sound-walking               ; movement sound
  1                           ; food
  0                           ; gold
  (* 60 60 5)                 ; turns to next meal (5 hours)
  nil                         ; formation
  m_campsite                  ; campsite map
  nil                         ; campsite formation
  nil                         ; vehicle
  ;; inventory
  (kern-mk-inventory nil)
  nil ;; party members (should be nil for initial load file)
  )
 (tbl-mk))

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)
;;(kern-party-add-member player ch_thorald_greybeard)


;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(load "shard.scm")

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
        ;;      non pla men cgb acc mon tro spd out gnt dem fgb prs gla                
        (list   2   0   0   0   -1  -2  -2  -2  0   -2  -2  0   0   0    ) ;; none
        (list   0   2   2   -2  -2  -2  -2  -2  -2  -2  -2  -2  2   2    ) ;; player
        (list   -1  2   2   -1  -2  -2  -2  -2  -2  -2  -2  -2  2   2    ) ;; men
        (list   -1  -2  -2  2   -1  -2  0   -2  -2  -1  -2  -2  0   -2   ) ;; cave goblin
        (list   -1  -2  -1  -1  2   -2  -1  -1  -2  -1  -2  -2  0   -2   ) ;; accursed
        (list   -2  -2  -2  -2  -2  2   -2  0   -2  0   -2  0   0   -2   ) ;; monsters
        (list   -2  -2  -2  0   -1  -2  2   -2  -2  -1  -2  -1  0   -2   ) ;; hill trolls
        (list   -2  -2  -2  -2  -1  0   -2  2   -2  -1  -2  0   0   -2   ) ;; wood spiders
        (list   0   -2  -2  -2  -2  -2  -2  -2  2   -2  -2  -1  0   -2   ) ;; outlaws
        (list   -2  -2  -2  -1  -1  0   -1  -1  -2  2   -2  -1  0   -2   ) ;; gint
        (list   -2  -2  -2  -2  -2  -2  -2  -2  -2  -2  2   -2  0   -2   ) ;; demon
        (list   0   -2  -2  -2  -2  0   -2  0   -1  -1  -2  2   0   -2   ) ;; forest goblin
        (list   0   2   2   0   0   0   0   0   0   0   0   0   2   2    ) ;; prisoners
        (list   -1  2   2   -1  -2  -2  -2  -2  -2  -2  -2  -2  2   2    ) ;; glasdrin
)																	


;;----------------------------------------------------------------------------
;; Startup - this is a one-time only script that runs when the player starts
;; the game for the first time (or whenever he starts over from scratch,
;; loading the game from this file). It sets up the story a bit.
;;
;; The camera should center on the moongate clearing. Then, a gate should rise
;; from the ground, pause, then sink back down, leaving the player's sleep
;; sprite on the ground. Another pause, and then the player should wake up.
;;----------------------------------------------------------------------------
(define (start-scene kplayer)

  (kern-log-msg "A dark gate rises in a quiet clearing...")
  (moongate-animate black-gate blackgate-stages)
  (kern-sleep 2000)

  (kern-log-enable #f)
  (kern-char-set-sleep ch_wanderer #t)
  (kern-obj-put-at kplayer (list p_moongate_clearing 11 12))

  (kern-log-enable #t)
  (kern-log-msg "Then closes without a trace...")
  (moongate-animate black-gate (reverse blackgate-stages))
  (kern-sleep 1000)
  
  (kern-log-msg "You lie dreaming for a while, of another life...")
  (kern-sleep 2000)

  (kern-log-enable #f)
  (kern-char-set-sleep ch_wanderer #f)
  (kern-player-set-follow-mode)
  (kern-log-enable #t)  
  (kern-log-msg "...then awaken to a strange new world.")
  (kern-log-msg "To the southwest you see a cave.")
  )

(define (simple-start kplayer)
  (kern-obj-put-at kplayer (list p_shard 76 114)))
  
(define (create-char kplayer)
 (kern-obj-put-at kplayer (list p_char_setup 10 17)
                   ))
      
;;----------------------------------------------------------------------------
;; To skip the extended start scene comment out this next line and uncomment
;; the line after it.
;;----------------------------------------------------------------------------
;;(kern-set-start-proc start-scene)
;;(kern-set-start-proc simple-start)
(kern-add-hook 'new_game_start_hook 'create-char)
(load "quests-data.scm")
(quest-assign (quest-data-get 'questentry-charcreate))

(kern-progress-bar-finish)
(println "r2g")


;;quickstart stuff for playtesting

(if #f
    (begin
      (kern-obj-put-at 
       (mk-chest
        nil ;; trap
        
        '(
         
         ;; Food
         ( 1000 t_food)
         
         ;; Gold
         ( 1000 t_gold_coins)
         
         ;; Reagents
         ( 55 sulphorous_ash)
         ( 55 ginseng)
         ( 55 garlic)
         ( 55 spider_silk)
         ( 53 blood_moss)
         ( 53 black_pearl)
         ( 51 nightshade)
         ( 51 mandrake)
         
         ;; Items
         ( 9 t_vas_mani_scroll)
         ( 9 t_in_ex_por_scroll)
         ( 9 t_sanct_lor_scroll)
         ( 9 t_wis_quas_scroll)
         ( 9 t_xen_corp_scroll)
         ( 9 t_an_xen_ex_scroll)
         ( 9 t_vas_rel_por_scroll)
         ( 59 t_gem)
         ( 9 t_cure_potion)
         ( 9 t_mana_potion)
         ( 99 t_xp_potion)
         ( 99 t_str_potion)
         ( 99 t_dex_potion)
         ( 99 t_int_potion)
         ( 99 t_info_potion)
         ( 99 t_torch)
         ( 99 t_picklock)
         ( 9 t_pick)
         ( 2 t_dragons_blood)
         ( 2 t_hydras_blood)
         ( 2 t_shovel)
        ;; ( 1 t_rune_k)
         
         
         ;; Arms
	 ( 1 t_dagger)
	 ( 1 t_mace)
	 ( 1 t_axe)
	 ( 1 t_sword)
	 ( 1 t_2H_axe)
	 ( 1 t_2H_sword)
	 ( 1 t_morning_star)
	 ( 1 t_halberd)
	 ( 1 t_staff)

	 ( 1 t_dagger_4)
         ( 1 t_sword_4)
         ( 1 t_morning_star_2)
	 ( 1 t_stun_wand)
	 ( 1 t_doom_staff)
	 ( 1 t_eldritch_blade)
	 ( 1 t_mystic_sword)
	 ( 1 t_flaming_sword)

         ( 1 t_shield)
	 ( 1 t_scratched_shield)

	 ( 1 t_leather_helm)
	 ( 1 t_chain_coif)
	 ( 1 t_iron_helm)

	 ( 1 t_armor_leather)
	 ( 1 t_armor_chain)
	 ( 1 t_armor_plate)

         ( 1 t_iron_helm_4)
	 ( 1 t_armor_leather_4)
	 ( 1 t_armor_chain_4)
         ( 1 t_armor_plate_4)

	 ( 1 t_chrono)
	 ( 1 t_spiked_shield)

	 ( 1 t_sling)
	 ( 3 t_spear)

         ( 1 t_sling_4)
	 ( 1 t_magic_axe)

	 (20 t_oil)
	 (20 t_slime_vial)

	 (  1 t_self_bow)
	 (  1 t_bow)
	 (  1 t_long_bow)
	 (  1 t_great_bow)
	 (500 t_arrow)

	 (  1 t_lt_crossbow)
	 (  1 t_crossbow)
	 (  1 t_hvy_crossbow)
	 (  1 t_trpl_crossbow)
	 (500 t_bolt)

         ))
       (list p_char_setup 16 17))
       
      (kern-obj-put-at
       (mk-ladder-down 'p_moongate_clearing 18 18) 
       (list p_char_setup 17 17))
       
      (kern-obj-put-at
       (mk-ladder-down 'p_trigrave 16 16) 
       (list p_char_setup 17 16))
       
       (kern-obj-put-at
       (mk-ladder-down 'p_fire_sea 4 4) 
       (list p_char_setup 17 15))
       
      (kern-obj-put-at
       (mk-ladder-down 'p_oparine 1 1) 
       (list p_trigrave 17 16))
       
       (kern-obj-put-at
       (mk-ladder-down 'p_lost_garrison 22 22) 
       (list p_oparine 1 1))
       
    ))
