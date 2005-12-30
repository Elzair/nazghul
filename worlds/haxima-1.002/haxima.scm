;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")

;;----------------------------------------------------------------------------
;; Load the read-only game data. See the note on 'kern-load' vs 'load' above.
;;----------------------------------------------------------------------------
(kern-load "game.scm")
(kern-load "camping-map.scm")
(kern-load "zones.scm")
(kern-load "runes.scm")
(kern-load "prices.scm")
(kern-load "special.scm")
(kern-load "town-entry.scm")

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

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(set-level
 (kern-mk-char 
  'ch_wanderer
  "The Wanderer"        ; name
  sp_human              ; species
  oc_wanderer           ; occ
  s_wanderer    ; sprite
  faction-player        ; starting alignment
  5 5 5                ; str/int/dex
  pc-hp-off
  pc-hp-gain
  pc-mp-off
  pc-mp-gain
  29 0 3 1              ; hp/xp/mp/lvl
  #f                    ; dead
  nil                   ; conv
  nil                   ; sched
  nil                   ; special ai
  nil                   ; container
  nil                   ; readied
  )
 1)


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
;  240 0 8 8             ; hp/xp/mp/lvl
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
 (kern-mk-container
  nil ;; type
  nil ;; trap
  nil ;; contents
  )

 nil ;; party members (should be nil for initial load file)
 )

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
;;----------------------------------------------------------------------------
(mk-moon 'lumis  ; tag
         "Lumis" ; name
         2       ; hours per phase
         24      ; hours per revolution
         90      ; initial arc
         0       ; initial phase
         'source-moon-ifc ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-5 'mg-6 'mg-7 'mg-8
               ))

;;----------------------------------------------------------------------------
;; Ord is the destination gate, which means its phase decides the destination
;; when the player steps through a moongate. We designate this by giving it a
;; nil ifc. Note that its gates do not need to be listed in the same order as
;; Lumis. In fact, they don't even need to be the same set of gates.
;;----------------------------------------------------------------------------
(mk-moon 'ord    ; tag
         "Ord"   ; name
         1       ; hours per phase
         12      ; hours per revolution
         180     ; initial arc
         1       ; initial phase
         nil     ; ifc
         ;; gates (moons are fixed at 8 phases in mk-moon):
         (list 'mg-1 'mg-2 'mg-3 'mg-4
               'mg-5 'mg-6 'mg-7 'mg-8
               ))

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;;
;; Note: factions should always be allied with themselves in order for
;; summoning AI to work properly.
;;       
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;     non pla men cgb acc mon tro spd out gnt dem fgb
 (list   2   0   0   0  -1  -2  -2  -2   0  -2  -2   0) ;; none
 (list   0   2   2  -2  -2  -2  -2  -2  -2  -2  -2  -2) ;; player
 (list  -1   2   2  -1  -2  -2  -2  -2  -2  -2  -2  -2) ;; men
 (list  -1  -2  -2   2  -1  -2   0  -2  -2  -1  -2  -2) ;; cave goblin
 (list  -1  -2  -1  -1   2  -2  -1  -1  -2  -1  -2  -2) ;; accursed
 (list  -2  -2  -2  -2  -2   2   0   0  -2   0  -2   0) ;; monsters
 (list  -2  -2  -2   0  -1  -1   2  -2  -2  -1  -2  -2) ;; hill trolls
 (list  -2  -2  -2  -2  -1   0  -2   2  -2  -1  -2   0) ;; wood spiders
 (list   0  -2  -2  -2  -2  -2  -2  -2   2  -2  -2  -1) ;; outlaws
 (list  -2  -2  -2  -1  -1   0  -1  -1  -2   2  -2  -1) ;; gint
 (list  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2  -2) ;; demon
 (list   0  -2  -2  -2  -2   0  -2   0  -1  -1  -2   2) ;; forest goblin
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
(define blackgate-stages
  (list (list '()                       0)
        (list s_blackgate_quarter        32)
        (list s_blackgate_half           64)
        (list s_blackgate_three_quarters 96)
        (list s_blackgate_full           128)))

(define (start-scene kplayer)

  (kern-log-msg "A dark gate rises in a quiet clearing...")
  (moongate-animate black-gate blackgate-stages)
  (kern-sleep 2000)

  (kern-log-msg "Then closes without a trace...")
  (moongate-animate black-gate (reverse blackgate-stages))
  (kern-log-enable #f)
  (kern-char-set-sleep ch_wanderer #t)

  (kern-obj-put-at kplayer (list p_moongate_clearing 11 11))

  (moongate-animate black-gate (reverse blackgate-stages))
  (kern-sleep 1000)
  
  (kern-log-enable #t)
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
  (kern-obj-put-at kplayer (list p_shard 52 32)
                   ))
  ;;(kern-obj-put-at kplayer (list p_moongate_clearing 11 11)))
      
;;----------------------------------------------------------------------------
;; To skip the extended start scene comment out this next line and uncomment
;; the line after it.
;;----------------------------------------------------------------------------
(kern-set-start-proc start-scene)
;;(kern-set-start-proc simple-start)
