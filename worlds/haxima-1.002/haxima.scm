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

;;----------------------------------------------------------------------------
;; Placees
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
(load "kurpolis.scm")
(load "bole.scm")
(load "glasdrin.scm")
(load "oparine.scm")
(load "traps_1.scm")
(load "traps_2.scm")
(load "traps_3.scm")
(load "traps_4.scm")
(load "great_hall.scm")

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
 2 2 2                ; str/int/dex
 2 2                   ; hp mod/mult
 2 2                  ; mp mod/mult
 29 0 3 1              ; hp/xp/mp/lvl
 #f                    ; dead
 nil                   ; conv
 nil                   ; sched
 nil                   ; special ai
 nil                   ; container
 nil)                  ; readied


;; For test
(kern-mk-char 
 'ch_thorald_greybeard ; tag
 "Thorald Greybeard"   ; name
 sp_human              ; species
 oc_wizard             ; occ
 s_companion_wizard    ; sprite
 faction-player        ; starting alignment
 0 10 2                ; str/int/dex
 0 1                   ; hp mod/mult
 10 5                  ; mp mod/mult
 240 0 8 8             ; hp/xp/mp/lvl
 #f                    ; dead
 nil                   ; conv
 nil                   ; sched
 nil                   ; special ai
 nil                   ; container
 (list t_doom_staff))         ; readied
 
;;----------------------------------------------------------------------------
;; Player Party
;;----------------------------------------------------------------------------
(kern-mk-player
 'player                     ; tag
 s_wanderer         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 10                          ; food
 0                           ; gold
 0                           ; turns to next meal
 nil                         ; formation
 m_campsite                  ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle
 ;; inventory
 (kern-mk-container
  nil ;; type
  nil ;; trap
  ;; contents:
  (mk-contents (add-content 1 t_wis_quas_scroll)
               (add-content 1 t_sanct_lor_scroll)
               (add-content 1 t_armor_leather)
               (add-content 1 t_leather_helm)
               (add-content 5 t_torch)
               (add-content 1 t_sword)))

 nil ;; party members (should be nil for initial load file)
 )

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)
;(kern-party-add-member player ch_thorald_greybeard)


;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(load "shard.scm")

;;----------------------------------------------------------------------------
;; Time
;;----------------------------------------------------------------------------
(define hour 12)
(define minutes 00)
(define time-in-minutes (+ (* hour 60) minutes))

(kern-set-clock 
 0 ; year
 0 ; month
 0 ; week
 0 ; day
 hour  ; hour
 minutes ; minutes(
 )

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

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;           none play men orks accu mons troll spid outl 
 (dtable-row   0    0    0   0   -1   -2   -2    -2    0) ;; none
 (dtable-row   0    2    1  -2   -1   -2   -2    -2   -2) ;; player
 (dtable-row  -1    1    2  -1   -2   -2   -2    -2   -2) ;; men
 (dtable-row  -1   -2   -1   2   -1   -2    0    -2   -2) ;; orks
 (dtable-row  -1   -1   -1  -1    2   -2   -1    -1   -2) ;; accursed
 (dtable-row  -2   -2   -2  -2   -2    0    0     0   -2) ;; monsters
 (dtable-row  -2   -2   -2   0   -1   -1    2    -2   -2) ;; hill trolls
 (dtable-row  -2   -2   -2  -2   -1    0   -2     2   -2) ;; wood spiders
 (dtable-row   0   -2   -2  -2   -2   -2   -2    -2    0) ;; outlaws
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
  ;(kern-obj-put-at kplayer (list p_moongate_clearing 11 11)))
  ;(kern-obj-put-at kplayer (list p_gregors_hut 11 11)))
  ;(kern-log-msg "You notice a cave to the southwest."))
  ;(kern-obj-put-at kplayer (list p_slimy_cavern 13 2)
  ;(kern-obj-put-at kplayer (list p_enchanters_tower 32 32)))
  ;(kern-obj-put-at kplayer (list p_shard 79 74)))
  (kern-obj-put-at kplayer (list p_traps_1 4 16)))
  ;(kern-obj-put-at kplayer (list p_test 15 15)))
  ;(kern-obj-put-at kplayer (list p_bole 44 5)))
  ;(kern-obj-put-at kplayer (list p_troll_den 9 0)))                  
;;----------------------------------------------------------------------------
;; To skip the extended start scene comment out this next line and uncomment
;; the line after it.
;;----------------------------------------------------------------------------
;(kern-set-start-proc start-scene)
(kern-set-start-proc simple-start)
