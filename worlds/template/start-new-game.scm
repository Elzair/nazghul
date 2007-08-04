;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "lib/naz.scm")

;;----------------------------------------------------------------------------
;; Load the read-only game data. See the note on 'kern-load' vs 'load' above.
;;----------------------------------------------------------------------------
(kern-load "lib/game.scm")
(kern-load "lib/camping-map.scm")
(kern-load "places/zones.scm")
(kern-load "lib/runes.scm")
(kern-load "lib/prices.scm")
(kern-load "lib/special.scm")
(kern-load "lib/town-entry.scm")
(kern-load "lib/pseudorandom-map.scm")

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
(kern-load "people/sample-char.scm")

;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(load "places/sample.scm")

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
  5 5 5                ; str/int/dex
  pc-hp-off
  pc-hp-gain
  pc-mp-off
  pc-mp-gain
  max-health 0 max-health speed-human-med-armor 1  ; hp/xp/mp/AP_per_turn/lvl
  #f                    ; dead
  nil                   ; conv
  nil                   ; sched
  nil                   ; special ai
  nil                   ; container
  nil                   ; readied
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
 m_campsite                  ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle
 ;; inventory
 (kern-mk-inventory nil)
 nil ;; party members (should be nil for initial load file)
 )

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)


;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(load "places/world.scm")

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
         5       ; hours per phase
         60      ; hours per revolution
         22      ; initial arc
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
         9       ; hours per phase
         36      ; hours per revolution
         67     ; initial arc
         7       ; initial phase
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
 (list  -2  -2  -2  -2  -2   2  -2   0  -2   0  -2   0) ;; monsters
 (list  -2  -2  -2   0  -1  -2   2  -2  -2  -1  -2  -1) ;; hill trolls
 (list  -2  -2  -2  -2  -1   0  -2   2  -2  -1  -2   0) ;; wood spiders
 (list   0  -2  -2  -2  -2  -2  -2  -2   2  -2  -2  -1) ;; outlaws
 (list  -2  -2  -2  -1  -1   0  -1  -1  -2   2  -2  -1) ;; gint
 (list  -2  -2  -2  -2  -2  -2  -2  -2  -2  -2   2  -2) ;; demon
 (list   0  -2  -2  -2  -2   0  -2   0  -1  -1  -2   2) ;; forest goblin
 )


;;----------------------------------------------------------------------------
;; Startup - this is a one-time only script that runs when the player starts
;; the game for the first time
;;----------------------------------------------------------------------------
  
(define (create-char kplayer)
  (kern-obj-put-at kplayer (list p_moongate_clearing 9 17)
                   ))
      
;;----------------------------------------------------------------------------
;; To skip the extended start scene comment out this next line and uncomment
;; the line after it.
;;----------------------------------------------------------------------------

(kern-set-start-proc create-char)
