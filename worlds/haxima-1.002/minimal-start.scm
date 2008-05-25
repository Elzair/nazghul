;;----------------------------------------------------------------------------
;; The very first line of any session file should be (load "naz.scm"). This
;; bootstraps some procedures that we need to continue. This is the only place
;; you should use 'load'. Every other place you want to load a file you should
;; user 'kern-load'. 'kern-load' ensures that a saved session will be able to
;; load the file, too.
;;----------------------------------------------------------------------------
(load "naz.scm")


;; Setup progress bar for loading. I arrived at the number by printing the
;; current number of steps in src/foogod.c:foogod_progress_bar_finish().
(kern-progress-bar-start "Loading" 205)

;; Wrap the original definition of (load ...) with one that advances the
;; progress bar.
(define original-load load)  
(define (load file)
  (println (kern-get-ticks) ":" file "...")
  (kern-progress-bar-advance 1)
  (original-load file)
  )


;;----------------------------------------------------------------------------
;; Load the read-only game data. See the note on 'kern-load' vs 'load' above.
;;----------------------------------------------------------------------------
(kern-load "minimal-game.scm")

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
  nil                         ; campsite map
  nil                         ; campsite formation
  nil                         ; vehicle
  ;; inventory
  (kern-mk-inventory nil)
  nil ;; party members (should be nil for initial load file)
  )
 (tbl-mk) ; gob
 )

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)
;;(kern-party-add-member player ch_thorald_greybeard)

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

; ;;----------------------------------------------------------------------------
; ;; Lumis is the source gate, which means it opens the source moongates on its
; ;; phases. We designate this by using the source-moon-ifc as its ifc.
; ;;
; ;; Note: the arc and phase are calculated to give the moon the right orientation
; ;; with respect to phase vs sun position
; ;;----------------------------------------------------------------------------
; (mk-moon 'lumis  ; tag
;          "Lumis" ; name
;          5       ; hours per phase
;          60      ; hours per revolution
;          22      ; initial arc
;          0       ; initial phase
;          'source-moon-ifc ; ifc
;          ;; gates (moons are fixed at 8 phases in mk-moon):
;          (list 'mg-1 'mg-2 'mg-3 'mg-4
;                'mg-5 'mg-6 'mg-7 'mg-8
;                )
;          "yellow")

; ;;----------------------------------------------------------------------------
; ;; Ord is the destination gate, which means its phase decides the destination
; ;; when the player steps through a moongate. We designate this by giving it a
; ;; nil ifc. Note that its gates do not need to be listed in the same order as
; ;; Lumis. In fact, they don't even need to be the same set of gates.
; ;;
; ;; Note: the arc and phase are calculated to give the moon the right orientation
; ;; with respect to phase vs sun position
; ;;----------------------------------------------------------------------------
; (mk-moon 'ord    ; tag
;          "Ord"   ; name
;          9       ; hours per phase
;          36      ; hours per revolution
;          67     ; initial arc
;          7       ; initial phase
;          nil     ; ifc
;          ;; gates (moons are fixed at 8 phases in mk-moon):
;          (list 'mg-1 'mg-2 'mg-3 'mg-4
;                'mg-5 'mg-6 'mg-7 'mg-8
;                )
;          "blue")

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


(kern-mk-place
 'p_minimal "Minimal Place" s_keep
 (kern-mk-map 
  nil 19 19 pal_expanded
  (list
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
   ))
 #f      ; wraps
 #f      ; underground
 #f      ; large-scale (wilderness)
 #f      ; tmp combat place
 nil     ; subplaces
 nil     ; neighbors
 nil ; objects
 nil ; hooks
 nil ; edge entrances
 )

(define (mk-npc name)
  (bind 
   (kern-mk-char (string->symbol (string-append "ch_" 
                                                (string-lower name))) ; tag
                 name              ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_wanderer          ; sprite
                 faction-men         ; starting alignment
                 0 10 5              ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 nil        ; conv
                 nil          ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil ; readied
                 )
   nil ; gob
   ))
 
(mk-npc "Andrea")
(mk-npc "Charlie")
(mk-npc "Gregor")


(kern-obj-set-conv ch_gregor 'gregors-conv)

;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;; Setup a quest-offer test

;;;; (define (attach kobj val-tag)
;;;;   (let ((val (eval val-tag))
;;;;         (obj (gob kobj)))
;;;;     (if (null? val) (error "attach: no val for " val-tag))
;;;;     (if (null? obj) (error "attach: no gob for " (kern-obj-get-name kobj)))
;;;;     (if (val 'can-attach? kobj)
;;;;         (tbl-append! obj val-tag)
;;;;         (val 'on-attach kobj)
;;;;         )))
;;;; 
;;;; (define (attached? kobj val-tag)
;;;;   (let ((obj (gob kobj)))
;;;;     (if (null? obj)
;;;;         #f
;;;;         (tbl-get obj (val-tag 'key)))
;;;;         ))
;;;; 
;;;; (define quest-offer-ifc
;;;;   (ifc nil
;;;;        (method 'can-attach? (lambda (knpc) (println "can-attach") #t))
;;;;        (method 'key (lambda () 'quest-offer))
;;;;        (method 'on-attach (lambda (knpc) ))
;;;;        (method 'is-avail? (lambda (knpc kpc) #t))
;;;;        (method 'offer (lambda (knpc kpc) ))
;;;;        ))
;;;; 
;;;; (define (gregors-quest-make-offer kpc knpc)
;;;;   (say knpc "Want a quest?")
;;;;   (cond ((yes? kpc)
;;;;          (say knpc "You got it.")
;;;;          (quest-assign gregors-quest (gob (kern-get-player))))
;;;;         (else
;;;;          (say knpc "Fine. Loser.")
;;;;          (kern-conv-end))
;;;;         ))
;;;; 
;;;; (define gregors-quest-offer
;;;;   (ifc quest-offer-ifc
;;;;        (method 'on-attach (lambda (knpc) (kern-add-hook 'conv_end_hook gregors-quest-make-offer)))
;;;;        (method 'key (lambda () 'gregors-quest))
;;;;        ))
;;;; 
;;;; (bind ch_gregor (tbl-mk))
;;;; (attach ch_gregor 'gregors-quest-offer)
;;;; (println "ch_gregor:" ch_gregor)

;;;; ;;----------------------------------------------------------------------------
;;;; ;; End-of-conv hook
;;;; (kern-mk-effect ef_conv_end ; tag
;;;;                 nil ; name
;;;;                 nil ; sprite
;;;;                 conv-end-exec ; exec
;;;;                 nil ; apply
;;;;                 nil ; conv-end-rm
;;;;                 nil ; restart
;;;;                 'conv-end-hook
;;;;                 nil ; ddc
;;;;                 #t ; cum 
;;;;                 0 ; dur
;;;;                 )

;;;; (kern-add-hook  
;;;;  'conv_end_hook
;;;;  (lambda (kpc knpc)
;;;;    (println "knpc:" knpc)
;;;;    (println "ch_gregor:" knpc)
;;;;    (cond ((equal? knpc ch_gregor)
;;;;           (say knpc "Want a quest?")
;;;;           (cond ((yes? kpc)
;;;;                  (say knpc "You got it.")
;;;;                  (quest-assign gregors-quest 
;;;;                                (gob (kern-get-player))))
;;;;                 (else
;;;;                  (say knpc "Fine. Loser.")
;;;;                  (kern-conv-end))
;;;;                 )))))

;;;; (define gregors-quest
;;;;   (quest-talk-to-for-xp-mk ch_gregor 10))
;;;; 
;;;; 
;;;; (define (simple-quest-offer knpc offer on-accept on-reject quest)
;;;;   (hook-end-of-conv knpc 'offer-quest offer on-accept on-reject quest))
;;;; 
;;;; (simple-quest-offer "Want a quest?" "You got it." "Fine. Loser." gregors-quest)

(kern-add-hook  
 'conv_end_hook
 (lambda (kpc knpc)
   (cond ((equal? knpc ch_gregor)
          (say knpc "Want a quest?")
          (cond ((yes? kpc)
                 (say knpc "You got it.")
                 (quest-assign gregors-quest 
                               (gob (kern-get-player))))
                (else
                 (say knpc "Fine. Loser.")
                 (kern-conv-end))
                )))))


;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;;----------------------------------------------------------------------------
;; Startup - this is a one-time only script that runs when the player starts
;; the game for the first time (or whenever he starts over from scratch,
;; loading the game from this file). It sets up the story a bit.
;;----------------------------------------------------------------------------

(define (simple-start kplayer)
  (println "simple-start")
  (kern-obj-put-at kplayer (list p_minimal 0 0))
  (kern-obj-put-at ch_gregor (list p_minimal 1 1))
  (quest-assign (quest-talk-to-for-xp-mk 'ch_gregor 10) (gob kplayer))
  )
      
(kern-add-hook 'new_game_start_hook simple-start)

(kern-progress-bar-finish)
