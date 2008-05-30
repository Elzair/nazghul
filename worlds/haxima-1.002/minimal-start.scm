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

;;----------------------------------------------------------------------------
;; end-of-conv hook handling

;; create the table for end-of-conv handlers
(kern-define 'end-of-conv-handlers (tbl-mk))

;; a procedure to run all the end-of-conv handlers
(define (run-end-of-conv-handlers kpc knpc args)
  (println "run-end-of-conv-handlers:args=" args)
  (tbl-for-each-val (lambda (val)
                      (println "val:" val)
                      (apply (eval (car val)) (cons kpc (cons knpc (cdr val)))))
                    (eval (car args))))

;; setup the end-of-conv hook to run the handlers (this must be done only once
;; per game, so keep it in the start-game file)
(kern-add-hook 'conv_end_hook
               'run-end-of-conv-handlers
               '(end-of-conv-handlers))

;;----------------------------------------------------------------------------
;; Offer a predefined quest in a piece of dialogue. 'args' should be a list
;; like this:
;;
;; (offer-string accept-string reject-string quest)
;;
;; Where 'quest' is an instance of a quest.
(define (basic-quest-offer kpc knpc args)
  (println "basic-quest-offer: args=" args)
  (println "knpc=" knpc)
  (define (offer t1 t2 t3 quest)
    (println "offer")
    (say knpc t1)
    (cond ((yes? kpc)
           (say knpc t2)
           (quest-assign (eval quest)
                         (gob (kern-get-player)))
           (tbl-rm! end-of-conv-handlers quest)
           )
          (else
           (say knpc t3)
           )))    
  (if (equal? knpc (safe-eval (car args)))
      (apply offer (cdr args))))


;;----------------------------------------------------------------------------
;; gregor's quest (test)

;; create the quest (for now)
(kern-define 'gregors-quest
             (quest-talk-to-for-xp-mk 'ch_gregor 10))

;; Add an end-of-conv handler to offer gregor's quest 
;;
;; (note 1: that the entry must be a list in a list because of the way
;; tbl-for-each and the apply within it work)
;;
;; (note 2: by convention, the key is the name of the quest; basic-quest-offer
;; assumes this)
(tbl-set! end-of-conv-handlers
          'gregors-quest
          '((basic-quest-offer (ch_gregor "Want a quest?" "You got it." "Fine. Loser" gregors-quest))))


;;----------------------------------------------------------------------------
;; random quest assignment (prototype)

(define (random-mailman-quest-offer kpc knpc)
  (say knpc 
       "I need someone to deliver THIS PACKAGE"
       " to SOMBEODY"
       " by SOMETIME"
       ". Will you do it?")
  (cond ((yes? kpc)
         (say knpc "GREAT")
         ;; (quest-assign ...)
         )
        (else
         (say knpc "WHAT? YOU SUCK!")
         ))
  (println "leaving")
  )

(define (select-random-quest-offer knpc)
  (println "select-random-quest-offer")
  (random-select (list random-mailman-quest-offer
                   )))

(define (offer-random-quest kpc knpc args)
  (println "offer-random-quest")
  (let ((offer (select-random-quest-offer knpc)))
    (if (notnull? offer)
        (offer kpc knpc))))
    
(tbl-set! end-of-conv-handlers 
          'random-quest
          '((offer-random-quest nil)))

;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;;----------------------------------------------------------------------------
;; Startup - this is a one-time only script that runs when the player starts
;; the game for the first time (or whenever he starts over from scratch,
;; loading the game from this file). It sets up the story a bit.
;;----------------------------------------------------------------------------

(define (simple-start kplayer)
  (kern-obj-put-at kplayer (list p_minimal 0 0))
  (kern-obj-put-at ch_gregor (list p_minimal 1 1))
  (quest-assign (quest-talk-to-for-xp-mk 'ch_gregor 10) (gob kplayer))
  )
      
(kern-add-hook 'new_game_start_hook 'simple-start)

(kern-progress-bar-finish)
