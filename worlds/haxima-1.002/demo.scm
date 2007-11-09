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
(kern-load "runes.scm")

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
;; Gate Traveler
;;----------------------------------------------------------------------------
(define (traveler-goto-dest kchar)
  (println "traveler-goto-dest")
  (let* ((trvl (gob kchar))
         (loc (kern-obj-get-location kchar))
         (dest (cons (loc-place loc) 
                     (npcg-get-post trvl)))
         )
    (println "loc=" loc " dest=" dest)
    (if (equal? loc dest)
        (begin
          (kern-obj-remove kchar)
          #t)
        (begin
          (pathfind kchar dest)))))

(define (wizard-traveler-ai kchar)
  (println "traveler-ai")
  (or (spell-sword-ai kchar)
      (traveler-goto-dest kchar)))

(define (normal-traveler-ai kchar)
  (or (std-ai kchar)
      (if (any-visible-hostiles? kchar)
          #f
          (traveler-goto-dest kchar))))

(define (traveler-mk kplace)
  (println "traveler-mk")
  (let* ((type-ai (random-select (list (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'wizard 'wizard-traveler-ai) 
                                       (cons 'paladin 'normal-traveler-ai)
                                       (cons 'tinker 'normal-traveler-ai)
                                       (cons 'ranger 'normal-traveler-ai)
                                       )))
         (path (random-select (list (list (loc-mk kplace 9 0) (list 9 9) #f)
                                    (list (loc-mk kplace 0 9) (list 9 9) #f)
                                    (list (loc-mk kplace 18 9) (list 9 9) #f)
                                    (list (loc-mk kplace 9 18) (list 9 9) #f)
                                    (list (loc-mk kplace 9 8) (list 9 0) #t)
                                    (list (loc-mk kplace 9 10) (list 9 18) #t)
                                    (list (loc-mk kplace 8 9) (list 0 9) #t)
                                    (list (loc-mk kplace 10 9) (list 18 9) #t)
                                    )))
         (kchar (mk-npc (car type-ai) 9))
         )
    (println "path=" path)
    (npcg-set-post! (gob kchar) (cadr path))
    (kern-char-set-ai kchar (cdr type-ai) kchar)
    (println "traveler-mk @ " (car path))
    ;;(if (caddr path) (kern-map-flash 100))
    (kern-obj-put-at kchar (car path))
    kchar))

;;----------------------------------------------------------------------------
;; Special Object Types
;;----------------------------------------------------------------------------
(define portal-ifc
  (ifc '()
       (method 'step (lambda (kportal kobj) 
                       (println "portal-step")
                       ;;(kern-map-flash 100)
                       (kern-obj-remove kobj)
                       ))
       ))
(mk-obj-type 't_portal "Portal" s_blackgate_full layer-mechanism portal-ifc)

;;----------------------------------------------------------------------------
;; Scene Manager
;;----------------------------------------------------------------------------
(define (scene-mgr-mk) (list 'scene-mgr 0 0 0))
(define (scene-mgr-state gob) (list-ref gob 1))
(define (scene-mgr-set-state! gob val) (set-car! (list-tail gob 1) val))
(define (scene-mgr-advance-state! gob) (set-car! (list-tail gob 1) (+ 1 (scene-mgr-state gob))))
(define (scene-mgr-get-num-demons gob) (list-ref gob 2))
(define (scene-mgr-set-num-demons! gob val) (set-car! (list-tail gob 2) val))
(define (scene-mgr-incr-num-demons! gob) (set-car! (list-tail gob 2) (+ 1 (scene-mgr-get-num-demons gob))))
(define (scene-mgr-get-num-travelers gob) (list-ref gob 3))
(define (scene-mgr-set-num-travelers! gob val) (set-car! (list-tail gob 3) val))
(define (scene-mgr-incr-num-travelers! gob) (set-car! (list-tail gob 3) (+ 1 (scene-mgr-get-num-travelers gob))))

(define (scene-mgr-intro-travelers-phase kobj)
  (println "scene-mgr-intro-travelers-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-travelers smgr))
         )
    (define (put-traveler)
      (println "put-traveler")
      (traveler-mk (loc-place (kern-obj-get-location kobj)))
      (scene-mgr-incr-num-travelers! smgr)
      )
    (cond ((< n 10) 
           (if (> (kern-dice-roll "1d3") 2)
               (put-traveler)))
          (else
           (scene-mgr-advance-state! smgr)))
    ))

(define (scene-mgr-intro-demons-phase kobj)
  (println "scene-mgr-intro-demons-phase")
  (let* ((smgr (kobj-gob-data kobj))
         (n (scene-mgr-get-num-demons smgr))
         )
    (define (put-demon dir)
      (let ((kdemon (mk-npc 'demon 9)))
        (kern-char-set-ai kdemon 'std-ai)
        (kern-map-flash 1000)
        (kern-obj-put-at kdemon 
                         (loc-offset (mk-loc (loc-place (kern-obj-get-location kobj)) 9 9)
                                     dir)))
      (scene-mgr-incr-num-demons! smgr)
      )
    (cond ((= n 0) 
           (kern-add-reveal 1000)
           (put-demon west))
          ((= n 1) (put-demon east))
          ((= n 2) (put-demon south))
          ((= n 3) (put-demon north))
          (else
           (scene-mgr-advance-state! smgr)))
    ))

(define (scene-mgr-exec kobj) 
  (let* ((smgr (kobj-gob-data kobj))
        (state (scene-mgr-state smgr)))
    (println "scene-mgr-exec: state=" state)
    (cond ((= 0 state) (scene-mgr-intro-travelers-phase kobj))
          ((= 1 state) (scene-mgr-intro-demons-phase kobj))
          )))

(define scene-mgr-ifc
  (ifc nil
       (method 'exec scene-mgr-exec)))

(mk-obj-type 't_scene_mgr nil nil layer-none scene-mgr-ifc)

(define (mk-scene-mgr)
  (bind (kern-obj-set-visible (kern-mk-obj t_scene_mgr 1) #f)
        (scene-mgr-mk)
        ))


;;----------------------------------------------------------------------------
;; Places
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_demo_scene 19 19 pal_expanded
 (list
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx x! ,, cc cc cc ,, x! xx xx xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx ,, cc cc cc cc ar cc cc cc cc ,, xx xx xx xx "
      "xx xx xx x! ,, cc ar cc cc cc cc cc ar cc ,, x! xx xx xx "
      ",, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc ,, ,, ,, ,, ,, "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
      "cc cc cc cc cc ar cc cc cc .. cc cc cc ar cc cc cc cc cc "
      "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
      ",, ,, ,, ,, ,, cc cc cc cc cc cc cc cc cc ,, ,, ,, ,, ,, "
      "xx xx xx x! ,, cc ar cc cc cc cc cc ar cc ,, x! xx xx xx "
      "xx xx xx xx ,, cc cc cc cc ar cc cc cc cc ,, xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx x! ,, cc cc cc ,, x! xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx ,, cc cc cc ,, xx xx xx xx xx xx xx "
  )
 )

(kern-mk-place
 'p_demo_scene   ; tag
 "Gate Portal"   ; name
 nil             ; sprite
 m_demo_scene    ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil             ; subplaces
 nil             ; neighbors

 (list ; objects
  (put (guard-pt 'halberdier) 12 4)
  (put (guard-pt 'halberdier) 4 6)
  (put (guard-pt 'halberdier) 6 14)
  (put (guard-pt 'halberdier) 14 12)
   (put (guard-pt 'crossbowman) 4 12)
   (put (guard-pt 'crossbowman) 6 4)
   (put (guard-pt 'crossbowman) 14 6)
   (put (guard-pt 'crossbowman) 12 14)
  (put (mk-monman) 0 0)
  (put (mk-scene-mgr) 0 0)
  (put (kern-mk-obj t_portal 1) 9 9)
  )
 (list 'on-entry-to-dungeon-room) ; hooks
 nil ;; edge entrances
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
  5 5 5                ; str/int/dex
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

;;----------------------------------------------------------------------------
;; Party members
;;----------------------------------------------------------------------------
(kern-party-add-member player ch_wanderer)

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
	;;	non	pla	men	cgb	acc	mon	tro	spd	out	gnt	dem	fgb	prs			
	(list	2	0	0	0	-1	-2	-2	-2	0	-2	-2	0	0	)	;;	none
	(list	0	2	2	-2	-2	-2	-2	-2	-2	-2	-2	-2	2	)	;;	player
	(list	-1	2	2	-1	-2	-2	-2	-2	-2	-2	-2	-2	2	)	;;	men
	(list	-1	-2	-2	2	-1	-2	0	-2	-2	-1	-2	-2	0	)	;;	cave goblin
	(list	-1	-2	-1	-1	2	-2	-1	-1	-2	-1	-2	-2	0	)	;;	accursed
	(list	-2	-2	-2	-2	-2	2	-2	0	-2	0	-2	0	0	)	;;	monsters
	(list	-2	-2	-2	0	-1	-2	2	-2	-2	-1	-2	-1	0	)	;;	hill trolls
	(list	-2	-2	-2	-2	-1	0	-2	2	-2	-1	-2	0	0	)	;;	wood spiders
	(list	0	-2	-2	-2	-2	-2	-2	-2	2	-2	-2	-1	0	)	;;	outlaws
	(list	-2	-2	-2	-1	-1	0	-1	-1	-2	2	-2	-1	0	)	;;	gint
	(list	-2	-2	-2	-2	-2	-2	-2	-2	-2	-2	2	-2	0	)	;;	demon
	(list	0	-2	-2	-2	-2	0	-2	0	-1	-1	-2	2	0	)	;;	forest goblin
	(list	0	2	2	0	0	0	0	0	0	0	0	0	2	)	;;	prisoners
)																	

(define (simple-start kplayer)
  (kern-obj-put-at kplayer (list p_demo_scene 9 16)))

(kern-set-start-proc simple-start)
