;; portals.scm


;; A portal destination is simply a Scheme list of (place x y).
;; 
;; Here are some convenience functions for:
;; - creating a destination:  portal-mk
;; - inquiring the place:     portal-place
;; - inquiring the (x y):     portal-coords
(define (portal-mk     place x y) (list place x y))
(define (portal-place  portal)    (safe-eval (car portal)))
(define (portal-coords portal)    (cons (portal-place portal) (cdr portal)))

;; portal-step:
;;     Function called when a portal is activated.
;;     Causes the stepper to be relocated to the portal destination.
(define (portal-step kportal kstepper)
  (let ((portal (kobj-gob-data kportal)))
    (kern-obj-relocate kstepper
                       (portal-coords portal)
                       nil)))

;; prompt-portal-step:
;;     Function called when a portal is activated.
;;     Prints two UI confirmation prompts, and on double-confirmation, 
;;     relocates the stepper.
(define (prompt-portal-step kportal kstepper)
  (kern-print "\n")
  (kern-print "Are you SURE you want to step there?\n")
  (if (kern-conv-get-yes-no? kstepper)
      (begin
        (kern-print "\n")
        (kern-print "\n")
        (kern-print "Are you REALLY REALLY SURE?\n")
        (if (kern-conv-get-yes-no? kstepper)
            (begin
              (kern-print "\n")
              (kern-print "\n")
              (kern-print "\n")
              (kern-print "Ok... you asked for it!\n")
              (portal-step kportal kstepper))
            (kern-print "Wise move.\n")))
        (kern-print "Make up your mind.\n")))
            

(define portal-ifc
  ;; This is the interface for a "use (E)nter command" portal, 
  ;; which acts when the (E)nter command is invoked while standing upon the portal.
  ;; 
  ;; By convention, small-scale places accessible from a large-scale wilderness
  ;; place are generally of this type.  It is thus possible to enter the 
  ;; (smaller, contained) place from any direction, or to walk through that
  ;; tile in the wilderness without entering.
  (ifc '()
       (method 'enter portal-step)))
       
(define bump-portal-ifc
  ;; This is the interface for a pseudo-edge tile
  ;; Marked impassable, but teleports you to the next area if yu bump into it
  (ifc '()
       (method 'init
               (lambda (kobj)
                 (kern-obj-set-pclass kobj pclass-wall)
                 ))
       (method 'bump portal-step)))

(define auto-portal-ifc
  ;; This is the interface for an "automatic enter" portal, 
  ;; which acts immediately when stepped upon.
  ;; 
  ;; By convention, this interface is used for most small-scale portals,
  ;; magical gates, and for trapdoors and other "surprise" portals.
  (ifc '()
       (method 'step portal-step)))

(define prompting-auto-portal-ifc
  ;; This is the interface for a "prompting" portal,
  ;; which prints a UI prompt for the user to confirm their intention to enter.
  ;; 
  ;; By convention, this interface is used for non-surprise portals
  ;; which present the appearance of not being able to readily return,
  ;; such as holes in the floor (without a ladder or rope attched),
  ;; or portals whose destination is (or appears to be) potentially hazardous,
  ;; such as certain magical gates to unknown destinations.
  (ifc '()
       (method 'step prompt-portal-step)))


;; Portal objects for some common portal types: 
;;     Instances of such objects are placed on the appropriate map on the
;;     "mechanisms" layer.  These object "types" specify a name/label, sprite,
;;     and a portal "interface" behavior.  Instances are created by "portal
;;     constructor" functions such as those below.
(mk-obj-type 't_ladder_down "ladder leading down" s_ladder_down   layer-mechanism portal-ifc)
(mk-obj-type 't_ladder_up   "ladder leading up"   s_ladder_up     layer-mechanism portal-ifc)
(mk-obj-type 't_trap_door   "trap door"           '()             layer-mechanism auto-portal-ifc)
(mk-obj-type 't_bump_door   nil           '()             layer-mechanism bump-portal-ifc)
(mk-obj-type 't_teleporter  "teleporter"          s_floor_plate   layer-mechanism prompting-auto-portal-ifc)
(mk-obj-type 't_dungeon "dungeon" s_dungeon layer-mechanism       auto-portal-ifc)
(mk-obj-type 't_ship_relic "ship relic" s_ship layer-mechanism auto-portal-ifc)

;; mk-portal -- generic helper constructor
(define (mk-portal type place-tag x y)
  (bind (kern-mk-obj type 1)
        (portal-mk place-tag x y)))
 
;; Portal constructor functions for some common portal types:
;;     These functions create an instance of a portal type,
;;     binding a destination (place-tag x y) to one of the "types" defined above.
(define (mk-ladder-down place-tag x y) (mk-portal t_ladder_down place-tag x y))
(define (mk-ladder-up   place-tag x y) (mk-portal t_ladder_up   place-tag x y))
(define (mk-trap-door   place-tag x y) (mk-portal t_trap_door   place-tag x y))
(define (mk-bump-door   place-tag x y) (mk-portal t_bump_door   place-tag x y))
(define (mk-teleporter  place-tag x y) (mk-portal t_teleporter  place-tag x y))
(define (mk-dungeon place-tag x y) (mk-portal t_dungeon place-tag x y))
(define (mk-ship-relic place-tag x y) (mk-portal t_ship_relic place-tag x y))

;;----------------------------------------------------------------------------
;; Special portal -- entrance to thief's cave near Bole. Invisible, but under
;; reveal it shows the letter 'O'. When stepped on at midnight the player is
;; transported to the Traps I dungeon.
;;----------------------------------------------------------------------------
(define (thief-door-step kportal kchar)
  (let ((time (kern-get-time)))
    (if (and (>= (time-hour time) 0)
             (< (time-hour time) 3))
        (portal-step kportal kchar))))

(define thief-door-ifc
  (ifc '()
       (method 'step thief-door-step)))

(mk-obj-type 't_thief_door "strange mark" s_O layer-mechanism thief-door-ifc)

(define (mk-thief-door place-tag x y)
  (make-invisible (mk-portal t_thief_door place-tag x y)))

;;----------------------------------------------------------------------------
;; Secret Path -- visible only when Reveal is in effect
;;----------------------------------------------------------------------------
(mk-obj-type 't_secret_path "secret path" s_cobblestone layer-none nil)
(define (mk-secret-path)
  (make-invisible (kern-mk-obj t_secret_path 1)))

;;----------------------------------------------------------------------------
;; Clue Trigger -- make a mechanism which will provide a clue when stepped
;; on. Technically not a portal, but it doesn't really fit anywhere else,
;; either.
;;----------------------------------------------------------------------------
(define (clue-mk msg) msg)
(define (clue-msg clue) clue)
(define (clue-trigger clue)
  (apply kern-log-msg (clue-msg clue)))

(define (clue-step kmech kchar)
  (clue-trigger (kobj-gob-data kmech)))

(define clue-step-ifc
  (ifc '()
       (method 'step clue-step)))

(mk-obj-type 't_step_clue nil nil layer-mechanism clue-step-ifc)

(define (mk-step-clue . msg)
  (bind (kern-mk-obj t_step_clue 1)
        (clue-mk msg)))

;;----------------------------------------------------------------------------
;; The riddle machine -- fills region with given terrain when answered
;; incorrectly
;;----------------------------------------------------------------------------
(define (riddle-mk ans ter-tag x y w h pos? msg)
  (list ans ter-tag x y w h pos? msg 
        #t  ;; rm-on-correct
        #t  ;; rm-on-wrong
        ))
(define (riddle-ans riddle) (car riddle))
(define (riddle-terrain riddle) (eval (cadr riddle)))
(define (riddle-x riddle) (caddr riddle))
(define (riddle-y riddle) (list-ref riddle 3))
(define (riddle-w riddle) (list-ref riddle 4))
(define (riddle-h riddle) (list-ref riddle 5))
(define (riddle-pos? riddle) (list-ref riddle 6))
(define (riddle-msg riddle) (list-ref riddle 7))
(define (riddle-rm-on-correct? riddle) (list-ref riddle 8))
(define (riddle-rm-on-wrong? riddle) (list-ref riddle 9))
(define (riddle-set-rm-on-correct! riddle val) (list-set-ref! riddle 8 val))
(define (riddle-set-rm-on-wrong! riddle val) (list-set-ref! riddle 9 val))

(define (riddle-trigger riddle kmech)
  (shake-map 10)
  (fill-terrain (riddle-terrain riddle)
                (loc-place (kern-obj-get-location kmech))
                (riddle-x riddle)
                (riddle-y riddle)
                (riddle-w riddle)
                (riddle-h riddle))
  )

(define (riddle-step kmech kchar)
  (if (is-player-party-member? kchar)
      (let ((riddle (kobj-gob-data kmech)))
        (kern-log-msg "*** STENTORIAN VOICE ***")
        (apply kern-log-msg (riddle-msg riddle))
        (let ((guess (kern-conv-get-reply kchar)))
          (cond ((eq? guess (riddle-ans riddle))
                 (kern-log-msg "YOU MAY PASS")
                 (if (riddle-pos? riddle)
                     (riddle-trigger riddle kmech))
                 (if (riddle-rm-on-correct? riddle)
                     (kern-obj-remove kmech))
                 )
                (else
                 (kern-log-msg "WRONG!")
                 (if (not (riddle-pos? riddle))
                     (riddle-trigger riddle kmech))
                 (if (riddle-rm-on-wrong? riddle)
                     (kern-obj-remove kmech))
                 )
                )))))
                

(define riddle-step-ifc
  (ifc '()
       (method 'step riddle-step)))

(mk-obj-type 't_step_riddle nil nil layer-mechanism riddle-step-ifc)

(define (mk-riddle ans ter-tag x y w h  pos? . msg)
  (bind (kern-mk-obj t_step_riddle 1)
        (riddle-mk ans ter-tag x y w h pos? msg)))

