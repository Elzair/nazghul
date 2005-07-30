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
(mk-obj-type 't_teleporter  "teleporter"          s_floor_plate   layer-mechanism prompting-auto-portal-ifc)
(mk-obj-type 't_dungeon "dungeon" s_dungeon layer-mechanism portal-ifc)

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
(define (mk-teleporter  place-tag x y) (mk-portal t_teleporter  place-tag x y))
(define (mk-dungeon place-tag x y) (mk-portal t_dungeon place-tag x y))
