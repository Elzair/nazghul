;; talk-to-quest.scm - utilities for quests that involve talking to something

;;----------------------------------------------------------------------------
;; constructor
;;
;; The talk-to quest provides standard assign and status procedures for the
;; base quest class.

;; Check if the quest can be assigned
(define (qtt-assign qst target) #t)

;; Return a string describing the progress of the quest (only called when the
;; quest is not marked done)
(define (qtt-status qst) "Incomplete")

;; Make a new instance of a talk-to quest.
;;
;; 'npc' is the symbol for the tag of the object to talk to (eg,
;; 'ch_gregor). This must evaluate to an actual object when (which means it
;; must be defined before) when this ctor is called or it will throw an
;; error. The object must have a conversation or the quest won't be
;; completable.
;;
;; 'desc' is the textual description to show in the UI.
;;
;; 'on-finish' is the symbol for a procedure to run when the quest is
;; completed; it should expect the qtt and the speaker kpc as parms,
;; respectively. In the unlikely event you don't need one then nil is ok.
;;
;; 'payload' are optional args. This is to allow subclasses. The qtt-payload
;; accessor will return them as a single list.
(define (qst-talk-to-mk npc desc on-finish . payload)
  (let ((kchar (safe-eval npc)))
    (if (null? kchar)
        (error "quest-talk-to-mk: " npc " is undefined")
        (qst-mk (string-append "Talk to " (kern-obj-get-name kchar))
                desc
                'qtt-assign
                'qtt-status
                'talk-to npc on-finish payload))
    ))

;;----------------------------------------------------------------------------
;; accessors

(define (qtt-npc qtt) (safe-eval (cadr qtt)))
(define (qtt-on-finish qtt) (safe-eval (list-ref qtt 2)))
(define (qtt-payload qtt) 
  (list-ref qtt 3)) ; this returns a list

;;----------------------------------------------------------------------------
;; utilities

;; Is this the target npc?
(define (qtt-is-npc? qtt knpc)
  (equal? (qtt-npc qtt) 
          knpc))

;; Run the on-finish procedure, if there is one
(define (qtt-finish qtt kpc)
  (let ((on-finish (qtt-on-finish qtt)))
    (if (notnull? on-finish)
        (apply on-finish (list qtt kpc)))))

;; Check if this is a talk-to quest
(define (qst-is-talk-to? qst)
  (let ((payload (qst-payload qst)))
    (and (pair? payload)
         (eq? (car payload)
              'talk-to))))

;;----------------------------------------------------------------------------
;; hook proc(s)

;; Whenever a conversation starts, check if a talk-to quest was just finished
;; (note that this hook only runs if the object actually has a conversation
;; script). If it was, run the on-finish proc and mark the quest as done.
(kern-add-hook 'conv_start_hook
               (lambda (kpc knpc)
                 (let ((qlst (find-field 'quests (gob (kern-get-player)))))
                   (for-each (lambda (qst)
                               (if (and (not (qst-done? qst))
                                        (qst-is-talk-to? qst))
                                   (let ((qtt (qst-payload qst)))
                                     (if (qtt-is-npc? qtt knpc)
                                         (begin
                                           (qtt-finish qtt kpc)
                                           (qst-done! qst)
                                           )))))
                             (cdr qlst)))))

;;----------------------------------------------------------------------------
;; generic talk-to-quest constructor (a helpful subclass)
;;
;; If you just want to grant some xp for a talk-to quest and don't need any
;; fancy description, this handles the boilerplate. Just provide the target npc
;; (as a symbol tag like 'ch_gregor) and the number of xp points to award.

(define (qtt-xp-on-finish qtt kchar)
  (kern-char-add-experience kchar 
                            (car (qtt-payload qtt))))

(define (quest-talk-to-for-xp-mk npc xp)
  (let ((kchar (safe-eval npc)))
    (if (null? kchar)
        (error "quest-talk-to-std-mk: " npc " is undefined")
        (qst-talk-to-mk npc
                        (string-append "Find and talk to " (kern-obj-get-name kchar))
                        'qtt-xp-on-finish
                        xp
                        )
        )))

