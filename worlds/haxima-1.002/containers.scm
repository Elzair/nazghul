;;----------------------------------------------------------------------------
;; Containers - objects that contain stuff
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------
(define (mk-contents . contents)
  (filter notnull? contents))

(define (roll-100 prob)
  (>= prob (modulo (random-next) 100)))

(define (roll-to-add prob dice type)
  (if (roll-100 prob)
      (list (kern-dice-roll dice) type)
      nil))

;;----------------------------------------------------------------------------
;; Corpse -- not really a container, if you search it then it sort of acts like
;; opening a container
;;----------------------------------------------------------------------------
(define (corpse-mk loot)
  (list loot))
(define (corpse-loot corpse) (car corpse))
(define (corpse-set-loot! corpse val) (set-car! corpse val))
(define (corpse-loot-entry-q loot) (car loot))
(define (corpse-loot-entry-type loot) (eval (cadr loot)))

(define (corpse-search kobj)
  (let* ((corpse (kobj-gob-data kobj))
         (loot (corpse-loot corpse)))
    (if (not (null? loot))
        (let ((loc (kern-obj-get-location kobj)))
          (map (lambda (entry) 
                 (kern-obj-put-at (kern-mk-obj (corpse-loot-entry-type entry)
                                               (corpse-loot-entry-q entry))
                                  loc))
               loot)
          (corpse-set-loot! corpse nil)))))

(define corpse-ifc
  (ifc nil
       (method 'search corpse-search)))

(mk-obj-type 't_corpse "corpse" s_corpse layer-item corpse-ifc)

(define (mk-corpse) 
  (bind (kern-mk-obj t_corpse 1)
        (corpse-mk nil)))

;; mk-corpse2 -- loot: a list of (quantity type) lists
(define (mk-corpse2 loot)
  (bind (kern-mk-obj t_corpse 1)
        (corpse-mk loot)))

(define (mk-corpse-with-loot)
  (mk-corpse2 (mk-quoted-treasure-list (+ 1(modulo (random-next)
                                                   3)))))

;;----------------------------------------------------------------------------
;; This next section is an experimental new container type. It attempts to
;; bypass the kernel's built-in Container class and implement everything in the
;; script as a normal kernel Object that responds to the 'open signal the same
;; way a kernel Container would respond to the open command.
;;
;; This currently works. The next step is to implement the ability to add (and
;; remove or disable) traps on a container. The means of doing so will be
;; implemented here in the script, so the kernel won't need to know about
;; trapped containers when this all works, and the kernel's Container class can
;; be stripped back to a basic Inventory class.

;; Define the gob structure and procedures. The contents should be a single
;; quoted list, for example:
;;
;;   '((t_sword 1)
;;     (t_arrow 5)
;;     (t_torch 2)))
;;
;; Using the quotes is not only cleaner in the declarations, it automatically
;; ensures that the contents are safe to save and reload as part of the gob
;; because they are stored in the gob merely as symbols.
;;
;; Each container has a (often empty) list of traps. See traps.scm for details
;; of trap implementations. When traps are attached to a container, the type of
;; trap is specified, and an instance of that type is added to the list.yn
;;
(define (mk-container contents sprites-tag)
  (list 'container 
        contents 
        nil ;; traps
        #f  ;; destroyed?
        sprites-tag
        #f  ;; locked?
        #f  ;; magic-locked?
        nil ;; key-tag
        ))
(define (is-container? gob) (eq? (car gob) 'container))

(define (container-contents gob) (cadr gob))
(define (container-set-contents! gob val) (set-car! (cdr gob) val))

(define (container-traps gob) (caddr gob))
(define (container-set-traps! gob traps) (set-car! (cdr (cdr gob)) traps))
(define (container-add-trap! gob trap-type)
  (container-set-traps! gob
                        (cons (mk-trap (eval trap-type))
                              (container-traps gob))))

(define (container-destroyed? gob) (cadddr gob))
(define (container-destroy! gob) (set-car! (cdddr gob) #t))

(define (container-sprites gob) (eval (list-ref gob 4)))

(define (container-locked? gob) (list-ref gob 5))
(define (container-set-locked! gob val) (set-car! (list-tail gob 5) val))

(define (container-magic-locked? gob) (list-ref gob 6))
(define (container-set-magic-locked! gob val) (set-car! (list-tail gob 6) val))

(define (container-key gob) (list-ref gob 7))
(define (container-set-key! gob ktype) (set-car! (list-tail gob 7) ktype))
(define (container-needs-key? gob) (not (null? (container-key gob))))
(define (container-key-fits? gob ktype)
  (let ((key (safe-eval (container-key gob))))
    (and (not (null? key))
         (eqv? key ktype))))

;; For now always false, since they destroy themselves on open. Might change
;; some day...
(define (container-open? gob) #f)


(define (content-type content) (cadr content))
(define (content-quantity content) (car content))

;; This is the heart of the implementation. This procedure runs when the
;; container object gets the 'open signal, which is sent by the kernel in
;; response to the player's o)pen command followed by selection of this
;; object. It expects kobj to be a kernel object which is bound to a container
;; gob following the above format (the constructors further down illustrate how
;; to create such an object).
;;
;; Opening the container creates objects based on the types and quantities
;; listed in the container's content list and deposits these objects on the
;; ground where the container is. Then it removes the container, which likely
;; results in its destruction.
;;
;; Before opening this applies all the traps attached to the container. Note
;; that the self-destruct trap, as currently implemented, does not work as
;; expected, because it relies on the removal of the container from the map as
;; a means of destroying it; and that is not sufficient here.
(define (kcontainer-open kobj kchar)
  (let ((container (kobj-gob-data kobj))
        (loc (kern-obj-get-location kobj))
        )

    (cond 
     ((container-magic-locked? container)
      (kern-log-msg "Magically locked!\n")
      #f)
     ((container-locked? container)
      (kern-log-msg "Locked!\n")
      #f)
     (else

      ;; Applying traps can destroy both kobj and kchar
      (kern-obj-inc-ref kobj)
      (kern-obj-inc-ref kchar)
      
      ;; Apply traps (see trap.scm for trap-trigger)
      (map (lambda (trap)
             (trap-trigger trap kobj kchar))
           (container-traps container))
      
      (cond ((container-destroyed? container)
             nil)
            (else
             ;; Spill contents
             (map (lambda (content)
                    (let ((newobj (kern-mk-obj (eval (content-type content))
                                               (content-quantity content))))
                      (kern-obj-put-at newobj loc)))
                  (container-contents container))
             
             ;; Remove the container from the map
             (kern-obj-remove kobj)
             ))
      
      
      ;; Done with references
      (kern-obj-dec-ref kobj)
      (kern-obj-dec-ref kchar)
      ))))

(define (kcontainer-add-trap kobj trap-sym)
  (container-add-trap! (kobj-gob-data kobj)
                       trap-sym))

(define (kcontainer-get-traps kobj)
  (container-traps (kobj-gob-data kobj)))

(define (kcontainer-rm-traps kobj)
  (container-set-traps! (kobj-gob-data kobj) nil))

(define (kcontainer-self-destruct kobj)
  (let ((container (kobj-gob-data kobj)))
    (container-set-contents! container nil)
    (container-destroy! container)
    (kern-obj-remove kobj)
    ))

(define (kcontainer-search kobj kchar)
  ;; Searching can trigger traps, which can destroy both kobj and kchar
  (kern-obj-inc-ref kobj)
  (kern-obj-inc-ref kchar)
  (kern-log-begin "Searching chest...")
  (let ((container (gob kobj)))
    (if (foldr (lambda (detected? trap)
                 (trap-search trap kobj kchar)
                 (if (trap-tripped? trap)
                     (container-set-traps! container
                                           (filter (lambda (trap2)
                                                     (not (equal? trap trap2)))
                                                   (container-traps container))))
                 (or detected? (trap-detected? trap)))
               #f
               (container-traps container))
        (kern-log-end "Trap detected!")
        (kern-log-end "No traps detected!")
        ))
  ;; Done with references
  (kern-obj-dec-ref kobj)
  (kern-obj-dec-ref kchar)
  )

(define (kcontainer-describe kcontainer count)
  (let ((container (gob kcontainer)))
    (kern-log-continue "a ")
    (if (container-magic-locked? container)
        (kern-log-continue "magically locked, "))
    (if (container-locked? container)
        (if (container-needs-key? container)
            (kern-log-continue "locked (with a key), ")
            (kern-log-continue "padlocked, ")))
    (if (container-open? container)
        (kern-log-continue "open container ")
        (kern-log-continue "closed container "))
    (kern-log-continue "(")
    (if (foldr (lambda (described? trap)
                 (cond ((trap-detected? trap)
                        (if described?
                            (kern-log-continue ", "))
                        (kern-log-continue (trap-name trap))
                        (if (trap-tripped? trap)
                            (kern-log-continue "[disarmed]"))
                        #t)
                       (else 
                        described?)))
               #f
               (container-traps container))
        (kern-log-continue " trap(s) detected")
        (kern-log-continue "no traps detected")
        )
    (kern-log-continue ")")
    ))

(define (container-get-sprite container)
  (list-ref (container-sprites container)
            (let ((index (if (container-magic-locked? container)
                             (if (container-locked? container)
                                 3 ;; magic locked & locked
                                 2 ;; magic locked
                                 )
                             (if (container-locked? container)
                                 1 ;; locked
                                 0 ;; normal
                                 ))))
              (println "sprite-index: " index)
              index)))

(define (kcontainer-update-sprite kcontainer)
  (kern-obj-set-sprite kcontainer (container-get-sprite (gob kcontainer)))
  )

(define (kcontainer-lock kcontainer khandler)
  (let ((container (gob kcontainer)))
    (println "container-lock: " container)
    (cond ((container-open? container) (kern-log-msg "Not closed!\n") #f)
          ((container-locked? container) (kern-log-msg "Already locked!\n") #f)
          (else
           (container-set-locked! container #t)
           (kcontainer-update-sprite kcontainer)
           #t))))

(define (kcontainer-unlock kcontainer khandler)
  (let ((container (gob kcontainer)))
    (cond ((container-open? container) (kern-log-msg "Not closed!\n") #f)
          ((not (container-locked? container)) (kern-log-msg "Not locked!\n") #f)
          ((container-needs-key? container) (kern-log-msg "Needs the key!\n") #f)
          (else
           (container-set-locked! container #f)
           (kcontainer-update-sprite kcontainer)
           #t))))

(define (kcontainer-magic-lock kcontainer khandler)
  (let ((container (gob kcontainer)))
    (cond ((container-open? container) (kern-log-msg "Not closed!\n") #f)
          ((container-magic-locked? container) 
           (kern-log-msg "Already magically locked!\n") #f)
          (else
           (container-set-magic-locked! container #t)
           (kcontainer-update-sprite kcontainer)
           #t))))

(define (kcontainer-magic-unlock kcontainer khandler)
  (let ((container (gob kcontainer)))
    (println "container-magic-unlock: " container)
    (cond ((container-open? container) (kern-log-msg "Not closed!\n") #f)
          ((not (container-magic-locked? container)) 
           (kern-log-msg "Not magically locked!\n") #f)
          (else
           (container-set-magic-locked! container #f)
           (kcontainer-update-sprite kcontainer)
           #t))))

(define (kcontainer-use-key kcontainer key-type)
  (let ((container (gob kcontainer)))
    (println "container-use-key: " container)
    (cond ((container-open? container) (kern-log-msg "Not closed!"))
          ((not (container-key-fits? container key-type)) (kern-log-msg "Key won't fit!"))
          ((container-locked? container)
           (container-set-locked! container #f)
           (kcontainer-update-sprite kcontainer)
           )
          (else
           (container-set-locked! container #t)
           (kcontainer-update-sprite kcontainer)
           ))))

(define (kcontainer-lock-with-key kcontainer ktype)
  (let ((container (gob kcontainer)))
    (println "container-lock-with-key: " container " " ktype)
    (cond ((container-open? container) (kern-log-msg "Not closed!"))
          ((container-locked? container) (kern-log-msg "Already locked!"))
          (else
           (container-set-key! container ktype)
           (container-set-locked! container #t)
           (kcontainer-update-sprite kcontainer)
           ))))
  
(define (kcontainer-get-unlock-dc kcontainer)
  (let ((val (container-locked? (gob kcontainer))))
    ;; make it backwards-compatible for old saved games where the value is a bool
    (if (number? val)
        val
        (if val dc-normal 0))))

(define (kcontainer-get-magic-unlock-dc kcontainer)
  (let ((val (container-magic-locked? (gob kcontainer))))
    ;; make it backwards-compatible for old saved games where the value is a bool
    (if (number? val)
        val
        (if val dc-normal 0))))

;; This interface binds the 'open signal to our open procedure above.
(define container-ifc
  (ifc '()
       (method 'open kcontainer-open)
       (method 'add-trap kcontainer-add-trap)
       (method 'get-traps kcontainer-get-traps)
       (method 'rm-traps kcontainer-rm-traps)
       (method 'self-destruct kcontainer-self-destruct)
       (method 'search kcontainer-search)
       (method 'describe kcontainer-describe)

       (method 'lock kcontainer-lock)
       (method 'unlock kcontainer-unlock)
       (method 'magic-lock kcontainer-magic-lock)
       (method 'magic-unlock kcontainer-magic-unlock)
       (method 'use-key kcontainer-use-key)
       (method 'get-unlock-dc kcontainer-get-unlock-dc)
       (method 'get-magic-unlock-dc kcontainer-get-magic-unlock-dc)

       ))

;; This constructor makes new types of objects that conform to the container
;; interface above. An example of usage is below, where I make a new chest
;; type.
(define (mk-container-type tag name sprite)
  (mk-obj-type tag name sprite layer-mechanism container-ifc))

;; Test it out. First, make a new chest type.
(mk-container-type 't_chest "chest" s_chest)

;; Define a constructor for an object of the new chest type. Example usage:
;;
;; (put (mk-chest2 '((1 t_sword)
;;                   (5 t_arrow)
;;                   (2 t_torch)))
;;      5 8)
;;
;; * Note the use of a quoted list.
;;
(define chest-sprites (list s_chest
                            s_locked_chest
                            s_magic_chest
                            s_magic_locked_chest)
  )

(define (mk-chest trap contents)
  (let ((kchest (bind (kern-mk-obj t_chest 1)
                      (mk-container contents 'chest-sprites))))
    (if (not (null? trap))
        (container-add-trap! (kobj-gob-data kchest) trap))
    kchest))

(define (chest-add-trap kobj trap)
  (container-add-trap! (kobj-gob-data kobj) trap))

;; mk-treasure-chest -- returns a chest with 1-10 random object types
(define (mk-treasure-chest)
  (mk-chest nil
            (mk-quoted-treasure-list (+ 1
                                        (modulo (random-next) 
                                                9)))))

;;----------------------------------------------------------------------------
;; Animal corpse
;;
;; This does not really belong here, since it is not a container, but rather an
;; object that implements the 'butcher interface.

;; This procedure must take two args and return a boolean in order to fit into
;; the skills yusage framework.
(define (animal-corpse-butcher kobj kactor)
  (kern-obj-put-at (kern-mk-obj t_food 1) (kern-obj-get-location kobj))
  (kern-obj-remove kobj)
  #t
  )

(define animal-corpse-ifc
  (ifc nil
       (method 'butcher animal-corpse-butcher)
       ))

(mk-obj-type 't_animal_corpse "animal corpse" s_corpse layer-item animal-corpse-ifc)