;;----------------------------------------------------------------------------
;; Containers - objects that contain stuff
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------
(define (add-content quantity type)
  (list quantity type))

(define (mk-contents . contents)
  (filter notnull? contents))

(define (roll-100 prob)
  (>= prob (modulo (random-next) 100)))

(define (roll-q dice type)
  (add-content (kern-dice-roll dice) type))

(define (roll-to-add prob dice type)
  (if (roll-100 prob)
      (roll-q dice type)
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
(define (mk-container contents) (list 'container contents nil #f))
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
    (println container)
    (println "contents: " (container-contents container))
    (println "traps: " (container-traps container))
    (println "destroyed: " (container-destroyed? container))

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
                  (println content)
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
    ))

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
    (println "container=" container)
    (if (foldr (lambda (detected? trap)
                 (println "trap=" trap)
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

(define (kcontainer-describe kobj count)
  (kern-log-continue "a chest (")
  (if (foldr (lambda (described? trap)
               (cond ((trap-detected? trap)
                      (if described?
                          (kern-log-continue ", "))
                      (kern-log-continue (trap-name trap))
                      #t)
                     (else described?)))
             #f
             (container-traps (gob kobj)))
      (kern-log-continue " trap(s) detected")
      (kern-log-continue "no traps detected")
      )
  (kern-log-continue ")")
  )

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
(define (mk-chest trap contents)
  (let ((kchest (bind (kern-mk-obj t_chest 1)
                      (mk-container contents))))
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