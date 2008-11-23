;;----------------------------------------------------------------------------
;; Doors
(define door-state-closed       0)
(define door-state-open         1)
(define door-state-locked       2)
(define door-state-magic-locked 3)

(define (mk-door-state sprite opacity pclass)
  (list sprite opacity pclass))
(define (door-state-sprite ds) (car ds))
(define (door-state-opacity ds) (cadr ds))
(define (door-state-pclass ds) (caddr ds))

(define (mk-door-states closed open locked magic-locked)
  (list closed open locked magic-locked))

;; Define the door gob structure and procedures.
(define (door-mk open? timeout port active? locked? magic-locked? type)
  (list open? timeout port active? locked? magic-locked? type nil nil))
(define (door-open? door) (car (gob-data door)))
(define (door-timeout door) (cadr (gob-data door)))
(define (door-port door) (list-ref (gob-data door) 2))
(define (door-active? door) (list-ref (gob-data door) 3))
(define (door-locked? door) (list-ref (gob-data door) 4))
(define (door-magic-locked? door) (list-ref (gob-data door) 5))
(define (door-states door) (eval (list-ref (gob-data door) 6)))
(define (door-traps door) (list-ref (gob-data door) 7))
(define (door-trapped? door) (not (null? (door-traps door))))
(define (door-key door) (list-ref (gob-data door) 8))
(define (door-needs-key? door) (not (null? (door-key door))))
(define (door-key-fits? door ktype)
  (let ((key (safe-eval (door-key door))))
    (and (not (null? key))
         (eqv? key ktype))))

(define (door-set-open door val) (set-car! (gob-data door) val))
(define (door-set-timeout! door time) (set-car! (cdr (gob-data door)) time))
(define (door-set-port! door port) (set-car! (cddr (gob-data door)) port))
(define (door-set-active! door val) (set-car! (cdddr (gob-data door)) val))
(define (door-set-locked! door val) (set-car! (cddddr (gob-data door)) val))
(define (door-set-magic-locked! door val)
  (list-set-ref! (gob-data door) 5 val))
(define (door-set-traps! door val) (list-set-ref! (gob-data door) 7 val))
(define (door-add-trap! door trap-type)
  (door-set-traps! door (cons (mk-trap (eval trap-type))
                              (door-traps door))))
(define (door-set-key! door key-type-tag) 
  (list-set-ref! (gob-data door) 8 key-type-tag))

(define (door-send-signal kdoor sig)
  (let ((door (kobj-gob kdoor)))
    (if (not (door-active? door))
        (begin
          (let ((port (door-port door)))
            (door-set-active! door #t)
            (if (not (null? port))
                (begin
                  ((kobj-ifc (eval port)) sig (eval port) kdoor)))
            (door-set-active! door #f))))))

(define (door-update-kstate kdoor)
  (define (update state-no)
    (let ((state (list-ref (door-states (kobj-gob kdoor)) state-no)))
      (kern-obj-set-sprite kdoor (door-state-sprite state))
      (kern-obj-set-opacity kdoor (door-state-opacity state))
      (kern-obj-set-pclass kdoor (door-state-pclass state))))
  (let ((door (kobj-gob kdoor)))
    (cond ((door-magic-locked? door) (update door-state-magic-locked))
          ((door-locked? door)       (update door-state-locked))
          ((door-open? door)         (update door-state-open))
          (else                      (update door-state-closed))))
  (kern-map-set-dirty)
  kdoor)

(define (door-trip-traps kdoor kchar)
  (let ((door (kobj-gob kdoor))
        )
    (kern-obj-inc-ref kdoor)
    (kern-obj-inc-ref kchar)
    (map (lambda (trap)
           (trap-trigger trap kdoor kchar))
         (door-traps door))
    (door-set-traps! door nil)
    (kern-obj-dec-ref kdoor)
    (kern-obj-dec-ref kchar)))

(define (door-open kdoor khandler) 
  (let ((door (kobj-gob kdoor)))
    (cond 
     ((door-magic-locked? door)
      (kern-log-msg "Magically locked!\n")
      #f)
     ((door-locked? door)
      (kern-log-msg "Locked!\n")
      #f)
     ((door-trapped? door)
      (door-trip-traps kdoor khandler)
      (door-open kdoor khandler)
      )
      (else
       (door-set-open door #t)
       (door-set-timeout! door 10)
       (door-update-kstate kdoor)
       (door-send-signal kdoor 'open)
       #t))))
  
(define (door-close kdoor khandler)
  ;;(display "door-close")(newline)
  (if (not (occupied? (kern-obj-get-location kdoor)))
      (let ((door (kobj-gob kdoor)))
        (door-set-open door #f)
        (door-set-timeout! door 0)
        (door-update-kstate kdoor)
        (door-send-signal kdoor 'close)
        #t)))

(define (door-lock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-lock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n") #f)
          ((door-locked? door) (kern-log-msg "Already locked!\n") #f)
          (else
           (door-set-locked! door #t)
           (door-update-kstate kdoor)
           #t))))

(define (door-unlock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-unlock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n") #f)
          ((not (door-locked? door)) (kern-log-msg "Not locked!\n") #f)
          ((door-needs-key? door) (kern-log-msg "Needs the key!\n") #f)
          (else
           (door-set-locked! door #f)
           (door-update-kstate kdoor)
           #t))))

(define (door-magic-lock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-magic-lock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n") #f)
          ((door-magic-locked? door) 
           (kern-log-msg "Already magically locked!\n") #f)
          (else
           (door-set-magic-locked! door #t)
           (door-update-kstate kdoor)
           #t))))

(define (door-magic-unlock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-magic-unlock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n") #f)
          ((not (door-magic-locked? door)) 
           (kern-log-msg "Not magically locked!\n") #f)
          (else
           (door-set-magic-locked! door #f)
           (door-update-kstate kdoor)
           #t))))

(define (door-handle kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    (if (door-open? door) 
        (door-close kdoor khandler)
        (door-open kdoor khandler))))

(define (door-exec kdoor)
  (let ((door (kobj-gob kdoor)))
    (if (door-open? door)
        (let ((timeout (door-timeout door)))
          (cond ((> timeout 1) (door-set-timeout! door (- timeout 1)))
                ((= timeout 1) (door-close kdoor '())))))))


(define (door-connect kobj kto-tag)
  (let ((door (kobj-gob kobj)))
    (door-set-port! door kto-tag)))

(define (door-add-trap kdoor trap-sym)
  (let ((door (kobj-gob kdoor)))
    (door-add-trap! door trap-sym)))
  
(define (door-get-traps kdoor)
  (door-traps (kobj-gob kdoor)))

(define (door-rm-traps kdoor)
  (let ((door (kobj-gob kdoor)))
    (door-set-traps! door nil)))

(define (door-use-key kdoor key-type)
  (let ((door (kobj-gob kdoor)))
    (cond ((door-open? door) (kern-log-msg "Not closed!"))
          ((not (door-key-fits? door key-type)) (kern-log-msg "Key won't fit!"))
          ((door-locked? door)
           (door-set-locked! door #f)
           (door-update-kstate kdoor))
          (else
           (door-set-locked! door #t)
           (door-update-kstate kdoor)))))

(define (door-search kdoor kchar)
  (kern-log-begin "Searching door...")
  (let ((door (kobj-gob kdoor)))
    (if (foldr (lambda (detected? trap)
                 (trap-search trap kdoor kchar)
                 (if (trap-tripped? trap)
                     (door-set-traps! door
                                      (filter (lambda (trap2)
                                                (not (equal? trap trap2)))
                                              (door-traps door))))
                 (or detected? (trap-detected? trap)))
               #f
               (door-traps door))
        (kern-log-end "Trap detected!")
        (kern-log-end "No traps detected!")
        )))

(define (door-describe kdoor count)
  (let ((door (kobj-gob kdoor)))
    (kern-log-continue "a ")
    (if (door-magic-locked? door)
        (kern-log-continue "magically locked, "))
    (if (door-locked? door)
        (if (door-needs-key? door)
            (kern-log-continue "locked (with a key), ")
            (kern-log-continue "padlocked, ")))
    (if (door-open? door)
        (kern-log-continue "open door ")
        (kern-log-continue "closed door "))
    (kern-log-continue "(")
    (if (foldr (lambda (described? trap)
                 (cond ((trap-detected? trap)
                        (if described?
                            (kern-log-continue ", "))
                        (kern-log-continue (trap-name trap))
                        (if (trap-tripped? trap)
                            (kern-log-continue "[disarmed]"))
                        #t)
                       (else described?)))
               #f
               (door-traps door))
        (kern-log-continue " trap(s) detected")
        (kern-log-continue "no traps detected")
        )
    (kern-log-continue ")")
    ))

(define (door-get-unlock-dc kdoor)
  (let ((val (door-locked? (kobj-gob kdoor))))
    ;; make it backwards-compatible for old saved games where the value is a bool
    (if (number? val)
        val
        (if val dc-normal 0))))

(define (door-get-magic-unlock-dc kdoor)
  (let ((val (door-magic-locked? (kobj-gob kdoor))))
    ;; make it backwards-compatible for old saved games where the value is a bool
    (if (number? val)
        val
        (if val dc-normal 0))))

(define door-ifc
  (ifc '()
       (method 'exec door-exec)
       (method 'handle door-handle)
       (method 'open door-open)
       (method 'close door-close)
       (method 'init door-update-kstate)
       (method 'connect door-connect)
       (method 'lock door-lock)
       (method 'unlock door-unlock)
       (method 'magic-lock door-magic-lock)
       (method 'magic-unlock door-magic-unlock)
       (method 'add-trap door-add-trap)
       (method 'get-traps door-get-traps)
       (method 'rm-traps door-rm-traps)
       (method 'use-key door-use-key)
       (method 'search door-search)
       (method 'describe door-describe)
       (method 'get-unlock-dc door-get-unlock-dc)
       (method 'get-magic-unlock-dc door-get-magic-unlock-dc)
       ))

;; Create the kernel "door" type
(mk-obj-type 't_door "door" s_stone_arch layer-mechanism 
             door-ifc)

(define (door-state-factory
				arch-sprite door-sprite magic-sprite
				open-opacity closed-opacity
				open-pclass closed-pclass)
	(mk-door-states
		(mk-door-state (mk-composite-sprite (list arch-sprite door-sprite))
					closed-opacity closed-pclass)
		(mk-door-state arch-sprite	open-opacity open-pclass)
		(mk-door-state (mk-composite-sprite (list arch-sprite door-sprite s_door_lock))
					closed-opacity closed-pclass)
		(mk-door-state (mk-composite-sprite (list arch-sprite door-sprite s_door_magiclock))
					closed-opacity closed-pclass)
	))

;; Types for common door types
(define solid-wood-door-in-stone
	(door-state-factory
			s_stone_arch s_door_wood s_door_magiclock
			#f #t
			pclass-none pclass-wall))

(define windowed-wood-door-in-stone
	(door-state-factory
			s_stone_arch s_door_windowed s_door_magiclock
			#f #f
			pclass-none pclass-window))

(define solid-wood-door-in-rock
	(door-state-factory
			s_rock_arch s_door_wood s_door_magiclock
			#f #t
			pclass-none pclass-wall))

(define windowed-wood-door-in-rock
	(door-state-factory
			s_rock_arch s_door_windowed s_door_magiclock
			#f #f
			pclass-none pclass-window))
   
;;----------------------------------------------------------------------------
;; mk-door -- make and initialize a door object
;;
;; Used by the startup scripts when creating new doors.
;;
;;          type: one of the door state sets listed above
;;        locked: true iff door starts out locked
;;  magic-locked: true iff door starts out magically locked
;;  connected-to: nil, or the tag of an object the door forwards signals to
;;----------------------------------------------------------------------------
(define (mk-door-full type locked? magic-locked? connected-to)
  (bind (kern-mk-obj t_door 1)
        (door-mk #f 0 connected-to #f locked? magic-locked? type)))

;; Backward-compatible curried constructors
(define (mk-door) (mk-door-full 'solid-wood-door-in-stone #f #f nil))
(define (mk-door-in-rock) (mk-door-full 'solid-wood-door-in-rock #f #f nil))
(define (mk-locked-door) (mk-door-full 'solid-wood-door-in-stone #t #f nil))
(define (mk-locked-door-in-rock) (mk-door-full 'solid-wood-door-in-rock #t #f nil))
(define (mk-connected-door tag) (mk-door-full 'solid-wood-door-in-stone #f #f tag))
(define (mk-windowed-door) (mk-door-full 'windowed-wood-door-in-stone #f #f nil))
(define (mk-windowed-door-in-rock) (mk-door-full 'windowed-wood-door-in-rock #f #f nil))
(define (mk-magic-locked-door) (mk-door-full 'solid-wood-door-in-stone #f #t nil))
(define (mk-locked-windowed-door) 
  (mk-door-full 'windowed-wood-door-in-stone #t #f nil))
(define (mk-locked-windowed-door-in-rock) 
  (mk-door-full 'windowed-wood-door-in-rock #t #f nil))

(define (lock-door-with-key kdoor key-type-tag)
  (lock-door kdoor nil)
  (door-set-key! (kobj-gob kdoor) key-type-tag)
  )
    

;; Add a trap to a door
(define (trap-door kdoor trap-tag)
  (ifccall kdoor 'add-trap trap-tag)
  kdoor
  )
  
(mk-obj-type 't_archway_rock "archway" s_rock_arch layer-mechanism 
             nil)

(mk-obj-type 't_archway_stone "archway" s_stone_arch layer-mechanism 
             nil)
        
(define (mk-archway-rock) (kern-mk-obj t_archway_rock 1))

(define (mk-archway-stone) (kern-mk-obj t_archway_rock 1))


