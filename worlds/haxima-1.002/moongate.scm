;; ----------------------------------------------------------------------------
;; Moongate sprites & light levels
;; ----------------------------------------------------------------------------
(define moongate-stages
  (list (list '()                       0)
        (list s_moongate_quarter        32)
        (list s_moongate_half           64)
        (list s_moongate_three_quarters 96)
        (list s_moongate_full           128)))

(define blackgate-stages
  (list (list '()                       0)
        (list s_blackgate_quarter        32)
        (list s_blackgate_half           64)
        (list s_blackgate_three_quarters 96)
        (list s_blackgate_full           128)))

(define (stage-sprite stage) (car stage))
(define (stage-light stage) (* 10 (cadr stage)))
(define moongate-default-ttl 10) ;; turns

;; ----------------------------------------------------------------------------
;; Moongate gob
;; ----------------------------------------------------------------------------
(define (moongate-mk moontag temp?)
  (list moontag #f '() #f temp? moongate-default-ttl))

(define (moongate-kdest gate) 
  ;;(println "gate:" gate)
  (let ((kmoon (safe-eval (car gate))))
    ;;(println "moon:" kmoon)
    (cond ((null? kmoon) nil)
          (else (moon-get-current-gate kmoon)))))
(define (moongate-open? gate) (cadr gate))
(define (moongate-sequence gate) (caddr gate))
(define (moongate-pending-open? gate) (car (cdddr gate)))
(define (moongate-closed? gate) (and (not (moongate-open? gate))
                                     (not (moongate-pending-open? gate))))
(define (moongate-is-temporary? gate) (car (cddddr gate)))
(define (moongate-get-ttl gate) (list-ref gate 5))
(define (moongate-set-ttl! gate val) (set-car! (list-tail gate 5) val))

(define (moongate-set-open! gate val)
  (set-car! (cdr gate) val))
(define (moongate-set-sequence! gate sequence)
  (set-car! (cddr gate) sequence))
(define (moongate-set-pending-open! gate open?)
  (set-car! (cdddr gate) open?))

(define (moongate-destroy kgate)
  ;;(println "moongate-destroy")
  (kern-obj-remove kgate))

;; ----------------------------------------------------------------------------
;; Moongate cut scene
;; ----------------------------------------------------------------------------
(define (moongate-animate kgate stages)
  (let ((view (kern-map-view-create))
        ;; Commented-these out to fix mouse.scm's moongate animation; doesn't
        ;; seem to effect the starting scene animation which is the only other
        ;; reference I see to this procedure. Leaving these as comments for now
        ;; just in case.  Update: I think commenting these out causes the
        ;; destination gate to remain open in normal moongate travel. Need to
        ;; revisit and fix all cases here. SF bug #1520871. Update 2: this was
        ;; fixed in moongate-cut-scene, below.
        ;;(original-sprite (kern-obj-get-sprite kgate))
        ;;(original-light (kern-obj-get-light kgate))
        (loc (kern-obj-get-location kgate)))
    (kern-map-view-add view)
    (kern-map-view-center view loc)
    (kern-map-center-camera loc)
    (map (lambda (stage)
           (kern-obj-set-sprite kgate (stage-sprite stage))
           (kern-obj-set-light kgate (stage-light stage))
           (kern-map-repaint)
           (kern-sleep 250))
         stages)
    ;;(kern-obj-set-sprite kgate original-sprite)
    ;;(kern-obj-set-light kgate original-light)
    (kern-map-view-rm view)
    (kern-map-view-destroy view)
    ))

(define (moongate-cut-scene src-kgate dest-kgate)
  (moongate-animate src-kgate (reverse moongate-stages))
  (kern-sound-play sound-moongate-enter)
  (kern-map-flash 1000)
  (kern-place-synch (car (kern-obj-get-location dest-kgate)))
  (moongate-animate dest-kgate moongate-stages)
  ;; "erase" the destination gate so it doesn't look like it remains open
  (if (not (eqv? dest-kgate src-kgate))
      (kern-obj-set-sprite dest-kgate (stage-sprite (car moongate-stages))))
  (let ((gate (kobj-gob-data src-kgate)))
    (if (moongate-is-temporary? gate)
        (moongate-destroy src-kgate))))

(define (mk-moongate-cut-scene src-kgate dest-kgate)
  (lambda () (moongate-cut-scene src-kgate dest-kgate)))


;; ----------------------------------------------------------------------------
;; Moongate signal handlers
;; ----------------------------------------------------------------------------
(define (moongate-step kgate kstepper)
  (let ((gate (kobj-gob-data kgate)))
    (if (moongate-open? gate)
        (let ((kdest (moongate-kdest gate)))
          (cond ((null? kdest) (kern-print "Leads nowhere!\n"))
                (else
                 (kern-obj-relocate kstepper 
                                    (kern-obj-get-location kdest)
                                    (mk-moongate-cut-scene kgate kdest))))))))

;; Opens/closes a moongate, running the animation on the timer tick (not to be
;; confused with the cut-scene animation that plays when somebody steps through
;; the gate)
(define (moongate-run-sequence kgate)
  (let* ((gate (kobj-gob-data kgate))
         (stages (moongate-sequence gate)))
    (if (null? stages)
        (moongate-set-open! gate (moongate-pending-open? gate))
        (let ((stage (car stages)))
          (kern-obj-set-sprite kgate (stage-sprite stage))
          (kern-obj-set-light kgate (stage-light stage))
          (kern-map-set-dirty)
          (moongate-set-sequence! gate (cdr stages))
          (kern-add-tick-job 1 moongate-run-sequence kgate)))))

;; The following version does not use the tick queue, however if you use any
;; delay at all it noticeably pauses the game. This is especially annoying when
;; the moongate is not visible or even in the same place as the player, who
;; sees only inexplicable pauses in responsiveness. To make this work smoothly
;; only moongates which have a visible or at least audible effect should cause
;; a map repaint and a pause. It will require another kernel call to determine
;; if this is the case.

; (define (moongate-run-sequence kgate)
;   (let* ((gate (kobj-gob-data kgate))
;          (stages (moongate-sequence gate)))
;     (if (null? stages)
;         (moongate-set-open! gate (moongate-pending-open? gate))
;         (let ((stage (car stages)))
;           (kern-obj-set-sprite kgate (stage-sprite stage))
;           (kern-obj-set-light kgate (stage-light stage))
;           (kern-map-repaint)
;           (kern-sleep 100)
;           (moongate-set-sequence! gate (cdr stages))
;           (moongate-run-sequence kgate)))))


(define (moongate-setup-sequence kgate gate open? stages)
  (moongate-set-pending-open! gate open?)
  (moongate-set-sequence! gate stages)
  (kern-add-tick-job 1 moongate-run-sequence kgate))

(define (moongate-open kgate)
  (let ((gate (kobj-gob-data kgate)))
    ;;(println "moongate-open:gob=" gate)
    (if (not (moongate-open? gate))
        (moongate-setup-sequence kgate gate #t moongate-stages)
        )))

(define (moongate-close kgate)
  ;;(println "moongate-close")
  (let ((gate (kobj-gob-data kgate)))
    (if (not (moongate-closed? gate))
        (moongate-setup-sequence kgate gate #f (reverse moongate-stages))
        )))

(define (moongate-init kgate)
  (let ((gate (kobj-gob-data kgate)))
    (if (moongate-open? gate)
        (moongate-setup-sequence kgate gate #t moongate-stages))))

(define (moongate-exec kgate)
  (let ((gate (gob kgate)))
    (if (moongate-is-temporary? gate)
        (let ((ttl (- (moongate-get-ttl gate) 1)))
          (moongate-set-ttl! gate ttl)
          (if (<= ttl 0)
              (begin
                (moongate-animate kgate (reverse moongate-stages))
                (moongate-destroy kgate)
                ))))))

;; ----------------------------------------------------------------------------
;; Moongate gifc, kobj-type & constructor
;; ----------------------------------------------------------------------------
(define moongate-ifc
  (ifc '()
       (method 'step moongate-step)
       (method 'on moongate-open)
       (method 'off moongate-close)
       (method 'init moongate-init)
       (method 'exec moongate-exec)
       ))

(mk-obj-type 't_moongate "moongate" '() layer-mechanism moongate-ifc)

(define (mk-moongate moontag)
  (bind (kern-mk-obj t_moongate 1)
        (moongate-mk moontag #f)))

(define (summon-moongate moontag)
  (bind (kern-mk-obj t_moongate 1)
        (moongate-mk moontag #t)))
