;;----------------------------------------------------------------------------
;; fields.scm - field types supported by the game
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; kern-mk-field-type <tag> <name> <sprite> <light> <dur> <pmask> <effect>
;; 
;; 'effect' is a procedure called whenever an object is positioned over the
;; field. Its only parameter is the object.
;;
;; 'light' is the amount of light radiated by the field.
;;
;; 'dur' is the number of turns the field will exist before disappearing.
;; 
;; 'pmask' is the objective pmask (the passability it permits to cross it)
;;
;; 'effect' is an optional procedure to run on an object which steps on the
;; field. See effects.scm.
;;----------------------------------------------------------------------------

(kern-mk-field-type 'F_illum  "glowing mote" s_magic          1024 5  pclass-none  nil	mmode-field)
(kern-mk-field-type 'F_fire   "fire field"   s_field_fire     512  20 pclass-none  'burn	mmode-field)
(kern-mk-field-type 'F_poison "poison field" s_field_poison   256  20 pclass-none  'apply-poison	mmode-field)
(kern-mk-field-type 'F_sleep  "sleep field"  s_field_sleep    256  20 pclass-none  'apply-field-sleep	mmode-field)
(kern-mk-field-type 'F_energy "energy field" s_field_energy   512  20 pclass-repel 'apply-lightning	mmode-field)
(kern-mk-field-type 'F_acid "acid field" s_field_acid   256  20 pclass-none 'apply-acid	mmode-field)
(kern-mk-field-type 'web-type "spider web"   s_spider_web     0    20 pclass-none  'ensnare	mmode-field)

(kern-mk-field-type 'F_poison_perm "poison field" s_field_poison 256 -1 pclass-none  'apply-poison	mmode-field)
(kern-mk-field-type 'F_sleep_perm  "sleep field"  s_field_sleep  256 -1 pclass-none  'apply-field-sleep	mmode-field)
(kern-mk-field-type 'F_energy_perm "energy field" s_field_energy 512 -1 pclass-repel 'apply-lightning	mmode-field)
(kern-mk-field-type 'F_fire_perm   "fire field"   s_field_fire   512 -1 pclass-none  'burn	mmode-field)
(kern-mk-field-type 'F_acid_perm "acid field" s_field_acid 256 -1 pclass-none 'apply-acid	mmode-field)
(kern-mk-field-type 'F_web_perm    "spider web"   s_spider_web   0   -1 pclass-none  'ensnare	mmode-field)
(kern-mk-field-type 'F_illum_perm    nil   nil   256   -1 pclass-none  nil	mmode-field)

(define all-field-types
  (list F_fire F_poison F_sleep F_energy web-type
        F_fire_perm F_poison_perm F_sleep_perm F_energy_perm F_web_perm))

(define (is-field-type? ktype)
  (foldr (lambda (x field-type) (or x (eqv? ktype field-type)))
         #f 
         all-field-types))

(define (is-field? kobj)
  (kern-obj-is-field? kobj))

(define (is-fire-field? ktype)
  (or (eqv? ktype F_fire)
      (eqv? ktype F_fire_perm)))

(define (is-poison-field? ktype)
  (or (eqv? ktype F_poison)
      (eqv? ktype F_poison_perm)))

(define (is-sleep-field? ktype)
    (or (eqv? ktype F_sleep)
        (eqv? ktype F_sleep_perm)))

(define (is-energy-field? ktype)
    (or (eqv? ktype F_energy)
        (eqv? ktype F_energy_perm)))

(define (is-immune-to-field? kchar kfield)
  (let ((ktype (kern-obj-get-type kfield)))
    (cond ((is-fire-field? ktype) (has-fire-immunity? kchar))
          ((is-poison-field? ktype) (has-poison-immunity? kchar))
          ((is-sleep-field? ktype) (has-sleep-immunity? kchar))
          (else #f))))
                
(define (apply-field-sleep kobj)
	(if (> (modulo (random-next) 40) 1)
    	(apply-sleep kobj)
    ))
    
;; smoke is here since it more closely resembles a field than anything else
;; TODO: smoke should calculate a duration and store that in a gob
;;   so that denser smoke can be created  

(define smoke-ifc
  (ifc nil
       (method 'exec (lambda (ksmoke)
                       (if (> (kern-dice-roll "1d20") 16)
                           (kern-obj-remove ksmoke)

                           ;; smoke drifts with the wind in wilderness combat
                           (let ((curloc (kern-obj-get-location ksmoke)))
                             (if (kern-place-is-combat-map? (loc-place curloc))
                                 (let ((loc (loc-offset (kern-obj-get-location ksmoke) 
                                                        (vector-ref opposite-dir (kern-get-wind)))))
                                   (if (not (kern-is-valid-location? loc))                            
                                       (kern-obj-remove ksmoke)
                                       (begin
                                         (kern-obj-relocate ksmoke loc nil)
                                         (kern-los-invalidate)
                                         ))))))))))


(mk-obj-type 't_smoke_cloud "smoke" s_smoke layer-projectile smoke-ifc)

(define (fields-smoke-apply kplace x y power)
	(define (tryput loc)
		(if (terrain-ok-for-field? loc)
			(let ((kfield (kern-mk-obj t_smoke_cloud 1)))
				(kern-obj-set-opacity kfield #t)
				(kern-obj-put-at kfield loc)))
	)
	(tryput (mk-loc kplace x y))
	(tryput (mk-loc kplace (- x 1) y))
	(tryput (mk-loc kplace (+ x 1) y))
	(tryput (mk-loc kplace x (- y 1)))
	(tryput (mk-loc kplace x (+ y 1)))
	(tryput (mk-loc kplace (- x 1) (- y 1)))
	(tryput (mk-loc kplace (- x 1) (+ y 1)))
	(tryput (mk-loc kplace (+ x 1) (- y 1)))
	(tryput (mk-loc kplace (+ x 1) (+ y 1)))
	(kern-los-invalidate)
)
