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

(kern-mk-field-type 'F_illum  "glowing mote" s_magic          1024 5  pclass-none  nil)
(kern-mk-field-type 'F_fire   "fire field"   s_field_fire     512  20 pclass-none  'burn)
(kern-mk-field-type 'F_poison "poison field" s_field_poison   256  20 pclass-none  'apply-poison)
(kern-mk-field-type 'F_sleep  "sleep field"  s_field_sleep    256  20 pclass-none  'apply-sleep)
(kern-mk-field-type 'F_sleep_perm "sleep field" s_field_sleep 256 -1  pclass-none  'apply-sleep)
(kern-mk-field-type 'F_energy "energy field" s_field_energy   512  20 pclass-repel 'apply-lightning)

(define field-tags
  (list F_fire F_poison F_sleep F_sleep_perm))

(define (is-field-type? ktype)
  (foldr (lambda (x tag) (or x (eqv? ktype tag)))
         #f 
         field-tags))
