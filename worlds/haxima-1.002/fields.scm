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
(kern-mk-field-type 'F_energy "energy field" s_field_energy   512  20 pclass-repel 'apply-lightning)
(kern-mk-field-type 'web-type "spider web"   s_spider_web     0    20 pclass-none 'ensnare)

(kern-mk-field-type 'F_poison_perm "poison field" s_field_poison 256 -1 pclass-none  'apply-poison)
(kern-mk-field-type 'F_sleep_perm  "sleep field"  s_field_sleep  256 -1 pclass-none  'apply-sleep)
(kern-mk-field-type 'F_energy_perm "energy field" s_field_energy 256 -1 pclass-repel 'apply-lightning)
(kern-mk-field-type 'F_fire_perm   "fire field"   s_field_fire   512 -1 pclass-none  'burn)
(kern-mk-field-type 'F_web_perm    "spider web"   s_spider_web   0   -1 pclass-none  'ensnare)

(define all-field-types
  (list F_fire F_poison F_sleep F_energy web-type
        F_fire_perm F_poison_perm F_sleep_perm F_energy_perm F_web_perm))

(define (is-field-type? ktype)
  (foldr (lambda (x field-type) (or x (eqv? ktype field-type)))
         #f 
         all-field-types))

(define (is-field? kobj)
  (is-field-type? (kern-obj-get-type kobj)))
