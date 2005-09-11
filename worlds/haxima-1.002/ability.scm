;;----------------------------------------------------------------------------
;; Ability "class"
;;----------------------------------------------------------------------------
(define (mk-ability name level mana ap proc)
  (list name level mana ap proc))

(define (ability-name ability) (car ability))
(define (ability-level-required ability) (cadr ability))
(define (ability-mana-cost ability) (caddr ability))
(define (ability-ap-cost ability) (cadddr ability))
(define (ability-proc ability) (list-ref ability 4))

(define (can-use-ability? ability kchar)
  (and (>= (kern-char-get-mana kchar)
           (ability-mana-cost ability))
       (>= (kern-char-get-level kchar)
           (ability-level-required ability))))

(define (use-ability ability kchar . args)
  (apply (ability-proc ability) (cons kchar args))
  (kern-char-dec-mana kchar (ability-mana-cost ability))
  (kern-obj-dec-ap kchar (ability-ap-cost ability)))

;;----------------------------------------------------------------------------
;; Ability procedures
;;----------------------------------------------------------------------------

(define (vampiric-touch-proc kchar ktarg)
  (let ((amount (* (kern-dice-roll "1d3")
                   (kern-char-get-level kchar))))
    (kern-obj-apply-damage ktarg "life drained" amount)
    (kern-obj-heal kchar amount)
    (kern-log-msg (kern-obj-get-name kchar)
                  " drains life from "
                  (kern-obj-get-name ktarg)
                  "!")))

(define (disease-touch-proc kchar ktarg)
  (if (kern-obj-add-effect ktarg ef_disease nil)
      (kern-log-msg (kern-obj-get-name kchar)
                    " inflicts "
                    (kern-obj-get-name ktarg)
                    " with Disease!")))

;;----------------------------------------------------------------------------
;; Ability declarations
;;----------------------------------------------------------------------------

(define vampiric-touch (mk-ability "vampiric touch" 3 3 3 vampiric-touch-proc))
(define disease-touch (mk-ability "disease touch" 6 6 2 disease-touch-proc))
