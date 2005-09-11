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
  ;(display "can-use-ability?")(display ability)(newline)
  (and (>= (kern-char-get-mana kchar)
           (ability-mana-cost ability))
       (>= (kern-char-get-level kchar)
           (ability-level-required ability))))

(define (use-ability ability kchar . args)
  (kern-char-dec-mana kchar (ability-mana-cost ability))
  (kern-obj-dec-ap kchar (ability-ap-cost ability))
  (apply (ability-proc ability) (cons kchar args)))


;;----------------------------------------------------------------------------
;; Ability procedures
;;----------------------------------------------------------------------------

(define (vampiric-touch-proc kchar ktarg)
  (let ((amount (min (* (kern-dice-roll "1d3")
                        (kern-char-get-level kchar))
                     (kern-char-get-hp ktarg))))
    (kern-obj-inc-ref ktarg)
    (kern-obj-apply-damage ktarg "life drained" amount)
    (kern-obj-heal kchar amount)
    (kern-log-msg (kern-obj-get-name kchar)
                  " drains life from "
                  (kern-obj-get-name ktarg)
                  "!")
    (kern-obj-dec-ref ktarg))
  #t)

(define (disease-touch-proc kchar ktarg)
  (if (kern-obj-add-effect ktarg ef_disease nil)
      (kern-log-msg (kern-obj-get-name kchar)
                    " inflicts "
                    (kern-obj-get-name ktarg)
                    " with Disease!"))
  #t)

(define (disarm kchar ktarg)
  (let ((readied (kern-char-get-readied-weapons ktarg)))
    (if (null? readied)
        #f
        (if (> (kern-char-get-level kchar)
               (+ (kern-dice-roll "1d3-1")
                  (kern-char-get-level ktarg)))
            (let ((ktype (random-select readied)))
              (kern-log-msg (kern-obj-get-name kchar)
                            " disarms "
                            (kern-obj-get-name ktarg))
              (kern-char-unready ktarg ktype)
              (kern-obj-remove-from-inventory ktarg ktype 1)
              (kern-obj-add-to-inventory kchar ktype 1))
            (kern-log-msg  (kern-obj-get-name kchar)
                           " fails to disarm "
                           (kern-obj-get-name ktarg))
            #t))))

;;----------------------------------------------------------------------------
;; Ability declarations
;;----------------------------------------------------------------------------

(define vampiric-touch (mk-ability "vampiric touch" 3 3 2 vampiric-touch-proc))
(define disease-touch (mk-ability "disease touch" 6 6 1 disease-touch-proc))
(define disarm (mk-ability "disarm" 4 2 2 disarm))
