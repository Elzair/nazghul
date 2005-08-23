;;----------------------------------------------------------------------------
;; gate-guard -- wizard guards of the Enchanter's Tower
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (mk-gate-guard-gob gate-tag)
  (list 'gate-guard gate-tag 0))

(define (gate-guard-gate-tag guard) (cadr guard))
(define (gate-guard-gate-timer guard) (caddr guard))
(define (gate-guard-set-gate-timer! guard val) (set-car! (cddr guard) val))
(define (gate-guard-start-timer! guard) (gate-guard-set-gate-timer! guard 10))

(define (char-is-gate-guard? kchar)
  (let ((gob (kobj-gob-data kchar)))
    (if (notnull? gob)
        (eq? (car gob) 'gate-guard)
        #f)))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

(define (gate-guard-default knpc kpc)
  (say knpc "[No reply]"))

(define (gate-guard-hail knpc kpc)
  (say knpc "Halt! What is the password?")
  (let ((passwd (kern-conv-get-reply kpc)))
    (if (eq? passwd 'void)
        (let* ((guard (kobj-gob-data knpc))
               (gate (eval (gate-guard-gate-tag guard))))
          (signal-kobj gate 'on gate nil)
          (gate-guard-start-timer! guard)
          (say knpc "You may pass")
          (kern-conv-end))
        (say knpc "That is not correct"))))

(define gate-guard-conv
  (ifc nil
       (method 'default gate-guard-default)
       (method 'hail gate-guard-hail)
       ))


;;----------------------------------------------------------------------------
;; AI
;;----------------------------------------------------------------------------
(define (guard-is-holding-gate-open? guard)
  (> (gate-guard-gate-timer guard) 0))

(define (guard-dec-gate-timer! guard)
  (gate-guard-set-gate-timer! guard (- (gate-guard-gate-timer guard) 1))
  (if (<= (gate-guard-gate-timer guard) 0)
      (let ((kgate (eval (gate-guard-gate-tag guard))))
        (signal-kobj kgate 'off kgate nil)
        (gate-guard-set-gate-timer! guard 0))))

(define (guard-start-gate-timer! guard)
  (gate-guard-start-timer! guard))

(define (gate-is-open? kgate)
  (signal-kobj kgate 'is-on? kgate nil))

(define (gate-guard-ai kchar)
  (let* ((guard (kobj-gob-data kchar))
         (kgate (eval (gate-guard-gate-tag guard))))
    (if (guard-is-holding-gate-open? guard)
        (guard-dec-gate-timer! guard)
        (if (gate-is-open? kgate)
            (guard-start-gate-timer! guard)))))

;;----------------------------------------------------------------------------
;; Constructor
;;----------------------------------------------------------------------------
(define (mk-gate-guard gate-tag)
  (bind
   (kern-char-arm-self
    (mk-stock-char
     "a mage guard" ;;......name
     sp_human ;;.........species
     oc_wizard ;;........occupation
     s_wizard ;;........sprite
     faction-men ;;...faction
     'gate-guard-ai ;;.......custom ai (optional)
     
     ;;..................container (and contents, used to arm char)
     (mk-chest
      'lightning-trap
      (mk-contents 
       (roll-to-add 100  "1d3-1" t_heal_potion)
       (roll-to-add 100  "1d3-1" t_mana_potion)
       (roll-to-add 100  "1"     t_dagger)
       (roll-to-add 50   "1d2"   t_oil)
       (roll-to-add 100  "1d5"   t_gold_coins)
       ))
     
     nil ;;...............readied arms (in addition to container contents)
     'gate-guard-conv ;;..conversation
     ))
   (mk-gate-guard-gob gate-tag)))

;;----------------------------------------------------------------------------
;; gate-guard generator
;;----------------------------------------------------------------------------
(define (mk-gate-guard-gen is-one? mk-one . mk-args)
  (mk-mongen2 0 1 is-one? mk-one mk-args))
