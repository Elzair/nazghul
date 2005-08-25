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
;; Mage Guard AI
;;
;; FIXME: when and if mundane gate guards are added don't hardcode the
;; gate-guard AI to use this "sub"-AI
;;----------------------------------------------------------------------------
(define (need-more-troops? kchar)
  (> (length (all-visible-hostiles kchar)) 
     (length (all-visible-allies kchar))))

(define (mguard-cast-spell kchar ktarg)
  (if (and (need-more-troops? kchar)
           (> (kern-char-get-mana kchar) 4))
      (begin
        (kern-log-msg "The mage guard summons help!")
        (summon (kern-obj-get-location ktarg) 
                mk-ranger
                (kern-being-get-current-faction kchar)
                (kern-dice-roll "1d3"))
        (kern-char-dec-mana kchar 4)
        (kern-obj-dec-ap kchar 4)
        #t)
      ;; don't need or can't summon more troops
      (if (and (is-undead? ktarg)
               (can-cast? kchar an-xen-corp))
          (begin
            (cast0 kchar (lookup-spell an-xen-corp))
            #t)
          ;; don't need or can't repel undead
          #f)))

(define (mguard-ai kchar)
  (let ((ktarg (ai-select-target kchar)))
    (if (null? ktarg)
        (ai-wander kchar)
        (or (mguard-cast-spell kchar ktarg)
            (ai-attack-target kchar ktarg)
            (ai-pathfind-to-target kchar ktarg)))))

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

(define (hostiles-visible? kguard)
  (notnull? (all-visible-hostiles kguard)))

(define (guard-close-gate! guard kgate)
  (signal-kobj kgate 'off kgate nil)
  (gate-guard-set-gate-timer! guard 0)
  (kern-log-msg "The guard closes the gate"))

(define (guard-too-far-from-gate? kguard kgate)
  (> (distance kguard kgate) 1))

(define (guard-return-to-post kguard kgate)
  (pathfind kguard (kern-obj-get-location kgate)))

(define (gate-guard-ai kchar)
  (let* ((guard (kobj-gob-data kchar))
         (kgate (eval (gate-guard-gate-tag guard))))
    (if (guard-too-far-from-gate? kchar kgate)
        (guard-return-to-post kchar kgate)
        (if (hostiles-visible? kchar)
            (if (gate-is-open? kgate)
                (guard-close-gate! guard kgate)
                ;; FIXME: shouldn't assume gate-guard is a mage, fetch "sub"-AI
                ;; from gob
                (mguard-ai kchar))
            (if (guard-is-holding-gate-open? guard)
                (guard-dec-gate-timer! guard)
                (if (gate-is-open? kgate)
                    (guard-start-gate-timer! guard)
                    ))))))

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
