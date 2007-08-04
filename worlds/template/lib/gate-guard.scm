;;----------------------------------------------------------------------------
;; gate-guard -- wizard guards of the Enchanter's Tower
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (mk-gate-guard-gob gate-tag post passwd)
  (list 'gate-guard gate-tag 0 post passwd))

(define (gate-guard-gate-tag guard) (cadr guard))
(define (gate-guard-gate-timer guard) (caddr guard))
(define (gate-guard-post guard) (cadddr guard))
(define (gate-guard-passwd guard) (list-ref guard 4))
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
    (if (eq? passwd (gate-guard-passwd (gob knpc)))
        (let* ((guard (kobj-gob-data knpc))
               (gate (eval (gate-guard-gate-tag guard))))
          (signal-kobj gate 'on gate nil)
          (gate-guard-start-timer! guard)
          (say knpc "You may pass")
          (kern-conv-end))
        (begin
          (say knpc "That is not correct")
          (kern-conv-end)
          ))))

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

(define (guard-return-to-post kguard)
  (pathfind kguard 
            (cons (loc-place (kern-obj-get-location kguard))
                  (gate-guard-post (gob kguard)))))

(define (gate-guard-ai kchar)
  (or (get-off-bad-tile? kchar)
      (use-potion? kchar)
      (let* ((guard (kobj-gob-data kchar))
             (kgate (eval (gate-guard-gate-tag guard))))
        (if (any-visible-hostiles? kchar)
            (if (gate-is-open? kgate)
                (guard-close-gate! guard kgate)
                #f)
            (begin
              (guard-return-to-post kchar)
              (if (guard-is-holding-gate-open? guard)
                  (guard-dec-gate-timer! guard)
                  (if (gate-is-open? kgate)
                    (guard-start-gate-timer! guard)
                    ))
              #t)))))

;;----------------------------------------------------------------------------
;; Constructor -- make a guard captain
;;----------------------------------------------------------------------------
(define (mk-gate-guard gate-tag post passwd)
  (println "mk-gate-guard: " gate-tag ", " post)
  (bind
   (set-level
    (kern-char-arm-self
     (mk-stock-char
      "a guard captain" ;;......name
      sp_human ;;.........species
      oc_warrior ;;........occupation
      s_companion_paladin ;;........sprite
      faction-men ;;...faction
      'gate-guard-ai ;;.......custom ai (optional)    
      ;;..................container (and contents, used to arm char)
      (mk-inventory
       (mk-contents
        (roll-to-add 100  "3"     t_heal_potion)
        (roll-to-add 100  "1"     t_sword)
        (roll-to-add 199  "1"     t_shield)
        (roll-to-add 100  "1"     t_armor_plate)
        (roll-to-add 100  "1"     t_iron_helm)
        (roll-to-add 100  "1d5"   t_gold_coins)
        ))
      
      nil ;;...............readied arms (in addition to container contents)
      'gate-guard-conv ;;..conversation
      ))
    8)
   (mk-gate-guard-gob gate-tag post passwd)))

(define (put-gate-guard ktrig gate-tag passwd)
  (println "put-gate-guard: " ktrig "," gate-tag)
  (kern-obj-put-at (mk-gate-guard gate-tag
                                  (cdr (kern-obj-get-location ktrig))
                                  passwd)                   
                   (kern-obj-get-location ktrig))
  #f)

;;----------------------------------------------------------------------------
;; gate-guard generator
;;----------------------------------------------------------------------------
(define (mk-gate-guard-gen is-one? mk-one . mk-args)
  (mk-mongen2 0 1 is-one? mk-one mk-args))
