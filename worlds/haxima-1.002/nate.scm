;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define nate-start-lvl 4)

;;----------------------------------------------------------------------------
;; Schedule
;;
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (nate-mk) (list #f #f))
(define (nate-caught? gob) (car gob))
(define (nate-caught! gob) (set-car! gob #t))
(define (nate-met? gob) (cadr gob))
(define (nate-met! gob) (set-car! (cdr gob) #t))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Nate is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define (nate-hail knpc kpc)
  (let ((nate (kobj-gob-data knpc)))
    (define (join)
      (say knpc "When it is convenient, ask me of the secret, milord. "
           "Meanwhile, you can trust me not to escape.")
      (join-player knpc)
      (give kpc t_arrow 20)
      (nate-caught! nate)
      )
    (nate-met! nate)
    (cond ((nate-caught? nate)
           (say knpc "How can I be of service?")
           )
          (else
           (say knpc "There's no need to kill me, adventurer. "
                "I may have robbed, but I have slain no one. "
                "Let me surrender, and I'll tell you a great secret. "
                "Agreed?")
           (cond ((yes? kpc) (join))
                 (else
                  (say knpc "Milord! Slay me and the secret will be lost with me. "
                       "Spare me and I will join you, fight for you, and lead you to a source of ancient power. "
                       "I will not try to escape. "
                       "Afterwards, you can turn me in or set me free, as you like. "
                       "Agreed?")
                  (cond ((yes? kpc) (join))
                        (else
                         (say knpc "It is a bloodthirsty fiend who slays those who have surrendered!")
                         (kern-conv-end)
                         ))))))))

(define (nate-secr knpc kpc)
  (cond ((is-player-party-member? knpc)
         (cond ((equal? (get-place knpc) p_shard)
                (say knpc "I will tell you where to find the hidden entrance to Brundegardt!"))
               (else
                (say knpc "Merciful sir! It is not safe here! Let us escape to the wilderness and I will tell "
                     "you there.")
                )))
        (else
         (say knpc "The secret is safe with me.")
         )))

(define (nate-brun knpc kpc)
  (if (is-player-party-member? knpc)
      (say knpc "Milord, we must search for Brundegardt where the forest road passes north through the mountains. "
           "I'll tell you the password when we are there.")
      (say knpc "Mums the word!")))

(define (nate-pass knpc kpc)
  (if (is-player-party-member? knpc)
      (if (equal? (get-place knpc) p_brundegardt)
          (say knpc "Yes, here we are. You've done well, milord. The password... "
               "[he clenches his teeth] "
               "...NOOR. [He sighs] It is NOOR.")
          (say knpc "But sir! We have not reached Brundegardt!"))
      (say knpc "Password? What password?")))

(define nate-conv
  (ifc basic-conv
       (method 'brun nate-brun)
       (method 'hail nate-hail)
       (method 'pass nate-pass)
       (method 'secr nate-secr)
       ))

(define nate-greetings
  (list
   "I surrender!"
   "Don't kill me!"
   "You've caught me!"
   "Please, take me prisoner!"
   ))

(define (nate-ai knpc)
  (let ((nate (kobj-gob-data knpc)))
    (cond ((nate-met? nate) (std-ai knpc))
          ((any-player-party-member-visible? knpc)
           (taunt knpc nil nate-greetings)
           #t)
          (else
           (std-ai knpc)
           )
          )))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-nate)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_nate ;;..........tag
     "Nate" ;;.......name
     sp_human ;;.....species
     oc_wrogue ;;.. .occupation
     s_brigand ;;..sprite
     faction-outlaw ;;..faction
     +1 ;;...........custom strength modifier
     0 ;;...........custom intelligence modifier
     +1 ;;...........custom dexterity modifier
     +1 ;;............custom base hp modifier
     +1 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     nate-start-lvl  ;;..current level
     #f ;;...........dead?
     'nate-conv ;;...conversation (optional)
     nil ;;sch_nate ;;.....schedule (optional)
     'nate-ai ;;..........custom ai (optional)

     ;;..............container (and contents)
     (mk-inventory
      (mk-contents 
       (add-content 20 t_arrow)
       (add-content 1   t_bow)
       (add-content 1   t_dagger)
       (add-content 1   t_sword)
       (add-content 1   t_leather_helm)
       (add-content 1   t_armor_leather)
       (add-content 5   t_heal_potion)
       ))

     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (nate-mk)))
