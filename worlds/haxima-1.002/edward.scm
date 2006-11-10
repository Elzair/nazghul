;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define edward-start-lvl 4)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Enchanter's Tower Ground Floor"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_edward
               (list 0  0  gtl-jailor-bed "sleeping")
               (list 7  0  gtl-jail       "working" )
               (list 21 0  gtl-jailor-bed "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (edward-mk) (list #f #f))
(define (edward-met? gob) (car gob))
(define (edward-meet! gob) (set-car! gob #t))
(define (edward-has-nate? gob) (cadr gob))
(define (edward-has-nate! gob) (set-car! (cdr gob) #t))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Edward is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define (edward-hail knpc kpc)
  (meet "You meet a pale old man whose eyes are milky white.")
  (let ((edward (kobj-gob-data knpc)))
    (cond ((not (edward-met? edward))
           (say knpc "Who goes there?")
           (reply? kpc)
           (edward-meet! edward)
           (say knpc "I don't know you. Are you hear to make a jail-break?")
           (if (yes? kpc)
               (say knpc "Well get on with it! I'm too old to stop you.")
               (say knpc "That's a relief. I'd have no-one to talk to.")
               )
           )
          (else
           (say knpc "Welcome back. I remember your footfalls.")
           ))))

(define (edward-pris knpc kpc)
  (let ((edward (kobj-gob-data knpc)))
    (if (edward-has-nate? edward)
        (say knpc "I've got a forest goblin and that fellow you brought in.")
        (say knpc "I've just got a forest goblin right now. "
             "He and I can't understand each other, "
             "but that doesn't stop us from swapping stories."))))

(define (edward-gobl knpc kpc)
  (say knpc "I'm not afraid of them. They never done me no harm."))

(define (edward-stor knpc kpc)
  (say knpc "Have you heard the one about Will-o-the-Woods?")
  (cond ((no? kpc)
         (say knpc "Will-o-the-Woods was a spirit that used to haunt these woods, "
              "in the old days when the gates worked and the gods were honored. ")
         (prompt-for-key kpc)
         (say knpc "If a pretty maiden ventured into the woods alone, "
              "he would appear as a handsome woodsman or ranger, "
              "and entice her back to his sacred grove. "
              "There, he would turn her into a tree. ")
         (prompt-for-key kpc)
         (say knpc "That's how the Great Wood got its beginning, in his grove. "
              "And that's why you find so many haunted trees in the woods even to this day.")
         )
        (else
         (say knpc "No one knows what become of Will-o-the-Woods. "
              "Passed into legend like the other gods, I suppose.")
         )))

(define (edward-gate knpc kpc)
  (say knpc "I don't know anything about them. They used to work, that's all I know."))

(define (edward-gods knpc kpc)
  (say knpc "It's not a good idea to talk about the old gods. "
       "Folks might think us Accursed if they were to overhear."))

(define (edward-accu knpc kpc)
  (say knpc "The Accursed seek power from the old gods... and other things. "
       "I've had a few in my jail."))

(define (edward-talk knpc kpc)
  (say knpc "The prisoners are the only ones I can talk to down here."))

(define (edward-blin knpc kpc)
  (say knpc "The dark doesn't bother me. Neither does the light, for that matter. "
       "It's all the same to me."))

(define (edward-guar knpc kpc)
  (say knpc "Truth be told, I'm not much of a guard."))

(define edward-conv
  (ifc ranger-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (say knpc "[He shrugs]")))
       (method 'hail edward-hail)
       (method 'bye 
               (lambda (knpc kpc) 
                 (say knpc "Watch your step in the dark!")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "I'm the jailor.")))
       (method 'name 
               (lambda (knpc kpc) 
                 (say knpc "I am Edward. And you already told me your name.")))
       (method 'join 
               (lambda (knpc kpc) 
                 (say knpc "Love to. [But he doesn't]")))
       (method 'jail
               (lambda (knpc kpc)
                 (cond ((in-player-party? 'ch_nate)
                        (say knpc "Are you delivering a prisoner?")
                        (cond ((yes? kpc)
                               (say knpc "Oh, good. More company. [He takes Nate into custody and "
                                    "locks him in a cell]")
                               (kern-char-leave-player ch_nate)
                               (kern-obj-relocate ch_nate 
                                                  (mk-loc p_green_tower_lower 
                                                          9 10)
                                                  nil)
                               (prompt-for-key)
                               (say knpc "Here's your receipt, in case the Captain wants it.")
                               (give kpc t_prisoner_receipt 1)
                               (edward-has-nate! (kobj-gob-data knpc))
                               )
                              (else
                               (say knpc "Hokay. [He gives Nate a look, "
                                    "then shrugs]")
                               )))
                       (else
                        (say knpc "I feed the prisoners and swap stories with them. "
                             "I'm supposed to guard them, to. Prevent escapes.")
                        ))))
       (method 'accu edward-accu)
       (method 'blin edward-blin)
       (method 'esca edward-guar)
       (method 'gate edward-gate)
       (method 'gobl edward-gobl)
       (method 'god  edward-gods)
       (method 'gods edward-gods)
       (method 'guar edward-guar)
       (method 'old  edward-gods)
       (method 'pris edward-pris)
       (method 'stor edward-stor)
       (method 'talk edward-talk)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-edward)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_edward ;;..........tag
     "Edward" ;;.......name
     sp_human ;;.....species
     oc_ranger ;;.. .occupation
     s_old_townsman ;;..sprite
     faction-men ;;..faction
     0 ;;...........custom strength modifier
     0 ;;...........custom intelligence modifier
     0 ;;...........custom dexterity modifier
     0 ;;............custom base hp modifier
     0 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     edward-start-lvl  ;;..current level
     #f ;;...........dead?
     'edward-conv ;;...conversation (optional)
     sch_edward ;;.....schedule (optional)
     'townsman-ai ;;..........custom ai (optional)

     ;;..............container (and contents)
     (mk-inventory
      (mk-contents 
       (add-content 1   t_torch)
       ))

     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (edward-mk)))