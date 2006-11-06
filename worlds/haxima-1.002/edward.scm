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
(define (edward-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Edward is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define edward-conv
  (ifc ranger-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (say knpc "[He shrugs]")))
       (method 'hail 
               (lambda (knpc kpc) 
                 (meet "You meet a skeletally thin, pale man.")))
       (method 'bye 
               (lambda (knpc kpc) 
                 (say knpc "[He nods as you leave].")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "[His voice rasps] Jailer.")))
       (method 'name 
               (lambda (knpc kpc) 
                 (say knpc "Edward.")))
       (method 'join 
               (lambda (knpc kpc) 
                 (say knpc "Love to. [He doesn't move]")))
       (method 'jail
               (lambda (knpc kpc)
                 (cond ((in-player-party? 'ch_nate)
                        (say knpc "You delivering a prisoner?")
                        (cond ((yes? kpc)
                               (say knpc "[He takes Nate into custody and "
                                    "locks him in a cell]")
                               (kern-char-leave-player ch_nate)
                               (kern-obj-relocate ch_nate 
                                                  (mk-loc p_green_tower_lower 
                                                          9 10)
                                                  nil)
                               (prompt-for-key)
                               (say knpc "Your receipt.")
                               (give kpc t_prisoner_receipt 1))
                              (else
                               (say knpc "[He gives Nate a look, "
                                    "then shrugs]")
                               )))
                       (else
                        (say knpc "I take care of the prisoners.")
                        ))))
       (method 'pris
               (lambda (knc kpc)
                 (say knpc "Go ahead and look around.")))
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
     s_companion_ranger ;;..sprite
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
       (add-content 1   t_dagger)
       (add-content 1   t_leather_helm)
       (add-content 1   t_armor_leather)
       (add-content 1   t_torch)
       ))

     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (edward-mk)))
