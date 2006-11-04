;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define nate-start-lvl 3)

;;----------------------------------------------------------------------------
;; Schedule
;;
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (nate-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Nate is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define (nate-hail knpc kpc)
  (join-player knpc)
  (kern-conv-end))

(define nate-conv
  (ifc basic-conv
       (method 'hail nate-hail)
       ))

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
     'std-ai ;;..........custom ai (optional)

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
