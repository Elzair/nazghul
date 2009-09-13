;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define tim-start-lvl 4)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the Tower of Brundegart (p_brundegardt_tower_4), locked outside.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (tim-mk) (list #f #f))
(define (tim-caught? gob) (car gob))
(define (tim-caught! gob) (set-car! gob #t))
(define (tim-met? gob) (cadr gob))
(define (tim-met! gob) (set-car! (cdr gob) #t))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Tim is a maimed, drooling madman, currently trapped outside 
;; the Tower of Brundegart.
;; 
;; Once a seeker of knowledge (and power), 
;; his body was ravaged by griffins (and their hungry chicks), 
;; and his mind broken by contact with the EYE of Brundegart.
;; (One rather assumes the maiming did not help his mental state, 
;; for that matter...)
;;----------------------------------------------------------------------------
(define (tim-hail knpc kpc)
  (meet "You meet a drooling madman with only one arm.")
  (say knpc "I have seen the eye!"))

(define (tim-eye knpc kpc)
  (say knpc "So wise I am now, because of the eye. Would you be wise?")
  (cond ((yes? knpc)
         (say knpc "Alas, my friend, I have lost the key!"))
        (else
         (say knpc "Fool!")
         (kern-conv-end))))

(define (tim-key knpc kpc)
  (say knpc "It was my key! I found it on the dead man. "
       "First they took my arm, then the lion-birds took my key!"))

(define (tim-arm knpc kpc)
  (say knpc "They chose me as I walked among the hills, "
       "and brought me here to feed their young."))

(define (tim-name knpc kpc)
  (say knpc "Do not pretend to not know me! "
       "One who is all-wise is necessarily famous! That's logic!"))

(define (tim-job knpc kpc)
  (say knpc "I will bring enlightenment to the world!"))

(define (tim-enli knpc kpc)
  (say knpc "Yes! The eye! The eye... [He curls into a fetal ball and sobs]")
  (kern-conv-end))

(define (tim-lion knpc kpc)
  (say knpc "[He shrieks and cowers] Do you see them?! "
       "Have they come for my other arm?  The chicks are so hungry! So cruel!"))

(define tim-conv
  (ifc nil
       (method 'hail tim-hail)
       (method 'eye  tim-eye)
       (method 'key  tim-key)
       (method 'arm  tim-arm)
       (method 'name tim-name)
       (method 'job  tim-job)
       (method 'enli tim-enli)
       (method 'lion tim-lion)
       ))


;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-tim)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_tim ;;..........tag
     "Tim" ;;.......name
     sp_human ;;.....species
     oc_wizard ;;.. .occupation
     s_wizard ;;..sprite
     faction-men ;;..faction
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
     0
     tim-start-lvl  ;;..current level
     #f ;;...........dead?
     'tim-conv ;;...conversation (optional)
     nil ;;sch_tim ;;.....schedule (optional)
     nil ;;..........custom ai (optional)
     nil ;;..............container (and contents)
     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (tim-mk)))
