;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define thud-start-lvl  6)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Bole
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_thud
               (list 0  0  bole-bedroom-thud "idle")
               (list 9  0  bole-dining-hall  "idle")
               (list 10 0  bole-courtyard   "idle")
               (list 12 0  bole-dining-hall   "idle")
               (list 23 0  bole-bedroom-thud "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (thud-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Thud is the bodyguard of Kathryn, and currently abides in Bole,
;; where they seek a certain thief.  Various other NPCs suggest
;; that Thud is at best half human, perhaps having ogre blood,
;; or a sorcerous (summoned or vat-born) origin...
;; 
;; Thud is a potential party member (and an eventual betrayer).
;; He accompanies Kathrun, and joins the party when and if she does.
;;----------------------------------------------------------------------------
(define (thud-hail knpc kpc)
  (say knpc "[You are certain that the figure before you is part ogre. "
       "Three meters tall and smoldering with menace, he regards you with "
       "half-lidded eyes.]"))

(define (thud-default knpc kpc)
  (say knpc "[His threatening gaze does not waver]"))

(define (thud-name knpc kpc)
  (say knpc "Thud no like you."))

(define (thud-join knpc kpc)
  (if (is-player-party-member? ch_kathryn)
      (begin
        (say knpc "[Seeing Kathryn with you, he grunts his assent]")
        (kern-char-join-player knpc)
        (kern-conv-end))
      (say knpc "[He sneers]")))

(define (thud-job knpc kpc)
  (say knpc "Thud love kill."))

(define (thud-kathryn knpc kpc)
  (say knpc "Thud no kill"))

(define (thud-thud knpc kpc)
  (say knpc "You talk me? YOU TALK ME?! THUD PICK TEETH WITH YOU BONES!!"))

(define (thud-thief knpc kpc)
  (say knpc "[He becomes enraged] THIEF TRICK THUD! THUD FIND THIEF! THUD KILL THIEF!"))

(define (thud-find knpc kpc)
  (say knpc "[He calms down a bit] Red Lady find thief. He no can hide."))

(define (thud-red-lady knpc kpc)
  (say knpc "[He gives you a murderous look] You stay away Red Lady."))

(define thud-conv
  (ifc nil
       (method 'default thud-default)
       (method 'hail thud-hail)
       (method 'bye 
               (lambda (knpc kpc) 
                 (say knpc "[His eyes bore into your back as you walk away]")))
       (method 'job  thud-job)
       (method 'name thud-name)
       (method 'join thud-join)

       (method 'find thud-find)
       (method 'kath thud-kathryn)
       (method 'kill thud-job)
       (method 'lady thud-red-lady)
       (method 'love thud-job)
       (method 'red  thud-red-lady)
       (method 'thie thud-thief)
       (method 'thud thud-thud)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-thud)
  (bind 
    (kern-char-arm-self
     (kern-mk-char 
      'ch_thud ;;.....tag
      "Thud" ;;.......name
      sp_troll ;;.....species
      oc_warrior ;;...occupation
      s_troll ;;......sprite
      faction-men ;;..faction
      4 ;;............custom strength modifier
      0 ;;............custom intelligence modifier
      2 ;;............custom dexterity modifier
      2 ;;............custom base hp modifier
      1 ;;............custom hp multiplier (per-level)
      0 ;;............custom base mp modifier
      0 ;;............custom mp multiplier (per-level)
      max-health;;..current hit points
      -1  ;;...........current experience points
      max-health ;;..current magic points
      0
      thud-start-lvl  ;;..current level
      #f ;;...........dead?
      'thud-conv ;;...conversation (optional)
      sch_thud ;;.....schedule (optional)
      'townsman-ai ;;..........custom ai (optional)
      nil ;;..........container (and contents)
      ;;.........readied arms (in addition to the container contents)
      (list
       t_2h_axe
       t_iron_helm
       t_armor_plate
       )
      nil ;;..........hooks in effect
      ))
   (thud-mk)))
