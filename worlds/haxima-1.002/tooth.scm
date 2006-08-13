;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define tooth-lvl 2)
(define tooth-species sp_rat)
(define tooth-occ oc_wrogue)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_tooth
               (list 0 0 campfire-4 "sleeping")
               (list 6 0 black-market-counter "working")
               (list 19 0 cantina-12 "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (tooth-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (tooth-hail knpc kpc)
  (kern-log-msg "You meet a shifty old rat with one enormous incisor. He shakes and shivers nervously. "
                "Too much caffeine? Black lotus withdrawal? You aren't sure.")
  (say knpc "Psst. Looking for a deal? I got deals. All kinds of deals! Good deals! Bad deals! "
       "You name it, you make it, it's your call! No questions asked. No answers given.")
  )

(define (tooth-default knpc kpc)
  (say knpc "Don't know. Don't know about that. Information? Not my game.")
  )

(define (tooth-name knpc kpc)
  (say knpc "Tooth. I'm well-known! Well-respected. My customers love me. "
       "They always come back. Tell their friends about me? No way! "
       "Deals are too good. They keep it to themselves. I'm a well-kept secret! A well-known secret!")
  )

(define (tooth-join knpc kpc)
  (say knpc "Can't, mate. Got my shop to run. Bills to pay. Guards to dodge. Too busy! Need the action!")
  )

(define (tooth-job knpc kpc)
  (say knpc "Deals! Deals! Deals! Deals! Deals! Let's make one. Right now. "
       "Right here. Let's do it. Ready? Here we go.")
  (tooth-trade knpc kpc)
  )

(define (tooth-bye knpc kpc)
  (say knpc "Going so soon? Leaving already? I've got more stuff! Lots more! "
       "Good stuff! But it won't stay! No! It flies off the shelves! Leave at your own risk...!"
       "[He goes on like this as you walk away]")
  )

(define (tooth-trade knpc kpc)
  (say knpc "Always open! I'm always ready! Look here, look at this marvelous stuff! "
       "All quality! Rare, valuable, hard-to-find! Reasonably priced! "
       "How do I stay in business? I don't know! See for yourself!")
        (kern-conv-trade knpc kpc
                         (list t_picklock          50)
                         (list t_gem               100)
                         (list t_oil               50)
                         (list t_dagger_4          (* 4 65))
                         (list t_sword_2           (* 2 85))
                         (list t_sword_4           (* 4 85))
                         (list t_morning_star_2    (* 2 105))
                         (list t_leather_helm_2    (* 2 100))
                         (list t_chain_coif_4      (* 4 100))
                         (list t_iron_helm_4       (* 4 150))
                         (list t_armor_leather_2   (* 2 150))
                         (list t_armor_leather_4   (* 4 150))
                         (list t_armor_chain_4     (* 4 330))
                         (list t_armor_plate_4     (* 4 660))
                         (list t_xen_corp_scroll   (* 7 base-scroll-cost))
                         (list t_sanct_lor_scroll  (* 7 base-scroll-cost))
                         (list t_an_xen_exe_scroll (* 6 base-scroll-cost))
                         (list t_in_ex_por_scroll  (* 4 base-scroll-cost))
                         (list t_wis_quas_scroll   (* 4 base-scroll-cost))
                         (list t_in_quas_xen_scroll (* 7 base-scroll-cost))
                         (list t_an_tym_scroll     (* 8 base-scroll-cost))
                         ))


(define tooth-conv
  (ifc nil

       ;; basics
       (method 'default tooth-default)
       (method 'hail tooth-hail)
       (method 'bye tooth-bye)
       (method 'job tooth-job)
       (method 'name tooth-name)
       (method 'join tooth-join)

       (method 'trad tooth-trade)
       (method 'buy tooth-trade)
       (method 'sell tooth-trade)
       (method 'deal tooth-trade)
       ))

(define (mk-tooth)
  (bind 
   (kern-mk-char 
    'ch_tooth           ; tag
    "Tooth"             ; name
    tooth-species         ; species
    tooth-occ              ; occ
    s_rat     ; sprite
    faction-men      ; starting alignment
    0 4 1            ; str/int/dex
    0  ; hp bonus
    0 ; hp per-level bonus
    0 ; mp off
    1 ; mp gain
    max-health ; hp
    -1                  ; xp
    max-health ; mp
    tooth-lvl
    #f               ; dead
    'tooth-conv         ; conv
    sch_tooth           ; sched
    'townsman-ai              ; special ai
    nil
    nil              ; readied
    )
   (tooth-mk)))
