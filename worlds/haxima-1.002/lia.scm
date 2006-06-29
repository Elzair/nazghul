;; Reagent-seller
;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define trigrave-inn-room-price 20)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_lia
               (list 0  0  sea-witch-bed        "sleeping")
               (list 6  0  sea-witch-beach      "idle")
               (list 8  0  sea-witch-counter    "working")
               (list 20 0  sea-witch-beach      "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (lia-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (lia-hail knpc kpc)
  (say knpc "[You meet a haunting beauty] Welcome."))

(define (lia-default knpc kpc)
  (say knpc "I cannot help you with that."))

(define (lia-name knpc kpc)
  (say knpc "I am Lia."))

(define (lia-join knpc kpc)
  (say knpc "I cannot leave these shores."))

(define (lia-job knpc kpc)
  (say knpc "I sell reagents, including the rare black pearl."))

(define (lia-bye knpc kpc)
  (say knpc "Goodbye, Wanderer."))

;; Trade...
(define (lia-trade knpc kpc)  
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "My shop is open from 8:00AM to 8:00PM.")
      (kern-conv-trade knpc kpc
                       (list sulphorous_ash         (* 2  reagent-price-mult))
                       (list garlic                 (* 6  reagent-price-mult))
                       (list ginseng                (* 7  reagent-price-mult))
                       (list black_pearl            (* 6  reagent-price-mult))
                       (list blood_moss             (* 11 reagent-price-mult))
                       (list nightshade             (* 20 reagent-price-mult))
                       (list mandrake               (* 24 reagent-price-mult))
                       (list t_in_an_scroll         (* 6 base-scroll-cost))
                       (list t_in_mani_corp_scroll  (* 8 base-scroll-cost))
                       (list t_vas_rel_por_scroll   (* 8 base-scroll-cost))
                       (list t_vas_mani_scroll      (* 5 base-scroll-cost))
                       (list t_wis_quas_scroll      (* 4 base-scroll-cost))
                       )))

(define (lia-pear knpc kpc)
  (say knpc "I have my own source for the rare black pearl. "
       "Would you like to purchase some?")
  (if (kern-conv-get-yes-no? kpc)
      (lia-trade knpc kpc)
      (say knpc "You won't get this quality anywhere else!")))

;; Shores...
(define (lia-shor knpc kpc)
  (say knpc "I must stay by the shore so I can be near my love."))

(define (lia-love knpc kpc)
  (say knpc "My beloved cannot leave the sea, "
       "he is a prince among the Sea People. "
       "He is bold and true, and has not left me despite my curse."
       ))

;; Sea
(define (lia-sea knpc kpc)
  (say knpc "How I long to return to my home! "
       "I miss the courtyards of the deep, "
       "and watching the ships cross my ceiling, "
       "and hearing the conversations of Men as only a distant murmur."))

(define (lia-curs knpc kpc)
  (say knpc "I'd prefer not to speak of it."))

;; Townspeople...
(define (lia-opar knpc kpc)
  (say knpc "All cities of Men are too dry for my liking."))

(define (lia-gher knpc kpc)
  (say knpc "The scavengers of the sea used to follow her ship. "
       "They were well-fed with her victims."))

(define (lia-alch knpc kpc)
  (say knpc "He sometimes visits me, and we discuss magic. "
       "I have few other friends among Men."))

(define (lia-osca knpc kpc)
  (say knpc "I know little of him, except that he has never been to sea."))

(define (lia-henr knpc kpc)
  (say knpc "He was a fine sailor once. The great ones of the deep sometimes "
       "spoke of his courage."))

(define (lia-bart knpc kpc)
  (say knpc "His ships are well-respected, but all ships are clumsy and "
       "ponderous in my sight."))

(define lia-conv
  (ifc basic-conv

       ;; basics
       (method 'default lia-default)
       (method 'hail lia-hail)
       (method 'bye lia-bye)
       (method 'job lia-job)
       (method 'name lia-name)
       (method 'join lia-join)
       
       ;; Shores
       (method 'shor lia-shor)
       (method 'love lia-love)
       (method 'sea  lia-sea)
       (method 'deep lia-sea)
       (method 'curs lia-curs)

       ;; trade
       (method 'trad lia-trade)
       (method 'reag lia-trade)
       (method 'buy lia-trade)
       (method 'sell lia-trade)
       (method 'blac lia-pear)
       (method 'pear lia-pear)

       ;; town & people
       (method 'opar lia-opar)
       (method 'alch lia-alch)
       (method 'gher lia-gher)
       (method 'osca lia-osca)
       (method 'henr lia-henr)
       (method 'bart lia-bart)
       (method 'fing lia-love)

       ))

(define (mk-lia)
  (bind 
   (kern-mk-char 'ch_lia           ; tag
                 "Lia"             ; name
                 sp_human            ; species
                 oc_wizard           ; occ
                 s_townswoman        ; sprite
                 faction-men         ; starting alignment
                 0 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 30 0 0 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'lia-conv         ; conv
                 sch_lia           ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (lia-mk)))
