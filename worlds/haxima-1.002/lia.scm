;; Reagent-seller
;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine.
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
;; 
;; Lia is a female wizard and reagent-seller who dwells in Oparine,
;; so that she may remain in the proximity to her true love,
;; the Nixie prince Fing.
;; 
;; She is said to be under a curse, of obscure nature.
;; One likely interpretation is that she is only in human form through 
;; having been transformed, being born a Nixie or some other water-folk.
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
(define lia-merch-msgs
  (list "My shop is open from 8:00AM to 8:00PM."
        "Here are my wares."
        "I will buy rare ingredients if they are a good value."
        "Do you wish to buy or do you have something to sell?"
        "A blessing on your magic."
        "Very well."
        "I will be happy to purchase more."
        "I only offer what is fair."
        "May the gods of the deep favour you."
        "As you wish."
   ))

(define lia-catalog
  (list
   (list sulphorous_ash         (*  2 reagent-price-mult) "This ash comes from the bed of dragons in the Fire Sea.")
   (list garlic                 (*  4 reagent-price-mult) "Garlic is a common enough herb, but useful in spells.")
   (list ginseng                (*  4 reagent-price-mult) "I grow this ginseng myself alongside mandrake.")
   (list black_pearl            (*  4 reagent-price-mult) "These come from my own special source. You will find no others like them.")
   (list blood_moss             (*  6 reagent-price-mult) "It is difficult to get blood moss here.")
   (list nightshade             (* 11 reagent-price-mult) "The nightshade is rare in these parts.")
   (list mandrake               (* 11 reagent-price-mult) "I grow mandrake with my ginseng.")
   
   (list t_in_an_scroll         (*  3 base-scroll-cost) "When faced with magic stronger than your own, use this to even the score.")
   (list t_in_mani_corp_scroll  (*  8 base-scroll-cost) "You need not mourn a fallen companion when you carry one of these.")
   (list t_vas_rel_por_scroll   (*  3 base-scroll-cost) "You can travel great distances or escape dire circumstances with this gate scroll.")
   (list t_vas_mani_scroll      (*  2 base-scroll-cost) "When sorely wounded this will restore you to health.")
   (list t_wis_quas_scroll      (*  2 base-scroll-cost) "This scroll will open your eyes to the invisible world.")
   ))

(define (lia-trade knpc kpc)  (conv-trade knpc kpc "trade" lia-merch-msgs lia-catalog))
(define (lia-buy knpc kpc)  (conv-trade knpc kpc "buy" lia-merch-msgs lia-catalog))
(define (lia-sell knpc kpc)  (conv-trade knpc kpc "sell" lia-merch-msgs lia-catalog))

(define (lia-pear knpc kpc)
  (say knpc "I have my own source for the rare black pearl. "
       "Would you like to purchase some?")
  (if (kern-conv-get-yes-no? kpc)
      (lia-buy knpc kpc)
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
       (method 'bye  lia-bye)
       (method 'job  lia-job)
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
       (method 'reag lia-buy)
       (method 'buy  lia-buy)
       (method 'sell lia-sell)
       (method 'blac lia-buy)
       (method 'pear lia-buy)

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
                 0 2 1               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 3  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'lia-conv         ; conv
                 sch_lia           ; sched
                 'townsman-ai                 ; special ai
                 (mk-inventory (list (list 1 t_dagger)))    ; container
                 nil                 ; readied
                 )
   (lia-mk)))
