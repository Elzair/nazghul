;;----------------------------------------------------------------------------
;; Special -- one-off stuff that needs to be kern-loaded and doesn't really fit
;; anywhere else.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Angriss Lair generator -- procedure invoked by a step trigger to create
;; Angriss's Lair. Should return true iff it triggers to remove the step
;; generator that invokes it.
;;----------------------------------------------------------------------------
(define angriss-lair-loc (list 'p_shard 66 41))
(define (mk-angriss-lair kbeing)
  (if (eqv? kbeing 
            (kern-get-player))
      (begin
        (kern-log-msg "You find the entrance to Angriss's Lair!")
        (kern-place-set-subplace p_angriss_lair 
                                 (eval-loc angriss-lair-loc))
        (kern-map-set-dirty)
        #t)
      #f))

;; ----------------------------------------------------------------------------
;; The Warritrix's note
;; ----------------------------------------------------------------------------
(mk-reusable-item 
 't_warritrix_orders "Military Orders" s_lexicon 1
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "Orders to the Warritrix"
   "Ever faithful servant of Glasdrin,"
   "we suspect a coven of the Accursed are hiding"
   "in the deeps of the Lost Halls. Proceed at"
   "once to investigate. Leave no cavern"
   "unexplored.\n"
   "--Commander Jeffries\n"
   "P.S. These orders are to be destroyed."
   )))


;; Kraken lakes kraken trigger
(define (spawn-kraken-lakes-sea-serpent kbeing)
  (kern-log-msg "Your disturb something in the water...")
  (kern-obj-put-at (spawn-npc 'kraken 8) (mk-loc p_deepness 31 34))
  (kern-obj-put-at (spawn-npc 'kraken 8) (mk-loc p_deepness 32 35))
  (kern-obj-put-at (spawn-npc 'kraken 8) (mk-loc p_deepness 30 29))
  #t)

;; Locations referred to more than once
(define the-mans-hideout-loc (list 'p_shard 92 10))
(define lost-halls-loc (list 'p_shard 39 75))
