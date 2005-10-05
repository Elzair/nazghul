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
