;;----------------------------------------------------------------------------
;; Camping map
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_campsite 7 7 pal_expanded
 (list
  ".. .. bb bb bb .. .."
  ".. .. .. .. .. .. .."
  "bb .. .. .. .. .. bb"
  "bb .. .. && .. .. bb"
  "bb .. .. .. .. .. bb"
  ".. .. .. .. .. .. .."
  ".. .. bb bb bb .. .."  
  ))

;;----------------------------------------------------------------------------
;; Camping proc - run every turn when player is camping in the wilderness
;;  kplayer = player party (kernel object pointer)
;;   kplace = camping place
;;----------------------------------------------------------------------------
(define (camping-proc kplayer kplace)
  (if (> (kern-dice-roll "1d20") 16)
      (let ((loc (kern-place-get-location kplace)))
        (define (nearest a b)
          (if (null? a)
              b
              (if (<= (kern-get-distance (kern-obj-get-location a) loc) 
                      (kern-get-distance (kern-obj-get-location b) loc)) 
                  a 
                  b)))
        (define (willattack? a)
          (and (is-hostile? kplayer a)
               (can-pathfind? a loc)))
        (define (choose-npc-party)
          (foldr nearest
                 nil
                 (filter willattack?
                         (kern-place-get-beings (loc-place loc)))))
        (let ((kparty (choose-npc-party))) 
          (if (not (null? kparty))
              (kern-ambush-while-camping kparty kplace))))))

(kern-set-camping-proc camping-proc)
