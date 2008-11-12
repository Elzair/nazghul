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
  (if (> (kern-dice-roll "1d20") 1)
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
              (begin
                (if (loc-8-adjacent? (kern-obj-get-location kplayer)
                                     (kern-obj-get-location kparty))
                    (kern-ambush-while-camping kparty kplace)
                    ;; Have the party actually move, so if they are in a
                    ;; vehicle, then when the ambush is over the vehicle will
                    ;; be in the proper location on the wilderness map.
                    (pathfind kparty (kern-obj-get-location kplayer)))))))))

(kern-add-hook 'camping_turn_start_hook 'camping-proc)
