;; ----------------------------------------------------------------------------
;; Disarm Trap skill
;; ----------------------------------------------------------------------------

;; Runs the 'disarm-trap' task. kchar is the person doing the task, ktarg is
;; the object the traps are attached to. Only one trap is worked on per
;; iteration. When there are no traps left the task is done. Each time in, we
;; roll to see if the char is done with the next trap. If so, we roll to see if
;; they succeed in disarming. If not, we trip the trap. Note that the
;; trap-trigger procedure gives the char another chance to roll to avoid. The
;; chief benefit of using the disarm skill is it gives an extra roll to avoid
;; tripping the trap. The only other advantage is when the on-damage hooks are
;; in place for tasks they will give the player a chance to abort before
;; tripping any more traps. By comparison, when you search or open a door (for
;; example) it will trip all the traps in sequence, which can do a lot of
;; damage if there are multiple traps.
(define (disarm-task kchar ktarg)
  (let* (
         (traps (filter (lambda (trap) 
                          (and (trap-detected? trap) 
                               (not (trap-tripped? trap))))
                        (ifccall ktarg 'get-traps)))
        )
    ;; Check if any unprocessed traps remaining
    (println "traps: " traps)
    (cond ((null? traps) 
           (kern-char-task-end kchar)
           )
          ((not (handles? ktarg 'rm-traps)) 
           (kern-log-msg "Traps can't be removed!")
           (kern-char-task-abort kchar)
           )
          (else
           ;; Roll to complete the task (on the current trap)
           (let* (
                  (trap (car traps))
                  (dc (trap-avoid-dc trap))
                  (roll (kern-dice-roll "1d20"))
                  (bonus (occ-thief-dice-roll kchar))
                  )
             (println "trap: " trap " dc: " dc " roll: " roll " bonus: " bonus)
             (if (or 
                  (= roll 20) 
                  (> (+ roll bonus) (* 2 dc))
                  )
                 ;; Roll to succeed
                 (let (
                       (roll (kern-dice-roll "1d20"))
                       (bonus (occ-thief-dice-roll kchar))
                       )
                   (println "roll2: " roll " bonus2: " bonus)
                   (cond ((or 
                           (= roll 20) 
                           (> (+ roll bonus) dc)
                           )
                          ;; Success - disarm the trap
                          (kern-log-msg (kern-obj-get-name kchar) " ^c+gdisarms^c- a " (trap-name trap) " trap!")
                          (trap-set-tripped! trap #t)
                          )
                      (else
                       ;; Failure - trip the trap (kchar will get another roll
                       ;; to avoid the damage)
                       (trap-trigger trap ktarg kchar)
                       ))
                   ;; Last trap just proceessed
                   (if (= 1 (length traps))
                       (kern-char-task-end kchar))
                   )))))))
                

(define (skill-disarm-trap kactor)

  ;; Called after the player has selected a target to disarm.
  (define (cb kactor ktarg power)
    (let (
          (traps (filter (lambda (trap) 
                           (and (trap-detected? trap) 
                                (not (trap-tripped? trap))))
                         (ifccall ktarg 'get-traps)))
          )
      ;; Check if there are any unprocessed traps
      (println "traps: " traps)
      (cond ((null? traps) 
             result-no-effect
             )
            (else
             ;; Start a task to disarm the traps
             (kern-char-task-begin kactor "disarming a trap" 'disarm-task ktarg)
             result-ok
             ))))

  ;; fixme: only checks topmost item.
  (cast-ui-ranged-any cb
                      kactor 
                      1
                      (occ-ability-thief kactor)
                      (lambda (kobj)
                        (and (kern-obj-is-visible? kobj)
                             (handles? kobj 
                                       'rm-traps)))))
