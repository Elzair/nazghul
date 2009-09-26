;;----------------------------------------------------------------------------
;; This is the special statue that stands in the courtyard of Glasdrin. When
;; attacked, it speaks. This requires two objects: an object with a type in
;; order to support the 'attack interface, and a character type to have the
;; conversation. The illusion is that they are both the same object.
;;----------------------------------------------------------------------------

(define (soj-strike kobj kpc)
  (let ((kchar (mk-talking-statue)))
    (kern-obj-put-at kchar (kern-obj-get-location kobj))
    (kern-conv-begin kchar)
    (kern-obj-remove kchar)
    ))


(mk-obj-type 't_statue_of_justice "ancient statue" s_headless_w_sword_statue layer-mechanism
             (ifc nil
                  (method 'xamine (lambda (knpc kpc)
                                    (kern-log-msg "A worn engraving reads 'STRIKE FOR JUSTICE'")))
                  (method 'attack soj-strike)
                  ))

(define (soj-accuse knpc kpc)
  (say knpc "Whom do you accuse?")
  (let ((kchar (ui-target (kern-obj-get-location kpc) 19 obj-is-char?)))
    (cond ((null? kchar)
           (say knpc "Do you wish to withdraw your charge?")
           (cond ((yes? kpc)
                  (say knpc "Cursed are you: for trifling with serious matters, "
                       "a trifling nuisance will haunt your sleep.")
                  (unrest-curse-apply-new kpc 'insect-party-l1)
                  (kern-conv-end))
                 (else
                  (soj-accuse knpc kpc))
                 ))
          (else
           (say knpc "Are you certain that you wish to accuse " (kern-obj-get-name kchar) "?")
           (cond ((no? kpc)
                  (soj-accuse knpc kpc))
                 (else
                  (say knpc (kern-obj-get-name kchar) ", you stand accused by " (kern-obj-get-name kpc) ". ")
                  (soj-get-evidence knpc kpc kchar)
                  ))
           ))
    ))

(define (soj-get-evidence knpc kpc kchar)
  (say knpc (kern-obj-get-name kpc) ", produce your evidence.")
  (let ((ktype (kern-ui-select-item kpc)))
    (cond ((null? ktype)
           (say knpc "Do you have any other evidence?")
           (cond ((no? kpc)
                  (shake-map 15)
                  (say knpc "For accusing another with insufficient evidence, "
                       "you are guilty of bearing false witness. "
                       "Your punishment is exile.")
                  ;; todo: implement exile
                  (make-enemies knpc kpc)
                  (kern-obj-relocate kpc
                                     (kern-place-get-location (loc-place (kern-obj-get-location knpc)))
                                     nil)
                  (kern-conv-end)
                  )
                 (else (soj-get-evidence knpc kpc kchar))
                 ))
          ((or (not (equal? ktype t_stewardess_journal))
              (not (defined? 'ch_steward))
              (not (equal? kchar ch_steward)))
           (say knpc "Justice will weigh the evidence.")
           (log-dots 10 1000)
           (say knpc "The evidence is insufficient.")
           (prompt-for-key)
           (soj-get-evidence knpc kpc kchar)
           )
          (else
           (say knpc "Justice will weigh the evidence.")
           (log-dots 10 1000)
           (say knpc (kern-obj-get-name kchar) ", you are guilty of betrayal. Your punishment is death, "
                "and may your name be a curse forevermore.")
           (aside kpc 'ch_ini "Justice at last!")
           (kern-being-set-current-faction kchar faction-monster)
           (quest-data-update-with 'questentry-warritrix 'avenged 1 (quest-notify (grant-party-xp-fn 200)))

           (if (defined? 'ch_jeffreys)
           		(begin
               		(kern-char-set-sched ch_jeffreys sch_jeff_resigned)
               		(kern-obj-set-sprite ch_jeffreys s_fallen_paladin)
               	)
               )

           (if (defined? 'ch_valus)
           		(begin
               		(kern-char-set-sched ch_valus sch_jeff)
               		(kern-obj-set-sprite ch_valus s_companion_paladin)
               	)
               )

           (kern-conv-end)
           ))
    ))
           
                  

(define (soj-hail knpc kpc)
  (say knpc "Do you seek justice?")
  (cond ((no? kpc)
         (say knpc "Then cursed are you: an unjust man would live among beasts, "
              "so beasts will give you no rest.")
         (unrest-curse-apply-new kpc 'wolf-party-l2)
         (kern-conv-end))
        (else
         (say knpc "Speak truly, or be cursed. Do you accuse another of theft, false witness, oath-breaking, or betrayal?")
         (cond ((no? kpc) 
                (say knpc "Then go in peace, and do not strike me again in vain.")
                (kern-conv-end))
               (else
                (say knpc "Be warned! If you accuse with insufficient evidence, you shall be guilty of bearing false witness, and shall be punished. "
                     "Are you certain you want to accuse another at this time?")
                (cond ((no? kpc)
                       (say knpc "If you accuse truly, then go in peace and gather more evidence. But if you accuse falsely, do not strike me again.")
                       (kern-conv-end))
                      (else
                       (say knpc "JUSTICE SUMMONS THE ASSEMBLY!")
                       (shake-map 5)
                       (soj-assemble-everyone (kern-obj-get-location knpc))
                       (soj-accuse knpc kpc))
                      ))
               ))
        ))

(define (soj-assemble-everyone loc)
  (define (assemble townsfolk)
    (kern-map-repaint)
    (if (not (null? townsfolk))
        (assemble (filter notnull? 
                          (map (lambda (kchar)
                                 (cond ((in-range? loc 2 kchar) nil)
                                       (else
                                        (pathfind kchar loc)
                                        kchar)))
                               townsfolk)))))
  (assemble (filter (lambda (kchar)
                      (let ((gob (gob kchar)))
                        (and (not (null? gob))
                             (pair? gob)
                             (eq? 'townsman (car gob)))))
                    (kern-place-get-beings (loc-place loc))))
  )

(define soj-conv
  (ifc basic-conv
       (method 'hail soj-hail)
       ))

(define (soj-ai knpc)
  #t)

(define (mk-talking-statue)
  (let ((kchar 
         (kern-mk-char 
          'ch_soj           ; tag
          "Statue of Justice"     ; name
          sp_statue         ; species
          nil              ; occ
          s_headless_w_sword_statue     ; sprite
          faction-glasdrin      ; starting alignment
          0 0 0            ; str/int/dex
          0 0              ; hp mod/mult
          0 0              ; mp mod/mult
          1000 ; hp
          0                   ; xp
          0 ; mp
          0
          9                ; lvl
          #f               ; dead
          'soj-conv         ; conv
          nil           ; sched
          'soj-ai              ; special ai
          nil              ; container
          nil              ; readied
          )))
    (kern-char-set-known kchar #t)
    kchar))
