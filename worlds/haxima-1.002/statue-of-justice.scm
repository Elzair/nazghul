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


(mk-obj-type 't_statue_of_justice "ancient statue" s_statue layer-mechanism
             (ifc nil
                  (method 'xamine (lambda (knpc kpc)
                                    (kern-log-msg "A worn engraving on the status reads "
                                                  "'STRIKE FOR JUSTICE'")))
                  (method 'attack soj-strike)
                  ))

(define (soj-accuse knpc kpc)
  (say knpc "Whom do you accuse?")
  (let ((kchar (ui-target (kern-obj-get-location kpc) 19 obj-is-char?)))
    (cond ((null? kchar)
           (say knpc "Do you wish to withdraw your charge?")
           (cond ((yes? kpc)
                  (say knpc "Cursed are you for trifling with serious matters. Be gone, and do not strike me again in vain.")
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

(define (log-dots n delay)
  (define (dots n)
    (cond ((> n 0)
           (kern-log-continue ".")
           (kern-log-flush)
           (kern-sleep delay)
           (dots (- n 1)))))
  (kern-log-begin)
  (dots n)
  (kern-log-end)
  )

(define (soj-get-evidence knpc kpc kchar)
  (println "kchar=" kchar " ch_steward=" ch_steward)
  (say knpc (kern-obj-get-name kpc) ", produce your evidence.")
  (let ((ktype (kern-ui-select-item)))
    (println "ktype=" ktype " t_stewardess_journal=" t_stewardess_journal)
    (cond ((null? ktype)
           (say knpc "Do you have any other evidence?")
           (cond ((no? kpc)
                  (shake-map 15)
                  (say knpc "For accusing another with insufficient evidence, you are guilty of bearing false witness. Your punishment is exile.")
                  ;; todo: implement exile
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
           (say knpc (kern-obj-get-name kchar) ", you are guilty of betrayal. Your punishment is death, and may your name be a curse forevermore.")
           ;; todo: implement punishment of stewardess
           ))
    ))
           
                  

(define (soj-hail knpc kpc)
  (say knpc "Do you seek justice?")
  (cond ((no? kpc)
         (say knpc "Then may injustice befall you.")
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
  (kern-mk-char 
   'ch_soj           ; tag
   "ancient statue"     ; name
   sp_statue         ; species
   nil              ; occ
   s_statue     ; sprite
   faction-men      ; starting alignment
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
   ))
