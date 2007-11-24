;;----------------------------------------------------------------------------
;; This is the special statue that stands in the courtyard of Glasdrin. When
;; attacked, it speaks. This requires two objects: an object with a type in
;; order to support the 'attack interface, and a character type to have the
;; conversation. The illusion is that they are both the same object.
;;----------------------------------------------------------------------------

(define (soj-strike kobj kpc)
  (println "soj-strike")
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

(define (soj-hail knpc kpc)
  (say knpc "Do you seek justice?")
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
