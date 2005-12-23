;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_miggs
               (list 0  0  trigrave-miggs-bed      "sleeping")
               (list 7  0  trigrave-tavern-kitchen "working")
               (list 23 0  trigrave-miggs-bed      "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (miggs-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Miggs is the tavern-keeper.
;;----------------------------------------------------------------------------
(define (miggs-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by the tavern when I'm open. I open at 7:00AM and close "
           "at midnight.")
      (begin
        (kern-conv-trade knpc kpc
                         (list t_food 5)
                         (list t_beer 3)
                         )
        )))

(define (miggs-hail knpc kpc)
  (kern-print "[You meet a large woman with a pretty face. She shyly avoids "
              "your gaze]\n"))

(define (miggs-job knpc kpc)
  (say knpc "I run the the Lusty Jugs."))

(define (miggs-lust knpc kpc)
  (say knpc "It's a tavern. Want something?")
  (if (kern-conv-get-yes-no? kpc)
      (miggs-trade knpc kpc)
      (say knpc "Ok")))

(define miggs-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "[She shrugs]")))
       (method 'hail miggs-hail)
       (method 'bye (lambda (knpc kpc) (say knpc "[She smiles slightly]")))
       (method 'job miggs-job)       
       (method 'name (lambda (knpc kpc) (say knpc "Miggs")))

       (method 'trad miggs-trade)
       (method 'buy miggs-trade)
       (method 'sell miggs-trade)
       (method 'food miggs-trade)
       (method 'lust miggs-lust)
       (method 'jugs miggs-lust)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-miggs tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Miggs"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_fat_townswoman    ; sprite
                 faction-men         ; starting alignment
                 12 10 8             ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 30 0 0 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'miggs-conv        ; conv
                 sch_miggs          ; sched
                 nil                 ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (miggs-mk)))
