;;----------------------------------------------------------------------------
;; Schedule
;;
;; no schedule...
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (roland-mk free?) (list free?))
(define (roland-is-free? knpc) (car (kobj-gob-data knpc)))
(define (roland-set-free! knpc) (set-car! (kobj-gob-data knpc) #t))

;;----------------------------------------------------------------------------
;; Custom AI
;; 
;; This AI controls Roland until he is freed. It constantly tries to pathfind
;; to the prison exit. Once it gets outside the cell it sets the "freed" flag
;; and resorts to the default kernel AI.
;;----------------------------------------------------------------------------
(define (roland-ai knpc)
  (define (out-of-cell?)
    (not (loc-in-rect? (kern-obj-get-location knpc)
                       slimey-cavern-prison-cell)))
  (define (try-to-escape)
    (kern-log-enable #f)
    (pathfind knpc (mk-loc (kobj-place knpc)
                           (rect-x slimey-cavern-prison-cell-exit)
                           (rect-y slimey-cavern-prison-cell-exit)))
    (kern-log-enable #t))
  (define (set-free)
    (roland-set-free! knpc)
    (kern-char-set-ai knpc nil))
  (if (out-of-cell?)
      (set-free knpc)
      (try-to-escape)))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Roland is an agent of Lord Froederick, imprisoned in a cell in the slimy
;; cave.
;;----------------------------------------------------------------------------
(define (roland-join knpc kpc)
  (if (roland-is-free? knpc)
      ;; yes - will the player accept his continued allegiance to Froederick?
      (begin
        (say knpc "I am honored to join you!")
        (join-player knpc))
      (say knpc "But I am locked in this cell!")))

(define roland-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (say knpc "I'm afraid I can't help you with that.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Well met.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Farewell.")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "I am a knight of Lord Froederick.")))
       (method 'name (lambda (knpc kpc) (say knpc "I am Roland.")))
       (method 'join roland-join)

       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "I know Trigave is a small town, a crossroad of "
                      "the north, with much history.")))
       (method 'knig 
               (lambda (knpc kpc)
                 (say knpc "Lord Froederick sent me north to evaluate the "
                      "threat of invasion from his enemies. I fear the "
                      "situation is much worse than he expected.")))
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-roland-first-time tag)
  (bind 
   (kern-mk-char tag                 ; tag
                 "Roland"            ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_knight            ; sprite
                 faction-men         ; starting alignment
                 0 10 5              ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 30 0 9 9            ; hp/xp/mp/lvl
                 'roland-conv        ; conv
                 nil                 ; sched
                 'roland-ai          ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (roland-mk #f)))
