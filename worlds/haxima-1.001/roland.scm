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
(define (roland-mk free? joined?) (list free? joined?))
(define (roland-is-free? knpc) (car (kobj-gob-data knpc)))
(define (roland-joined? knpc) (cadr (kobj-gob-data knpc)))
(define (roland-set-free! knpc) (set-car! (kobj-gob-data knpc) #t))
(define (roland-set-joined! knpc) (set-car! (cdr (kobj-gob-data knpc)) #t))

;;----------------------------------------------------------------------------
;; Custom AI
;; 
;; This AI controls Roland until he is freed. It constantly tries to pathfind
;; to the prison exit. Once it gets outside the cell it sets the "freed" flag
;; and resorts to the default kernel AI.
;;----------------------------------------------------------------------------
(define (roland-exit-point knpc)
  (mk-loc (kobj-place knpc)
          (rect-x slimey-cavern-prison-cell-exit)
          (rect-y slimey-cavern-prison-cell-exit)))

(define (roland-ai knpc)
  (define (out-of-cell?)
    (not (loc-in-rect? (kern-obj-get-location knpc)
                       slimey-cavern-prison-cell)))
  (define (try-to-escape)
    (kern-log-enable #f)
    (pathfind knpc (roland-exit-point knpc))
    (kern-log-enable #t))
  (define (set-free)
    (roland-set-free! knpc)
    (kern-char-set-ai knpc nil))
  (if (out-of-cell?)
      (set-free knpc)
      (try-to-escape)))

(define (roland-is-or-can-be-free? knpc)
  (or (roland-is-free? knpc)
      (can-pathfind? knpc (roland-exit-point knpc))))

(define (roland-join-player knpc)
  (or (roland-joined? knpc)
      (begin
        (join-player knpc)
        (roland-set-joined! knpc #t))))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Roland is an agent of Lord Froederick, imprisoned in a cell in the slimy
;; cave.
;;----------------------------------------------------------------------------
(define (roland-join knpc kpc)
  (if (roland-joined? knpc)
      (say knpc "Yes, I am still with you. Lead on!")
      (if (roland-is-or-can-be-free? knpc)
          ;; yes - will the player accept his continued allegiance to
          ;; Froederick?
          (begin
            (say knpc "I thank you for freeing me! I have sworn fealty to Lord "
                 "Froederick. But if you swear not to raise your hand against "
                 "him, I will join you. What say you?")
            (if (kern-conv-get-yes-no? kpc)
                (begin
                  (say knpc "I am honored to join you! Those vile rogues took my "
                       "iron chest which contains my equipment. It should be "
                       "around here somewhere.")
                  (roland-join-player knpc))
                (say knpc "[sadly] As you will. May our swords never cross.")))
          (say knpc "I am locked in this cell! Free me from this dishonour, "
               "and you will gain an ally. I might even JOIN you if you are "
               "willing."))))
  
(define roland-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (say knpc "I'm afraid I can't help you with that.")))
       (method 'hail 
               (lambda (knpc kpc) 
                 (if (roland-joined? knpc)
                     (say knpc "I will aid thee any way I can.")
                     (roland-join knpc kpc))))

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
                 30 0 9 3            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'roland-conv        ; conv
                 nil                 ; sched
                 'roland-ai          ; special ai
                 nil                 ; container
                 nil                 ; readied
                 )
   (roland-mk #f #f)))
