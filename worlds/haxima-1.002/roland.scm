;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (roland-mk free? joined? greeted?) (list free? joined? greeted?))
(define (roland-is-free? knpc) (car (kobj-gob-data knpc)))
(define (roland-joined? knpc) (cadr (kobj-gob-data knpc)))
(define (roland-greeted? knpc) (caddr (kobj-gob-data knpc)))
(define (roland-set-free! knpc) (set-car! (kobj-gob-data knpc) #t))
(define (roland-set-joined! knpc) (set-car! (cdr (kobj-gob-data knpc)) #t))
(define (roland-set-greeted! knpc) (set-car! (cddr (kobj-gob-data knpc)) #t))

(define roland-greetings
  (list
   "Well met!"
   "Hail, Wanderer!"
   ))

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
    (kern-char-set-ai knpc nil)
    (kern-being-set-base-faction knpc faction-men)
    )
  (or (roland-greeted? knpc)
      (and (any-player-party-member-visible? knpc)
           (begin
             (taunt knpc nil roland-greetings)
             (roland-set-greeted! knpc))))
  (if (out-of-cell?)
      (set-free knpc)
      (try-to-escape)))

;; Note: (can-pathfind? ...) will pathfind through the locked door nowadays, so
;; it cannot be relied on. Let's just let Roland try to get out and he'll know
;; he's free.
(define (roland-is-or-can-be-free? knpc)
  (roland-is-free? knpc))

(define (roland-join-player knpc)
  (or (roland-joined? knpc)
      (begin
        (join-player knpc)
        (roland-set-joined! knpc #t))))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Roland is a knight errant, serving Lord Froederick.
;; He is currently imprisoned by bandits in a cell in the slimy cave.
;; Roland is a potential party member.
;;----------------------------------------------------------------------------
(define (roland-join knpc kpc)
  (if (is-player-party-member? knpc)
      (say knpc "Yes, I am still with you. Lead on!")
      (if (roland-joined? knpc)
          (begin
            (say knpc "I am honored to rejoin you.")
            (join-player knpc)
            (kern-conv-end)
            )
          (if (roland-is-or-can-be-free? knpc)
              ;; yes - will the player accept his continued allegiance to
              ;; Froederick?
              (begin
                (say knpc "I thank you for freeing me! I owe you my life, and will gladly join you. What say you?")
                (if (yes? kpc)
                    (begin
                      (say knpc "I am honored! Those vile rogues took my "
                           "iron chest which contains my equipment. It should be "
                           "around here somewhere.")
                      (roland-join-player knpc))
                    (say knpc "[sadly] As you will.")))
              (say knpc "I am locked in this cell! Free me from this dishonour, "
                   "and you will gain an ally.")
              ))))
  
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
                 (say knpc "I am a knight errant.")))
       (method 'name (lambda (knpc kpc) (say knpc "I am Roland.")))
       (method 'join roland-join)

       (method 'cell
               (lambda (knpc kpc)
                 (say knpc "Use picklocks on a locked door to open it. Or, cast "
                      "an unlock spell.")))
       (method 'clov
               (lambda (knpc kpc)
                 (say knpc "I was with him the day he fell in battle. "
                      "The enemy ambushed us, and I was knocked senseless. "
                      "I dreamt that a hideous beast dragged the King down into Tartos. "
                      "When I awoke I was in a camp hospital.")))
       (method 'free
               (lambda (knpc kpc)
                 (say knpc "I was waylaid and kidnapped by the bandits in this "
                      "cave. They've locked me in this cell to hold me for "
                      "ransom.")))
       (method 'pick
               (lambda (knpc kpc)
                 (say knpc "Bandits and thieves usually carry picklocks.")))
       (method 'spel
               (lambda (knpc kpc)
                 (say knpc "You should ask a Wizard about spells.")))
       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "I know Trigave is a small town, a crossroad of "
                      "the north, with much history.")))
       (method 'knig 
               (lambda (knpc kpc)
                 (say knpc "I was a squire of King Clovis in the Goblin Wars. "
                      "When the wars ended, I took to wandering.")
                 ))
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-roland)
  (bind 
    (kern-mk-char 
     'ch_roland          ; tag
     "Roland"            ; name
     sp_human            ; species
     oc_warrior          ; occ
     s_knight            ; sprite
     faction-prisoner    ; starting alignment
     6 0 6               ; str/int/dex
     pc-hp-off           ; hp bonus
     pc-hp-gain          ; hp per-level bonus
     0                   ; mp off
     0                   ; mp gain
     max-health          ; hp
     -1                  ; xp
     max-health          ; mp
     0                   ; ap
     3                   ; lvl
     #f                  ; dead
     'roland-conv        ; conv
     nil                 ; sched
     'roland-ai          ; special ai
     nil                 ; container
     nil                 ; readied
     )
    (roland-mk #f #f #f)))
