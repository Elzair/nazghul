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
(define (roland-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Roland is an agent of Lord Froederick, imprisoned in a cell in the slimy
;; cave.
;;----------------------------------------------------------------------------
(define (roland-is-free? knpc) #f)

(define (roland-join knpc kpc)
  (if (roland-is-free? kpc)
      ;; yes - will the player accept his continued allegiance to Froederick?
      (begin
        (say knpc "I am a vassal of Lord Froederick and cannot turn my "
             "hand against him, and must aid him any way I can. Do you still "
             "wish me to join you?")
        (if (kern-conv-get-yes-no? kpc)
            ;; yes - join
            (begin 
              (say knpc "Then I am honored to join you!")
              (kern-char-join-party knpc (kern-char-get-party kpc)))
            ;; no - don't join
            (say knpc "I understand. I must proceed with haste to Trigrave!")))
      ;; no - roland is still imprisoned
      (say knpc "Good sir, I am in no position to help you from this cell!")))

(define roland-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "I'm afraid I can't help you with that.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Well met.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Farewell.")))
       (method 'job (lambda (knpc kpc) (say knpc "I am a knight in the service of Lord Froederick.")))
       (method 'name (lambda (knpc kpc) (say knpc "I am Roland.")))
       (method 'join roland-join)

       (method 'trig (lambda (knpc kpc) (say knpc "I know Trigave is a small town, a crossroad of the north, with much history.")))
       (method 'knig 
               (lambda (knpc kpc)
                 (say "Lord Froederick sent me north to evaluate the threat of "
                      "invasion from his enemies. I fear the situation is much "
                      "worse than he expected.")))
       ))
