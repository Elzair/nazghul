;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Gregor's Hut".
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ilya
               (list 0  0  51 9  1  1  "sleeping")
               (list 8  0  40 11 3  3  "idle")
               (list 9  0  49 6  7  1  "working")
               (list 12 0  50 9  1  1  "eating")
               (list 13 0  49 6  7  1  "working")
               (list 18 0  56 54 1  1  "eating")
               (list 19 0  53 50 4  7  "idle")
               (list 21 0  51 9  1  1  "sleeping"))

;;----------------------------------------------------------------------------
;; Gob
;;
;; Ilya's quest is to find her stuffed horse, which she left behind at the
;; homestead when she fled the trolls that killed her family. The quest flags
;; are stored in her gob.
;;----------------------------------------------------------------------------
(define (ilya-mk gave-quest? finished-quest?) (list gave-quest? finished-quest?))
(define (ilya-gave-quest? ilya) (car ilya))
(define (ilya-quest-done? ilya) (cadr ilya))
(define (ilya-give-quest ilya) (set-car! ilya #t))
(define (ilya-finish-quest ilya) (set-cadr! ilya #t))

;;----------------------------------------------------------------------------
;; Quest
;;
;; This is a single response in Ilya's conversation. I've called it our here
;; separately to make it obvious.
;;----------------------------------------------------------------------------
(define (ilya-quest knpc kpc)
  (let ((ilya (kobj-gob-data knpc)))
    (if (ilya-gave-quest? ilya)

        ;; yes - gave quest already
        (if (ilya-quest-done? ilya)
            (say knpc "Puska is happy now!")
            (begin
              (say knpc "Did you find Puska yet?")
              (if (kern-conv-get-yes-no? kpc)

                  ;; yes - puska found
                  (begin 
                    (say knpc "May I have her please?")
                    (if (kern-conv-get-yes-no? kpc)

                        ;; yes - ilya may have puska
                        (if (kern-obj-in-inventory? kpc t_stuffed_animal)

                            ;; yes - player has puska
                            (begin
                              (kern-obj-remove-from-inventory kpc 1 k_stuffed_animal)
                              (say knpc "There, there, puska. You're safe with me."))

                            ;; no - puska not in player inventory
                            (begin
                              (say knpc "[Sob] You don't have her!")
                              (kern-conv-end)))

                        ;; no - ilya can't have puska
                        (begin
                          (say knpc "When I grow up I'll be a sorceress! "
                               "And I'LL BURN YOU TO A CRISP!")
                          (kern-conv-end))))

                  ;; no - didn't find her yet
                  (begin
                    (say knpc "Do you remember where our farm was?")
                    (if (kern-conv-get-yes-no? kpc)
                        (say kpc "She must be there somewhere!")
                        (say kpc "North and east, in a nook in the hills."))))))

        ;; no - didn't give quest yet
        (begin
          (say knpc "I think I left Puska back at home. If you find her will you give her to me?")
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (say kpc "Our farm was north and east through the woods. "
                     "Watch out for the trolls! They hate fire.")
                (ilya-give-quest ilya))
              (begin
                (say knpc "If you keep her I will find you when I grow up.")
                (kern-conv-end)))))))

;;----------------------------------------------------------------------------
;; Join
;;
;; Ilya will join the player if he finishes her quest and gets permission from
;; Gregor. (NOTE: need to setup a flag in the player gob to mark when he gets
;; permission).
;;----------------------------------------------------------------------------
(define (ilya-join knpc kpc)
  (let ((ilya (kobj-gob-data knpc)))
    (if (ilya-quest-done? ilya)
        (begin
          (say knpc "Will you teach me magic?")
          (if (kern-conv-get-yes-no? kpc)
              (say knpc "Ok, but first ask my Grandpa Gregor.")
              (say knpc "Then I can't join you. I must study!")))
        (say knpc "I'm just a little girl, silly!"))))
;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------
(define ilya-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "I don't know.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Hi.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Bye-bye.")))
       (method 'job (lambda (knpc kpc) (say knpc "I help Grandpa with chores.")))
       (method 'name (lambda (knpc kpc) (say knpc "I'm Ilya.")))
       (method 'gran (lambda (knpc kpc) (say knpc "I live with Grandpa now because Mommy and Daddy died.")))
       (method 'died (lambda (knpc kpc) (say knpc "Our farm was attacked by trolls! "
                                                "I hid in the cellar, and snuck out when they were sleeping. "
                                                "But I lost my Puska!")))
       (method 'trol (lambda (knpc kpc) (say knpc "I hate them! When I grow up I want to kill them all.")))
       (method 'hate (lambda (knpc kpc) (say knpc "I will be a sorceress some day and I will kill anyone I hate! "
                                              "I won't be afraid of anything ever again!")))
       (method 'afra (lambda (knpc kpc) (say knpc "I was afraid in the cellar. "
                                                "I heard Mommy and Daddy crying when the trolls ate them... "
                                                "[sniffling] I thought they would find me and eat me too...")))
       (method 'momm (lambda (knpc kpc) (say knpc "I miss my Mommy. She taught me to make fire with my mind. "
                                               "She burned up one of the trolls when they attacked!")))
       (method 'dadd (lambda (knpc kpc) (say knpc "I miss daddy. He tried to fight the trolls but "
                                               "he was just a farmer.")))
       (method 'pusk (lambda (knpc kpc) (say knpc "Puska is my stuffed horse. But now I can't find her!")))
       (method 'home (lambda (knpc kpc) (say knpc "Our farm was north and east through the woods.")))
       (method 'join ilya-join)
       (method 'find ilya-quest)
       ))

