;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Gregor's Hut".
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_ilya
               (list 0  0  gh-ilyas-bed   "sleeping")
               (list 6  0  gh-stable      "working")
               (list 7  0  gh-kitchen     "working")
               (list 12 0  gh-table-1     "eating")
               (list 13 0  gh-pasture     "working")
               (list 15 0  gh-all         "idle")
               (list 17 0  gh-table-1     "eating")
               (list 18 0  gh-living-room "idle")
               (list 20 0  gh-ilyas-bed   "sleeping"))

;;----------------------------------------------------------------------------
;; Gob
;;
;; Ilya's quest is to find her stuffed horse, which she left behind at the
;; homestead when she fled the trolls that killed her family. The quest flags
;; are stored in her gob.
;;----------------------------------------------------------------------------
(define (ilya-mk gave-quest? finished-quest?) 
  (list gave-quest? finished-quest?))
(define (ilya-gave-quest? ilya) (car ilya))
(define (ilya-quest-done? ilya) (cadr ilya))
(define (ilya-give-quest ilya) (set-car! ilya #t))
(define (ilya-finish-quest ilya) (set-car! (cdr ilya) #t))

;;----------------------------------------------------------------------------
;; Puska
;;
;; Puska -- ilya's stuffed horse toy -- is a quest item. Nothing special about
;; it really but it is unique and needs its own object type. The object itself
;; is declared in the p_abandoned_cellar constructor. But the type declaration
;; needs to be in a file that is reloaded, so here is as good a place as any.
;;----------------------------------------------------------------------------
(define puska-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-log-msg "Some child probably misses this toy!")
                      (kobj-get kobj getter)))))

(mk-obj-type 't_puska "stuffed toy horse" s_toy_horse layer-item puska-ifc)

;;----------------------------------------------------------------------------
;; Quest
;;
;; This is a single response in Ilya's conversation. I've called it our here
;; separately to make it obvious.
;;----------------------------------------------------------------------------
(define (ilya-quest knpc kpc)
  (let ((ilya (kobj-gob-data knpc)))
    (display ilya)(newline)
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
                        (if (kern-obj-has? kpc t_puska)

                            ;; yes - player has puska
                            (begin
                              (kern-obj-remove-from-inventory kpc t_puska 1)
                              (say knpc "There, there, puska. "
                                   "You're safe with me. [She turns to you] Thank you so much! I wish I could pay you. "
                                   "Wait, take these, Mommy said wizards use them, so I pick them whenever I can.")
                              (ilya-finish-quest ilya)
                              (kern-obj-add-to-inventory kpc nightshade 23)
                              )

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
                        (say knpc "She must be there somewhere!")
                        (say knpc "West through the pass, then north against the hills."))))))

        ;; no - didn't give quest yet
        (begin
          (say knpc "Puska is my stuffed horse. But I lost her! "
               "If you find her will you tell me?")
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (say knpc "Our farm was west through the pass, then north against the hills. "
                     "Watch out for the trolls!")
                (ilya-give-quest ilya))
              (begin
                (say knpc "If you keep her I will find you when I grow up.")
                (kern-conv-end)))))))

(define (ilya-join knpc kpc)
  (say knpc "I'm just a little girl, silly!")
  )

;;----------------------------------------------------------------------------
;; Animals
;;
;; Ilya has an odd relationship with spiders. She'll teach the player a spell
;; to ward off spiders if he plays along. Spiders will dominate the woods
;; around the Abandoned Farm (Ilya's old home). In fact, I intend to have them
;; locked in a battle with the trolls the first time the player enters the
;; Abandoned Farm. I'm planning on having a "great mother" spider known around
;; these parts as Angril or Angriss, perhaps she was one of Ilya's pets as a
;; child - I'm not sure how I want to play that one out yet.
;;----------------------------------------------------------------------------
(define (ilya-animals knpc kpc)
  (say knpc "We have some sheep, and Charm the cat, and some chickens. "
       "Do you like animals?")
  (if (kern-conv-get-yes-no? kpc)

      ;; yes - the player likes animals
      (begin
        (say knpc "What's your favorite animal?")
        (let ((fav (kern-conv-get-reply kpc)))
          (if (eq? fav 'spid)

              ;; yes - the player's favorite animal is spiders
              (begin
                (say knpc "Mine too! I know how to make them harmless. "
                     "Want me to teach you?")
                (if (kern-conv-get-yes-no? kpc)

                    ;; yes - the player wants to learn the spider ward
                    (say knpc "It's easy! Mix spider silk and garlic, "
                         "and chant An Xen Bet.")

                    ;; no - the player does not want to learn the spider ward
                    (say knpc "Ok, but they sometimes attack people.")))
                    

              ;; no - the player's favorite animal is NOT spiders
              (say knpc "Spiders are my favorite!"))))

      ;; no - the player does not like animals
      (say knpc "Well don't hurt them!")))

(define (ilya-fire knpc kpc)
  (say knpc "Making fire is easy. Just mix black pearl and sulphos..."
       "surephous... that ashy stuff you know? "
       "And say Vas Flam!"))

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
       (method 'age (lambda (knpc kpc) (say knpc "I'm eight.")))
       (method 'chor (lambda (knpc kpc) (say knpc "I feed the animals, and keep the fire, and help cook.")))
       (method 'anim ilya-animals)
       (method 'gran (lambda (knpc kpc) (say knpc "I live with Grandpa now because Mommy and Daddy died.")))
       (method 'died (lambda (knpc kpc) (say knpc "Trolls attacked our farm! "
                                                "Mommy hid me in the cellar, "
                                                "and I snuck out when the trolls were sleeping. "
                                                "But I lost my Puska...")))
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
       (method 'pusk ilya-quest)
       (method 'home (lambda (knpc kpc) (say knpc "Our farm was north and south through the woods.")))
       (method 'spid (lambda (knpc kpc) (say knpc "There are lots of spiders in the woods around here.")))
       (method 'wood (lambda (knpc kpc) (say knpc "Grandpa says to stay out of the woods.")))
       
       (method 'fire ilya-fire)
       (method 'vas ilya-fire)
       (method 'flam ilya-fire)
       (method 'greg (lambda (knpc kpc) (say knpc "He's my grandpa.")))
       (method 'join ilya-join)
       ))

