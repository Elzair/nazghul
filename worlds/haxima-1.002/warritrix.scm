;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define warr-lvl 8)
(define warr-species sp_ghast)
(define warr-occ oc_warrior)

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (warr-mk) (list 3))
(define (warr-must-go? gob) (= 0 (car gob)))
(define (warr-end-conv gob) (set-car! gob (- (car gob) 1)))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Clarissa (known as the Warritrix) was in life a powerful female Warrior
;; of superlative might and warcraft.  Now slain, she may be encountered
;; as a shade, summoned by the use of the Rune of Leadership,
;; found among her effects where her corpse lies (in the Broken Sanctuary
;; in the Lost Halls).
;;----------------------------------------------------------------------------

;; Basics...
(define (warr-hail knpc kpc)
  (meet "The spectre of a calm, stately woman confronts you.")
  (say knpc "Hail, Wanderer.")
  (quest-data-update 'questentry-warritrix 'found 1)
  (quest-data-icon! 'questentry-warritrix 's_ghost)
  (quest-data-complete 'questentry-warritrix)
  )

(define (warr-name knpc kpc)
  (say knpc "I was Clarissa, known to many as the Warritrix.")
  )

(define (warr-join knpc kpc)
  (say knpc "Would that we had met in life!")
  )

(define (warr-job knpc kpc)
  (say knpc "To serve justice. Do you the same?")
  (if (yes? kpc)
      (begin
        (say knpc "To serve justice you must know what is just. "
             "I was ordered to explore these caves completely, "
             "and when my party was weak from fighting off monsters, "
             "we were ambushed by Accursed assassins, and all slain. "
             "Tell me, is it just to avenge my death on my betrayers?")
        (yes? kpc)
        (say knpc "Justice hastens not to punish, but rather to reward. "
             "Betrayers will be rewarded with ruin, "
             "schemers and plotters will bring their own downfall, "
             "those who seek power will be powerless in their hour of need. "
             "Do not avenge me, but rather seek the truth.")
        )
      (say knpc "Injustice is served well by inaction."))
  )

(define (warr-trut knpc kpc)
  (say knpc "To find the truth, you must find evidence."))

(define (warr-warr knpc kpc)
  (say knpc "It was a title I bore to gratify others. "
       "In truth, I was just another paladin."))

(define (warr-evid knpc kpc)
  (say knpc "The wrogues are masters of finding what others try to hide, "
       "and the MAN is master of all wrogues.")
       (quest-wise-subinit 'questentry-the-man)
       (quest-data-update 'questentry-the-man 'common 1)
       )

(define (warr-wise knpc kpc)
  (say knpc "The Wise are deceived and divided in their opinions. "
       "The Accursed have done their work well."))

(define (warr-bye knpc kpc)
  (if (warr-must-go? (gob knpc))
      (begin
        (say knpc "We will not meet again, Wanderer.")
        (kern-log-msg "The spirit vanishes.")
        (kern-obj-remove knpc))
      (begin
        (say knpc "Have you asked me all you need to know?")
        (if (yes? kpc)
            (say knpc "Know that I must go soon, I am called to the Void.")
            (say knpc "Then I will try to remain a bit longer.")
            )
        (warr-end-conv (gob knpc)))))

;; Quest-related
(define (warr-rune knpc kpc)
  (say knpc "You may keep the Rune. I know not its purpose. I have carried it "
       "all these years in honour of King Clovis, my godfather, "
       "who gave it to me."))

(define (warr-clov knpc kpc)
  (say knpc "King Clovis carried a Rune of his own. He fell in battle during the "
       "Goblin Wars and the Rune was lost. Do you wish to find it?")
  (if (yes? kpc)
  		(begin
      (say knpc "If anyone knows where it is, it would be the goblins. "
           "Go to Green Tower and seek out Gen. Ask him of ^c+mClovis^c-.")
       	(quest-data-update 'questentry-rune-f 'gen 1)
        (quest-data-assign-once 'questentry-rune-f)
           )
      (say knpc "If you do, ask me now, for like all who are dead I am pulled "
           "to the Void, and cannot resist for long."
           ))
  )

(define (warr-just knpc kpc)
  (say knpc "Justice is the only ideal which needs no theory "
       "or philosophy or deity to uphold it. It is the undeniable, "
       "irresistible ideal."))

(define (warr-absa knpc kpc)
  (say knpc "The Stewardess ordered the destruction of Absalot under the "
       "pretense that it spread Accursed practices. "
       "But is not the lust for power a true sign of one Accursed?")
  (yes? kpc)
  (say knpc "Indeed it is.")
  )

(define (warr-void knpc kpc)
  (say knpc "All spirits are Wanderers in the Void."))

(define (warr-assa knpc kpc)
  (say knpc "They were expecting us, although no one knew we were here "
       "except the Stewardess of Glasdrin and Commander Jeffries."))

(define (warr-jeff knpc kpc)
  (say knpc "The silver deeds of noble youth are tarnished by the guilt of "
       "age. Better is one who striving, dies, than a hero ruined "
       "by success."))

(define (warr-powe knpc kpc)
  (say knpc 
       "Those who see an enemy in every face can never have enough power, "
       "but the fearless hold it in contempt."))

(define warr-conv
  (ifc basic-conv

       ;; basics
       (method 'hail warr-hail)
       (method 'bye  warr-bye)
       (method 'job  warr-job)
       (method 'name warr-name)
       (method 'join warr-join)
       
       (method 'rune warr-rune)
       (method 'clov warr-clov)
       (method 'just warr-just)
       (method 'absa warr-absa)
       (method 'stew warr-absa)
       (method 'warr warr-warr)
       (method 'evid warr-evid)
       (method 'wise warr-wise)
       (method 'void warr-void)
       (method 'trut warr-trut)
       (method 'assa warr-assa)
       (method 'jeff warr-jeff)
       (method 'powe warr-powe)
       ))

(define (mk-warritrix)
  (bind 
   (kern-mk-char 
    'ch_warr           ; tag
    "Warritrix"        ; name
    warr-species         ; species
    warr-occ              ; occ
    s_ghost     ; sprite
    faction-men      ; starting alignment
    10 0 10            ; str/int/dex
    5 2              ; hp mod/mult
    5 2              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0  ;; AP_per_turn
    warr-lvl
    #f               ; dead
    'warr-conv         ; conv
    nil              ; schedule
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (warr-mk)))
