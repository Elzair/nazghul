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
;;----------------------------------------------------------------------------

;; Basics...
(define (warr-hail knpc kpc)
  (meet "The spectre of a calm, stately woman confronts you.")
  (say knpc "Hail, Wanderer.")
  )

(define (warr-name knpc kpc)
  (say knpc "I was Yvonne, known to many as the Warritrix.")
  )

(define (warr-join knpc kpc)
  (say knpc "Would that we had met in life!")
  )

(define (warr-job knpc kpc)
  (say knpc "To serve justice. Do you the same?")
  (if (yes? kpc)
      (say knpc "Then ask the Stewardess of Glasdrin of SILAS, "
           "and your words will be put to the test.")
      (say knpc "Injustice is served well by inaction."))
  )

(define (warr-warr knpc kpc)
  (say knpc "It was a title I bore to gratify others. "
       "In truth, I was just another paladin."))

(define (warr-sila knpc kpc)
  (say knpc "Search beneath Absalot. Do not go alone."))

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
  (say knpc "You may keep the Rune. I know not it's purpose. I have carried it "
       "all these years in honour of King Clovis, my godfather, "
       "who gave it to me."))

(define (warr-clov knpc kpc)
  (say knpc "King Clovis carried a Rune of his own. He fell in battle during the "
       "Goblin Wars and the Rune was lost. Do you wish to find it?")
  (if (yes? kpc)
      (say knpc "If anyone knows where it is, it would be the goblins. "
           "Go to Green Tower and seek out Gen. Ask him of CLOVIS.")
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

(define warr-conv
  (ifc basic-conv

       ;; basics
       (method 'hail warr-hail)
       (method 'bye warr-bye)
       (method 'job warr-job)
       (method 'name warr-name)
       (method 'join warr-join)
       
       (method 'rune warr-rune)
       (method 'clov warr-clov)
       (method 'just warr-just)
       (method 'absa warr-absa)
       (method 'stew warr-absa)
       (method 'warr warr-warr)
       (method 'sila warr-sila)
       (method 'wise warr-wise)
       (method 'void warr-void)
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
    (max-hp warr-species warr-occ warr-lvl 0 0) ; hp
    0                   ; xp
    (max-mp warr-species warr-occ warr-lvl 0 0) ; mp
    warr-lvl
    #f               ; dead
    'warr-conv         ; conv
    nil              ; schedule
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (warr-mk)))