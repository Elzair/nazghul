;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define nossifer-x 16)
(define nossifer-y 6)
(define noss-lvl 9)
(define noss-species sp_balron)
(define noss-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (noss-mk) (list #f))
(define (noss-spoke? gob) (car gob))
(define (noss-spoke! gob) (set-car! gob #t))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (noss-hail knpc kpc)
  (say knpc "I am summoned at last! Whom shall I punish for this delay?"))

(define (noss-default knpc kpc)
  (say knpc "I slay fools who waste words with me."))

(define (noss-name knpc kpc)
  (say knpc "I am Nossifer, the Sleeper."))

(define (begin-last-battle knpc kpc)
  (say knpc "Your soul will never know the bliss of the Void, "
       "I will torment it FOREVER!")
  (kern-being-set-base-faction knpc faction-demon)
  (kern-conv-end))

(define (noss-job knpc kpc)
  (say knpc "I bring oblivion to worlds. You've heard of Wizard's who summon Demons to do their bidding?")
  (yes? kpc)
  (say knpc "I summon men to do mine. What do you think YOU are?")
  (kern-log-msg "He laughs, and the air reeks of sulphur.")
  (say knpc "You have opened the way, and served your purpose well. "
       "Now, receive your reward... ")
  (kern-being-set-base-faction knpc faction-demon)
  (kern-conv-end))

(define (noss-bye knpc kpc)
  (say knpc "Not yet. We have unfinished business to discuss.")
  (prompt-for-key)
  (noss-job knpc kpc))

(define noss-conv
  (ifc basic-conv

       ;; basics
       (method 'default noss-default)
       (method 'hail noss-hail)
       (method 'bye noss-bye)
       (method 'job noss-job)
       (method 'name noss-name)

       ))

(define (noss-ai kchar)
  (spell-sword-ai kchar))

(define (mk-nossifer)
  (let ((kchar (bind 
                (kern-char-set-level
                 (kern-mk-char 
                  'ch_nossifer           ; tag
                  "Nossifer"             ; name
                  noss-species         ; species
                  noss-occ              ; occ
                  s_balron          ; sprite
                  faction-men      ; starting alignment
                  20 5 20            ; str/int/dex
                  10 5              ; hp mod/mult
                  10 5              ; mp mod/mult
                  (max-hp noss-species noss-occ noss-lvl 0 0) ; hp
                  0                   ; xp
                  (max-mp noss-species noss-occ noss-lvl 0 0) ; mp
                  noss-lvl
                  #f               ; dead
                  'noss-conv       ; conv
                  nil           ; sched
                  'noss-ai  ; special ai
                  nil              ; container
                  nil              ; readied
                  )
                 noss-lvl)
                (noss-mk))))
    (map (lambda (eff) (kern-obj-add-effect kchar eff nil))
         (list ef_fire_immunity))
    kchar))
 