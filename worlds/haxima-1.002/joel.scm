;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define joel-lvl 3)
(define joel-species sp_human)
(define joel-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In front of the Gate to Absalot.
;;----------------------------------------------------------------------------
(define joel-bed (list 'p_gate_to_absalot 8 9 1 1))
(define joel-mealplace joel-bed)
(define joel-workplace (list 'p_gate_to_absalot 7 10 5 5))
(define joel-leisureplace joel-workplace)
(kern-mk-sched 'sch_joel
               (list 0  0 joel-bed          "sleeping")
               (list 5  0 joel-mealplace    "eating")
               (list 6  0 joel-workplace    "working")
               (list 12 0 joel-mealplace    "eating")
               (list 13 0 joel-workplace    "working")
               (list 18 0 joel-mealplace    "eating")
               (list 19 0 joel-leisureplace "idle")
               (list 21 0 joel-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (joel-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Joel is a simple cowherd, whose stock is grazing in front of
;; the gates to the destroyed city of Absalot.
;;----------------------------------------------------------------------------

;; Basics...
(define (joel-hail knpc kpc)
  (kern-log-msg "You meet a laconic herder.")
  (say knpc "Hi")
  )

(define (joel-default knpc kpc)
  (say knpc "Don't know, pardner."))

(define (joel-name knpc kpc)
  (say knpc "Joel."))

(define (joel-join knpc kpc)
  (say knpc "Nope."))

(define (joel-job knpc kpc)
  (say knpc "I herd my cows here. Nice and peaceful."))

(define (joel-peac knpc kpc)
  (say knpc "Yep. This is the old gate to Absalot."))

(define (joel-absa knpc kpc)
  (say knpc "You're not thinking of going in there, are you?")
  (if (yes? kpc)
      (say knpc "Crazy. Theres nasty stuff down there.")
      (say knpc "Don't blame you.")))

(define (joel-nast knpc kpc)
  (say knpc "Gazers? Undead? Who knows what else? The place was bad enough before "
       "the paladins sealed it up and left it to fester."))

(define (joel-fest knpc kpc)
  (say knpc "Paladins from Glasdrin destroyed Absalot. And when humans get out of the way, "
	"monsters move in."))
	
(define (joel-gaze knpc kpc)
  (say knpc "Mind-slavers. And whatever they're controlling. "
       "If you dont watch out you'll wind up their slaves too."))
	   
(define (joel-unde knpc kpc)
  (say knpc "Ghosts, skeletons, and I'd rather not know what else."))

(define (joel-bye knpc kpc)
  (say knpc "So long"))

(define joel-conv
  (ifc basic-conv

       ;; basics
       (method 'default joel-default)
       (method 'hail joel-hail)
       (method 'bye joel-bye)
       (method 'job joel-job)
       (method 'name joel-name)
       (method 'join joel-join)

       (method 'peac joel-peac)
       (method 'nice joel-peac)
       (method 'absa joel-absa)
       (method 'nast joel-nast)
       (method 'stuf joel-nast)
       (method 'mons joel-nast)
       (method 'fest joel-fest)
       (method 'pala joel-fest)
       (method 'unde joel-unde)
       (method 'gaze joel-gaze)	   
       ))

(define (mk-joel)
  (bind 
   (kern-mk-char 
    'ch_joel           ; tag
    "Joel"             ; name
    joel-species         ; species
    joel-occ              ; occ
    s_companion_shepherd     ; sprite
    faction-men      ; starting alignment
    1 0 1            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    joel-lvl
    #f               ; dead
    'joel-conv         ; conv
    sch_joel           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_staff
					         )              ; readied
    )
   (joel-mk)))
