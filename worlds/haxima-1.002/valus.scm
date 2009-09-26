;;----------------------------------------------------------------------------
;; Valus
;;
;; Initially a prisoner of Glasdrin, he becomes the new Steward after the
;; trial.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; Initially, in a prison cell in the dungeons below Glasdrin.
;; After the trial of the Stewardess, Valus is assigned Jeffries schedule.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (valus-mk) (list 'townsman))

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Valus is an imprisoned nobleman, once in the service of the late 
;; Steward of Glasdrin, now languishing in the dungeons below Glasdrin.
;; 
;;----------------------------------------------------------------------------

;; Basics...
(define (valus-hail knpc kpc)
  (say knpc "Welcome, stranger.")
  )

(define (valus-name knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "I am Valus, Steward of Glasdrin.")
      (say knpc "I am Valus, lord of all I survey [he gestures around the cell].")
  ))

(define (valus-job knpc kpc)
  (cond ((player-stewardess-trial-done?)
         (say knpc "The people of Glasdrin released me from prison, thanks to the evidence in that diary you found. "
              "It seems I am in your debt.")
         (prompt-for-key)
         (say knpc "After I was released they elected me their new Steward.")
         )
        (else
         (say knpc "I'm just enjoying my retirement.")
         )))

(define (valus-join kpc kpc)
  (cond ((player-stewardess-trial-done?) (say knpc "My duties keep me here."))
        (else (say knpc "On the contrary, why don't you open the door and join me?"))
        ))

;; Special
(define (valus-comm knpc kpc)
  (if (player-stewardess-trial-done?)
         (say knpc "Janice will make a fine commander. I remember when she was just a squad leader.")
         (say knpc "Jeffries is the new Commander. He makes a good pet for the Stewardess. I wasn't such a good dog, myself.")
         ))

(define (valus-pet knpc kpc)
  (say knpc "After Absalot, she blamed me for the deaths of civilians, "
       "and accused me of drunkenness and unnatural acts with trolls.")
  (aside 'ch_ini "I thought the part about the trolls was true.")
  )

(define (valus-trol knpc kpc)
  (say knpc "What can I say? Those trolls know how to party. [He smiles dryly]")
  (prompt-for-key)
  (say knpc "But seriously, she had me imprisoned for asking too many questions. "
       "The former Steward went insane under mysterious circumstances. "
       "For some reason she didn't like me poking into the matter.")
  )

(define (valus-absa knpc kpc)
  (say knpc "War is hell, friend. All of this nonsense about the Accursed aside, "
       "the mages of Absalot were growing too powerful. "
       "Every commander knows the legend of Isin.")
  )

(define (valus-isin knpc kpc)
  (say knpc "According to legend, the sorceress Isin almost single-handedly defeated two armies. "
       "The army of Tulemane was so utterly vanquished that their city has been forgotten. ")
  (prompt-for-key)
  (say knpc "Glasdrin's army finally overcame her, but with such heavy losses that it had to abandon its objective. "
       "We have never forgotten the lesson.")
  )

(define (valus-less knpc kpc)
  (say knpc "The lesson of the Battle of Isin is to never let wizards get too powerful. "
       "And if they do, don't try to face them in open combat. "
       "You know, I've always wondered why that battle took place.")
  )

;; new....
(define (valus-stew knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "Now that I am steward there will be some changes around here.")
      (say knpc "That mad-woman's policies will be the ruin of Glasdrin.")
      ))

(define (valus-poli knpc kpc)
  (say knpc "The goal of the Stewardess is to increase her own power at the expense of all else. ")
  )

(define (valus-chan knpc kpc)
  (say knpc "My first concern is the security, stability and dominance of Glasdrin.")
  )

(define (valus-domi knpc kpc)
  (if (ask? knpc kpc "The only way a city or state can ensure its own security is to dominate others, don't you agree?")
      (say knpc "Yes, all wise statesmen know this, and strive to make their own state the strongest.")
      (say knpc "If we permit other states to surpass us in strength, then they will dominate us. "
           "As Steward, I am responsible for taking action to prevent that.")
      ))

(define valus-conv
  (ifc glasdrin-conv

       ;; basics
       (method 'hail valus-hail)
       (method 'job  valus-job)
       (method 'name valus-name)
       (method 'join valus-join)

       (method 'absa valus-absa)
       (method 'chan valus-chan)
       (method 'civi valus-absa)
       (method 'comm valus-comm)
       (method 'dog  valus-pet)
       (method 'domi valus-domi)
       (method 'drun valus-trol)
       (method 'isin valus-isin)
       (method 'jeff valus-comm)
       (method 'less valus-less)
       (method 'pet  valus-pet)
       (method 'poli valus-poli)
       (method 'reti (lambda (knpc kpc) (say knpc "Formerly, I was the Commander of Glasdrin.")))
       (method 'ruin valus-poli)
       (method 'secu valus-domi)
       (method 'stab valus-domi)
       (method 'stew valus-stew)
       (method 'trol valus-trol)
       (method 'unna valus-trol)
       ))

(define (mk-valus)
  (bind 
   (kern-mk-char 'ch_valus       ; tag
                 "Valus"          ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_fallen_paladin ; sprite
                 faction-glasdrin         ; starting alignment
                 2 1 1               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 5  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'valus-conv         ; conv
                 nil           ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_sword
                       ))         ; readied
   (valus-mk)))
