;; Thorald is a character in the Tutorial game.


(define (thorald-mixi knpc kpc)
  (say knpc "To M)ix spells first press the 'M' key. "
       "Then type the first letter of each spell syllable followed by ENTER. "
       "For example, In Ex Por would be: i, e, p, ENTER.")
  (prompt-for-key)
  (say knpc "Next, select the reagents using the space bar. ")
  (prompt-for-key)
  (say knpc "Next, press ENTER and type the number of mixtures to make. "
       "Just mix 1 for now. ")
  (prompt-for-key)
  (say knpc "You can see what spells you have mixed with the Z)tatus command. "
       "Why don't you try mixing one now? Talk to me again when you've succeeded.")
  (kern-conv-end)
  )

(define conv-thorald
  (ifc nil
       (method 'bye
               (lambda (knpc kpc)
                 (say knpc "Oh, I'm sure we'll be seeing more of each other.")))
       (method 'default 
               (lambda (knpc kpc)
                 (say knpc "I can't help you with that.")))
       (method 'hail
               (lambda (knpc kpc)
                 (kern-log-msg "You meet a bored old man.")
                 (if (in-inventory? kpc in_ex_por)
                     (say knpc "I see you have an In Ex Por spell. "
                          "Ask me about casting it if you don't know how.")
                     (say knpc "Welcome to the tutorial. Why don't you ask me about my job, "
                          "or maybe that door over there.")
                     )))
       (method 'join
               (lambda (knpc kpc)
                 (say knpc "Yes, let's get this show on the road. "
                      "Press 'F' and I'll follow you. "
                      "Later if you want me to take turns moving with you, press 'F' again. "
                      "And if you want me to explore while you wait, press '2'. "
                      "You can always press 'F' again to get me to follow you.")
                 (join-player knpc)
                 (kern-conv-end)
                 ))
       (method 'name
               (lambda (knpc kpc)
                 (say knpc "I'm Thorald. Hi.")))
       (method 'job
               (lambda (knpc kpc)
                 (say knpc "I'm just the hired help.")))
       (method 'door
               (lambda (knpc kpc)
                 (say knpc "Yes, I know a spell that can open that magically locked door. "
                      "Imagine that! Why don't you ask me about it?")
                 ))
       (method 'spel
               (lambda (knpc kpc)
                 (say knpc "The In Ex Por spell unlocks magically locked doors. "
                      "To mix it you'll need sulphurous ash and blood moss. "
                      "Ask me about mixing if you don't know how.")))
       (method 'mix thorald-mixi)
       (method 'mixi thorald-mixi)
       (method 'cast
               (lambda (knpc kpc)
                 (say knpc "To C)ast a spell press 'c' and then enter the first letter of each magic word. "
                      "For example, to cast In Ex Por you enter 'i', 'e' and 'p'. "
                      "Use backspace if you mess up. "
                      "When the spell is right hit ENTER to cast it. "
                      "In Ex Por requires you to target the door.")))
       ))
       
(define (thorald-ai kchar) #t)

(define (mk-thorald)
  (kern-mk-char 
   'ch_thorald ; tag
   "Thorald Greybeard"   ; name
   sp_human              ; species
   oc_wrogue             ; occ
   s_companion_wizard    ; sprite
   faction-player        ; starting alignment
   0 10 2                ; str/int/dex
   0 1                   ; hp mod/mult
   10 5                  ; mp mod/mult
   240 -1 8 0 8             ; hp/xp/mp/AP_per_turn/lvl
   #f                    ; dead
   'conv-thorald         ; conv
   nil                   ; sched
   'thorald-ai           ; special ai
   nil                   ; container
   (list t_sling
         t_armor_leather
         )
   nil
   ))
