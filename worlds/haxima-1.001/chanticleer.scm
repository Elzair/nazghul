;; ----------------------------------------------------------------------------
;; Chanticleer
;;
;; Chanticleer is a roaming bard who spends most of his time in Trigrave. He's
;; a confident, easy-going fellow who is also an unstoppable showman. He
;; possesses a great deal of knowledge about the region, loves rumours and
;; gossip, has friends in low and high places (uncluding the reclusive
;; Enchanter) and is probably a spy for *somebody* but it isn't clear who.
;;
;; ----------------------------------------------------------------------------
(define chanticleer-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default 
               (lambda (knpc kpc) 
                 (say knpc "Surprisingly, I know nothing of that subject.")))
       (method 'hail 
               (lambda (knpc kpc) 
                 (say knpc "Well met, my fine fellow!")))
       (method 'bye 
               (lambda (knpc kpc) 
                 (say knpc "May the road kiss your feet!")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "Verily I am a vessel of wit and song!")))
       (method 'name 
               (lambda (knpc kpc) 
                 (say knpc "Chanticleer the Bard, at your service "
                      "[He bows and flourishes]")))
       (method 'join 
               (lambda (knpc kpc) 
                 (say knpc "Some bards adventure, others are wise. "
                      "I am among the latter.")))

       (method 'song 
               (lambda (knpc kpc) 
                 (say knpc "A thousand songs are caged behind the bars of my "
                      "dry throat. Perhaps a little coin to whet the pipes?"
                      "[He looks at you expectantly. "
                      "Do you give him some gold?]")))
       
       (method 'jim
               (lambda (knpc kpc)
                 (say knpc
                      "Ha! Here's a bit of gossip!\n"
                      "Full fell and grim\n"
                      " the righteous Jim,\n"
                      " His armour slicked in gore,\n"
                      " Slew his master on the field,\n"
                      " Scratched the emblem from his shield\n"
                      " And fled that cursed shore."
                      )))

       (method 'gwen
               (lambda (knpc kpc)
                 (say knpc
                      "Ah, our mysterious inkeeper...\n"
                      "When the gray dove is weeping\n"
                      "And the whole world is sleeping\n"
                      "When ghosts rise like mists from the sea\n"
                      "The owl in the moonlight\n"
                      "Inquires of the still night\n"
                      "The question we all ask of she!\n")))
       
       (method 'chan
               (lambda (knpc kpc)
                 (say knpc
                      "Who is that wise, enchanting bard,\n"
                      "Who plays the lute like song made flesh\n"
                      "And tells tales fate herself would tell\n"
                      "If fate could only lie so well."

       ))
