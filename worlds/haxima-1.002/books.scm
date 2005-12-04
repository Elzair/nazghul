;; ----------------------------------------------------------------------------
;; books.scm -- reading material
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_books 32 32 2 2 0 0 "books.png")

(kern-mk-sprite 's_lexicon ss_books 1 0 #f 0)
(kern-mk-sprite 's_manual  ss_books 1 1 #f 0)
(kern-mk-sprite 's_scroll  ss_books 1 2 #f 0)

;;----------------------------------------------------------------------------
;; player manual
(define (basic-survival-manual-commands)
  (kern-ui-page-text
   "Command Summary"
   "Use the arrow keys to indicate direction."
   "Use the ESC key to cancel commands."
   "Use the first letter to start a command."
   "For detailed info see the Users Guide."
   ""
   "A)ttack something"
   "B)oard a ship or other vehicle"
   "C)ast a spell"
   "E)nter a town or dungeon"
   "F)ire a ship's cannon or other ordnance"
   "G)et something on the ground"
   "H)andle a lever or other mechanism"
   "N)ew-Order (rearrange party order)"
   "O)pen a chest, door or other closed object"
   "Q)uit and save the game"
   "R)eady arms for a party member"
   "S)earch for hidden stuff"
   "T)alk to somebody"
   "U)se an item in inventory"
   "Z)tats (show party status)"
   "X)amine around"
   "@)AT (info about place & time)"
   "<space> (pass a turn)"
   "CTRL-Q)uit saves without quitting"
   "CTRL-R)eload the last saved game"
   ))

(mk-reusable-item 't_manual "Basic Survival Manual" s_manual 1 
                  basic-survival-manual-commands)

;;----------------------------------------------------------------------------
;; letter from enchanter
(mk-reusable-item 
 't_letter_from_enchanter "Important" s_lexicon 1
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "IMPORTANT"
   ""
   "It is most important that you FIND ME. "
   "The goodman who cares for this shrine can "
   "point the way. Beware."
   ""
   "--Enchanter\n"
   )))
