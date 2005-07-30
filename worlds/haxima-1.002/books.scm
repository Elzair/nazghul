;; ----------------------------------------------------------------------------
;; books.scm -- reading material
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_books 32 32 1 2 0 0 "books.png")

(kern-mk-sprite 's_lexicon ss_books 1 0 #f 0)
(kern-mk-sprite 's_manual  ss_books 1 0 #f 0)

;; ----------------------------------------------------------------------------
;; Gen's Goblin Lexicon
;; ----------------------------------------------------------------------------
(mk-reusable-item 
 't_goblin_lexicon "A Goblin Lexicon" s_lexicon 1
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "Goblin Lexicon"
   "I compiled these notes to help others learn the goblin language. "
   "I hope they are useful."
   "--Gen"
   ""
   "Bo.....My, Myself"
   "Cho....Mankind"
   "Da.....Abode, World"
   "Eh.....'What?'"
   "Gu.....Spirit, Ancestor"
   "Ha.....Good, Yes, Skillful"
   "Hi.....Magic"
   "Ka.....Kill, Destroy, End"
   "Ki.....Health, Life-Force, Power"
   "Ma.....Forest, Hidden Ways"
   "Me.....Duty, Job, Destiny"
   "Na.....Yours, Yourself"
   "Nu.....Give Birth, Create, Begin"
   "No.....Name"
   "Ru.....Ancient, Primordal, Deep, Cave"
   "To.....Individual"
   "Tu.....Bad, No, Useless"
   "Zu.....Watch, Seek"
   )))

;;----------------------------------------------------------------------------
;; Player manual
;;----------------------------------------------------------------------------
(define (basic-survival-manual-intro)
  (kern-ui-page-text
   "Basic Survival Manual"
   "by Gorn the Wayward"
   ""
   "Dear Pilgrim,"
   ""
   "Welcome to the Shard! Not the brightest spot "
   "on our Journey, but if you're here then it is"
   "The Way that brought you here, and it did so "
   "for a reason. You'll have to figure out what "
   "that reason is if you want to continue the "
   "Journey."
   ""
   "I recommend you seek the Hermit. Usually "
   "there's a local yokel standing around who "
   "knows where to find him (or her!). It may "
   "have been ten years or a hundred since the "
   "last Pilgrim stood where you are now (with "
   "that same stunned expression on their "
   "face!), but there's always a Hermit."
   ""
   "Meanwhile, I've jotted down some quick notes "
   "about how to survive in the Shard. They'll "
   "get you started. Just remember: you don't "
   "know who is friend or foe yet, and that can "
   "always change anyway, so don't be too quick "
   "with the sword or too careless with your "
   "mouth. "
   ""
   "Good luck!"
   ""
   "And don't forget: SEEK THE HERMIT!"
   ))

(define (basic-survival-manual-commands)
  (kern-ui-page-text
   "Command Summary"
   "Use the arrow keys to indicate direction."
   "Use the ESC key to cancel commands."
   "Use the first letter to start a command."
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

(define (basic-survival-manual-use manual user)
  (basic-survival-manual-intro))

(define (basic-survival-manual-contents manual user)
  (let ((selection (kern-ui-select-from-list "Intro" "Commands")))
    (display "selection:")(display selection)(newline)
    (if (null? selection)
        '()
        (begin
          (if (string=? selection "Intro")
              (basic-survival-manual-intro)
              (if (string=? selection "Commands")
                  (basic-survival-manual-commands manual user)))
          (basic-survival-manual-contents manual user)))))

(mk-reusable-item 't_manual "Basic Survival Manual" s_manual 1 
                  basic-survival-manual-contents)
 
