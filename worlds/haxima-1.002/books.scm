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
   "L)oiter a few hours"
   "N)ew-Order (rearrange party order)"
   "O)pen a chest, door or other closed object"
   "Q)uit and optionally save the game"
   "R)eady arms for a party member"
   "S)earch for hidden stuff"
   "T)alk to somebody"
   "U)se an item in inventory"
   "Z)tats (show party status)"
   "X)amine around"
   "@)AT (info about place & time)"
   "<space> (pass a turn)"
   "CTRL-S)ave the game"
   "CTRL-R)eload a game"
   "SHIFT+ARROWKEY pan viewer"
   ))

(mk-reusable-item 't_manual "Basic Survival Manual" s_manual 1 
                  basic-survival-manual-commands)

;;----------------------------------------------------------------------------
;; letter from enchanter
(mk-reusable-item 
 't_letter_from_enchanter "Important Letter" s_lexicon 1
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "IMPORTANT"
   ""
   "It is most ^c+rcritical^c- that you FIND ME. "
   "The peasant who cares for this shrine can "
   "point the way. Beware."
   ""
   "--Enchanter\n"
   )))

;;----------------------------------------------------------------------------
;; book of the demon gate
(mk-reusable-item 
 't_demon_gate_book "Ragged Text" s_scroll 1
 (lambda (kbook kuser)
   (kern-ui-page-text
   "THE DEMON GATE"
   ""
   "...the Wise of old locked the Demon Gate and scattered the keys... "
   "the Shrine of the Demon Gate was not to be revealed again until the world is made ANEW... "
   "they concealed the road to the Shrine with an illusion... "
   "where there is a path, the unwise see only a mountain... "
   ""
   "... the River of Stars, and follow it to its source... "
   ""
   "...to he who opens the Gate, power unimaginable..."
   ""
   "...Nossifer awaits."
   ""
   "--Fildex the Unclean\n"
   )))

;;----------------------------------------------------------------------------
;; Kathryn's Letter
(mk-reusable-item 
 't_kathryns_letter "Letter" s_scroll 1
 (lambda (kletter kuser)
   (kern-ui-page-text
   "Letter"
   "K,"
   "The Enchanter has one of the Runes. Acquire "
   "it by any means necessary, and leave no one "
   "to tell the tale. Not even a ghost."
   "--S")))

;;----------------------------------------------------------------------------
;; Spell books
(mk-reusable-item 
 't_ylem_an_ex_book "spell book" s_manual 1
 (lambda (kletter kuser)
   (kern-ui-page-text
   "Web Spell- Ylem An Ex"
   ""
   "A useful spell for beginning spellcasters, "
   "web allows a distant opponent to be "
   "entangled and made helpless for a short "
   "period of time."
   ""
   "If used correctly, it can also block an "
   "area with webs temporarily, although this is "
   "hard for novices to achieve."
   ""
   "The spell requires spider silk, of course, "
   "and also black pearl, to project the web at "
   "the desired target."
   ""
   "With the appropriate mixture, intone "
   "Ylem An Ex (Matter to Negate Freedom) and "
   "hurl the net of web at your foe."
   )))

(mk-reusable-item
 't_ranger_orders "Ranger orders" s_scroll 0
 (lambda (kletter kuser)
   (kern-ui-page-text
    "Ranger Orders"
    ""
    "The bearer of this letter may conscript "
    "one (1) ranger into limited temporary "
    "service.")))

(mk-reusable-item
 't_prisoner_receipt "Prisoner Receipt" s_scroll 0
 (lambda (kletter kuser)
   (kern-ui-page-text
    "Prisoner Receipt"
    ""
    "The bearer of this letter has deliverd "
    "one (1) prisoner to the gaoler.")))