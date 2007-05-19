;; ----------------------------------------------------------------------------
;; books.scm -- reading material
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_books 32 32 2 2 0 0 "books.png")

(kern-mk-sprite 's_lexicon ss_books 1 0 #f 0)
(kern-mk-sprite 's_manual  ss_books 1 1 #f 0)
(kern-mk-sprite 's_scroll  ss_books 1 2 #f 0)

;; Fixme: see if something like this will work for books:
(define (mk-book tag name . text)
  (mk-reusable-item 
   tag name s_manual norm
   (lambda ()
     (kern-ui-page-text text))))

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
   )
  result-ok)

(mk-reusable-item 't_manual "Basic Survival Manual" s_manual (/ norm 3)
                  basic-survival-manual-commands)

;;----------------------------------------------------------------------------
;; letter from enchanter
(mk-reusable-item 
 't_letter_from_enchanter "Important Letter" s_lexicon norm
 (lambda (klexicon kuser)
   (kern-ui-page-text
   "IMPORTANT"
   ""
   "It is most ^c+rcritical^c- that you FIND ME. "
   "The peasant who cares for this shrine can "
   "point the way. Beware."
   ""
   "--Enchanter\n"
   )
   result-ok))

;;----------------------------------------------------------------------------
;; book of the demon gate
(mk-reusable-item 
 't_demon_gate_book "Ragged Text" s_scroll vhard
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
   )
   result-ok))

;;----------------------------------------------------------------------------
;; Kathryn's Letter
(mk-reusable-item 
 't_kathryns_letter "Letter" s_scroll norm
 (lambda (kletter kuser)
   (kern-ui-page-text
   "Letter"
   "K,"
   "The Enchanter has one of the Runes. Acquire "
   "it by any means necessary, and leave no one "
   "to tell the tale. Not even a ghost."
   "--S")
   result-ok))

;;----------------------------------------------------------------------------
;; Spell books
(mk-reusable-item 
 't_ylem_an_ex_book "spell book: YAE" s_manual hard
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
   ) 
   result-ok))

(mk-reusable-item 
 't_bet_flam_hur_book "spell book: BFH" s_manual hard
 (lambda (kletter kuser)
   (kern-ui-page-text
   "Fire Spray- Bet Flam Hur"
   ""
   "While the effectiveness of the Flame Wind spell is renowned, it is difficult to cast, exhausting, and well beyond the means of all but the greatest mages."
   ""
   "The Fire Spray spell is an adaption of the Flame Wind spell to more modest requirements. While it cannot be cast by an absolute beginner, it requires minimal ability. The effectiveness of the spell increases, of course, with the caster's ability."
   ""
   "The Fire Spray spell has a limited range- not much greater than a polearm- and does not have sufficient power to leave the blazing fields associated with the Flame Wind spell."
   ""
   "The spell requires sulphurous ash to power the flame. The initial launch of the spell is provided by black pearl, and blood moss instills the motion of the spreading cone of fire."
   ""
   "With this mixture, intone Bet Flam Hur (Small Fire Wind) and direct the cone to engulf your foes."
   )
   result-ok))

(mk-reusable-item
 't_ranger_orders "Ranger orders" s_scroll norm
 (lambda (kletter kuser)
   (kern-ui-page-text
    "Ranger Orders"
    ""
    "The bearer of this letter may conscript "
    "one (1) ranger into limited temporary "
    "service.")
   result-ok))

(mk-reusable-item
 't_prisoner_receipt "Prisoner Receipt" s_scroll norm
 (lambda (kletter kuser)
   (kern-ui-page-text
    "Prisoner Receipt"
    ""
    "The bearer of this letter has delivered "
    "one (1) prisoner to the gaoler.")
   result-ok))

(mk-reusable-item
 't_basic_spell_book "Spell Book" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Spell Book"
    ""
    "+-----------------------------------------+"
    "|   M)ix thy reagents to craft a spell,   |"
    "|   then C)ast it in thine hour of need.  |"
    "|   This tome will always serve thee well |"
    "|   but oh so carefully thou must read!   |"
    "+-----------------------------------------+"
    "                                           "
    "................ Reagents ................."
    "GI Ginseng                   SS Spider Silk"
    "GA Garlic                    BP Black Pearl"
    "SA Sulphurous Ash            BM Blood Moss "
    "NI Nightshade                MA Mandrake   "
    "................. Spells .................."
    "-------------- First Circle ---------------"
    "An Nox           Cure Poison    GI, GA     "
    "An Zu            Awaken         GI, GA     "
    "Grav Por         Magic Missile  SA, BP     "
    "In Lor           Light          SA         "
    "Mani             Heal           GI, SS     "
    "Wis Sanct        Detect Trap    SA         "
    "An Sanct Ylem    Disarm Trap    BM         "
    "-------------- Second Circle --------------"
    "An Sanct         Unlock         SA, BM     "
    "An Xen Corp      Repel Undead   GA, SA     "
    "In Wis           Locate         NI         "
    "Kal Xen          Summon Snakes  SS, MA     "
    "Rel Hur          Change Wind    SA, BM     "
    "In Nox Por       Poison Missile NI, BM, BP "
    "-------------- Third Circle ---------------"
    "In Flam Grav     Fire Field     SA, BP, SS "
    "In Zu Grav       Sleep Field    GI, BP, SS "
    "In Nox Grav      Poison Field   NI, BP, SS "
    "Vas Flam         Fire Ball      BP, SA     "
    "Vas Lor          Great Light    SA, MA     "
    "-------------- Fourth Circle --------------"
    "An Grav          Dispel Field   BP, SA     "
    "In Sanct Grav    Energy Field   MA, BP, SS "
    "In Sanct         Protection     SA, GI, GA "
    "Wis Quas         Reveal Hidden  NI, SA     "
    "Bet Por          Blink (Caster) BP, BM     "
    "-------------- Fifth Circle ---------------"
    "In Ex Por        Unlock Magic   SA, BM     "
    "An Ex Por        Magic Lock     SA, BM, GA "
    "In Bet Xen       Insect Swarm   SS, BM, SA "
    "In Zu            Sleep          GI, NI, SS "
    "Vas Mani         Great Heal     GI, SS, MA "
    "Rel Tym          Quickness      SA, BM, MA "
    "-------------- Sixth Circle ---------------"
    "An Xen Ex        Charm          BP, NI, SS "
    "In An            Negate Magic   GA, MA, SA "
    "In Vas Por Ylem  Earthquake     BM, MA, SA "
    "Quas An Wis      Confuse        MA, NI     "
    "Wis An Ylen      Xray Vision    MA, SA     "
    "In Rel Por       Telekinesis    BP, BM, SS "
    "Vas Por          Blink (Party)  MA, BP, BM "
    "------------- Seventh Circle --------------"
    "In Nox Hur       Poison Wind    NI, SA, BM "
    "In Quas Corp     Fear           NI, MA, GA "
    "In Quas Wis      Peer           NI, MA     "
    "In Quas Xen      Clone          NI, MA, SA,"
    "...              ...            SS, BM, GI "
    "Sanct Lor        Invisibility   NI, MA, BM "
    "Xen Corp         Kill           NI, BP     "
    "-------------- Eighth Circle --------------"
    "An Tym           Stop Time      MA, GA, BM "
    "In Flam Hur      Flame Wind     MA, SA, BM "
    "In Vas Grav Corp Energy Wind    MA, SA, NI "
    "In Mani Corp     Resurrect      GA, GI, SS,"
    "...              ...            SA, BM, MA "
    "Kal Xen Corp     Summon Undead  SS, MA, NI "
    "Vas Rel Por      Gate Travel    SA, MA, BP "
    )
   result-ok))
   
;;----------------------------------------------------------------------------
;; Anaxes letters
(mk-reusable-item
 't_anaxes_letter "Letter to Anaxes" s_scroll vhard
 (lambda (kletter kuser)
   (kern-ui-page-text
    "Letter"
    ""
    "Anaxes,                                      "
    "                                             "
    "My dearest love. The Twelve have cast you out" 
    "(Luximene has them thoroughly cowed). He hath"
    "issued an edict that the old religion is     "
    "illegal, and all who show piety to any god   "
    "but he are Accursed.                         "
    "                                             "
    "As I write, the legions of Glasdrin and      "
    "Tulemane march on Brundegardt. I go to       "
    "destroy them before they reach you. Seal the "
    "entrance lest my sacrifice be in vain.       "
    "                                             "
    "Know, my love, that I never betrayed you. May"
    "our spirits meet in the void, and Vale exact "
    "his price from our enemies.                  "
    "                                             "
    "Love,                                        "
    "  Isin                                       "
    "                                             "
    "P.S. The Titans are safe. I have seen that   "
    "those who knew of them were slain.           "
    )
   result-ok))
