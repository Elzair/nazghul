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
    "To the Wanderer -- IMPORTANT"
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
 't_demon_gate_book "Ragged Text" s_scroll v-hard
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


;;;;;;;;;;;;;;;;;;;; White Magick ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_white_magick_1 "White Magick: Booke I" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "White Magick: Booke I"
    "Spells of Healing, of the First Circle"
    "+-----------------------------------------+"
    "|  M)ix thy reagents to craft a spell,    |"
    "|  then C)ast it in thine hour of need.   |"
    "|  Through lore of WHITE shall ye HEAL,   |"
    "|  though as ye MEND be ye yet WISE!      |"
    "+-----------------------------------------+"
    "Cure Poison <AN NOX>"
    "- Mix Ginseng and Garlic"
    ""
    "Awaken <AN ZU>"
    "- Mix Ginseng and Garlic"
    ""
    "Minor Healing <MANI>"
    "- Mix Ginseng and Spider Silk"
    )
   result-ok))

(mk-reusable-item
 't_spell_book_white_magick_2 "White Magick: Booke II"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "White Magick: Booke II"
    "Spells of Healing and Protection"
    "+-----------------------------------------+"
    "|  The Lore ye know to Mix and Cast.      |"
    "|  Well-served by these chants be.        |"
    "|  Yet forget not, oh Wise, of Duty,      |"
    "|  to Heal all which come to thee.        |"
    "+-----------------------------------------+"
    "Poison Ward <SANCT NOX>"
    "- Mix Garlic and Nightshade"
    ""
    "Fire Ward <IN FLAM SANCT>"
    "- Mix Sulphurous Ash, Garlic, "
    "- and forget ye not: [SMUDGED WORD]"
    ""
    "Protection <IN SANCT>"
    "- Mix Sulphurous Ash, Garlic, Ginseng"
    ""
    "Mass Cure Poison <VAS AN NOX>"
    "- Mix Garlic, Ginseng, Mandrake"
    ""
    "Great Heal <VAS MANI>"
    "- Mix Ginseng, Spider Silk, Mandrake"
    )
   result-ok))

(mk-reusable-item
 't_spell_book_white_magick_3 "White Magick: Booke III"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "White Magick: Booke III"
    "Spells of Healing, of High Magick"
    "+-----------------------------------------+"
    "|  Mix ye now, and Crowned Master, Cast!  |"
    "|   But beckon, Power to thee hastens!    |"
    "|    If ye be Wise, then act ye thus:     |"
    "|  KAL WIS - IN MANI - AN CORP - ZU QUAS  |"
    "+-----------------------------------------+"
    "Mass Cure Poison <VAS AN NOX>"
    "- Mix Ginseng, Garlic, Mandrake"
    ""
    "Mass Poison Ward <VAS SANCT NOX>"
    "- Mix Garlic, Nightshade, Mandrake"
    ""
    "Great Heal <VAS MANI>"
    "- Mix Ginseng, Spider Silk, Mandrake"
    ""
    ""
    "--------------- HIGH MAGICK --------------"
    ""
    "Resurrection <IN MANI CORP>"
    "- Mix Ginseng, Garlic, Spider Silk"
    "-     Sulphurous Ash, Blood Moss, Mandrake"
    ""
    "  -- Slay not that which should live! -- "
    "  -- Raise not that which ought rest! -- "

    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Force Magick ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_force_magick_12 "Magick of Force: Booke I" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Booke I"
    "Spells of Might, of the Circles I, II      "
    "+-----------------------------------------+"
    "|  M)ix thy reagents to craft a spell,    |"
    "|  then C)ast it in thine hour of need.   |"
    "|  By FORCE shall thy foes know FEAR,     |"
    "|  yet FOOLS thus casting shall know WOE! |"
    "+-----------------------------------------+"
    "Spells of the Circle I:"
    "-----------------------"
    "Magic Missile <GRAV POR>"
    "- Mix Sulphurous Ash and Black Pearl"
    ""
    "Light <IN LOR>"
    "- Mix Sulphurous Ash"
    ""
    "------------------------"
    "Spells of the Circle II:"
    "------------------------"
    "Change Wind <REL HUR>"
    "- Mix Sulphurous Ash and Blood Moss"
    ""
    "Fire Spray <BET FLAM HUR>"
    "- Mix Sulphurous Ash, Blood Moss, "
    "-     and Black Pearl"
    )
   result-ok))

(mk-reusable-item
 't_spell_book_force_magick_battle "Magick of Force: Of Battle"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Of Battle"
    "+-----------------------------------------+"
    "|    Battle near: Chant runes and Mix     |"
    "|      Battle on: Cast and hurl           |"
    "|  Thy foes know pain, and death indeed   |"
    "|  Yet wield ye but Might, or be ye Wise? |"
    "+-----------------------------------------+"
    "Spells of the Circle I:"
    "-----------------------"
    "Magic Missile <GRAV POR>"
    "- Mix Sulphurous Ash and Black Pearl"
    ""
    "Lightning Bolt <ORT GRAV>"
    "- Mix Sulphurous Ash, Black Pearl, Mandrake"
    ""
    "-------------------------"
    "Spells of the Circle III:"
    "-------------------------"
    "Fire Ball <VAS FLAM>"
    "- Mix Sulphurous Ash, Black Pearl"
    ""
    "Fire Ward <IN FLAM SANCT>"
    "- Mix Sulphurous Ash, [CHARRED WORD], "
    "- and forget ye not: Royal [ASHY SMUDGE]"
    ""
    "------------------------"
    "Spells of the Circle VI:"
    "------------------------"
    "Tremor <IN VAS POR YLEM>"
    "- Mix Sulphurous Ash, Blood Moss, Mandrake"
    )
   result-ok))


(mk-reusable-item
't_spell_book_force_magick_winds "Magick of Force: Deadly Winds"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Deadly Winds"
    "+-----------------------------------------+"
    "|  For Wind ye shall Mix, for Wind Cast!  |"
    "|  Thy foe, sleeping, shall know naught,  |"
    "|  Thy foe, struck, shall burn or fester, |"
    "|       Yet, can ye Change the Wind?      |"
    "|  For what Wind changes not, blows not.  |"
    "|      Shall ye then have Changed it?     |"
    "+-----------------------------------------+"
    "Spells of the Circle II:"
    "------------------------"
    "Change Wind <REL HUR>"
    "- Mix Sulphurous Ash, Blood Moss"
    ""
    "Fire Spray <BET FLAM HUR>"
    "- Mix Sulphurous Ash, Blood Moss, "
    "-     and Black Pearl"
    ""
    "-------------------------"
    "Spells of the Circle VII:"
    "-------------------------"
    "Wind of Sleep <IN ZU HUR>"
    "- Mix Ginseng, Blood Moss, Mandrake"
    ""
    "Poison Wind <IN NOX HUR>"
    "- Mix Sulphurous Ash, Blood Moss, "
    "-     and Nightshade"
    ""
    "--------------------------"
    "Spells of the Circle VIII:"
    "--------------------------"
    "Flame Wind <IN FLAM HUR>"
    "- Mix Sulphurous Ash, Blood Moss, "
    "-     and Mandrake"
    )
   result-ok))
(mk-reusable-item
't_spell_book_force_magick_matter "Magick of Force: Upon Matter"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Upon Matter"
    "+-----------------------------------------+"
    "|  Mix and Cast, by Force control Matter. |"
    "|  Powers subtle and grand both wield.    |"
    "|  If ye be wise then riddle this rede:   |"
    "|    What secret, steel? (Flesh is soft)  |"
    "|    What sound, one hand? (Butterfly)    |"
    "+-----------------------------------------+"
    "Spells of the Circles I and II:"
    "-------------------------------"
    "Disarm Trap <AN SANCT YLEM>"
    "- Mix Blood Moss"
    ""
    "Unlock <AN SANCT>"
    "- Mix Sulphurous Ash, Blood Modd"
    ""
    "Lock <SANCT>"
    "- Mix Sulphurous Ash, Spider Silk"
    ""
    "Change Wind <REL HUR>"
    "- Mix Sulphurous Ash, Blood Moss"
    ""
    "------------------------"
    "Spells of the Circle VI:"
    "------------------------"
    "Tremor <IN VAS POR YLEM>"
    "- Mix Sulphurous Ash, Blood Moss, Mandrake"
    ""
    "Telekinesis <IN REL POR>"
    "- Mix Spider Silk, Blood Moss, Black Pearl"
    ""
    "----------------"
    "[Scrawled Notes]"
    "----------------"
    "Raise Matter Above Water <VAS UUS YLEM>"
    "- Trial 1: Mix Ash, Pearl, Mandrake ???"
    "- Trial 2: Mix [CHARRED], Eye of Newt ?"
    "- Gazer eyes? Dragon spleen? Ogre spittle?"
    )
   result-ok))

(mk-reusable-item
't_spell_book_force_magick_mechanismus "Magick of Force: Mechanismus"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Mechanismus"
    "+-----------------------------------------+"
    "|  Knowing, Mix, and with craft, Cast!    |"
    "|  Fear ye luggage? Chest dangers, past!  |"
    "|  Keys bereft? Claviger be thou despite! |"
    "|    Glowing portal bar not this wight!   |"
    "|      Keep in tempo, keep in vogue       |"
    "|        Keep in Time (or its Lord)       |"
    "|      With such runes, let no wrogue     |"
    "|     snatch the closing [MISSING WORD]   |"
    "+-----------------------------------------+"
    "Spells of the Circles I and II:"
    "-------------------------------"
    "Disarm Trap <An Sanct Ylem>"
    "- Mix Blood Moss"
    ""
    "Unlock <An Sanct>"
    "- Mix Sulphurous Ash, Blood Moss"
    ""
    "Lock <Sanct>"
    "- Mix Sulphurous Ash, Spider Silk"
    ""
    "-----------------------"
    "Spells of the Circle V:"
    "-----------------------"
    "Magic Unlock <In Ex Por>"
    "- Mix Sulphurous Ash, Blood Moss"
    ""
    "Magic Lock <An Ex Por>"
    "- Mix Sulphurous Ash, Garlic, Blood Moss"
    )
   result-ok))

(mk-reusable-item
't_spell_book_force_magick_fields "Magick of Force: Upon Fields"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: Upon Fields"
    "+-----------------------------------------+"
    "|  Mix and Cast, by powers form Fields.   |"
    "|  Conjure Sleep, Poison, Fire, or Force, |"
    "|  or should such bar ye, then Dispel.    |"
    "|  IN GRAV - AN POR - AN GRAV - EX POR    |"
    "+-----------------------------------------+"
    "Spells of the Circle III:"
    "-------------------------"
    "Sleep Field <In Zu Grav>"
    "- Mix Ginseng, Spider Silk, Black Pearl"
    ""
    "Poison Field <In Nox Grav>"
    "- Mix Spider Silk, Black Pearl, Nightshade"
    ""
    "Fire Field <In Flam Grav>"
    "- Mix Sulphurous Ash, Spider Silk, "
    "-     Black Pearl"
    ""
    "------------------------"
    "Spells of the Circle IV:"
    "------------------------"
    "Force Field <In Sanct Grav>"
    "- Mix Spider Silk, Black Pearl, Mandrake"
    ""
    "Dispel Field <An Grav>"
    "- Mix Sulphurous Ash, Black Pearl"
    )
   result-ok))

(mk-reusable-item
't_spell_book_force_magick_high_magick "Magick of Force: High Magick"  s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Magick of Force: High Magick"
    "+-----------------------------------------+"
    "|  BE WARE, be ye not Mighty in deed!     |"
    "|    BEWARE, be ye not Wise indeed!       |"
    "|  Written here are runes of HIGH MAGICK  |"
    "|  If ye be a FOOL, ye shall know DEATH   |"
    "|  Yet if you would, here is graven POWER |"
    "+-----------------------------------------+"
    "Circle VI - Negate Magic <In An>"
    "- Mix Sulphurous Ash, Garlic, Mandrake"
    ""
    "Circle VII - Death Bolt <Xen Corp>"
    "- Mix Black Pearl, Mandrake"
    ""
    "Circle VIII - Death Wind <In Vas Grav Corp>"
    "- Mix Sulphurous Ash, Nighthsade, Mandrake"
    ""
    "Circle VIII - Time Stop <An Tym>"
    "- Mix Garlic, Blood Moss, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Necromancy ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_necromancy "Mysteries of Death and Undeath" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Mysteries of DEATH and UNDEATH             "
    "+-----------------------------------------+"
    "| Sanctus Corporem ex Nihilo, Rel Oculume |"
    "| De Vermiis Mysteriis, Astralis Sangrem  |"
    "+-----------------------------------------+"
    "II - Turn Undead <An Xen Corp>"
    "- Mix Sulphurous Ash, Garlic"
    ""
    "VIII - Summon Undead <Kal Xen Corp>"
    "- Mix Spider Silk, Nightshade, Mandrake"
    ""
    "II - Poison Bolt <In Nox Por>"
    "- Mix Blood Moss, Black Pearl, Nightshade"
    ""
    "VII - Poison Wind <In Nox Hur>"
    "- Mix Sulphrous Ash, Blood Moss, Nightshade"
    ""
    "VII - Death Bolt <Xen Corp>"
    "- Mix Black Pearl, Nightshade"
    ""
    "VII - Clone <In Quas Xen>"
    "- Mix Sulphurous Ash, Ginseng, Spider Silk,"
    "-     Blood Moss, Nightshade, Mandrake"
    ""
    "VIII - Resurrection <In Mani Corp>"
    "- Mix Sulphurous Ash, Ginseng, Garlic, "
    "-     Spider Silk, Blood Moss, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Master Spellbook ;;;;;;;;;;;;;;;;;;;;

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
    "Rel Hur          Change Wind    SA, BM     "
    "In Nox Por       Poison Missile NI, BM, BP "
    "In Bet Xen       Summon Vermin  SS, BM, SA "
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
    "In Zu            Sleep          GI, NI, SS "
    "Vas Mani         Great Heal     GI, SS, MA "
    "Rel Tym          Quickness      SA, BM, MA "
    "Kal Xen          Summon Beast   SS, MA     "
    "-------------- Sixth Circle ---------------"
    "An Xen Ex        Charm          BP, NI, SS "
    "In An            Negate Magic   GA, MA, SA "
    "In Vas Por Ylem  Earthquake     BM, MA, SA "
    "Quas An Wis      Confuse        MA, NI     "
    "Wis An Ylen      Xray Vision    MA, SA     "
    "In Rel Por       Telekinesis    BP, BM, SS "
    "Vas Por          Teleport Party MA, BP, BM "
    "------------- Seventh Circle --------------"
    "In Nox Hur       Poison Wind    NI, SA, BM "
    "In Quas Corp     Fear           NI, MA, GA "
    "In Quas Wis      Peer           NI, MA     "
    "In Quas Xen      Clone          NI, MA, SA,"
    "...              ...            SS, BM, GI "
    "Sanct Lor        Invisibility   NI, MA, BM "
    "Xen Corp         Kill           NI, BP     "
    "-------------- Eighth Circle --------------"
    "An Tym           Time Stop      MA, GA, BM "
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
 't_anaxes_letter "Letter to Anaxes" s_scroll v-hard
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
