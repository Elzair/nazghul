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
   (quest-data-assign-once 'questentry-calltoarms)
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
;; The soliloquy from Hamlet:
(mk-reusable-item 
 't_playbook_hamlet "Playbook" s_scroll norm
 (lambda (kletter kuser)
   (kern-ui-page-text
   "A partial playbook"

   "To be, or not to be: that is the question:"
   "Whether 'tis nobler in the mind to suffer"
   "The slings and arrows of outrageous fortune,"
   "Or to take arms against a sea of troubles,"
   "And by opposing end them? To die: to sleep;"
   "No more; and by a sleep to say we end"
   "The heart-ache and the thousand natural shocks"
   "That flesh is heir to, 'tis a consummation"
   "Devoutly to be wish'd. To die, to sleep;"
   "To sleep: perchance to dream: ay, there's the rub;"
   ""
   "For in that sleep of death what dreams may come"
   "When we have shuffled off this mortal coil,"
   "Must give us pause: there's the respect"
   "That makes calamity of so long life;"
   ""
   "For who would bear the whips and scorns of time,"
   "The oppressor's wrong, the proud man's contumely,"
   "The pangs of despised love, the law's delay,"
   "The insolence of office and the spurns"
   "That patient merit of the unworthy takes,"
   "When he himself might his quietus make"
   "With a bare bodkin? who would fardels bear,"
   "To grunt and sweat under a weary life,"
   "But that the dread of something after death,"
   "The undiscover'd country from whose bourn"
   "No traveller returns, puzzles the will"
   "And makes us rather bear those ills we have"
   "Than fly to others that we know not of?"
   ""
   "Thus conscience does make cowards of us all;"
   "And thus the native hue of resolution"
   "Is sicklied o'er with the pale cast of thought,"
   "And enterprises of great pith and moment"
   "With this regard their currents turn awry,"
   "And lose the name of action.--Soft you now!"
   "The fair Ophelia! Nymph, in thy orisons"
   "Be all my sins remember'd."
   )))

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
    "- Mix Garlic, Nightshade and Royal Cape"
    ""
    "Fire Ward <IN FLAM SANCT>"
    "- Mix Sulphurous Ash, Garlic, "
    "- and forget ye not: [SMUDGED WORD]"
    ""
    "Dispel Magic <An Ort Xen>"
    "- Mix Sulphurous Ash, Garlic, Mandrake"
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
    "- Mix Garlic, Nightshade, Mandrake and Royal Cape"
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
    ""
    "Poison Bolt <IN NOX POR>"
    "- Mix Nightshade, Blood Moss, "
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
    "Spells of the Circle I and II:"
    "------------------------------"
    "Conjure Smoke <Bet Ylem Hur>"
    "- Mix Sulphurous Ash"
    ""
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


;;;;;;;;;;;;;;;;;;;; Summoning ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_summoning "Summoning: Beasts and Entities" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Summoning: Beasts and Entities"
    "+-----------------------------------------+"
    "| KAL     To the Summoner - BEWARE    KAL |"
    "| CORP         Lest ye Call Up,       XEN |"
    "| BET  That which ye cannot Send Back ORT |"
    "| SANCT  [ABRAD] - [ACAD] - [ABRA]   YLEM |"
    "+-----------------------------------------+"
    "II - Summon Vermin <In Bet Xen>"
    "- Mix Sulphurous Ash, Blood Moss, "
    "-     Spider Silk"
    ""
    "V - Summon Beast <Kal Xen>"
    "- Mix Spider Silk, Mandrake"
    ""
    "VIII - Summon Slime <Kal Xen Nox>"
    "- Mix Spider Silk, Nightshade, Mandrake"
    ""
    "VIII - Summon Undead <Kal Xen Corp>"
    "- Mix Spider Silk, Nightshade, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Dimensions and Gate Magick ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_gate_magick "Upon Dimensions: Gates" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Upon Dimensions: Gates"
    "+-----------------------------------------+"
    "| In Travel, is Freedom indeed, oh Seeker |"
    "|       IN POR - IN WIS - EX POR WIS      |"
    "|                                         |"
    "| The Lore of Moons - Controls the Gates! |"
    "|      SUN - FYER  Radiant 24  (12,12)    |"
    "|     MOON - LUMIS Yellow  60, (5)x8      |"
    "|     MOON - ORD   Blue    36, (9)x8      |"
    "|                                         |"
    "| If ye be Wise        Watch Wax and Wane |"
    "+-----------------------------------------+"
    "Locate <In Wis>"
    "- Mix Nightshade"
    ""
    "Vision <In Quas Wis>"
    "- Mix Nightshade, Mandrake"
    ""
    "Blink <Bet Por>"
    "- Mix Blood Moss, Black Pearl"
    ""
    "Ascend <Uus Por>"
    "- Mix it not, cast it not! Space tears!"
    ""
    "Descend <Des Por>"
    "- Beware the warp! Seek ye not this lore!"
    ""
    "Quickness <Rel Tym>"
    "- Mix Sulphurous Ash, Blood Moss, Mandrake"
    ""
    "Teleport Party <Vas Por>"
    "- Mix Blood Moss, Black Pearl, Mandrake"
    ""
    "Gate <Vas Rel Por>"
    "- Mix Sulphurous Ash, Black Pearl, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Enchantment ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_enchantment_wards "Enchantment: Wards" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Enchantment: Wards"
    "+-----------------------------------------+"
    "| SANCT -     Protect Thyself!    - SANCT |"
    "|  Mix Wards!  Cast spells of Protection! |"
    "|  Surround thyself with Fields of Force  |"
    "| Seeker   - Ye are in Danger! -  Beware! |"
    "+-----------------------------------------+"
    "Poison Ward <Sanct Nox>"
    "- Mix Garlic, Nightshade, Royal Cape"
    ""
    "Fire Ward <In Flam Sanct>"
    "- Mix [BLURRED], [SMEARED],"
    "      and the Cap of the Coastal Prince"
    ""
    "Mass Poison Ward <Vas Sanct Nox>"
    "- Mix Garlic, Nightshade, Mandrake, Royal Cape"
    ""
    "Protection <In Sanct>"
    "- Mix Sulphurous Ash, Ginseng, Garlic"
    ""
    "Dispel Magic <An Ort Xen>"
    "- Mix Sulphurous Ash, Garlic, Mandrake"
    ""
    "Negate Magic <In An>"
    "- Mix Sulphurous Ash, Garlic, Mandrake"
    ""
    "Force Field <In Sanct Grav>"
    "- Mix Spider Silk, Black Pearl, Mandrake"
    )
   result-ok))

(mk-reusable-item
 't_spell_book_enchantment_curses "Enchantment: Curses" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Enchantment: Curses"
    "+-----------------------------------------+"
    "|             To the Reader               |"
    "|    Be ye ACCURSED, I strike ye BLIND!   |"
    "|    ----------------------------------   |"
    "|        Gentle Reader - Be ye Wise       |"
    "|     Mix and Cast these Runes at Need    |"
    "+-----------------------------------------+"
    "Web <Ylem An Ex>"
    "- Mix Spider Silk, Black Pearl"
    ""
    "Calm Spiders <An Xen Bet>"
    "- Mix Garlic, Spider Silk"
    ""
    "Sleep <Xen Zu>"
    "- Mix Ginseng, Spider Silk"
    ""
    "Charm Monster <An Xen Ex>"
    "- Mix Spider Silk, Black Pearl, Nightshade"
    ""
    "Confusion <Quas An Wis>"
    "- Mix Nightshade, Mandrake"
    ""
    "Fear <In Quas Corp>"
    "- Mix Garlic, Nightshade, Mandrake"
    )
   result-ok))

(mk-reusable-item
 't_spell_book_enchantment_miscellanea "Enchantment: Miscellanea" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Enchantment: Miscellanea"
    "+-----------------------------------------+"
    "| [SMUDGED TEXT] the Secrets!   [BLURRED] |"
    "| Conceal the [STAIN] from [SMEARED WORD] |"
    "| AN EX WIS -                  - KAL QUAS |"
    "+-----------------------------------------+"
    "Quickness <Rel Tym>"
    "- Mix Sulphurous Ash, Blood Moss, Mandrake"
    ""
    "X-Ray Vision <Wis An Ylem>"
    "- Mix Sulphurous Ash, Mandrake"
    ""
    "Invisibility <Sanct Lor>"
    "- Mix Blood Moss, Nightshade, Mandrake"
    ""
    "Telekinesis <In Rel Por>"
    "- Mix Spider Silk, Blood Moss, Black Pearl"
    ""
    "Poison Wind <In Nox Hur>"
    "- Mix Sulphurous Ash,Blood Moss, Nightshade"
    ""
    "Wind of Sleep <In Zu Hur>"
    "- Mix Ginseng, Blood Moss, Mandrake"
    ""
    "Time Stop <An Tym>"
    "- Mix Garlic, Blood Moss, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Illusions, Misdirections, and Schemes ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
 't_spell_book_illusion_1 "Illusions: Lesser Secrets" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Illusions: Lesser Secrets"
    "+-----------------------------------------+"
    "|          Be ye Wrogue, or Mage?         |"
    "|   By Artifice, by Cunning, ye Survive   |"
    "| Use well these Runes, Mix and Cast them |"
    "+-----------------------------------------+"
    "Conjure Smoke <Bet Ylem Hur>"
    "- Mix Sulphurous Ash"
    ""
    "Detect Traps <Wis Sanct>"
    "- Mix Sulphurous Ash"
    ""
    "Sleep <Xen Zu>"
    "- Mix Ginseng, Spider Silk"
    ""
    "Calm Spiders <An Xen Bet>"
    "- Mix Garlic, Spider Silk"
    )
   result-ok))

(mk-reusable-item
't_spell_book_illusion_2 "Illusions: Greater Secrets" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Illusions: Greater Secrets"
    "+-----------------------------------------+"
    "|   Be ye Master Thief, or Cloaked Mage?  |"
    "| Artifice Superlative                    |"
    "|     Cunning Extraordinary               |"
    "|         Survival Quotidian              |"
    "|             Mix and Cast the Runes      |"
    "|                 CAVEAT LECTOR           |"
    "| NON CARBORUNDUM ILLEGITEMI              |"
    "+-----------------------------------------+"
    "Confusion <Quas An Wis>"
    "- Mix Nightshade, Mandrake"
    ""
    "Charm Monster <An Xen Ex>"
    "- Mix Spider Silk, Black Pearl, Nightshade"
    ""
    "Fear <In Quas Corp>"
    "- Mix Garlic, Nightshade, Mandrake"
    ""
    "Invisibility <Sanct Lor>"
    "- Mix Blood Moss, Nightshade, Mandrake"
    )
   result-ok))


;;;;;;;;;;;;;;;;;;;; Divination Magick ;;;;;;;;;;;;;;;;;;;;

(mk-reusable-item
't_spell_book_divination "Mysteries of Divination" s_manual (/ norm 3)
 (lambda (kbook kuser)
   (kern-ui-page-text
    "Mysteries of Divination"
    "+-----------------------------------------+"
    "|     IN WIS - AN QUAS - VAS KAL EX WIS   |"
    "| Oh Seeker, are ye Wise? Riddle me then  |"
    "|   I Seek the Seeker,  I Slay the Slayer |"
    "|   I Wreak the Wrogue, I Work the Wright |"
    "|              What then am I?            |"
    "+-----------------------------------------+"
    "Detect Traps <Wis Sanct>"
    "- Mix Sulphurous Ash"
    ""
    "Disarm Trap <An Sanct Ylem>"
    "- Mix Blood Moss"
    ""
    "Locate <In Wis>"
    "- Mix Nightshade"
    ""
    "Vision <In Quas Wis>"
    "- Mix Nightshade Mandrake"
    ""
    "Reveal <Wis Quas>"
    "- Mix Sulphurous Ash, Nightshade"
    ""
    "X-Ray Vision <Wis An Ylem>"
    "- Mix Sulphurous Ash, Mandrake"
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

;;----------------------------------------------------------------------------
;; Stewardess's Journal
;;
;; TODO: update last entry time to match when warritrix quest becomes available
;; Journal should not be updated if stolen by wanderer (This would also destroy its applicability as evidence)
;;
(mk-reusable-item 
 't_stewardess_journal "Victoria's Journal" s_lexicon norm
 (lambda (klexicon kuser)
   (kern-ui-page-text
    "Victoria's Journal"
    ""
    "1.3.1610"
    "I fear that if the Warritrix grows more      "
    "popular, then the next time she defies me it "
    "will lead to open revolt. Extreme measures   "
    "must be taken, I fear. I must be most        "
    "careful. Perhaps 'our friends' can help us.  "
    "They owe us a large debt. Not that they can  "
    "be trusted. "
    ""
    "..."
    ""
    "11.13.1610"
    "I finally managed to get that pest Valus     "
    "out of the way. A little evidence in the     "
    "right place, a few nasty rumours, and the    "
    "people are ready to accept that he has become"
    "a deviant drunkard. I believe the desperate  "
    "fool would have had the temerity to strike   "
    "the statue if I had not detained him in a    "
    "cell. The statue's powers are probably a     "
    "myth, but I see no point in taking chances.  "
    ""
    "..."
    ""
    "4.1.1611"
    "S. has warned me that rumours of a Wanderer  "
    "are afoot. S. is such a useful tool. I do    "
    "wonder where he gets his information. I don't"
    "know why he is worked up about a Wanderer,   "
    "anyway. I'm sure the legends about them are  "
    "greatly exaggerated. The last time a Wanderer"
    "appeared, the Old Gods still walked the      "
    "Shard, if the myths are true.                "
    ""
    "..."
    ""
    "6.12.1611"
    "I have received word from S. All is prepared."
    "I will suggest to Jeffries that the rumours  "
    "of feral gints occupying the Lost Halls must "
    "be investigated, and will insist on the      "
    "Warritrix leading the expedition. They will  "
    "expect gint riff-raff, not sorcery, and S.   "
    "will ensure the way is shut behind them.     "
    "There will be no escape, and if there is, I  "
    "am not culpable. If anything, that fool      "
    "Jeffries will go down for incompetence.      "
    ""
    "The only thing that could condemn me is this "
    "journal. But no one will ever find it, and   "
    "when I am queen of all the Shard the scribes "
    "will need it for their histories.            "
    ""
    "..."
   )
   result-ok))
