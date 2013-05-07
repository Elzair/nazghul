;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Quest Data Table
;;
;; The basic quest data mechanism is based on a tbl of quest information. This allows quests
;; to be created once, then tracked and updated from a central repository.
;; While not suitable for quests generated on the fly, this is a lot more convenient for
;; complex plot based quests. On the fly quests can still interface directly with the
;; quest-sys module.
;;
;; The questadd function here handles creation of quests (it also makes sure the quest is
;; created iff it is needed, so savegames have some chance of updating right)
;;
;; It is inside a let definition, and hence wont work elsewhere. The quest to be added is
;; a quest using qst-mk from the quest-sys module
;;
;; (questadd (qst-mk
;;    "Name of Quest"
;;    'tag-to-refer-to-quest
;;    "Text description of quest, probably using kern-ui-paginate-text"
;;    'function-called-on-quest-assignment  ;; probably 'quest-assign-notify, or nil
;;    'function-called-before-quest-is-displayed  ;; probably nil
;;    'sprite-for-quest
;;    quest-payload
;;	)
;;
;; The various quest-data-* methods assume that quest-payload is a tbl, containing various
;; info possibly including:
;;
;; 		'on-update  a method name that will be called in response to a quest-data-update call
;;		'bonus-xp   a storage space for experience rewards that are accrued before the player
;;					knows about the quest (the xp will increase the rewards given once the
;;					the player knows why they are being given xp)
;;
;;
;; Using quests basically boils down to:
;;      defining the quest here
;;      adding a (quest-data-assign-once 'tag-to-refer-to-quest) at the relevent place in the game code
;;      adding a (quest-data-complete 'tag-to-refer-to-quest)
;;                                   at the relevent place in the game code
;;		
;;		for nicely updating quest information, add the on-update method as described above, and sprinkle
;;      the plot with
;;          (quest-data-update 'tag-to-refer-to-quest 'name-of-quest-flag value-to-set-tag-to)
;;             and
;;          (quest-data-update-with 'tag-to-refer-to-quest 'name-of-quest-flag value-to-set-tag-to
;;                  function-to-perform-if-the-tag-wasnt-already-set-that-way)
;;      a common example of the latter would be giving the party an xp reward:
;;          (quest-data-update-with 'tag-to-refer-to-quest 'name-of-quest-flag value-to-set-tag-to
;;                  (grant-party-xp-fn amount-of-xp-to-share-out))
;;
;;

(let*
	(
		(newtbl (tbl-mk))
		(oldtbl (tbl-get (gob (kern-get-player)) 'questdata))
		(questdata (if (null? oldtbl)
						(begin 
							(tbl-set! (gob (kern-get-player)) 'questdata newtbl)
							newtbl
						)
						oldtbl))
		(questadd (lambda (quest)
			(if (null? (tbl-get questdata (qst-tag quest)))
				(tbl-set! questdata (qst-tag quest) quest)
			)))
	)
	
(questadd (qst-mk 
	"Character Creation"
	'questentry-charcreate
	(kern-ui-paginate-text
		"Move to the moongate at the north side of the room to begin the game."
		""
		"Along the way you will be prompted for your characters name. You may also customize your attributes by talking to the statues in the room."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_quest_start
	0
))

(questadd (qst-mk "Where am I?"
	'questentry-whereami
	(kern-ui-paginate-text
		"You have found yourself in a world you have no knowledge of, with barest impressions of what might have gone before."
		""
		"Where are you?"
		"How and why are you here?"
		"And what are you going to do now?"
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_quest_start
	(tbl-build
		'on-update 'quest-whereami-update
		)
	;; 'shard- pc knows about shard(1), cosmology(2)
	;; 'wanderer- pc knows about wanderers(1), potential(2)
	;; 'nossifer- pc knows about N's summoning(3)
))

(questadd (qst-mk "A Call to Arms"
	'questentry-calltoarms
	(kern-ui-paginate-text
		"You have recieved an urgent message to contact someone called the Enchanter as soon as possible."
		""
		"The message suggests that you ask the caretaker of the clearing that you arrived in for directions."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_enchanter
	(tbl-build
		'on-update 'quest-calltoarms-update
		'bonus-xp 0
		)
	;; 'directions- pc has directions to tower
	;; 'tower- pc has reached tower
	;; 'talked- pc has talked to the enchanter
	;; 'done- pc has been enlisted
))
	
(questadd (qst-mk "To Catch a Thief"
	'questentry-thiefrune
	(kern-ui-paginate-text
		"The Enchanter has asked you to investigate a theft from his tower."
		""
		"The ^c+mthief^c- has been tracked as far as Trigrave. The townsfolk there may be able to give you further information."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_brigand
	(tbl-build
		'on-update 'quest-thiefrune-update
		'bonus-xp 0
		)
	;; '
	;; 'tower- pc has reached tower
	;; 'talked- pc has talked to the enchanter
	;; 'done- pc has been enlisted
))

(questadd (qst-mk "The Secret of the Runes"
	'questentry-runeinfo
	(kern-ui-paginate-text
		"The stolen rune that you recovered must have great significance to prompt it's theft. The Enchanter has given you the task of seeking out this reason."
		""
		"He suggests that you start with the ^c+mAlchemist^c-, who may be found at Oparine."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_k
	(tbl-build
		'on-update 'quest-runeinfo-update
		'bonus-xp 0
		)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runes questgroup

(questadd (qst-mk "The Search for the Runes"
	'questentry-allrunes
	(kern-ui-paginate-text
		"The Enchanter believes that the Accursed are seeking the runes for nefarious purposes. It is up to you to collect them first."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_group
	(tbl-build
		;;'on-update 'quest-allrunes-update
		'bonus-xp 0
		)
))

;; TODO- alternate path in which the rune is lost?
(questadd (qst-mk "A Rune in Hand"
	'questentry-rune-k
	(kern-ui-paginate-text
		"The Rune of Knowledge belongs to the Enchanter. You may be able to convince him to turn it over to you."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_k
	(tbl-build
		'on-update 'quest-rune-k-update
		'entrusted-with-rune 0
		'player-got-rune 0
		'ench-should-have-rune 0
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Rune in the Deeps"
	'questentry-rune-p
	(kern-ui-paginate-text
		"The Alchemist provided you with information on a rune buried in the deeps of Kurpolis."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'on-update 'quest-rune-p-update
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Soldier's Rune"
	'questentry-rune-l
	(kern-ui-paginate-text
		"One of the Runes is carried by the Warritrix."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'on-update 'quest-rune-l-update
		'bonus-xp 0
		'located 0
		'know-hall 0
		'approx-hall 0
		)
))

(questadd (qst-mk "A Lost Rune"
	'questentry-rune-f
	(kern-ui-paginate-text
		"King Clovis once possessed a rune, but he fell during the Goblin Wars."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'on-update 'quest-rune-f-update
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Rune in the Void"
	'questentry-rune-d
	(kern-ui-paginate-text
		"Legends tell of a temple in the void, which housed a rune."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'on-update 'quest-rune-d-update
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Rune in Fire"
	'questentry-rune-w
	(kern-ui-paginate-text
		"A rune was found amongst the hoard of a dragon lairing in the Fire Sea."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Rune in the Ruins"
	'questentry-rune-s
	(kern-ui-paginate-text
		"A rune can be found in Old Absalot, beneath the ruins."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		;;'on-update 'quest-allrunes-update
		'bonus-xp 0
		)
))

(questadd (qst-mk "A Rune in the Sea"
	'questentry-rune-c
	(kern-ui-paginate-text
		"A rune once belonged to the pirate Ghertie."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_r
	(tbl-build
		'on-update 'quest-rune-c-update
		'bonus-xp 0
		)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wise questgroup

(questadd (qst-mk "The Wise"
	'questentry-wise
	(kern-ui-paginate-text
		"The Wise have great influence over affairs in the Shard. Seeking them out may be critical to your success."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_quest_wise
	(tbl-build
		;;'on-update 'quest-wise-update
		)
))

(questadd (qst-mk "The Enchanter"
	'questentry-enchanter
	(kern-ui-paginate-text
		"The Enchanter is a great and knowledgable Wizard, one of the Wise of the present age."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_enchanter
	(tbl-build
		;;'on-update 'quest-enchanter-update
		'qparent 'questentry-wise
		)
))

(questadd (qst-mk "The Alchemist"
	'questentry-alchemist
	(kern-ui-paginate-text
		"The Alchemist is one of the wise."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_companion_tinker
	(tbl-build
		;;'on-update 'quest-alchemist-update
		'qparent 'questentry-wise
		)
))

(questadd (qst-mk "The MAN"
	'questentry-the-man
	(kern-ui-paginate-text
		"The MAN is one of the wise."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_companion_bard
	(tbl-build
		;;'on-update 'quest-the-man-update
		'qparent 'questentry-wise
		)
))

(questadd (qst-mk "The Engineer"
	'questentry-engineer
	(kern-ui-paginate-text
		"The Alchemist is one of the wise."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_companion_tinker
	(tbl-build
		;;'on-update 'quest-engineer-update
		'qparent 'questentry-wise
		)
))

(questadd (qst-mk "The Necromancer"
	'questentry-necromancer
	(kern-ui-paginate-text
		"The Necromancer is one of the wise."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_black_mage
	(tbl-build
		;;'on-update 'quest-necromancer-update
		'qparent 'questentry-wise
		)
))

(questadd (qst-mk "The Warritrix"
	'questentry-warritrix
	(kern-ui-paginate-text
		"The Warritrix is one of the wise."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_avatar
	(tbl-build
		'on-update 'quest-warritrix-update
		'qparent 'questentry-wise
		)
		;;'name - knows existance, but no info
		;;'common - basic info
		;;'general-loc - common knowledge info
		;;'assignment - failed to find her
		;;'lost-hall - where she has been sent
		;;'lost-hall-loc - additionally, knows location of hall
		;;'slain - indirect evidence of her fall
		;;'reached - reached location of corpse - used by losthalls mechs
		;;'found - located corpse
		;;'avenged - completed justice quest TODO- pull from that quest info instead
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(questadd (qst-mk "A Haunted Inn"
	'questentry-ghertie
	(kern-ui-paginate-text
		"A ghost haunts the inn at Oparine. Why does it linger in undeath?"
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_ghost
	(tbl-build
		'on-update 'quest-ghertie-update
		;; ghertieloc  - gherties loc
		;; ghertieid - ghertie info
		;; revenge	- gherties trigger
		;; questinfo - quest info revealed
		;; meaney-loc - location of meaney
		;; ring-meaney - got ring from meaney
		;; jorn-forest - rough info on jorn
		;; jorn-loc - jorn found
		;; ring-jorn - got ring from jorn
		;; gholet-prison - rough info on gholet
		;; gholet-glasdrin - found gholet
		;; gholet-price - gholets offer for ring
		;; ring-gholet - got ring from gholet
		)
))

(questadd (qst-mk "Blood Price: Dragon"
	'questentry-dragon
	(kern-ui-paginate-text
		"The Alchemist has offered to trade you information on the wherabouts of a Rune, in exchange for the blood of a dragon."
	)
	'quest-assign-notify
	'quest-dragon-update
	's_dragon_party
	(tbl-build
		)
))

(questadd (qst-mk "A Soldiers Justice"
	'questentry-warrjustice
	(kern-ui-paginate-text
		"The Warritrix has been slain by treachery. Will this crime go unpunished?"
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_ghost
	(tbl-build
		'on-update 'quest-warrjustice-update
		)
		;;'statue - info about statue
		;;'book - info about journal
		;;'havejournal - found journal
		;;'avenged - completed justice quest
))

(questadd (qst-mk "Bandit Troubles"
	'questentry-bandits
	(kern-ui-paginate-text
		"Gregor, an old charcoal burner, has asked for your help in dealing with some troublesome bandits that have been plaguing the great forest."
		""
		"He suggests that the Rangers at Green Tower will be able to assist in this task."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_brigand
	(tbl-build
         'on-update 'quest-bandits-update
         )
))

(questadd (qst-mk "The Lost Toy"
		  'questentry-puska
		  (kern-ui-paginate-text "Ilya lost her toy while hiding in the basement from trolls on her parents homestead. "
					 "It's too late to save her parents, but maybe you can do something about that toy. "
					 ""
					 "The homestead is west of the mountain pass.")
		  'quest-assign-notify
		  'quest-status-inprogress
		  's_toy_horse
		  (tbl-build 'on-update 'quest-puska-update)))

(questadd (qst-mk "Shroom's Quest"
		  'questentry-shroom
		  (kern-ui-paginate-text "Shroom of Green Tower has promised to teach you a fire warding spell if you recover some purple mushrooms for her from a cave."
					 ""
					 "The mushrooms can be found in a cave south of Green Tower in the mountains by the sea. She mentioned something about slimes.")
		  'quest-assign-notify
		  'quest-status-inprogress
		  's_royal_cape
		  (tbl-build 'on-update 'quest-shroom-update)))
		  
)