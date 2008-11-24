
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

(questadd (qst-mk "A Rune in Hand"
	'questentry-rune-k
	(kern-ui-paginate-text
		"The Rune of Knowledge belongs to the Enchanter. You may be able to convince him to turn it over to you."
	)
	'quest-assign-notify
	'quest-status-inprogress
	's_runestone_k
	(tbl-build
		;;'on-update 'quest-allrunes-update
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
		;;'on-update 'quest-allrunes-update
		'bonus-xp 0
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
		;;'on-update 'quest-allrunes-update
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
		;;'on-update 'quest-allrunes-update
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
	's_enchanter
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

(questadd (qst-mk "The Necromancer"
	'questentry-necromancer
	(kern-ui-paginate-text
		"The Necromancer is a Wise Wizard who specializes in death magic."
	)
	'quest-assign-subquest
	'quest-status-inprogress
	's_necromancer
	(tbl-build
		'qparent 'questentry-wise
		;;'on-update 'quest-necromancer-update
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
	0
))

)

