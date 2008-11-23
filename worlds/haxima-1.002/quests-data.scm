
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
	'quest-assign-always
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
	'quest-assign-always
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
	'quest-assign-always
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
	'quest-assign-always
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
	'quest-assign-always
	'quest-status-inprogress
	's_runestone_k
	(tbl-build
		'on-update 'quest-runeinfo-update
		'bonus-xp 0
		)
))


(questadd (qst-mk "Blood Price: Dragon"
	'questentry-dragon
	(kern-ui-paginate-text
		"The Alchemist has offered to trade you information on the wherabouts of a Rune, in exchange for the blood of a dragon."
	)
	'quest-assign-always
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
	'quest-assign-always
	'quest-status-inprogress
	's_brigand
	0
))

)

