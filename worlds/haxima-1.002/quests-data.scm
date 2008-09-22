(let
	((questdata (tbl-mk)))
	
	(tbl-set! questdata 'questentry-charcreate
(qst-mk "Character Creation"
	'questentry-charcreate
	'(
		"Move to the moongate at the north side of"
		"the room to begin the game."
		""
		"Along the way you will be prompted for your"
		"characters name. You may also customize your"
		"attributes by talking to the statues in the"
		"room."
	)
	'quest-assign-always
	'quest-status-inprogress
	's_quest_start
	0
))

	(tbl-set! questdata 'questentry-whereami
(qst-mk "Where am I?"
	'questentry-whereami
	'(
		"You have found yourself in a world you have no"
		"knowledge of, with barest impressions of what"
		"might have gone before."
		""
		"Where are you?"
		"How and why are you here?"
		"And what are you going to do now?"
	)
	'quest-assign-always
	'quest-status-inprogress
	's_quest_start
	(tbl-mk)
	;; 'shard- pc knows about shard(1), cosmology(2)
	;; 'wanderer- pc knows about wanderers(1), potential(2), N's summoning(3)
))

	(tbl-set! questdata 'questentry-calltoarms
(qst-mk "A Call to Arms"
	'questentry-calltoarms
	'(
		"You have recieved an urgent message to contact"
		"someone called the Enchanter as soon as"
		"possible."
		""
		"The message suggests that you ask the caretaker"
		"of the clearing that you arrived in for"
		"directions."
	)
	'quest-assign-always
	'quest-status-inprogress
	's_enchanter
	(tbl-mk)
))
	
	(tbl-set! questdata 'questentry-bandits
(qst-mk "Bandit Troubles"
	'questentry-bandits
	'(
		"Gregor, an old charcoal burner, has asked for"
		"your help in dealing with some troublesome"
		"bandits that have been plaguing the great"
		"forest."
		""
		"He suggests that the Rangers at Green Tower"
		"will be able to assist in this task."
	)
	'quest-assign-always
	'quest-status-inprogress
	's_brigand
	0
))

	(tbl-set! (gob (kern-get-player)) 'questdata questdata)
)
