;; You can replace these with your own music files if you like.
;; It should be reasonably easy to keep thematically correct.
;; You can use full paths for stuff outside the haxima directory
;; Use '/' instead of '\' unless you know better

;; SDL doesnt seem to like all mp3s- may be some encoding issues or something
;; I get some playing half speed, turning a fast paced combat song into something
;; more suitable for 'the Morgue of Satan'

;; These should be a short fanfare
(define ml-battle-intro
	(music-list
"music/dragon-quest.mid"
"music/into-battle.mid"
))

;; These should be a short fanfare
(define ml-battle-over
	(music-list
"music/game-music2.mid"
"music/dragon-slayer.mid"
))

;; These should be a short fanfare
(define ml-battle-lost
	(music-list
"music/fair-camelot.mid"
"music/dragon-slayer.mid"
))

(define ml-battle-music
	(music-list
"music/double-trios.mid"
"music/ballad.mid"
))

(define ml-outdoor-adventure
	(music-list
"music/double-trios.mid"
"music/ballad.mid"
))

(define ml-dungeon-adventure
	(music-list
"music/double-trios.mid"
"music/ballad.mid"	
))

(define ml-creepy-area
	(music-list
"music/double-trios.mid"
"music/ballad.mid"	
))

(define ml-travelling
	(music-list
"music/plainchant-recorder-trio.mid"
"music/harpsichord-piece.mid"
))

(define ml-small-town
	(music-list
"music/plainchant-recorder-trio.mid"
"music/minstrel-dance.mid"
"music/Minuet-like-Mozart.mid"
))

(define ml-large-town
	(music-list
"music/bassoons-and-harpsichord.mid"
"music/Minuet-like-Mozart.mid"
))

(define ml-castle
	(music-list
"music/double-trios.mid"
"music/ballad.mid"	
))

(define ml-dungeon-town
	(music-list
"music/double-trios.mid"
"music/ballad.mid"	
))

(define ml-sailing
	(music-list
"music/wind-trio.mid"
))
