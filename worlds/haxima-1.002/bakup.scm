;; bakup.scm -- a nazghul session file
;; Created Wed Dec 31 17:00:00 1969

;; Load the standard definitions file
(load "naz.scm")

(kern-load "game.scm")

; (kern-mk-map 'm_dark_passage 32 32 std_palette
;   (list
;    "x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x "
;    "x , , , , , , x x x x x x x x x x x x x x x x x x ~ ~ x x x x x "
;    "x , , , , , , x x x x x x x x x x x x x x x x x x ~ x x x x x x "
;    "x , , , , , , x x x x x x x x x x x x x x x x ~ ~ ~ x x x x x x "
;    "x , , , , , , x x x x x x x x x x x x x x x x ~ x x x x x x x x "
;    "x , , , , , , x x x x x x x x x x x x x x x x ~ x x x x x x x x "
;    "x x x x , , , x x x x x x x x x x x x x ~ ~ ~ ~ x x x x x x x x "
;    "x x x x , , , x x x x x x x x x x x x x ~ x x x x x x x x x x x "
;    "x x x x , , , x x x x x x x x x x x x x ~ x x x x x x x x x x x "
;    "x x x x , , , x x x x x , , , , , ~ ~ ~ ~ x x x x x x x x x x x "
;    "x x x x , , , x x x x , , , , , , ~ ~ ~ ~ ~ ~ x x x x x x x x x "
;    "x x x x , , , x x , , , , , , , ~ ~ ~ ~ , , , x x x x x x x x x "
;    "x x x x x , , , , , , , , , , , ~ , , , , , , x x x x x x x x x "
;    "x x x x x x , , , , , , , , , , ~ , , , , , , x x x x x x x x x "
;    "x x x x x x x x x , , , , , , , ~ , , , , , , x x x x x x x x x "
;    "x x x x x x x x x , , , , , , , ~ , , , , , , x x x x x x x x x "
;    "x x x x x x x x x x , , , , , , ~ , , , , , , , x x x x x x x x "
;    "x x x x x x x x x x , , , , , ~ ~ , , , , , , , x x x x x x x x "
;    "x x x x x x x x x x , , , , ~ ~ , , , , , , , , , , , , x x x x "
;    "x x x x x x x x x x , , ~ ~ ~ , , , , , , , , , , , , , x x x x "
;    "x x x x x x x x x x , ~ ~ , , , , , , , , , , , x x , , , x x x "
;    "x x x x x x x x x x ~ ~ , , , , , , , , , , x x x x , , , x x x "
;    "x x x x x x x x x x ~ x x x x x x x x x x x x x x x , , , x x x "
;    "x x x x x x x x x ~ ~ x x x x x x x x x x x x x x x , , , x x x "
;    "x x x x x x x x x ~ x x x x . . . . . . . x x x x x , , , x x x "
;    "x x x x x x x x ~ ~ x x x . . . . . . . . . x x x x , , , x x x "
;    "x x x x x x x x ~ x x x x . . . . . . . . . x x x x , , , , , x "
;    "x x x x x x x ~ ~ x x x x . . . . . . . . . x x x x , , , , , x "
;    "x x x x x x x ~ ~ x x x x ~ . . . . . . . . x x x x , , , , , x "
;    "x x x x x x x x ~ ~ ~ ~ ~ ~ ~ . . . . . . . x x x x , , , , , x "
;    "x x x x x x x x x x x x x ~ . . . . . . . x x x x x , , , , , x "
;    "x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x "
;   ))

(kern-mk-map 'm_green_tower 64 64 pal_expanded
  (list
   "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || || tt b .. /7 .. b tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
   "|| || || || || || || || || || || || r r r r r r r || || || || || || || || || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || || "
   "|| || r r r r r || || || r r r & cc cc cc cc r || || || || || || || || || || tt b .. /7 .. b tt || || || || || || || || || || || || xx xx xx xx xx xx xx xx xx tt tt || || || || || "
   "|| || r cc cc cc r || || || r cc r cc cc cc cc cc r || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || xx .. .. .. .. .. .. .. xx tt tt tt || || || || "
   "|| || r cc cc cc r || || || r cc cc cc cc cc cc cc r || || || || || || || || || tt tt b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /d /1 /d .. .. .. .. .. .. .. .. ws tt tt tt || || || || "
   "|| || r cc cc cc r || || || r cc r cc cc cc r r r || || || || || || || || tt tt tt tt .. /7 .. b tt || || || || || || || || || || /7 tt xx @ @ @ @ @ @ @ xx tt tt tt || || || || "
   "|| || r r cc r r || || || r cc r cc cc cc cc cc r || || || || || || || || tt tt tt b .. /7 .. tt tt || || || || || || || || || || /7 .. .. .. .. .. .. .. .. .. xx tt tt || || || || || "
   "|| || || || /7 || || || || || || /7 r r r r r r r || || || || || || || tt tt tt tt tt .. /7 .. tt tt || || || || || || || || || || /7 tt xx .S .H .R .O .O .M .S xx || || || || || || || "
   "|| || || || /8 /d /d /d /1 /d /d /a || || || || || || || || || || || || || || tt tt tt tt b .. /7 .. b tt || || || || || || || || || tt /7 tt xx xx xx xx xx xx xx xx xx || || || || || || || "
   "|| || || || || || || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt tt .. /7 .. tt tt || || || || || || || || tt tt /7 tt xx .. .. .. xx .. .. .. xx || || || || || || || "
   "|| || || r r r || || /7 || || || || || || || || || || || || || || || || tt tt tt tt tt b .. /7 .. b tt || || || || || || || || tt tt /8 /d .. .. .. .. ? .. .. .. ? tt tt tt tt tt tt tt "
   "|| r r r cc r r || /4 /d /d /d /d /d /d /d /d /d /d /2 || || || || tt tt tt tt tt tt tt .. /7 .. tt tt || || || || tt tt tt tt tt tt tt tt xx .. .. tt xx .. .. .. xx || || || || || || || "
   "|| r cc cc cc cc r r /7 || || || || || || || || || || /7 || || || || tt tt tt tt tt tt b .. /7 .. b tt || || || || tt .. tt tt tt || || || xx xx ws xx xx xx .. xx xx || || || || || || || "
   "|| r cc cc cc cc cc cc /a || || || || || || || || tt || /7 || || || tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt tt tt tt .. .. tt || || || || || || || || || || || || || || || || || || || || "
   "|| r r cc cc cc r r || || || || || tt || || tt tt tt /7 || || || tt tt tt tt tt tt tt b .. /7 .. b tt || tt || || tt tt tt tt || || || || || || || || || || || || || || || || || || || || "
   "|| || r r & r r || tt || || || tt tt b || || tt || /7 || tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt || tt || || || || || || || || || || || || || || || || || || || || || || || || || || "
   "|| || || r r r || tt tt tt || tt tt .. tt || || tt b /7 b .. b tt b tt b tt b tt .. .. /7 .. .. tt b tt b tt tt b tt b tt b tt tt || || || || || || || || || || || || || || || || "
   "|| || || tt || || || || tt || tt tt .. .. .. tt || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || || || || || || || || || "
   "|| || tt tt tt || || || || || || tt tt .. tt || || b .. /4 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /2 .. || tt || || || || || || || || || || || || || || || "
   "|| || || tt || || || || || tt tt tt tt tt tt || || tt .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || || || tt tt tt tt tt tt tt || || || b .. /7 .. .. .. b d b d b d .. .. .. /7 .. .. .. b tt tt b tt b tt .. .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || || tt tt tt .. tt tt tt || || || || tt .. /7 .. .. d d d d d d d b .. .. /7 .. .. b tt tt tt tt tt tt b tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || b tt tt .. .. .. tt tt tt || || || b .. /7 .. b d || || || || || d xx w+ xx cc xx w+ xx tt || || || || || tt tt .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || tt tt .. .. .. .. .. tt tt || || || || .. /7 .. d d || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt b .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || tt tt tt .. .. .. tt tt tt || || || b .. /7 .. b d || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || tt tt tt tt .. tt tt tt || || || || || .. /7 .. d d || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt b .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || tt tt tt tt || tt tt tt tt b || || tt tt || b .. /7 .. b d || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || tt || || || || || tt tt tt || || || || || || tt .. /7 .. d d xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt b .. /7 .. b tt || || || || || || || || || || || || || || tt "
   "tt tt tt || || || || || || || || || || || || || tt b .. /7 .. b d xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt tt .. /7 .. tt tt || || || || || || || || || || || || || || tt "
   "b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. .. /7 .. d b xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx b .. .. /7 .. b tt || || || || || || || || || || || || || || tt "
   ".. .. b tt b tt b tt b tt b tt b tt b b .. .. .. /7 .. .. .. w+ cc cc cc xx tt .. b .. cc .. b .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
   "/d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d /d "
   ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. xx cc cc cc xx .. .. .. .. cc .. .. .. .. xx cc cc cc xx .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
   ".. .. b tt b tt b tt b tt b tt b tt tt tt b .. .. /7 .. .. .. w+ cc cc cc xx tt .. b .. cc .. b .. tt xx cc cc cc w+ .. .. .. /7 .. tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt "
   "b tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. .. b xx cc cc cc x! || tt .. .. cc .. .. tt || x! cc cc cc xx b .. .. /7 .. b tt || || || || || || || || || || || tt tt tt tt "
   "tt tt || || || || tt tt tt tt tt tt tt tt || || tt tt .. /7 .. .. tt xx cc cc cc xx xx || tt .. cc .. tt || xx xx cc cc cc xx tt .. .. /7 .. tt tt || || || || || || || || || || || || || || tt "
   "tt tt || || || || tt tt tt tt tt tt tt tt || || tt b .. /7 .. b tt xx xx cc cc cc xx x! xx xx cc xx xx x! xx cc cc cc xx xx tt b .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || || || tt tt tt tt tt tt tt || || tt tt .. /7 .. tt tt || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || || || || tt tt tt tt tt tt || || tt b .. /7 .. b tt || xx xx cc cc cc cc cc cc cc cc cc cc cc cc cc xx xx || tt b .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || || || || || || || || || || || || tt tt .. /7 .. tt tt || || xx xx xx cc cc cc cc cc cc cc cc cc xx xx xx || || tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || || || || || || || || || || || || tt b .. /7 .. b tt || || || || xx w+ xx cc cc cc cc cc xx xx xx || || || || tt b .. /7 .. b tt || || || || || || || || || || || || || || || "
   "|| || || || || || || || || || || || || || r r tt tt .. /7 .. tt tt || || || || || tt xx w+ xx cc xx w+ xx tt || || || || tt tt tt .. /7 .. tt tt || || || || || || || || || || || || || || || "
   "|| || || || || || || || || || || || || || || || tt b .. /7 .. b tt tt tt tt tt tt tt b .. .. /7 .. .. b tt tt tt tt tt tt tt b .. /7 .. b tt tt tt tt tt tt tt || || || || || || || || || "
   "|| || || || || || || tt tt tt tt || || || || || tt tt .. /7 .. .. tt b tt b tt b .. .. .. .. /7 .. .. .. .. b tt b tt b tt .. .. /7 .. .. tt b tt b tt b tt || || || || || || || || || "
   "|| || || || || tt r tt r r tt r r || || || tt b .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt tt || || || || || || || || || "
   "|| || || r r tt tt tt || || tt tt r r r || tt tt .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /5 /d /d /d /d /d /d /d /d /d /d /d /d /9 /d /d /d /d /d /2 .. b tt || || || || || || || || || "
   "|| || || r || || || tt tt tt || tt tt || r || || b .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. tt tt || || || || || || || || || "
   "|| || || || tt tt tt tt .. .. tt || tt tt || r r || || tt b tt b tt b tt b tt b tt b .. /7 .. tt b tt b tt b tt b tt b tt b tt b tt tt b /4 /2 b tt || || || || || || || || || "
   "|| r r || tt tt || .. .. b .. tt tt tt tt || r r || tt tt tt tt tt tt tt tt tt tt tt tt .. /7 .. tt tt tt tt tt tt tt tt tt tt tt xx xx xx xx ws xx cc cc xx ws xx xx xx xx || || || || || "
   "|| || || || tt tt .. .. .. .. .. tt .. tt tt || || r || || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
   "|| tt tt tt tt .. .. .. .. .. .. .. .. .. tt tt || tt tt || || || || || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc xx || || || || || "
   "|| tt tt tt .. b .. .. .. .. .. .. .. b .. tt .. tt tt tt tt tt tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 0 cc cc cc cc cc cc 0 cc cc xx || || || || || "
   "|| || || tt .. .. .. .. .. a .. .. .. .. .. tt || r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || || || xx cc cc 0 cc cc & & cc cc 0 cc cc xx || || || || || "
   "|| r || tt tt tt tt .. .. .. .. .. .. tt tt || || r || || || || tt || || || || || || || tt .. /7 .. tt || || || || || || || || xx xx xx cc cc 0 cc cc cc cc cc cc 0 cc cc xx xx xx || || || "
   "|| r || || || .. tt .. .. .. .. .. .. tt tt || r || || || r r tt r r || || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
   "|| r r || tt tt tt b .. .. .. b .. tt tt || r || || r r tt tt tt r r || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
   "|| || r || tt tt tt tt tt .. .. .. .. tt || r r || || r tt tt || tt || r || || || tt tt .. /7 .. tt || || || || || || || || xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || || "
   "|| || || || || || || || tt tt tt tt tt || tt r || || || r tt || || || || r || || || tt tt .. /7 .. || || || || || || || || xx xx cc xx xx @ @ @ @ @ @ @ @ @ @ xx xx cc xx xx || || "
   "|| || || r r || || || tt tt tt || tt tt || r || || || r tt tt || || || r || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc xx || || "
   "|| || || || r r || r r r || r r r r || || || || r r || || || tt r || || || tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx .W .H .I .T .E @ .S .T .A .G xx cc cc cc xx || || "
   "|| || || || || || || || || || || || || || || || || || || || r r r r tt || || || tt tt tt .. /7 .. tt || || || || || || || xx cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx cc cc cc xx || || "
   "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt tt .. /7 .. tt tt || || || || || || xx xx xx xx xx || || || || || || || || || || xx xx xx xx xx || || "
   "|| || || || || || || || || || || || || || || || || || || || || || || || || || || || tt tt .. .. /7 .. .. tt tt || || || || || || || || || || || || || || || || || || || || || || || || || || || "
  ))

(kern-mk-map 'm_wilderness 40 40 pal_expanded
  (list
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- .. .. .. .. .. .. .. .. .. -- -- -- -- -- __ __ __ "
   "__ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt tt .. {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ tt tt .. .. .. %% {{ -- __ __ __ "
   "__ __ __ -- -- -- -- -- -- .. .. .. tt tt tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. .. -- __ __ __ "
   "__ __ __ -- .. .. .. .. tt tt tt tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. .. -- -- .. __ "
   "__ __ __ -- .. .. tt tt tt || {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt .. .. .. .. .. -- .. .. "
   ".. __ __ -- .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ {{ {{ {{ tt .. .. .. .. -- .. .. "
   ".. __ __ -- .. tt tt {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ tt tt tt {{ {{ {{ {{ {{ {{ {{ tt .. /0 /d /d -- /d /a "
   ".. __ __ -- .. .. tt tt {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ tt tt tt .. .. .. .. .. .. .. .. .. .. /6 .. .. -- .. .. "
   ".. -- -- -- .. .. .. .. tt {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ tt tt tt tt .. .. /7 .. .. -- -- .. "
   "__ -- .. .. .. .. .. .. .. tt tt {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ {{ {{ {{ {{ tt .. .. /7 .. .. .. -- .. "
   "__ -- .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt .. .. /7 .. .. .. -- __ "
   "__ -- .. .. .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ tt tt .. .. /7 .. .. .. -- __ "
   "__ -- .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. /7 .. .. -- -- __ "
   "__ -- .. .. .. .. {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ tt tt tt .. .. .. .. .. /7 .. -- -- __ __ "
   "-- -- .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt tt tt tt .. .. .. .. tt tt tt /7 tt -- -- __ __ "
   "-- -- tt tt tt tt .. tt tt tt {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. tt tt tt tt || || /7 || || -- -- -- "
   "|| ~~ || || tt tt .. .. tt tt tt tt .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt .. .. tt tt || || || || || /8 /2 || || ~~ tt "
   "|| ~~ ~~ || || tt .. .. tt tt .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt .. .. tt || || || || || || || /7 || || ~~ ~~ "
   ".. .. -- .. .. .. .. tt tt tt tt tt tt tt .. .. .. {{ {{ ^^ {{ {{ {{ .. .. .. .. tt || || || b  tt b  tt /b .. || || ~~ "
   ".. || ~~ ~~ || || tt tt tt tt ~~ ~~ ~~ -- -- -- .. .. {{ {{ {{ -- -- -- .. .. .. tt tt || || tt tt .. .. .. .. || ~~ ~~ "
   ".. || || ~~ tt tt tt tt .. %% %% ~~ ~~ -- __ -- -- .. .. .. .. .. -- -- .. .. .. .. tt || || b  tt b  tt .. .. ~~ ~~ .. "
   "|| || || ~~ tt tt tt .. .. .. %% %% ~~ -- __ __ -- -- -- -- .. .. -- -- -- .. .. .. tt || || || || || || .. .. -- .. .. "
   "|| || || ~~ tt tt .. .. .. %% .. %% %% -- __ __ __ __ __ -- -- -- -- -- .. .. .. .. tt tt tt || || || || || -- -- -- || "
   "|| || || ~~ .. .. .. %% %% .. %% %% ~~ -- __ __ __ __ __ __ __ __ __ -- -- .. .. .. .. .. tt tt tt -- -- -- -- __ -- -- "
   "-- || -- -- ~~ ~~ %% %% ~~ %% %% ~~ -- -- __ __ __ __ __ __ __ __ __ -- -- -- -- -- -- -- -- -- -- -- __ __ __ __ __ -- "
   "-- -- -- __ __ -- -- -- ~~ ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ -- -- -- %% %% %% %% -- -- __ __ ^^ ^^ ^^ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ -- -- -- .. .. .. -- -- -- -- -- -- ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ -- -- -- ^^ {{ ^^ {{ {{ {{ ^^ -- ^^ ^^ ^^ -- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ -- -- {{ {{ {{ ^^ {{ ^^ -- {{ {{ ^^ ^^ -- -- __ __ __ __ __ -- ^^ ^^ ^^ __ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ ^^ -- ^^ ^^ ^^ ^^ -- -- __ __ __ __ __ -- -- !! ^^ ^^ ^^ ^^ -- __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !! !! !! ^^ !! -- -- __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ !! !! !! ^^ -- __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !! !! ^^ ^^ ^^ ^^ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ ^^ !! ^^ ^^ __ __ __ __ __ __ "
   "__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- -- -- __ __ __ __ __ __ __ "
  ))

(kern-mk-map 'm_campsite 7 7 std_palette
  (list
   "b b . . . b b "
   "b . . . . . b "
   ". . . . . . . "
   ". . . & . . . "
   ". . . . . . . "
   "b . . . . . b "
   "b b . . . b b "
  ))

(kern-mk-place 'p_dark_passage "A Dark Passage" m_dark_passage #f #t #f
)

(kern-mk-place 'p_wilderness "The Great Wild" m_wilderness #t #f #t
)

(kern-mk-place 'p_green_tower "Town of GreenTower" m_green_tower #f #f #f
)

(kern-mk-town p_green_tower p_wilderness 35 23 s_town)
(kern-mk-char 'ch_thorald_greybeard
              "Thorald Greybeard"
              sp_human
              oc_wizard
              s_companion_wizard
              200 210 202
              0 1
              10 5
              0 0
              0 0
              39 0
              "G"
              240 8
              nil
              (list
                       t_dagger
              ))
(kern-mk-player
 p_wilderness 33 16
 s_companion_fighter
 "Walk"
 "data/sounds/walk.wav"
 0 0 3
 nil
 m_campsite
 nil
 (list
  (list 11 t_dagger)
 )
 (list
  ch_thorald_greybeard
 )
)
;;--------------
;; Miscellaneous
;;--------------
(kern-set-frame s_frame_ulc s_frame_urc s_frame_llc s_frame_lrc s_frame_td s_frame_tu s_frame_tl s_frame_tr s_null s_frame_horz s_frame_vert s_frame_endl s_frame_endr)
(kern-set-cursor ls_whirlpool)
(kern-set-crosshair t_crosshair)
(kern-set-ascii ss_little_sprites 32)
(kern-set-clock 0 0 0 0 18 54)
