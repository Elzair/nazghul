//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//
#include "ascii.h"
#include "cfg.h"
#include "common.h"
#include "dimensions.h"
#include "images.h"
#include "screen.h"

#include <assert.h>
#include <SDL_image.h>


/* This is a default character image set in XPM format. For emergency use
 * only. */
static const char * charset_xpm[] = {
"128 144 44 1",
" 	c None",
".	c #000000",
"+	c #004040",
"@	c #20FCFC",
"#	c #E0FCFC",
"$	c #000060",
"%	c #000080",
"&	c #0000A0",
"*	c #0000C0",
"=	c #E8E8E8",
"-	c #C0C0C0",
";	c #808080",
">	c #FCFCFC",
",	c #B0B0B0",
"'	c #D4D4D4",
")	c #008080",
"!	c #006060",
"~	c #800000",
"{	c #FC1000",
"]	c #404040",
"^	c #505050",
"/	c #4040FC",
"(	c #600000",
"_	c #400000",
":	c #A0A0A0",
"<	c #C0C0FC",
"[	c #C00000",
"}	c #303030",
"|	c #909090",
"1	c #8080FC",
"2	c #000040",
"3	c #0000FC",
"4	c #6060FC",
"5	c #0000E0",
"6	c #A00000",
"7	c #202020",
"8	c #101010",
"9	c #606060",
"0	c #FCFC80",
"a	c #FCFCE0",
"b	c #FCFCA0",
"c	c #A0A0FC",
"d	c #FCFC40",
"e	c #FCFCC0",
".+@##@+.$%%&*&%$=-;-=-;-=-;-=-;-...;>......;-......;>....+@##@+............->>.....->>.............->>..........,-''''-,........",
".+@##@+.$%%&*&%$=-;-=-;-=-;-=-;-...;>......;-......;>....+@##@+............->>.....->>.............->>..........,-''''-,........",
".@#)!#@.%~{{{{~%;.;;;;.;;;;;;;;;..;;->.....;-.....;;->...@#)!#@...];;]....-^^^>...-^^^>......->>..->^->.////////-......-..$&&$..",
".@#)!#@.%~{{{{~%;.;;;;.;;;;;;;;;..;;->.....;-.....;;->...@#)!#@...];;]....-^^^>...-^^^>......->>..->^->.////////-......-..$&&$..",
".@#.+#@.&~(__(~&;-=-;-=-;-=-;-=-.;;-->>....;-....;;-->>..........^:'=:^..-.->>^>.-.-->^>....^^->.^->^->.<<<<<<<<'......'.%*//*%.",
".@#.+#@.&~(__(~&;-=-;-=-;-=-;-=-.;;-->>....;-....;;-->>..........^:'=:^..-.->>^>.-.-->^>....^^->.^->^->.<<<<<<<<'......'.%*//*%.",
".+@##@+.*~[__[~*;;;;;;;;;;;;;;;;...;;......;-......;;....+@##@+.}|'>>'|}^-^-^.^>^-^->^^>..->>-^>.^^->>..11111111'......'2&341/&2",
".+@##@+.*~[__[~*;;;;;;;;;;;;;;;;...;;......;-......;;....+@##@+.}|'>>'|}^-^-^.^>^-^->^^>..->>-^>.^^->>..11111111'......'2&341/&2",
"+@####@+&~(__(~&=-;-=-;-=-;-=-;=...;-......;-......;-...........}|--''|}^-^->>^>^-^-^>^>.->^->^...^^>...////////'......'2&55/3&2",
"+@####@+&~(__(~&=-;-=-;-=-;-=-;=...;-......;-......;-...........}|--''|}^-^->>^>^-^-^>^>.->^->^...^^>...////////'......'2&55/3&2",
"..+@#+..%~6__6~%;;;;;;;;;;;;;;;;.;;-->>..;;-->>....;-.....+@#+...^:--:^.^^-^^.>.^^-^^.>.^->^->.....->>..********'......'.%*55*%.",
"..+@#+..%~6__6~%;;;;;;;;;;;;;;;;.;;-->>..;;-->>....;-.....+@#+...^:--:^.^^-^^.>.^^-^^.>.^->^->.....->>..********'......'.%*55*%.",
"..+@#+..2%~66~%2;.=-;-.-;-=-;-=-..;;->....;;->.....;-.....+@#+....];;]...^^-->...^^-->..^^->>.....^^-...%%%%%%%%-......-..$&&$..",
"..+@#+..2%~66~%2;.=-;-.-;-=-;-=-..;;->....;;->.....;-.....+@#+....];;]...^^-->...^^-->..^^->>.....^^-...%%%%%%%%-......-..$&&$..",
"..+@#+...2%~~%2.;;;;;;;;;;;;;;;;...;>......;>......;-.....+@#+............^^^.....^^^....^^^.......^............,-''''-,........",
"..+@#+...2%~~%2.;;;;;;;;;;;;;;;;...;>......;>......;-.....+@#+............^^^.....^^^....^^^.......^............,-''''-,........",
"..................................;78.....|^8.....|,7.....|,|.....7,|.....8^|.....87;.....888....2%%&&$..%*55*$..%5/5$2..222$$2.",
"..................................;78.....|^8.....|,7.....|,|.....7,|.....8^|.....87;.....888....2%%&&$..%*55*$..%5/5$2..222$$2.",
"/*%..........%*/.................,9778...,>978...,>0,7...,>a>,...7,>b,...879>,...8779,...87778..2$2$$%*%$*%%%*5%$5*&*5%22%**%2$2",
"/*%..........%*/.................,9778...,>978...,>0,7...,>a>,...7,>b,...879>,...8779,...87778..2$2$$%*%$*%%%*5%$5*&*5%22%**%2$2",
"<5*$........$*5c................;'77778.;>b^778.;>>>b97.|>00a0|.79b>b>;.877^b>;.87777';.8777778.$2&53&%*&%&3/**5***/%/*2$5//5&2%",
"<5*$........$*5c................;'77778.;>b^778.;>>>b97.|>00a0|.79b>b>;.877^b>;.87777';.8777778.$2&53&%*&%&3/**5***/%/*2$5//5&2%",
"135%........%531................,=77778.,>a9778.,>bbd;7.,>>>0b,.7;eded-.8779a>,.87777=,.8777778.$%5**3%5&$3**/&/5%/*%/*25*%%*5$%",
"135%........%531................,=77778.,>a9778.,>bbd;7.,>>>0b,.7;eded-.8779a>,.87777=,.8777778.$%5**3%5&$3**/&/5%/*%/*25*%%*5$%",
"//5%........%5//................;'77778.;>e^778.;>d>097.|>b>ad|.79b>>>;.877^b>;.87777';.8777778.2*/%*/%5%$5*%%*55%3**5%$/&/**3$&",
"//5%........%5//................;'77778.;>e^778.;>d>097.|>b>ad|.79b>>>;.877^b>;.87777';.8777778.2*/%*/%5%$5*%%*55%3**5%$/&/**3$&",
"**%$........$%**........->.->.->.,9778...,>978...,>b,7...,>>0,...7,b>,...879>,...8779,...87778..2*/%/***%2&5//5$*%&35&2$5**/3&%&",
"**%$........$%**........->.->.->.,9778...,>978...,>b,7...,>>0,...7,b>,...879>,...8779,...87778..2*/%/***%2&5//5$*%&35&2$5**/3&%&",
"%%$..........$%%........->^->^->..;78.....|^8.....|,7.....|,|.....7,|.....8^|.....87;.....888...2%5*&*5$2$2%**%2%*%$$2$2%5*%%%*$",
"%%$..........$%%........->^->^->..;78.....|^8.....|,7.....|,|.....7,|.....8^|.....87;.....888...2%5*&*5$2$2%**%2%*%$$2$2%5*%%%*$",
"........................^.^^.^^..................................................................2$5/5%..2$$222..$&&%%2..$*55*%.",
"........................^.^^.^^..................................................................2$5/5%..2$$222..$&&%%2..$*55*%.",
"...........->.....->.->.............>..............->>....->........->....->..................................................->",
"...........->.....->.->.............>..............->>....->........->....->..................................................->",
"..........^->....^->^->...->.->...->>>>..->...->..->^->..^->.......->....^^->.....->.->....->................................->.",
"..........^->....^->^->...->.->...->>>>..->...->..->^->..^->.......->....^^->.....->.->....->................................->.",
"..........^->....^->^->..->>>>>>.->^>^..^->..->..^^->>...->.......->......^^->...^^->>....^->...............................->..",
"..........^->....^->^->..->>>>>>.->^>^..^->..->..^^->>...->.......->......^^->...^^->>....^->...............................->..",
"..........^->....^^.^^..^^->^->.^^->>>>.^^..->....->>...^^.......^->.......^->...->>>>>>.->>>>>..........->>>>.............->...",
"..........^->....^^.^^..^^->^->.^^->>>>.^^..->....->>...^^.......^->.......^->...->>>>>>.->>>>>..........->>>>.............->...",
"..........^->............->>>>>>.^^^>^->...->....->^->->.........^->.......^->..^^^->>^.^^^->...........^^^^^.............->....",
"..........^->............->>>>>>.^^^>^->...->....->^->->.........^->.......^->..^^^->>^.^^^->...........^^^^^.............->....",
"..........^^............^^->^->...->>>>...->..->^->.^->..........^^->......->.....->^->...^->......->..............->....->.....",
"..........^^............^^->^->...->>>>...->..->^->.^->..........^^->......->.....->^->...^->......->..............->....->.....",
"...........->............^^.^^...^^^>....->..^->^^->>>............^^->....->.....^^.^^....^^......^->.............^->...->......",
"...........->............^^.^^...^^^>....->..^->^^->>>............^^->....->.....^^.^^....^^......^->.............^->...->......",
"..........^^.......................^....^^...^^..^^^^..............^^....^^.......................->..............^^....^.......",
"..........^^.......................^....^^...^^..^^^^..............^^....^^.......................->..............^^....^.......",
"...->>.....->.....->>>....->>>...->.^->..->>>>>....->>...->>>>>...->>>....->>>......................->............->......->>>..",
"...->>.....->.....->>>....->>>...->.^->..->>>>>....->>...->>>>>...->>>....->>>......................->............->......->>>..",
"..->^->...->>....->^^->..->^^->.^->.^->.^->^^^....->^...^^^^^->..->^^->..->^^->....................->............^^->....->^^->.",
"..->^->...->>....->^^->..->^^->.^->.^->.^->^^^....->^...^^^^^->..->^^->..->^^->....................->............^^->....->^^->.",
".->.^->..^^->...^^..^->.^^..^->.^->.^->.^->>>>...->.........->..^->.^->.^->.^->....->......->.....->.....->>>>....^^->..^^..^->.",
".->.^->..^^->...^^..^->.^^..^->.^->.^->.^->>>>...->.........->..^->.^->.^->.^->....->......->.....->.....->>>>....^^->..^^..^->.",
"^->.^->...^->......->>.....->>..^->>>>>.^^^^^->.^->>>>.....^->..^^->>>..^^->>>>...^->.....^->....->.....^^^^^......^^->.....->..",
"^->.^->...^->......->>.....->>..^->>>>>.^^^^^->.^->>>>.....^->..^^->>>..^^->>>>...^->.....^->....->.....^^^^^......^^->.....->..",
"^->.^->...^->.....->^.....^^^->.^^^^^->.....^->.^->^^->....->....->^^->..^^^^->...^^......^^....^^->................->.....->...",
"^->.^->...^->.....->^.....^^^->.^^^^^->.....^->.^->^^->....->....->^^->..^^^^->...^^......^^....^^->................->.....->...",
"^->.^->...^->....->......->.^->.....^->..->.^->.^->.^->...^->...^->.^->.....->.....->......->....^^->....->>>>.....->.....^^....",
"^->.^->...^->....->......->.^->.....^->..->.^->.^->.^->...^->...^->.^->.....->.....->......->....^^->....->>>>.....->.....^^....",
"^^->>>....->>>..^->>>>>.^^->>>......^->.^^->>>..^^->>>....^->...^^->>>....->>.....^->.....^->.....^^->..^^^^^.....->.......->...",
"^^->>>....->>>..^->>>>>.^^->>>......^->.^^->>>..^^->>>....^->...^^->>>....->>.....^->.....^->.....^^->..^^^^^.....->.......->...",
".^^^^....^^^^...^^^^^^...^^^^.......^^...^^^^....^^^^.....^^.....^^^^....^^^......^^......->.......^^............^^.......^^....",
".^^^^....^^^^...^^^^^^...^^^^.......^^...^^^^....^^^^.....^^.....^^^^....^^^......^^......->.......^^............^^.......^^....",
"..->>>.....->....->>>>....->>>...->>>>...->>>>>..->>>>>...->>>...->..->...->>>.....->>>..->..->..->......->...->.->..->...->>>..",
"..->>>.....->....->>>>....->>>...->>>>...->>>>>..->>>>>...->>>...->..->...->>>.....->>>..->..->..->......->...->.->..->...->>>..",
".->^^->...->>>..^->^^->..->^^->.^->^^->.^->^^^..^->^^^...->^^->.^->.^->..^^->.....^^^->.^->.^->.^->.....^->>.->>^->>^->..->^^->.",
".->^^->...->>>..^->^^->..->^^->.^->^^->.^->^^^..^->^^^...->^^->.^->.^->..^^->.....^^^->.^->.^->.^->.....^->>.->>^->>^->..->^^->.",
"^->.->>..->^^->.^->.^->.^->.^^..^->.^->.^->.....^->.....^->.^^..^->.^->...^->.......^->.^->.->..^->.....^->>>>>>^->->->.^->.^->.",
"^->.->>..->^^->.^->.^->.^->.^^..^->.^->.^->.....^->.....^->.^^..^->.^->...^->.......^->.^->.->..^->.....^->>>>>>^->->->.^->.^->.",
"^->^-^>.^->.^->.^->>>>..^->.....^->.^->.^->>>...^->>>...^->.->>.^->>>>>...^->.......^->.^->>>...^->.....^->^>^->^->^->>.^->.^->.",
"^->^-^>.^->.^->.^->>>>..^->.....^->.^->.^->>>...^->>>...^->.->>.^->>>>>...^->.......^->.^->>>...^->.....^->^>^->^->^->>.^->.^->.",
"^->^->>.^->>>>>.^->^^->.^->.....^->.^->.^->^....^->^....^->^^->.^->^^->...^->....->.^->.^->^->..^->.....^->^.^->^->^^->.^->.^->.",
"^->^->>.^->>>>>.^->^^->.^->.....^->.^->.^->^....^->^....^->^^->.^->^^->...^->....->.^->.^->^->..^->.....^->^.^->^->^^->.^->.^->.",
"^->^^^..^->.^->.^->.^->.^->..->.^->.^->.^->.....^->.....^->..->.^->.^->...^->...^->.^->.^->^^->.^->.....^->..^->^->.^->.^->.^->.",
"^->^^^..^->.^->.^->.^->.^->..->.^->.^->.^->.....^->.....^->..->.^->.^->...^->...^->.^->.^->^^->.^->.....^->..^->^->.^->.^->.^->.",
"^^->>>..^->.^->.^->>>>..^^->>>..^->>>>..^->>>>>.^->.....^^->>>..^->.^->...->>>..^^->>>..^->.^->.^->>>>>.^->..^->^->.^->.^^->>>..",
"^^->>>..^->.^->.^->>>>..^^->>>..^->>>>..^->>>>>.^->.....^^->>>..^->.^->...->>>..^^->>>..^->.^->.^->>>>>.^->..^->^->.^->.^^->>>..",
".^^^^...^^..^^..^^^^^....^^^^...^^^^^...^^^^^^..^^.......^^^^...^^..^^...^^^^....^^^^...^^..^^..^^^^^^..^^...^^.^^..^^...^^^^...",
".^^^^...^^..^^..^^^^^....^^^^...^^^^^...^^^^^^..^^.......^^^^...^^..^^...^^^^....^^^^...^^..^^..^^^^^^..^^...^^.^^..^^...^^^^...",
".->>>>....->>>...->>>>....->>>...->>>>>..->..->..->..->..->...->.->..->..->..->..->>>>>..^>-....^>.......^->.......^............",
".->>>>....->>>...->>>>....->>>...->>>>>..->..->..->..->..->...->.->..->..->..->..->>>>>..^>-....^>.......^->.......^............",
"^->^^->..->^^->.^->^^->..->^^->.^^^->...^->.^->.^->.^->.^->..^->^->.^->.^->.^->.^^^^^->..^>......^>........>......^->...........",
"^->^^->..->^^->.^->^^->..->^^->.^^^->...^->.^->.^->.^->.^->..^->^->.^->.^->.^->.^^^^^->..^>......^>........>......^->...........",
"^->.^->.^->.^->.^->.^->.^->.^^....^->...^->.^->.^->.^->.^->..^->^^->->..^->.^->.....->...^>.......^>.......>.....^-.^>..........",
"^->.^->.^->.^->.^->.^->.^->.^^....^->...^->.^->.^->.^->.^->..^->^^->->..^->.^->.....->...^>.......^>.......>.....^-.^>..........",
"^->>>>..^->.^->.^->>>>..^^->>>....^->...^->.^->.^->.^->.^->.>^->.^^->...^^->>>.....->....^>........^>......>....^-...^>.........",
"^->>>>..^->.^->.^->>>>..^^->>>....^->...^->.^->.^->.^->.^->.>^->.^^->...^^->>>.....->....^>........^>......>....^-...^>.........",
"^->^^...^->.^->.^->.^->..^^^^->...^->...^->.^->.^->.^->.^->>>>->..->->...^^->.....->.....^>.........^-.....>....................",
"^->^^...^->.^->.^->.^->..^^^^->...^->...^->.^->.^->.^->.^->>>>->..->->...^^->.....->.....^>.........^-.....>....................",
"^->.....^->.>>>.^->.^->..->.^->...^->...^->.^->.^^->>>..^->>^->>.->^^->...^->....->......^>..........^-....>....................",
"^->.....^->.>>>.^->.^->..->.^->...^->...^->.^->.^^->>>..^->>^->>.->^^->...^->....->......^>..........^-....>....................",
"^->.....^^->->..^->.^->.^^->>>....^->...^^->>>...^^->...^->.^^->^->.^->...^->...^->>>>>..^>-..........^..^->....................",
"^->.....^^->->..^->.^->.^^->>>....^->...^^->>>...^^->...^->.^^->^->.^->...^->...^->>>>>..^>-..........^..^->....................",
"^^.......^^^^->.^^..^^...^^^^.....^^.....^^^^.....^^....^^...^^.^^..^^....^^....^^^^^^..................................>>>>>>>>",
"^^.......^^^^->.^^..^^...^^^^.....^^.....^^^^.....^^....^^...^^.^^..^^....^^....^^^^^^..................................>>>>>>>>",
".^>..............->..................->............->>...........->........->........->..->.......->>...........................",
".^>..............->..................->............->>...........->........->........->..->.......->>...........................",
".^>^............^->.................^->...........->^->.........^->.......^^........^^..^->......^^->...........................",
".^>^............^->.................^->...........->^->.........^->.......^^........^^..^->......^^->...........................",
"..^-......->>>..^->.......->>>......^->...->>>...^->^^....->>>>.^->.......->>.......->>.^->..->...^->....->>.>>..->>>>....->>>..",
"..^-......->>>..^->.......->>>......^->...->>>...^->^^....->>>>.^->.......->>.......->>.^->..->...^->....->>.>>..->>>>....->>>..",
".........^^^^->.^->>>>...->^^->...->>>>..->^^->..->>>....->^^->.^->>>>...^^->......^^->.^->.->....^->...^->>>>>>^->^^->..->^^->.",
".........^^^^->.^->>>>...->^^->...->>>>..->^^->..->>>....->^^->.^->>>>...^^->......^^->.^->.->....^->...^->>>>>>^->^^->..->^^->.",
"..........->>>>.^->^^->.^->.^^...->^^->.^->>>>>.^^->....^->.^->.^->^^->...^->.......^->.^->>>.....^->...^->^>^->^->.^->.^->.^->.",
"..........->>>>.^->^^->.^->.^^...->^^->.^->>>>>.^^->....^->.^->.^->^^->...^->.......^->.^->>>.....^->...^->^>^->^->.^->.^->.^->.",
".........->^^->.^->.^->.^->..->.^->.^->.^->^^^...^->....^^->>>>.^->.^->...^->....->.^->.^->^->....^->...^->^.^->^->.^->.^->.^->.",
".........->^^->.^->.^->.^->..->.^->.^->.^->^^^...^->....^^->>>>.^->.^->...^->....->.^->.^->^->....^->...^->^.^->^->.^->.^->.^->.",
"........^^->>>>.^->>>>..^^->>>..^^->>>>.^^->>>...^->.....^^^^->.^->.^->...->>>..^->.^->.^->^^->...->>>..^->..^->^->.^->.^^->>>..",
"........^^->>>>.^->>>>..^^->>>..^^->>>>.^^->>>...^->.....^^^^->.^->.^->...->>>..^->.^->.^->^^->...->>>..^->..^->^->.^->.^^->>>..",
".........^^^^^..^^^^^....^^^^....^^^^^...^^^^....^^.......->>>..^^..^^...^^^^...^^->>>..^^..^^...^^^^...^^...^^.^^..^^...^^^^...",
".........^^^^^..^^^^^....^^^^....^^^^^...^^^^....^^.......->>>..^^..^^...^^^^...^^->>>..^^..^^...^^^^...^^...^^.^^..^^...^^^^...",
"..................................->........................................................->.....->.....->......->>.->........",
"..................................->........................................................->.....->.....->......->>.->........",
".................................^->.......................................................->.....^->....^^->....->^->>....^>...",
".................................^->.......................................................->.....^->....^^->....->^->>....^>...",
".->>>>....->>>>..->>>>....->>>...->>>....->..->..->..->..->...->.->..->..->..->..->>>>>....->.....^->.....^->...^^.^^^....^-->..",
".->>>>....->>>>..->>>>....->>>...->>>....->..->..->..->..->...->.->..->..->..->..->>>>>....->.....^->.....^->...^^.^^^....^-->..",
"^->^^->..->^^->.^->^^->..->^^...^^->....^->.^->.^->.^->.^->.>^->^^->->..^->.^->.^^^^->....->......^^......^^->...........^->^->.",
"^->^^->..->^^->.^->^^->..->^^...^^->....^->.^->.^->.^->.^->.>^->^^->->..^->.^->.^^^^->....->......^^......^^->...........^->^->.",
"^->.^->.^->.^->.^->.^^..^^->>>...^->....^->.^->.^->.^->.^->>>>->.^^->...^->.^->....->....^^->......->......->...........^->..^->",
"^->.^->.^->.^->.^->.^^..^^->>>...^->....^->.^->.^->.^->.^->>>>->.^^->...^->.^->....->....^^->......->......->...........^->..^->",
"^->>>>..^^->>>>.^->......^^^^->..^->.->.^->.^->.^^->>>..^->>^->>..->->..^^->>>>...->......^->.....^->.....^->...........^->..^->",
"^->>>>..^^->>>>.^->......^^^^->..^->.->.^->.^->.^^->>>..^->>^->>..->->..^^->>>>...->......^->.....^->.....^->...........^->..^->",
"^->^^....^^^^->.^->.......->>>...^^->>..^^->>>>..^^->...^->.^^->.->^^->..^^^^->..->>>>>...^^->....^->.....->............^------>",
"^->^^....^^^^->.^->.......->>>...^^->>..^^->>>>..^^->...^->.^^->.->^^->..^^^^->..->>>>>...^^->....^->.....->............^------>",
"^->.........^->.^^.......^^^^.....^^^....^^^^^....^^....^^...^^.^^..^^....->>>..^^^^^^.....^^.....^^.....^^.....................",
"^->.........^->.^^.......^^^^.....^^^....^^^^^....^^....^^...^^.^^..^^....->>>..^^^^^^.....^^.....^^.....^^.....................",
"................                                                                                                                ",
"....>>..........                                                                                                                ",
"..>>>>..........                                                                                                                ",
">>---->>>>>>>>>>                                                                                                                ",
";;;;--;;;;;;;;;;                                                                                                                ",
"..;;;;..........                                                                                                                ",
"....;;..........                                                                                                                ",
"................                                                                                                                ",
"................                                                                                                                ",
"..........>>....                                                                                                                ",
"..........>>>>..                                                                                                                ",
">>>>>>>>------>>                                                                                                                ",
";;;;;;;;;;--;;;;                                                                                                                ",
"..........;;;;..                                                                                                                ",
"..........;;....                                                                                                                ",
"................                                                                                                                "};

static struct ascii {
        struct images *images;
} Ascii;

static Uint32 ascii_decode_color(char clr)
{
        switch (clr) {
        case 'w': return White;
        case 'B': return Black;
        case 'r': return TextRed;
        case 'g': return TextGreen;
        case 'b': return TextBlue;
        case 'y': return TextYellow;
        case 'c': return TextCyan;
        case 'm': return TextMagenta;
        case 'G': return Gray;
        default:
                warn("Color '%c' unknown\n", clr);
                return White;
        }
}

static void ascii_blit_colored_16(SDL_Surface *src, SDL_Rect *srcrect,
                               SDL_Surface *dst, SDL_Rect *dstrect,
                               Uint32 color)
{
        Uint16 mask = color;
        Uint16 *srcpix, *dstpix;
        int x=0, y=0;

        assert(dst->format->BitsPerPixel==16);

        for (y=0; y<dstrect->h; y++) {

                srcpix = (Uint16*)src->pixels 
                        + (srcrect->y+y)*src->w 
                        + srcrect->x;
                dstpix = (Uint16*)dst->pixels 
                        + (dstrect->y+y)*dst->w 
                        + dstrect->x;

                for (x=0; x<dstrect->w; x++) {
                        *dstpix = *srcpix&mask;
                        srcpix++;
                        dstpix++;
                }
        }
}

static void ascii_blit_colored_32(SDL_Surface *src, SDL_Rect *srcrect,
                               SDL_Surface *dst, SDL_Rect *dstrect,
                               Uint32 color)
{
        Uint32 mask = color;
        Uint32 *srcpix, *dstpix;
        int x=0, y=0;

        assert(dst->format->BitsPerPixel==32);

        for (y=0; y<dstrect->h; y++) {

                srcpix = (Uint32*)src->pixels 
                        + (srcrect->y+y)*src->w 
                        + srcrect->x;
                dstpix = (Uint32*)dst->pixels 
                        + (dstrect->y+y)*dst->w 
                        + dstrect->x;

                for (x=0; x<dstrect->w; x++) {
                        *dstpix = *srcpix&mask;
                        srcpix++;
                        dstpix++;
                }
        }
}

static void ascii_blit_colored(SDL_Surface *src, SDL_Rect *srcrect,
                             SDL_Surface *dst, SDL_Rect *dstrect,
                             Uint32 color)
{
	assert(src->format->BitsPerPixel==dst->format->BitsPerPixel);
        assert(dstrect->w==srcrect->w);
        assert(dstrect->h==srcrect->h);

        switch (dst->format->BitsPerPixel) {
        case 16:
                ascii_blit_colored_16(src, srcrect, dst, dstrect, color);
                break;
        case 32:
                ascii_blit_colored_32(src, srcrect, dst, dstrect, color);
                break;
        default:
                err("ascii_blit_colored: unsupported BitsPerPixel: %d\n",
                    dst->format->BitsPerPixel);
                break;
        }
}

static void ascii_paint_colored(unsigned char c, int x, int y, 
                              SDL_Surface *surf, Uint32 color)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Ascii.images);

	if (c == '\t')
		c = ' ';

	assert(c >= ' ');

        if (c<' ') {
                warn("c==%d\n", c);
                c='?';
        }

	/* fixme -- put these calcs in a table or something. Don't need to do
	 * it every time. */
	col = c % Ascii.images->cols;
	row = c / Ascii.images->cols;

	src.x = (col * ASCII_W) + Ascii.images->offx;
	src.y = (row * ASCII_H) + Ascii.images->offy;
	src.w = ASCII_W;
	src.h = ASCII_H;

	dest.x = x;
	dest.y = y;
	dest.w = ASCII_W;
	dest.h = ASCII_H;

        ascii_blit_colored(Ascii.images->images, &src, 
                         surf, &dest, 
                         color);
}

static void ascii_paint_default(unsigned char c, int x, int y, 
                              SDL_Surface * surf)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Ascii.images);

	if (c == '\t')
		c = ' ';

	/* fixme -- put these calcs in a table or something. Don't need to do
	 * it every time. */
	col = c % Ascii.images->cols;
	row = c / Ascii.images->cols;

	src.x = (col * ASCII_W) + Ascii.images->offx;
	src.y = (row * ASCII_H) + Ascii.images->offy;
	src.w = ASCII_W;
	src.h = ASCII_H;

	dest.x = x;
	dest.y = y;
	dest.w = ASCII_W;
	dest.h = ASCII_H;

	SDL_BlitSurface(Ascii.images->images, &src, surf, &dest);
}

/**
 * Load the emergency ASCII image set from XPM.
 *
 * @returns The images struct ready for use, our 0 if there was some kind of
 * error in loading it.
 */
static struct images *ascii_load_fixed_charset(void)
{
	struct images *images;

	images = new struct images;
        assert(images);
	memset(images, 0, sizeof(*images));

        images->w       = 8;
        images->h       = 16;
        images->offx    = 0;
        images->offy    = 0;
        images->rows    = 9;
        images->cols    = 16;

        images->images = IMG_ReadXPMFromArray((char**)charset_xpm);
	if (!images->images) {
		err("IMG_ReadXPMFromArray() failed: '%s'\n", SDL_GetError() );
		images_del(images);
		return NULL;
	}
	if (!images_convert2display(images)) {
		images_del(images);
		return NULL;
	}
	return images;
}

int ascii_init(void)
{
        char *fname = cfg_get("ascii-image-filename");
        
        /* This lib might be unitialized twice: once early in startup so that
         * error messages can be displayed, and again later after the
         * configuration file has been found and parsed. */
        if (Ascii.images) {
                images_del(Ascii.images);
                Ascii.images = 0;
        }

        if (fname) {
                Ascii.images = images_new(0, 8, 16, 9, 16, 0, 0, fname);
        } else {
                Ascii.images = ascii_load_fixed_charset();
        }
        assert(Ascii.images);

        return 0;
}

void ascii_paint_glyph(glyph_t glyph, int x, int y, SDL_Surface *surf)
{
        char color = glyph_get_color(glyph);
        char cc = glyph_get_char(glyph);
        if ('w' == color) {
                ascii_paint_default(cc, x, y, surf);
        } else {
                ascii_paint_colored(cc, x, y, surf, 
                                    ascii_decode_color(color));
        }
}
