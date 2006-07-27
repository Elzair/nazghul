(kern-cfg-set 

 ;; This is the image file for the UI border. The pieces need to be arranged in
 ;; a specific order in this image.
 "frame-image-filename"  "frame.png"

 ;; These are the letters used by the console, etc, in the UI. The character
 ;; sprites need to be arranged in a specific order in this image.
 "ascii-image-filename"  "charset.png"

 ;; This is the cursor prompt used by the command window in the UI. It should
 ;; have four animation frames.
 "cursor-image-filename" "cursor.png"

 ;; This is the script file run when the user selects the "Start New Game"
 ;; option from the main menu.
 "new-game-filename"     "start-new-game.scm"

 ;; This is the script file run when the user selects the "Journey Onward"
 ;; option from the main menu, and is also the file written when the game is
 ;; saved.
 "save-game-filename"     "save.scm"

 ;; This is the script file run when the user selects the "Tutorial"
 ;; option from the main menu.
 "tutorial-filename"     "tutorial.scm"
 )
