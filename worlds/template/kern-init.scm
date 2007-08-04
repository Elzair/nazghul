(kern-cfg-set 

 ;; This is the image file for the UI border. The pieces need to be arranged in
 ;; a specific order in this image.
 "frame-image-filename"  "gfx/frame.png"

 ;; These are the letters used by the console, etc, in the UI. The character
 ;; sprites need to be arranged in a specific order in this image.
 "ascii-image-filename"  "gfx/charset.png"

 ;; This is the cursor prompt used by the command window in the UI. It should
 ;; have four animation frames.
 "cursor-image-filename" "gfx/cursor.png"

 ;; This is the script file run when the user selects the "Start New Game"
 ;; option from the main menu.
 "new-game-filename"     "start-new-game.scm"

 ;; This is the script file run when the user selects the "Journey Onward"
 ;; option from the main menu. It lists the current save files.
 "save-game-filename"     "saved-games.scm"

 ;; This is the script file run when the user selects the "Tutorial"
 ;; option from the main menu.
 "tutorial-filename"     "gfx/tutorial.scm"

 ;; These are the filenames of the splash image shown on startup for the
 ;; various supported screen sizes. The format of the key must be
 ;; <width>x<height>-splash-image-filename.
 "1280x960-splash-image-filename" "gfx/splash.png"
 "640x480-splash-image-filename" "gfx/640x480_splash.png"

 )
