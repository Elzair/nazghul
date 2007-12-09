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
 ;; option from the main menu. It lists the current save files.
 "save-game-filename"     "saved-games.scm"

 ;; This is the script file run when the user selects the "Tutorial"
 ;; option from the main menu.
 "tutorial-filename"     "tutorial.scm"

 ;; This is the script file which runs the demo scene on startup.
 "demo-filename" "demo.scm"

 ;; These are the filenames of the splash image shown on startup for the
 ;; various supported screen sizes. The format of the key must be
 ;; <width>x<height>-splash-image-filename.
 "1280x960-splash-image-filename" "splash.png"
 "640x480-splash-image-filename" "640x480_splash.png"
 "800x480-splash-image-filename" "640x480_splash.png"

;; This is the image for the sprite pieces of the progress bar.
"progress-bar-image-filename" "progress_bar_image.png"

 )
