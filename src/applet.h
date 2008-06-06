/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#ifndef applet_h
#define applet_h

/**
 * An applet is a little mini-program that runs in the status window. It is
 * invoked via statusRunApplet. Applets can start and run other applets in a
 * strictly nested fashion.
 */

#include "list.h"

#include <SDL.h>

/**
 * The applet's call table. These functions are called by the status code when
 * it runs the applet.
 */
struct applet_ops {

        /**
         * Run an applet. This is called to start the applet. It should not
         * return until the applet is done. This function is required.
         *
         * @param applet is the applet instance.
         * @param dims gives the screen coordinates (in pixels) of the window
         * the applet occupies. The applet will paint to this window in its
         * paint operation.
         * @param session is the current session (identical to the global
         * variable Session, but using this parameter is preferred for all new
         * code).
         */
        void (*run)(struct applet *applet, SDL_Rect *dims, struct session *session);

        /**
         * Paint the applet. This function is required. The status code calls
         * this function for animation repaints. If the applet wants to repaint
         * itself it should typically call statusRepaint(), which will invoke
         * this function in a standard way.
         *
         * @param applet is the applet instance.
         */
        void (*paint)(struct applet *applet);

        /**
         * Get the desired height (in lines) that the applet wants to use for
         * its window. This is called prior to run, so the status code can
         * resize the window before starting the applet. This function is
         * optional: if the pointer is NULL then the status code will assume
         * the window should be the standard tall mode size.
         *
         * @param applet is the applet instance.
         */
        int (*get_desired_height)(struct applet *applet);
};

/**
 * The applet instance structure. Most applet implementations will extend this
 * structure with their own fields.
 */
struct applet {
        struct list list; /* used to stack applets in the status code */
        SDL_Rect dims; /* window dimensions passed into run may be kept here */
        struct session *session; /* the session pointer passed to run may be kept here */
        struct applet_ops *ops; /* the applet's call table */
};

#endif
