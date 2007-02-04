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
#ifndef cursor_h
#define cursor_h

#include "object.h"

class Cursor:public Object {
      public:
	Cursor();

	virtual ~ Cursor();
	virtual void init(class ObjectType * type);
	virtual enum MoveResult move(int dx, int dy);
        virtual void relocate(struct place *newplace, int newx, int newy, 
                              bool noStep = true, 
                              struct closure *place_switch_hook = NULL);
        virtual void remove();

        bool is_active(void);
        void setViewportBounded(bool val);
        int getRange();
        void setRange(int val);
	void setOrigin(int x, int y);
        int getOriginX();
        int getOriginY();
        bool inRange(int x, int y);
        void shadeRange(bool val);
        bool isRangeShaded();
        void setZone(struct templ *zone);
        void reset();

      protected:
	int range, bounded, originX, originY;
        bool active;
        bool shade;
        bool useRange;
        bool useZone;
        struct templ *zone;
};

#endif // cursor_h
