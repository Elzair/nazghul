/* Copyright (c) 2002 Gordon McNutt */
#ifndef cursor_h
#define cursor_h

#include "object.h"

class Cursor:public Object {
      public:
	Cursor();
	virtual ~ Cursor();
	virtual void init(class ObjectType * type);
	virtual bool move(int dx, int dy);
	virtual void setRange(int range);
	virtual void setOrigin(int x, int y);
      protected:
	int range, originX, originY;
};

extern class Cursor *Cursor;

#endif
