/* Copyright (c) 2002 Gordon McNutt */
#ifndef Container_h
#define Container_h

#include "object.h"

class TrapType;

class Container:public Object {
      public:
	Container();
	virtual ~ Container();
	virtual void add(class ObjectType * type, int quantity);
	virtual void open();
	virtual void subtract(class ObjectType * type, int quantity);
	virtual struct inv_entry *search(class ObjectType * type);
	virtual bool isTrapped();
	virtual void setTrap(class TrapType * trap);
	virtual class TrapType *getTrap();
	void forEach(void (*fx) (struct inv_entry *, void *), void *);
      protected:
	struct list contents;
	class TrapType *trap;
};

#endif
