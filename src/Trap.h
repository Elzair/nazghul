/* Copyright (c) 2002 Gordon McNutt */
#ifndef Trap_h
#define Trap_h

#include "object.h"

class TrapType:public ObjectType {
      public:
								virtual bool isType(int classID);
								virtual int getType();
								TrapType();
	 virtual ~ TrapType();
								virtual void setEffects(int val);
								virtual void setAmount(int val);
								virtual int getEffects();
								virtual int getAmount();

      protected:
								int effects;
								int amount;
};

#endif				// Trap_h
