/* Copyright (c) 2002 Gordon McNutt */
#ifndef status_h
#define status_h

#ifdef __cplusplus
extern "C" {
#endif

        struct trade_info {
                struct sprite *sprite;
                char *name;
                int quantity;
                int cost;
                void *data;
		char show_quantity:1;
		char show_sprite:1;
        };

        enum StatusScrollDir {
                ScrollUp,
                ScrollDown,
                ScrollRight,
                ScrollLeft,
                ScrollPageUp,
                ScrollPageDown,
        };
								
        enum StatusMode {
                ShowParty,
                SelectCharacter,
                Ztats,
                Ready,
                Use,
                Page,
                Trade,
                MixReagents,
        };
								
        enum StatusSelection {
                Character,
                InventoryItem,
                TradeItem,
                Reagents,
        };

        extern void statusInit(void);
        extern void statusRepaint(void);
        extern void statusFlash(int line, unsigned int color);
								
        extern void statusSetMode(enum StatusMode mode);
        extern enum StatusMode statusGetMode(void);
        extern void statusScroll(enum StatusScrollDir dir);
        extern void *statusGetSelected(enum StatusSelection sel);
        extern void statusSelectCharacter(int partyOrderIndex);

        extern void statusSetPageText(char *title, char *text);
	extern void statusSetTradeInfo(int n_trades, struct trade_info *trades);
        extern void statusUpdateTradeInfo(int n_trades, 
                                          struct trade_info *trades);

        extern int status_get_h(void);
        
#ifdef __cplusplus
}
#endif

#endif
