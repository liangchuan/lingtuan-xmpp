-ifndef(_constantsDemo_types_included).
-define(_constantsDemo_types_included, yeah).

-define(constantsDemo_enumconstants_ONE, 1).
-define(constantsDemo_enumconstants_TWO, 2).

%% struct thing

-record(thing, {hello = undefined :: integer(), 
                goodbye = undefined :: integer()}).

%% struct blah

-record(blah, {bing = undefined :: integer()}).

%% struct gak

-record(gak, {}).

-endif.
