-ifndef(_smallTest_types_included).
-define(_smallTest_types_included, yeah).

%% struct goodbyez

-record(goodbyez, {val = 325 :: integer()}).

%% struct boolPasser

-record(boolPasser, {value = true :: boolean()}).

%% struct hello

-record(hello, {simple = 53 :: integer(), 
                complex = dict:from_list([{23,532},{6243,632},{2355,532}]) :: dict(), 
                complexer = dict:new() :: dict(), 
                words = "words" :: string(), 
                thinz = #goodbyez{val = 36632} :: #goodbyez{}}).

%% struct goodbye

-record(goodbye, {simple = undefined :: integer(), 
                  complex = dict:new() :: dict(), 
                  complexer = dict:new() :: dict()}).

-endif.
