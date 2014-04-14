-ifndef(_denseLinkingTest_types_included).
-define(_denseLinkingTest_types_included, yeah).

%% struct oneOfEachZZ

-record(oneOfEachZZ, {im_true = undefined :: boolean(), 
                      im_false = undefined :: boolean(), 
                      a_bite = undefined :: integer(), 
                      integer16 = undefined :: integer(), 
                      integer32 = undefined :: integer(), 
                      integer64 = undefined :: integer(), 
                      double_precision = undefined :: float(), 
                      some_characters = undefined :: string(), 
                      zomg_unicode = undefined :: string(), 
                      what_who = undefined :: boolean()}).

%% struct bonkZZ

-record(bonkZZ, {type = undefined :: integer(), 
                 message = undefined :: string()}).

%% struct nestingZZ

-record(nestingZZ, {my_bonk = #bonkZZ{} :: #bonkZZ{}, 
                    my_ooe = #oneOfEachZZ{} :: #oneOfEachZZ{}}).

%% struct holyMoleyZZ

-record(holyMoleyZZ, {big = [] :: list(), 
                      contain = sets:new() :: set(), 
                      bonks = dict:new() :: dict()}).

%% struct backwardsZZ

-record(backwardsZZ, {first_tag2 = undefined :: integer(), 
                      second_tag1 = undefined :: integer()}).

%% struct emptyZZ

-record(emptyZZ, {}).

%% struct wrapperZZ

-record(wrapperZZ, {foo = #emptyZZ{} :: #emptyZZ{}}).

%% struct randomStuffZZ

-record(randomStuffZZ, {a = undefined :: integer(), 
                        b = undefined :: integer(), 
                        c = undefined :: integer(), 
                        d = undefined :: integer(), 
                        myintlist = [] :: list(), 
                        maps = dict:new() :: dict(), 
                        bigint = undefined :: integer(), 
                        triple = undefined :: float()}).

-endif.
