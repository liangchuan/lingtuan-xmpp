-ifndef(_docTest_types_included).
-define(_docTest_types_included, yeah).

-define(docTest_Numberz_ONE, 1).
-define(docTest_Numberz_TWO, 2).
-define(docTest_Numberz_THREE, 3).
-define(docTest_Numberz_FIVE, 5).
-define(docTest_Numberz_SIX, 6).
-define(docTest_Numberz_EIGHT, 8).

%% struct xtruct

-record(xtruct, {string_thing = undefined :: string(), 
                 byte_thing = undefined :: integer(), 
                 i32_thing = undefined :: integer(), 
                 i64_thing = undefined :: integer()}).

%% struct xtruct2

-record(xtruct2, {byte_thing = undefined :: integer(), 
                  struct_thing = #xtruct{} :: #xtruct{}, 
                  i32_thing = undefined :: integer()}).

%% struct insanity

-record(insanity, {userMap = dict:new() :: dict(), 
                   xtructs = [] :: list()}).

%% struct xception

-record(xception, {errorCode = undefined :: integer(), 
                   message = undefined :: string()}).

%% struct xception2

-record(xception2, {errorCode = undefined :: integer(), 
                    struct_thing = #xtruct{} :: #xtruct{}}).

%% struct emptyStruct

-record(emptyStruct, {}).

%% struct oneField

-record(oneField, {field = #emptyStruct{} :: #emptyStruct{}}).

-endif.
