-ifndef(_debugProtoTest_types_included).
-define(_debugProtoTest_types_included, yeah).

-define(debugProtoTest_SomeEnum_ONE, 1).
-define(debugProtoTest_SomeEnum_TWO, 2).

%% struct doubles

-record(doubles, {nan = undefined :: float(), 
                  inf = undefined :: float(), 
                  neginf = undefined :: float(), 
                  repeating = undefined :: float(), 
                  big = undefined :: float(), 
                  small = undefined :: float(), 
                  zero = undefined :: float(), 
                  negzero = undefined :: float()}).

%% struct oneOfEach

-record(oneOfEach, {im_true = undefined :: boolean(), 
                    im_false = undefined :: boolean(), 
                    a_bite = 127 :: integer(), 
                    integer16 = 32767 :: integer(), 
                    integer32 = undefined :: integer(), 
                    integer64 = 10000000000 :: integer(), 
                    double_precision = undefined :: float(), 
                    some_characters = undefined :: string(), 
                    zomg_unicode = undefined :: string(), 
                    what_who = undefined :: boolean(), 
                    base64 = undefined :: string(), 
                    byte_list = [1,2,3] :: list(), 
                    i16_list = [1,2,3] :: list(), 
                    i64_list = [1,2,3] :: list()}).

%% struct bonk

-record(bonk, {type = undefined :: integer(), 
               message = undefined :: string()}).

%% struct nesting

-record(nesting, {my_bonk = #bonk{} :: #bonk{}, 
                  my_ooe = #oneOfEach{} :: #oneOfEach{}}).

%% struct holyMoley

-record(holyMoley, {big = [] :: list(), 
                    contain = sets:new() :: set(), 
                    bonks = dict:new() :: dict()}).

%% struct backwards

-record(backwards, {first_tag2 = undefined :: integer(), 
                    second_tag1 = undefined :: integer()}).

%% struct empty

-record(empty, {}).

%% struct wrapper

-record(wrapper, {foo = #empty{} :: #empty{}}).

%% struct randomStuff

-record(randomStuff, {a = undefined :: integer(), 
                      b = undefined :: integer(), 
                      c = undefined :: integer(), 
                      d = undefined :: integer(), 
                      myintlist = [] :: list(), 
                      maps = dict:new() :: dict(), 
                      bigint = undefined :: integer(), 
                      triple = undefined :: float()}).

%% struct base64

-record(base64, {a = undefined :: integer(), 
                 b1 = undefined :: string(), 
                 b2 = undefined :: string(), 
                 b3 = undefined :: string(), 
                 b4 = undefined :: string(), 
                 b5 = undefined :: string(), 
                 b6 = undefined :: string()}).

%% struct compactProtoTestStruct

-record(compactProtoTestStruct, {a_byte = undefined :: integer(), 
                                 a_i16 = undefined :: integer(), 
                                 a_i32 = undefined :: integer(), 
                                 a_i64 = undefined :: integer(), 
                                 a_double = undefined :: float(), 
                                 a_string = undefined :: string(), 
                                 a_binary = undefined :: string(), 
                                 true_field = undefined :: boolean(), 
                                 false_field = undefined :: boolean(), 
                                 empty_struct_field = #empty{} :: #empty{}, 
                                 byte_list = [] :: list(), 
                                 i16_list = [] :: list(), 
                                 i32_list = [] :: list(), 
                                 i64_list = [] :: list(), 
                                 double_list = [] :: list(), 
                                 string_list = [] :: list(), 
                                 binary_list = [] :: list(), 
                                 boolean_list = [] :: list(), 
                                 struct_list = [] :: list(), 
                                 byte_set = sets:new() :: set(), 
                                 i16_set = sets:new() :: set(), 
                                 i32_set = sets:new() :: set(), 
                                 i64_set = sets:new() :: set(), 
                                 double_set = sets:new() :: set(), 
                                 string_set = sets:new() :: set(), 
                                 binary_set = sets:new() :: set(), 
                                 boolean_set = sets:new() :: set(), 
                                 struct_set = sets:new() :: set(), 
                                 byte_byte_map = dict:new() :: dict(), 
                                 i16_byte_map = dict:new() :: dict(), 
                                 i32_byte_map = dict:new() :: dict(), 
                                 i64_byte_map = dict:new() :: dict(), 
                                 double_byte_map = dict:new() :: dict(), 
                                 string_byte_map = dict:new() :: dict(), 
                                 binary_byte_map = dict:new() :: dict(), 
                                 boolean_byte_map = dict:new() :: dict(), 
                                 byte_i16_map = dict:new() :: dict(), 
                                 byte_i32_map = dict:new() :: dict(), 
                                 byte_i64_map = dict:new() :: dict(), 
                                 byte_double_map = dict:new() :: dict(), 
                                 byte_string_map = dict:new() :: dict(), 
                                 byte_binary_map = dict:new() :: dict(), 
                                 byte_boolean_map = dict:new() :: dict(), 
                                 list_byte_map = dict:new() :: dict(), 
                                 set_byte_map = dict:new() :: dict(), 
                                 map_byte_map = dict:new() :: dict(), 
                                 byte_map_map = dict:new() :: dict(), 
                                 byte_set_map = dict:new() :: dict(), 
                                 byte_list_map = dict:new() :: dict()}).

%% struct singleMapTestStruct

-record(singleMapTestStruct, {i32_map = dict:new() :: dict()}).

%% struct exceptionWithAMap

-record(exceptionWithAMap, {blah = undefined :: string(), 
                            map_field = dict:new() :: dict()}).

%% struct blowUp

-record(blowUp, {b1 = dict:new() :: dict(), 
                 b2 = dict:new() :: dict(), 
                 b3 = dict:new() :: dict(), 
                 b4 = dict:new() :: dict()}).

%% struct reverseOrderStruct

-record(reverseOrderStruct, {first = undefined :: string(), 
                             second = undefined :: integer(), 
                             third = undefined :: integer(), 
                             fourth = undefined :: integer()}).

%% struct structWithSomeEnum

-record(structWithSomeEnum, {blah = undefined :: integer()}).

%% struct testUnion

-record(testUnion, {string_field = undefined :: string(), 
                    i32_field = undefined :: integer(), 
                    struct_field = #oneOfEach{} :: #oneOfEach{}, 
                    struct_list = [] :: list(), 
                    other_i32_field = undefined :: integer(), 
                    enum_field = undefined :: integer(), 
                    i32_set = sets:new() :: set(), 
                    i32_map = dict:new() :: dict()}).

%% struct testUnionMinusStringField

-record(testUnionMinusStringField, {i32_field = undefined :: integer(), 
                                    struct_field = #oneOfEach{} :: #oneOfEach{}, 
                                    struct_list = [] :: list(), 
                                    other_i32_field = undefined :: integer(), 
                                    enum_field = undefined :: integer(), 
                                    i32_set = sets:new() :: set(), 
                                    i32_map = dict:new() :: dict()}).

%% struct comparableUnion

-record(comparableUnion, {string_field = undefined :: string(), 
                          binary_field = undefined :: string()}).

%% struct structWithAUnion

-record(structWithAUnion, {test_union = #testUnion{} :: #testUnion{}}).

%% struct primitiveThenStruct

-record(primitiveThenStruct, {blah = undefined :: integer(), 
                              blah2 = undefined :: integer(), 
                              bw = #backwards{} :: #backwards{}}).

%% struct structWithASomemap

-record(structWithASomemap, {somemap_field = undefined :: dict()}).

%% struct bigFieldIdStruct

-record(bigFieldIdStruct, {field1 = undefined :: string(), 
                           field2 = undefined :: string()}).

%% struct breaksRubyCompactProtocol

-record(breaksRubyCompactProtocol, {field1 = undefined :: string(), 
                                    field2 = #bigFieldIdStruct{} :: #bigFieldIdStruct{}, 
                                    field3 = undefined :: integer()}).

%% struct tupleProtocolTestStruct

-record(tupleProtocolTestStruct, {field1 = undefined :: integer(), 
                                  field2 = undefined :: integer(), 
                                  field3 = undefined :: integer(), 
                                  field4 = undefined :: integer(), 
                                  field5 = undefined :: integer(), 
                                  field6 = undefined :: integer(), 
                                  field7 = undefined :: integer(), 
                                  field8 = undefined :: integer(), 
                                  field9 = undefined :: integer(), 
                                  field10 = undefined :: integer(), 
                                  field11 = undefined :: integer(), 
                                  field12 = undefined :: integer()}).

-endif.
