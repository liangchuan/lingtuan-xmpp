-ifndef(_optionalRequiredTest_types_included).
-define(_optionalRequiredTest_types_included, yeah).

%% struct oldSchool

-record(oldSchool, {im_int = undefined :: integer(), 
                    im_str = undefined :: string(), 
                    im_big = [] :: list()}).

%% struct simple

-record(simple, {im_default = undefined :: integer(), 
                 im_required = undefined :: integer(), 
                 im_optional = undefined :: integer()}).

%% struct tricky1

-record(tricky1, {im_default = undefined :: integer()}).

%% struct tricky2

-record(tricky2, {im_optional = undefined :: integer()}).

%% struct tricky3

-record(tricky3, {im_required = undefined :: integer()}).

%% struct complex

-record(complex, {cp_default = undefined :: integer(), 
                  cp_required = undefined :: integer(), 
                  cp_optional = undefined :: integer(), 
                  the_map = dict:new() :: dict(), 
                  req_simp = #simple{} :: #simple{}, 
                  opt_simp = #simple{} :: #simple{}}).

%% struct manyOpt

-record(manyOpt, {opt1 = undefined :: integer(), 
                  opt2 = undefined :: integer(), 
                  opt3 = undefined :: integer(), 
                  def4 = undefined :: integer(), 
                  opt5 = undefined :: integer(), 
                  opt6 = undefined :: integer()}).

%% struct javaTestHelper

-record(javaTestHelper, {req_int = undefined :: integer(), 
                         opt_int = undefined :: integer(), 
                         req_obj = undefined :: string(), 
                         opt_obj = undefined :: string(), 
                         req_bin = undefined :: string(), 
                         opt_bin = undefined :: string()}).

-endif.
