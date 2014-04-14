-ifndef(_manyTypedefs_types_included).
-define(_manyTypedefs_types_included, yeah).

%% struct struct1

-record(struct1, {myint = undefined :: integer(), 
                  mylist = undefined :: list()}).

%% struct exception1

-record(exception1, {alist = undefined :: list(), 
                     mystruct = #struct1{} :: #struct1{}}).

-endif.
