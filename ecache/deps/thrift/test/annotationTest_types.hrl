-ifndef(_annotationTest_types_included).
-define(_annotationTest_types_included, yeah).

%% struct foo

-record(foo, {bar = undefined :: integer(), 
              baz = undefined :: integer(), 
              qux = undefined :: integer(), 
              bop = undefined :: integer()}).

-endif.
