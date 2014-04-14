-ifndef(_javaBeansTest_types_included).
-define(_javaBeansTest_types_included, yeah).

%% struct oneOfEachBeans

-record(oneOfEachBeans, {boolean_field = undefined :: boolean(), 
                         a_bite = undefined :: integer(), 
                         integer16 = undefined :: integer(), 
                         integer32 = undefined :: integer(), 
                         integer64 = undefined :: integer(), 
                         double_precision = undefined :: float(), 
                         some_characters = undefined :: string(), 
                         base64 = undefined :: string(), 
                         byte_list = [] :: list(), 
                         i16_list = [] :: list(), 
                         i64_list = [] :: list()}).

-endif.
