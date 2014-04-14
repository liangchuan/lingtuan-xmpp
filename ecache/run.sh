#erl -sname ecache@localhost -config ecache.config -noshell -pa ebin/ -pa deps/*/ebin/ -s ecache_run start & 
erl -sname ecache@localhost -config ecache.config -pa ebin/ -pa deps/*/ebin/ -s ecache_run start

