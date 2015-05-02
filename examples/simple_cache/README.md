
### Compile and Execute

```bash
$> cd examples/simple_cache/ebin
$> erlc ../src/*.erl
$> cd ../..
$> erl -pa simple_cache/ebin
1> application:start(simple_cache).
ok
2> simple_cache:insert(pippo, pluto).
ok
3> simple_cache:lookup(pippo).
{ok,pluto}
4> simple_cache:delete(pippo).
ok
5> simple_cache:delete(pippo).
{error,not_found}
```
