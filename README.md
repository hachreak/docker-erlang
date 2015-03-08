Erlang Examples
===============

How to build docker image?
--------------------------

`docker build -t myerlang .`

Or, you can use my image with:

`docker pull hachreak/erlang`

See details here: https://registry.hub.docker.com/u/hachreak/erlang/


How to run the docker image?
----------------------------

```bash
docker run -i -t -v `pwd`/examples:/var/www:rw --rm myerlang /bin/bash
```


Examples
--------

**factorial.erl**

```bash
# Start Erlang Shell
laptop> erl
# Compile
1> c(factorial)
{ok,factorial}
# Run
2> factorial:fac(5)
120
```

**converter.erl**

```bash
1> converter:convert_length(converter:convert_length({inch, 5})).
{inch,5.0}
```

**list.erl**

```bash
1> list:list_length([1,2,3,4,5,6,7,8]).
8
```

**format_temps.erl**

```bash
1> format_temps:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
         moscow -10 c
      cape_town 21.11111111111111 c
      stockholm -4 c
          paris -2.2222222222222223 c
         london 2.2222222222222223 c
ok
```

**find_list_max.erl**

```bash
1> c(find_list_max).
{ok,find_list_max}
2> find_list_max:list_max([1,9,3,7,5,2,11,8,9]).
11
```

**robustness.erl**

Try to test the robustness of a erlang application.
You find instruction on how to start the test directly inside the file.
