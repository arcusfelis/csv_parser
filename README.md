CSV parser for Erlang
=====================

__License__: MIT

__Author__: Uvarov Michael ([`cytozoon@gmail.com`](mailto:cytozoon@gmail.com))



Tests
=====

Next command runs tests:

```bash
./rebar eunit skip_deps=true
```


Code examples
=============

* [CSV reader](examples/csv_reader.escript)
    It reads and shows the list of the states from 
    [the file](examples/states.csv).

    To run, use:

    ```
    examples/csv_reader.escript examples/states.csv
    ```
