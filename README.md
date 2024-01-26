## Scaffolding

Simply run the scaffolding script:

```sh
./scaffold && eval $(opam env)
```

## Development

Assuming you've run the scaffolding script, use `dune build -w` to keep dune running in the background. 

### Running the client

```sh
dune exec bin/client.exe 
```

### Running the server

For all the options see `dune exec bin/server.exe -- --help`. 

An example command for running the server, using the included `database.bootp` ([BOOTP spec, page 11](https://datatracker.ietf.org/doc/html/rfc951#page-11)) file:

```sh
dune exec bin/server.exe -- -p 8080 -d database.bootp
```
