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

```sh
dune exec bin/server.exe 
```
