# Lwt Echo Server
Simple TCP/ UDP echo servers (and clients).

## Prequisites
Install prequisites using opam.
```
opam install core lwt lwt_ppx dune
```

## Build
```
dune build
```

## Run
```
dune exec ./tcp_echo.exe
```
The server listens to localhost port 9000. You can connect to the server using ./tcp_client or tools like netcat/ telnet.
Exit the server/ client by pressing Ctrl-C.