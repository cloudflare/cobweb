# Example

## Usage

In the parent directory, run the following commands.

Compile:
```sh
EM_OUT=example/out.js ./cobweb.sh example/hello.cob
```

In the `example` directory, serve the content statically. The web server should
support WebAssembly. For instance `http-server`:
```sh
http-server .
```

and go to [127.0.0.1:8080](http://127.0.0.1:8080).
