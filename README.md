# cobaul

Uses GnuCOBOL 2.2.

## Usage with Docker

To avoid setting up the compiler environment locally, you can use a pre-built
Docker image: `xtuc/cobaul`:

```sh
docker run \
  -e EM_OUT \
  -e EM_ARGS \
  -v /tmp/cobol-worker:/root/.emscripten_cache/ \
  -v $PWD:/worker \
  -v $PWD/build:/build \
  xtuc/cobaul \
  /worker/src/worker.cob
```

## Usage

Compile dependecies:
```sh
make all
```

Compile COBOL to WebAssembly:
```sh
./cobaul.sh source.cob
```

## `program-id`

The `program-id` should be `worker` otherwise the program will fail to compile.

## Worker API

Set the HTTP status code:
```cobol
CALL "set_http_status" USING "400".
```

Set the response body to a string:
```cobol
CALL "set_http_body" USING "something".
```

Append a string to the response body:
```cobol
CALL "append_http_body" USING "something".
```

Get HTTP form values:
```cobol
WORKING-STORAGE SECTION.
    01 THE-VALUE PIC S9(9) BINARY.
PROCEDURE DIVISION.
    CALL "get_http_form" USING "key" RETURNING THE-VALUE.
```

## Example

See the [example](./example).
