#include <libcob.h>
#include <emscripten.h>

// Cobaul runtime with JavaScript implementations
#define set_http_status js_set_http_status
#define set_http_body js_set_http_body
#define append_http_body js_append_http_body
#define get_http_form js_get_http_form

EM_JS(int, js_set_http_status, (void* code), {
    const get_string = Module.cwrap('get_string', 'string', ['number']);
    globalThis.response.status = parseInt(get_string(code));
});
EM_JS(int, js_append_http_body, (void* body), {
    const get_string = Module.cwrap('get_string', 'string', ['number']);
    globalThis.response.body += get_string(body);
});
EM_JS(int, js_set_http_body, (void* body), {
    const get_string = Module.cwrap('get_string', 'string', ['number']);
    globalThis.response.body = get_string(body);
});
EM_JS(int, js_get_http_form, (void* name), {
    const get_string = Module.cwrap('get_string', 'string', ['number']);
    const v = globalThis.request.params[get_string(name)];
    if (v === undefined) {
        return 0;
    } else {
        return v;
    }
});

const char* get_string(void* ptr) {
    return ptr;
}

#include "/tmp/out.c"

// program entry point
int entry()
{
    cob_init(0, NULL);
    worker();
    return 0;
}
