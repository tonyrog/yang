//
//  Nif used to tokenize YANG
//

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include "erl_nif.h"

// #define DEBUG

#ifdef DEBUG
#define DBG(...) printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)

#define MAX_TOKEN_SIZE (64*1024)
#define TOKEN_WORD     256
#define TOKEN_STRING   257

typedef struct _token_t {
    struct _token_t* next;
    int type;          // WORD | STRING | <char>
    int line;          // token start line
    ErlNifBinary bin;
} token_t;

typedef enum {
    STATE_WSP,  
    STATE_WSP_SLASH,           // seen /
    STATE_LINE_COMMENT, 
    STATE_BLOCK_COMMENT,
    STATE_BLOCK_COMMENT_STAR,  // seen *
    STATE_WORD,
    STATE_WORD_SLASH,
    STATE_DQUOTE_STRING,
    STATE_DQUOTE_STRING_BSLASH,
    STATE_SQUOTE_STRING
} state_t;

typedef struct {
    int line;       // scanner line number
    int column;     // scanner column
    int t_line;     // line number of current token
    int t_column;   // start column for current token
    int t_skip;     // skip white space flag
    state_t state;  // scanner state
    int n_tokens;   // number of tokens in queue
    token_t* first;
    token_t* last;
    int  pos;
    uint8_t data[MAX_TOKEN_SIZE];
} yang_scanner_t;

// Type names
DECL_ATOM(more);
DECL_ATOM(string);
DECL_ATOM(word);
DECL_ATOM(semi);
DECL_ATOM(curl_left);
DECL_ATOM(curl_right);

ErlNifResourceType* yang_scan_resource;

static int yang_scan_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int yang_scan_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int yang_scan_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info);
static void yang_scan_unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM yang_scan_new(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM yang_scan_next_token(ErlNifEnv* env, int argc, 
					 const ERL_NIF_TERM argv[]);

ErlNifFunc yang_scan_funcs[] =
{
    { "new",         0, yang_scan_new },
    { "next_token",  1, yang_scan_next_token },
    { "next_token",  2, yang_scan_next_token },
};

static ERL_NIF_TERM make_token(ErlNifEnv* env,token_t* t)
{
    ERL_NIF_TERM r;

    switch(t->type) {
    case ';': 
	// {';',<line-number>> }
	r = enif_make_tuple2(env, atm_semi, 
			     enif_make_int(env,t->line));
	return r;
    case '{':
	// {'{',<line-number>> }
	r = enif_make_tuple2(env, atm_curl_left,
			     enif_make_int(env, t->line));
	return r;
    case '}':
	// {'}',<line-number>> }
	r = enif_make_tuple2(env, atm_curl_right,
			     enif_make_int(env, t->line));
	return r;
    case TOKEN_WORD:
	// {word, <linenumber>, binary() }
	r = enif_make_tuple3(env, atm_word,
			     enif_make_int(env, t->line),
			     enif_make_binary(env, &t->bin));
	return r;
		
    case TOKEN_STRING:
	// {string, <linenumber>, binary() }
	r = enif_make_tuple3(env, atm_string,
			     enif_make_int(env, t->line),
			     enif_make_binary(env, &t->bin));
	return r;
    default:
	return enif_make_badarg(env);
    }
}

static void inline add_char(int c, yang_scanner_t* obj)
{
    if (obj->pos < MAX_TOKEN_SIZE)
	obj->data[obj->pos++] = c;
    else {
	fprintf(stderr, "%d:%d: token too long (max=%d)\r\n", 
		obj->t_line, obj->line, MAX_TOKEN_SIZE);
    }

}

static void trim_wsp(yang_scanner_t* obj)
{
    int pos = obj->pos;

    while(pos) {
	switch(obj->data[pos-1]) {
	case ' ':  pos--; break;
	case '\t': pos--; break;
	default: obj->pos = pos; return;
	}
    }
    obj->pos = pos;
}


static token_t* deq_token(yang_scanner_t* obj)
{
    token_t* t;

    if ((t = obj->first) != NULL) {
	if ((obj->first = t->next) == NULL)
	    obj->last = NULL;
	obj->n_tokens--;
    }
    return t;
}

static void enq_token(yang_scanner_t* obj, token_t* t)
{
    if (obj->last)
	obj->last->next = t;
    else
	obj->first = t;
    t->next   = NULL;
    obj->last = t;
    obj->n_tokens++;
}

static void enq_word(yang_scanner_t* obj)
{
    token_t* t = (token_t*) enif_alloc(sizeof(token_t));
    t->type = TOKEN_WORD;
    t->line = obj->t_line;
    enif_alloc_binary(obj->pos, &t->bin);
    memcpy(t->bin.data, obj->data, obj->pos);
    obj->pos = 0;
    enq_token(obj, t);    
}

static void enq_string(yang_scanner_t* obj)
{
    token_t* t = (token_t*) enif_alloc(sizeof(token_t));
    t->type = TOKEN_STRING;
    t->line = obj->t_line;
    enif_alloc_binary(obj->pos, &t->bin);
    memcpy(t->bin.data, obj->data, obj->pos);
    obj->pos = 0;
    enq_token(obj, t);    
}

static void enq_char(yang_scanner_t* obj, int c)
{
    token_t* t = (token_t*) enif_alloc(sizeof(token_t));
    t->type = c;
    t->line = obj->line;
    enq_token(obj, t);
}


static void release_token(ErlNifEnv* env, token_t* t)
{
    if ((t->type == TOKEN_WORD) || (t->type == TOKEN_STRING))
	enif_release_binary(&t->bin);
    enif_free(t);
}

static void yang_scan_resource_dtor(ErlNifEnv* env, yang_scanner_t* obj)
{
    token_t* t;

    t = obj->first; 
    while (t != NULL) {
	token_t* tn = t->next;
	release_token(env, t);
	t = tn;
    }
}


// Create a new scanner resource
ERL_NIF_TERM yang_scan_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    yang_scanner_t* obj;
    ERL_NIF_TERM r;

    if (!(obj = enif_alloc_resource(yang_scan_resource, sizeof(yang_scanner_t))))
	return enif_make_badarg(env);
    obj->line = 1;
    obj->column = 1;
    obj->t_line = 0;
    obj->t_column = 0;
    obj->t_skip = false;
    obj->state = STATE_WSP;
    obj->n_tokens = 0;
    obj->first = NULL;
    obj->last  = NULL;
    obj->pos   = 0;
    r = enif_make_resource(env, obj);
    enif_release_resource(obj);
    return r;
}

// Feed scanner a binary data block
ERL_NIF_TERM yang_scan_next_token(ErlNifEnv* env, int argc, 
				  const ERL_NIF_TERM argv[])
{    
    yang_scanner_t* obj;
    token_t* t;
    uint8_t* ptr;
    uint8_t* ptr_end;
    ErlNifBinary bin;
    state_t state;

#define STATE(st) state_##st: case STATE_##st
#define GOTO(st)  state = STATE_##st; goto state_##st
#define SET_STATE(st) state = STATE_##st

    if (!enif_get_resource(env, argv[0], yang_scan_resource, (void**) &obj)) {
	DBG("argv[0] not a resource\r\n");
	return enif_make_badarg(env);
    }

    state = obj->state;

    if (argc == 2) {
	if (!enif_inspect_binary(env, argv[1], &bin)) {
	    DBG("argv[1] not a binary\r\n");
	    return enif_make_badarg(env);
	}
	ptr = bin.data;
	ptr_end = ptr + bin.size;
	// now scan the data and push tokens

	switch (state) {
	STATE(WSP_SLASH):
	    DBG("WSP_SLASH remain=%ld\r\n", ptr_end - ptr);
	    if (ptr >= ptr_end)
		goto out;
	    else {
		if (*ptr == '/')      { ptr++; GOTO(LINE_COMMENT); }
		else if (*ptr == '*') { ptr++; GOTO(BLOCK_COMMENT); }
		else { obj->t_line = obj->line; GOTO(WORD); }
	    }
	    SET_STATE(WSP);
	    // FALL THROUGH
	STATE(WSP):
	    DBG("WSP remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		switch(c) {
		case '/': GOTO(WSP_SLASH); break;
		case ' ':  obj->column += 1; break;
		case '\t': obj->column += 8; break;
		case '\r': break;
		case '\n': obj->line += 1; obj->column = 1; break;
		case '\"':
		    obj->t_column = obj->column; // save column
		    obj->t_line   = obj->line;   // save start line
		    obj->t_skip   = false;
		    obj->column++;
		    GOTO(DQUOTE_STRING);
		case '\'': 
		    obj->t_column = obj->column; // save column		    
		    obj->t_line   = obj->line;   // save start line
		    obj->column++;
		    GOTO(SQUOTE_STRING);
		case '{':
		case '}':
		case ';':
		    enq_char(obj, c);
		    break;
		default:
		    obj->t_line = obj->line;
		    add_char(c, obj);
		    GOTO(WORD);
		}
	    }
	    break;

	STATE(LINE_COMMENT):
	    DBG("LINE_COMMENT remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		if (c == '\n') {
		    obj->line++;
		    obj->column = 1;
		    GOTO(WSP);
		}
	    }
	    break;

	STATE(BLOCK_COMMENT_STAR):
	    DBG("BLOCK_COMMENT_STAR remain=%ld\r\n", ptr_end - ptr);
	    if (ptr >= ptr_end)
		goto out;
	    else if (*ptr == '/') {
		ptr++; obj->column += 2; GOTO(WSP); 
	    }
	    SET_STATE(BLOCK_COMMENT);
	    // FALL TROUGH
	STATE(BLOCK_COMMENT):
	    DBG("BLOCK_COMMENT remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		if (c == '\n') {
		    obj->line++;
		    obj->column = 1;
		}
		else if (c == '*') {
		    GOTO(BLOCK_COMMENT_STAR);
		}
		else {
		    obj->column++;
		}
	    }
	    break;

	STATE(WORD_SLASH):
	    DBG("WORD_SLASH remain=%ld\r\n", ptr_end - ptr);
	    if (ptr >=  ptr_end)
		goto out;
	    else {
		if (*ptr == '/') {
		    enq_word(obj); ptr++; GOTO(LINE_COMMENT); 
		}
		else if (*ptr == '*') { 
		    enq_word(obj); ptr++; GOTO(BLOCK_COMMENT); 
		}
		else {
		    add_char('/', obj);
		}
	    }
	    SET_STATE(WORD);
	    // FALL THROUGH	    
	STATE(WORD):
	    DBG("WORD remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		switch(c) {
		case '/':  GOTO(WORD_SLASH);
		case ' ':  enq_word(obj); obj->column += 1; GOTO(WSP);
		case '\t': enq_word(obj); obj->column += 8; GOTO(WSP);
		case '\r': enq_word(obj); GOTO(WSP);
		case '\n': case ';': case '{': case '}':
		    enq_word(obj); ptr--; GOTO(WSP);
		default:
		    obj->column++;
		    add_char(c, obj);
		}
	    }
	    break;

	STATE(DQUOTE_STRING_BSLASH):
	    DBG("DQUOTE_STRING_BSLASH remain=%ld\r\n", ptr_end - ptr);
	    if (ptr >= ptr_end) 
		goto out;
	    else {
		int c = *ptr++;
		switch(c) {
		case 'n':  add_char('\n', obj); break;
		case 't':  add_char('\t', obj); break;
		case '"':  add_char(c, obj); break;
		case '\\': add_char(c, obj); break;
		default: 
		    add_char('\\', obj); 
		    add_char(c, obj);
		    break;
		}
		obj->t_skip = false;
	    }
	    SET_STATE(DQUOTE_STRING);
	    // FALL THROUGH
	STATE(DQUOTE_STRING):
	    DBG("DQUOTE_STRING remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		switch(c) {
		case '\\':
		    GOTO(DQUOTE_STRING_BSLASH);
		case '\"':
		    enq_string(obj);
		    obj->column++;
		    GOTO(WSP);
		case '\n':
		    trim_wsp(obj); // remove trailing wsp
		    obj->line++;
		    obj->column = 1;
		    obj->t_skip = true;
		    add_char(c, obj);
		    break;
		case ' ':
		    if (obj->t_skip && (obj->column <= obj->t_column))
			obj->column += 1;
		    else {
			add_char(' ', obj);
			obj->t_skip = false;
			obj->column += 1;
		    }
		    break;
		case '\t':
		    if (obj->t_skip && (obj->column <= obj->t_column))
			obj->column += 8;
		    else {
			add_char('\t', obj);
			obj->t_skip = false;
			obj->column += 8;
		    }
		    break;
		default:
		    add_char(c, obj);
		    obj->column += 1;
		    obj->t_skip = false;
		    break;
		}
	    }
	    break;

	STATE(SQUOTE_STRING):
	    DBG("SQUOTE_STRING remain=%ld\r\n", ptr_end - ptr);
	    while(ptr < ptr_end) {
		int c = *ptr++;
		switch(c) {
		case '\'':
		    enq_string(obj);
		    obj->column++; 
		    GOTO(WSP);
		case '\n':
		    obj->line++;
		    obj->column = 1;
		    // FALL TROUGH
		default:
		    add_char(c, obj);
		    break;
		}
	    }
	    break;

	default:
	    DBG("STATE = %d,  remain=%ld\r\n", state, ptr_end - ptr);
	    return enif_make_badarg(env);
	}
    }

out:
    obj->state = state;

    t = deq_token(obj);
    if (t == NULL) {
	DBG("deq_token return NULL\r\n");
	return atm_more;
    }
    else {
	ERL_NIF_TERM r;
	r = make_token(env, t);
	release_token(env, t);
	return r;
    }
}


static int yang_scan_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) load_info;
    ErlNifResourceFlags tried;

    LOAD_ATOM(more);
    LOAD_ATOM(string);
    LOAD_ATOM(word);
    LOAD_ATOM_STRING(semi, ";");
    LOAD_ATOM_STRING(curl_left, "{");
    LOAD_ATOM_STRING(curl_right, "}");

    DBG("yang_scan_load\r\n");

    yang_scan_resource = enif_open_resource_type(env, 0, "yang_scan",
						 (ErlNifResourceDtor*)
						 yang_scan_resource_dtor,
						 ERL_NIF_RT_CREATE,
						 &tried);
    *priv_data = 0;
    return 0;
}

static int yang_scan_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("yang_scan_reload\r\n");
    return 0;
}

static int yang_scan_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("yang_scan_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void yang_scan_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    DBG("yang_scan_unload\r\n");
}



ERL_NIF_INIT(yang_scan5, yang_scan_funcs,
	     yang_scan_load, yang_scan_reload, 
	     yang_scan_upgrade, yang_scan_unload)
