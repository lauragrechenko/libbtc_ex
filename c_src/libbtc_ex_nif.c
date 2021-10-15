#include <erl_nif.h>
#include <stdint.h>
#include <string.h>

#include "tool.h"
#include "chainparams.h"

const btc_chainparams btc_chainparams_main = {
    "main",
    0x00,
    0x05,
    "bc",
    0x80,
    0x0488ADE4,
    0x0488B21E,
    {0xf9, 0xbe, 0xb4, 0xd9},
    {0x6f, 0xe2, 0x8c, 0x0a, 0xb6, 0xf1, 0xb3, 0x72, 0xc1, 0xa6, 0xa2, 0x46, 0xae, 0x63, 0xf7, 0x4f, 0x93, 0x1e, 0x83, 0x65, 0xe1, 0x5a, 0x08, 0x9c, 0x68, 0xd6, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00},
    8333,
    {{"seed.bitcoin.jonasschnelli.ch"}, 0},
};

static ERL_NIF_TERM error_result(ErlNifEnv* env, char* error_msg);
static ERL_NIF_TERM ok_result(ErlNifEnv* env, ERL_NIF_TERM *r);

static ERL_NIF_TERM derive_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned char* output;
	ErlNifBinary root_key, path;
	ERL_NIF_TERM result;
    
    unsigned int maxlen = 1024;
    int posanum = -1;
    int posbnum = -1;
    int end = -1;
    uint64_t from = 0;
    uint64_t to = 0;
	
    size_t sizeout = 128;
    char newextkey[sizeout];

    if (!enif_inspect_binary(env, argv[0], &root_key)) {
       return enif_make_badarg(env);
    }

	if (!enif_inspect_binary(env, argv[1], &path)) {
       return enif_make_badarg(env);
    }
    
    char* keypath = (char*)(path.data);
    char* pkey = (char*)root_key.data; 
 	
    const btc_chainparams* chain = &btc_chainparams_main;

    output = enif_make_new_binary(env, 128, &result);
//----------------------------------------------------------------------------------
        static char digits[] = "0123456789";
        for (unsigned int i = 0; i<strlen(keypath); i++) {
            if (i > maxlen) {
                break;
            }
            if (posanum > -1 && posbnum == -1) {
                if (keypath[i] == '-') {
                    if (i-posanum >= 9) {
                        break;
                    }
                    posbnum = i+1;
                    char buf[9] = {0};
                    memcpy (buf, &keypath[posanum], i-posanum);
                    from = strtoull(buf, NULL, 10);
                }
                else if (!strchr(digits, keypath[i])) {
                    posanum = -1;
                    break;
                }
            }
            else if (posanum > -1 && posbnum > -1) {
                if (keypath[i] == ']' || keypath[i] == ')') {
                    if (i-posbnum >= 9) {
                        break;
                    }
                    char buf[9] = {0};
                    memcpy (buf, &keypath[posbnum], i-posbnum);
                    to = strtoull(buf, NULL, 10);
                    end = i+1;
                    break;
                }
                else if (!strchr(digits, keypath[i])) {
                    posbnum = -1;
                    posanum = -1;
                    break;
                }
            }
            if (keypath[i] == '[' || keypath[i] == '(') {
                posanum = i+1;
            }
        }

        if (end > -1 && from <= to) {
            for (uint64_t i = from; i <= to; i++) {
                char keypathnew[strlen(keypath)+16];
                memcpy(keypathnew, keypath, posanum-1);
                char index[9] = {0};
                sprintf(index, "%lld", (long long)i);
                memcpy(keypathnew+posanum-1, index, strlen(index));
                memcpy(keypathnew+posanum-1+strlen(index), &keypath[end], strlen(keypath)-end);


                if (!hd_derive(chain, pkey, keypathnew, newextkey, sizeout))
                    //return showError("Deriving child key failed\n");
		            return error_result(env, "Deriving child key failed\n");
                else
                    memcpy(output, newextkey, sizeout); 
                    //hd_print_node(chain, newextkey);
            }
        }
        else {
            if (!hd_derive(chain, pkey, keypath, newextkey, sizeout))
                //return showError("Deriving child key failed\n");
		        return error_result(env, "Deriving child key failed\n");
            else
                memcpy(output, newextkey, sizeout); 
                //hd_print_node(chain, newextkey);
        }

//----------------------------------------------------------------------------------
   return result;
}

static ERL_NIF_TERM error_result(ErlNifEnv* env, char* error_msg)
{
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, error_msg, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM ok_result(ErlNifEnv* env, ERL_NIF_TERM *r)
{
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), *r);
}

static ErlNifFunc nif_funcs[] = {
    {"derive", 2, derive_nif},
};

ERL_NIF_INIT(libbtc_ex, nif_funcs, NULL, NULL, NULL, NULL)
