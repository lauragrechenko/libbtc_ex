#include <erl_nif.h>
#include <stdint.h>
#include <string.h>

#include "secp256k1/include/secp256k1.h"
#include "ecc.h"

#include "tool.h"
#include "chainparams.h"

static ERL_NIF_TERM error_result(ErlNifEnv* env, char* error_msg);
static ERL_NIF_TERM ok_result(ErlNifEnv* env, ERL_NIF_TERM *r);

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    btc_ecc_start();
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    btc_ecc_stop();
    return;
}

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
    char keypath[sizeout];
    char pkey[sizeout];

    if (!enif_inspect_binary(env, argv[0], &root_key) || root_key.size > sizeout-1) {
       return enif_make_badarg(env);
    }

	if (!enif_inspect_binary(env, argv[1], &path) || path.size > sizeout-1) {
       return enif_make_badarg(env);
    }
    
    memcpy(keypath, path.data, path.size);
    keypath[path.size]='\0';
    memcpy(pkey, root_key.data, root_key.size); 
    pkey[root_key.size]='\0';
    
    btc_chainparams* chain = &btc_chainparams_main;
    
    if (strncmp(pkey, "tprv", 4) == 0) {
     chain = &btc_chainparams_test; 
    } else if (strncmp(pkey, "xprv", 4) == 0) {
     chain = &btc_chainparams_main;
    } else
       return enif_make_badarg(env);

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
	            return error_result(env, "Key derivation error");
            else {
                size_t result_len = strlen(newextkey); 
                output = enif_make_new_binary(env, result_len, &result);
                memcpy(output, newextkey, result_len); 
                //hd_print_node(chain, newextkey);
	            return ok_result(env, &result);
            }
        }
    }
    else {
        if (!hd_derive(chain, pkey, keypath, newextkey, sizeout))
	        return error_result(env, "Key derivation error");
        else{
            size_t result_len = strlen(newextkey); 
            output = enif_make_new_binary(env, result_len, &result);
            memcpy(output, newextkey, result_len); 
            //hd_print_node(chain, newextkey);
	        return ok_result(env, &result);
        }
    }
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

ERL_NIF_INIT(libbtc_ex, nif_funcs, &load, NULL, &upgrade, &unload);
