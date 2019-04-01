#ifndef RLANG_INTERNAL_UTILS_H
#define RLANG_INTERNAL_UTILS_H


sexp* new_preserved_empty_list();
void signal_soft_deprecated(const char* msg);
sexp* rlang_ns_get(const char* name);
sexp* rlang_enquo(sexp* sym, sexp* frame);
sexp* rlang_callback(sexp* ptr, sexp* data);


#endif
