#ifndef RLANG_VECTOR_LGL_H
#define RLANG_VECTOR_LGL_H


int r_as_optional_bool(sexp* lgl);

static inline sexp* r_lgl(bool x) {
  extern SEXP R_TrueValue, R_FalseValue;
  return x ? R_TrueValue : R_FalseValue;
}

bool r_is_true(sexp* x);

static inline sexp* r_shared_lgl(bool x) {
  if (x) {
    return r_shared_true;
  } else {
    return r_shared_false;
  }
}

r_ssize r_lgl_sum(sexp* x, bool na_true);
sexp* r_lgl_which(sexp* x, bool na_propagate);


#endif
