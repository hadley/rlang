#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H

namespace rlang {
namespace sxp {


inline
sexp_e kind(sexp* x) {
  return TYPEOF(x);
}

inline
sexp_e kind(const char* x) {
  return Rf_str2type(x);
}

inline
sexp* attrs(sexp* x) {
  return ATTRIB(x);
}
inline
sexp* attr(sexp* x, sexp* attr_sym) {
  return Rf_getAttrib(x, attr_sym);
}

inline
r::size_t length(sexp* x) {
  return Rf_length(x);
}

inline
bool is_null(sexp* x) {
  return x == r::null;
}
inline
bool is_object(sexp* x) {
  return OBJECT(x) != 0;
}

inline
bool is_logical(sexp* x) {
  return TYPEOF(x) == r::logical_t;
}
inline
bool is_integer(sexp* x) {
  return TYPEOF(x) == r::integer_t;
}
inline
bool is_double(sexp* x) {
  return TYPEOF(x) == r::double_t;
}
inline
bool is_complex(sexp* x) {
  return TYPEOF(x) == r::complex_t;
}
inline
bool is_character(sexp* x) {
  return TYPEOF(x) == r::character_t;
}
inline
bool is_bytes(sexp* x) {
  return TYPEOF(x) == r::bytes_t;
}
inline
bool is_list(sexp* x) {
  return TYPEOF(x) == r::list_t;
}

inline
bool inherits(sexp* x, const char* c) {
  return Rf_inherits(x, c);
}


} // namespace sxp
} // namespace rlang

#endif
