#include <rlang.h>
#include "expr-interp.h"

// Capture

sexp* rlang_ns_get(const char* name);

sexp* capture(sexp* sym, sexp* frame, SEXP* arg_env) {
  static sexp* capture_call = NULL;
  if (!capture_call) {
    sexp* args = KEEP(r_new_node(r_null, r_null));
    capture_call = r_new_call(rlang_ns_get("captureArgInfo"), args);
    r_mark_precious(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  if (r_typeof(sym) != SYMSXP) {
    r_abort("`arg` must be a symbol");
  }

  r_node_poke_cadr(capture_call, sym);
  sexp* arg_info = KEEP(r_eval(capture_call, frame));
  sexp* expr = r_list_get(arg_info, 0);
  sexp* env = r_list_get(arg_info, 1);

  // Unquoting rearranges the expression
  // FIXME: Only duplicate the call tree, not the leaves
  expr = KEEP(r_copy(expr));
  expr = call_interp(expr, env);

  if (arg_env) {
    *arg_env = env;
  }

  FREE(2);
  return expr;
}

sexp* rlang_enexpr(sexp* sym, sexp* frame) {
  return capture(sym, frame, NULL);
}
sexp* rlang_ensym(sexp* sym, sexp* frame) {
  sexp* expr = capture(sym, frame, NULL);

  if (rlang_is_quosure(expr)) {
    expr = rlang_quo_get_expr(expr);
  }

  switch (r_typeof(expr)) {
  case r_type_symbol:
    break;
  case r_type_character:
    if (r_length(expr) == 1) {
      KEEP(expr);
      expr = r_sym(r_chr_get_c_string(expr, 0));
      FREE(1);
      break;
    }
    // else fallthrough
  default:
    r_abort("Only strings can be converted to symbols");
  }

  return expr;
}


sexp* rlang_enquo(sexp* sym, sexp* frame) {
  sexp* env;
  sexp* expr = KEEP(capture(sym, frame, &env));
  sexp* quo = forward_quosure(expr, env);
  FREE(1);
  return quo;
}

sexp* rlang_ext2_is_missing(sexp* _call, sexp* _op, sexp* args, sexp* env) {
  args = r_node_cdr(args);

  sexp* missing = r_eval(r_node_car(args), env);
  if (r_lgl_get(missing, 0)) {
    return r_shared_true;
  }

  return r_lgl(r_eval(r_x_sym, env) == r_missing_sym);
}
