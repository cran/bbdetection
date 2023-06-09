// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// setpar_filtering_alg
void setpar_filtering_alg(double tr_bull, double tr_bear);
RcppExport SEXP _bbdetection_setpar_filtering_alg(SEXP tr_bullSEXP, SEXP tr_bearSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type tr_bull(tr_bullSEXP);
    Rcpp::traits::input_parameter< double >::type tr_bear(tr_bearSEXP);
    setpar_filtering_alg(tr_bull, tr_bear);
    return R_NilValue;
END_RCPP
}
// run_filtering_alg
LogicalVector run_filtering_alg(const NumericVector index);
RcppExport SEXP _bbdetection_run_filtering_alg(SEXP indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type index(indexSEXP);
    rcpp_result_gen = Rcpp::wrap(run_filtering_alg(index));
    return rcpp_result_gen;
END_RCPP
}
// setpar_dating_alg
void setpar_dating_alg(int t_window, int t_censor, int t_phase, int t_cycle, double max_chng);
RcppExport SEXP _bbdetection_setpar_dating_alg(SEXP t_windowSEXP, SEXP t_censorSEXP, SEXP t_phaseSEXP, SEXP t_cycleSEXP, SEXP max_chngSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type t_window(t_windowSEXP);
    Rcpp::traits::input_parameter< int >::type t_censor(t_censorSEXP);
    Rcpp::traits::input_parameter< int >::type t_phase(t_phaseSEXP);
    Rcpp::traits::input_parameter< int >::type t_cycle(t_cycleSEXP);
    Rcpp::traits::input_parameter< double >::type max_chng(max_chngSEXP);
    setpar_dating_alg(t_window, t_censor, t_phase, t_cycle, max_chng);
    return R_NilValue;
END_RCPP
}
// run_dating_alg
LogicalVector run_dating_alg(const NumericVector index);
RcppExport SEXP _bbdetection_run_dating_alg(SEXP indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type index(indexSEXP);
    rcpp_result_gen = Rcpp::wrap(run_dating_alg(index));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bbdetection_setpar_filtering_alg", (DL_FUNC) &_bbdetection_setpar_filtering_alg, 2},
    {"_bbdetection_run_filtering_alg", (DL_FUNC) &_bbdetection_run_filtering_alg, 1},
    {"_bbdetection_setpar_dating_alg", (DL_FUNC) &_bbdetection_setpar_dating_alg, 5},
    {"_bbdetection_run_dating_alg", (DL_FUNC) &_bbdetection_run_dating_alg, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_bbdetection(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
