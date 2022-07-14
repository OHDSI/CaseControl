// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// selectControlsInternal
DataFrame selectControlsInternal(const List& nestingCohorts, const List& cases, const List& visits, const bool firstOutcomeOnly, const int washoutPeriod, const int controlsPerCase, const bool matchOnAge, const double ageCaliper, const bool matchOnGender, const bool matchOnProvider, const bool matchOnCareSite, const bool matchOnVisitDate, const int visitDateCaliper, const bool matchOnTimeInCohort, const int daysInCohortCaliper, const int minAgeDays, const int maxAgeDays, const int seed);
RcppExport SEXP _CaseControl_selectControlsInternal(SEXP nestingCohortsSEXP, SEXP casesSEXP, SEXP visitsSEXP, SEXP firstOutcomeOnlySEXP, SEXP washoutPeriodSEXP, SEXP controlsPerCaseSEXP, SEXP matchOnAgeSEXP, SEXP ageCaliperSEXP, SEXP matchOnGenderSEXP, SEXP matchOnProviderSEXP, SEXP matchOnCareSiteSEXP, SEXP matchOnVisitDateSEXP, SEXP visitDateCaliperSEXP, SEXP matchOnTimeInCohortSEXP, SEXP daysInCohortCaliperSEXP, SEXP minAgeDaysSEXP, SEXP maxAgeDaysSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type nestingCohorts(nestingCohortsSEXP);
    Rcpp::traits::input_parameter< const List& >::type cases(casesSEXP);
    Rcpp::traits::input_parameter< const List& >::type visits(visitsSEXP);
    Rcpp::traits::input_parameter< const bool >::type firstOutcomeOnly(firstOutcomeOnlySEXP);
    Rcpp::traits::input_parameter< const int >::type washoutPeriod(washoutPeriodSEXP);
    Rcpp::traits::input_parameter< const int >::type controlsPerCase(controlsPerCaseSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnAge(matchOnAgeSEXP);
    Rcpp::traits::input_parameter< const double >::type ageCaliper(ageCaliperSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnGender(matchOnGenderSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnProvider(matchOnProviderSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnCareSite(matchOnCareSiteSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnVisitDate(matchOnVisitDateSEXP);
    Rcpp::traits::input_parameter< const int >::type visitDateCaliper(visitDateCaliperSEXP);
    Rcpp::traits::input_parameter< const bool >::type matchOnTimeInCohort(matchOnTimeInCohortSEXP);
    Rcpp::traits::input_parameter< const int >::type daysInCohortCaliper(daysInCohortCaliperSEXP);
    Rcpp::traits::input_parameter< const int >::type minAgeDays(minAgeDaysSEXP);
    Rcpp::traits::input_parameter< const int >::type maxAgeDays(maxAgeDaysSEXP);
    Rcpp::traits::input_parameter< const int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(selectControlsInternal(nestingCohorts, cases, visits, firstOutcomeOnly, washoutPeriod, controlsPerCase, matchOnAge, ageCaliper, matchOnGender, matchOnProvider, matchOnCareSite, matchOnVisitDate, visitDateCaliper, matchOnTimeInCohort, daysInCohortCaliper, minAgeDays, maxAgeDays, seed));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_CaseControl_selectControlsInternal", (DL_FUNC) &_CaseControl_selectControlsInternal, 18},
    {NULL, NULL, 0}
};

RcppExport void R_init_CaseControl(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
