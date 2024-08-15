logSumExp = matrixStats::logSumExp

#' Sensitivity and Specificity calculation
#'
#' @param sensitivity Numeric. P(+|P), the probability of a positive test result
#'        given that the result is truly positive.
#' @param specificity Numeric. P(-|N), the probability of a negative test result
#'        given that the result is truly negative.
#' @param prevalence Numeric. P(P), the probability of truly positive results in
#'        the population.
#' @param TP Integer. True positive count.
#' @param TN Integer. True negative count.
#' @param FP Integer. False positive count.
#' @param FN Integer. False negative count.
#' @export
#' @examples
#' post = prob_input(sensitivity=.9, specificity=.97, prevalence=.008)
#' post
#'
#' # Assume there are 2957 positive cases and 77043 negative cases.
#' # The code below computes TP, FP, TN, FN
#' counts = post * c(2957, 2957, 77043, 77043)
#' counts
#'
#' confusion_input(TP=576, FP=2381, TN=76979, FN=64)
prob_input = function(sensitivity, specificity, prevalence) {
    log_sens = log(sensitivity)
    log_spec = log(specificity)
    log_prev = log(prevalence)

    log_prev.sens = log_prev + log_sens
    log_nprev.nspec = log(1 - prevalence) + log(1 - specificity)
    log_P.p = logSumExp( c(log_prev.sens, log_nprev.nspec) )
    log_P.n = logSumExp( c(log_prev + log(1-sensitivity), log(1-prevalence) + log_spec) )
    log_P.T.p = log_prev + log_sens - log_P.p              # P(T|+)
    log_P.F.n = log_spec + log(1 - prevalence) - log_P.n   # P(F|-)

    P_T.p = exp(log_P.T.p)
    P_F.n = exp(log_P.F.n)

    return(c(
        "P(P|+)" = P_T.p,
        "P(N|+)" = 1 - P_T.p,
        "P(N|-)" = P_F.n,
        "P(P|-)" = 1 - P_F.n
    ))
}

# for ( prev in seq(.005, .009, by=.001) ) {
#     post = prob_input(.9, .97, prev)
#     count = post * c(2957, 2957, 77043, 77043)
#     print(prev*100)
#     print(post)
#     print(count)
#     cat('\n')
# }


#' @rdname prob_input
#' @export
confusion_input = function(TP, TN, FP, FN) {
    N = sum(TP, TN, FP, FN)
    prevalence = (TP + FN) / N
    sensitivity = TP / (TP + FN)
    specificity = TN / (FP + TN)
    return(c(
        N = N,
        prevalence = prevalence,
        sensitivity = sensitivity,
        specificity = specificity
    ))
}
