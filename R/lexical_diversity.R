#' Lexical diversity measures
#'
#' @export
TTR = function(x) {
    V = length(unique(x))
    N = length(x)
    exp(log(V) - log(N))
}


#' @rdname TTR
#' @export
MATTR = function(x, window=50) {
    l = length(x)
    if (l < window) {
        warning("Corpus size below ", window, ", NA returned.")
    }

    TTRs = sapply(1:(l-window+1), \(s) {
        end = s + window - 1
        t = x[s:end]
        TTR(t)
    })
    mean(TTRs)
}


#' @rdname TTR
#' @export
MSTTR = function(x, seg.len=50) {
    koRpus:::MSTTR.calc(x, segment = seg.len)$MSTTR
}


#' @rdname TTR
#' @export
HDD = function(x, drawn=42) {
    koRpus:::hdd.calc(x, drawn=drawn)$HDD
}


#' @rdname TTR
#' @export
ATTR = function(x, drawn=42) {
    koRpus:::hdd.calc(x, drawn=drawn)$ATTR
}


#' @rdname TTR
#' @export
MTLD = function(x, factor.size=.72) {
    koRpus:::MTLD.calc(x, factor.size = factor.size)$MTLD
}


#' @rdname TTR
#' @export
YuleK = function(x) {
    koRpus:::k.calc(x)
}


#' Implementation of CLAN's VOCD command
#'
#' @param tokens Character. A vector of tokens
#' @param rep Integer. The number of resamplings used for calculating the mean
#'        Type-Token ratio (TTR) at a particular sampled token size
#'        (range from 35 to 50 tokens).
#' @param rng Integer vector. A sequence of sample sizes used for calculating
#'        the D measure. By default, 35:50, which is the values used in the
#'        VOCD program.
#' @param as_CLAN Logical. See details. By default, `TRUE`, which corresponds
#'        to the default behavior in CLAN.
#'
#' @details This function follows the algorithm described in the CLAN manual (2024)
#'          and Durán et al. (2004). The equation below relates the VOCD measure
#'          (i.e., D) to TTR.
#'
#' \deqn{TTR = \frac{D}{N} \left[ \left( 1 + 2 \frac{N}{D} \right)^{\frac{1}{2}} - 1 \right]}
#'
#' Note that in CLAN, VOCD is called thrice.
#' The average of the three D measures is then used as the final
#' D measure. This is done when setting the argument `as_CLAN = TRUE`
#' in `VOCD()`. Otherwise, the D measure will only be calculated once.
#'
#' @references
#' MacWhinney, B. (2000). The CHILDES Project: Tools for Analyzing Talk. 3rd Edition.
#' MacWhinney, Brian. “CLAN Manual.” TalkBank, 2024. https://doi.org/10.21415/T5G10R. (p.111-114)
#' Durán, Pilar, David Malvern, Brian Richards, and Ngoni Chipere. “Developmental Trends in Lexical Diversity.”
#'    Applied Linguistics 25, no. 2 (June 1, 2004): 220–42. https://doi.org/10.1093/applin/25.2.220.
#' @export
VOCD = function(tokens, rep=100, rng=35:50, as_CLAN=T) {
    if (as_CLAN) {
        results = sapply(1:3, \(i) VOCD_1(tokens, rep, rng))
        return(mean(results, na.rm=T))
    }
    VOCD_1(tokens, rep, rng)
}


VOCD_1 = function(tokens, rep=100, rng=35:50) {

    ntk_ttr = sapply(rng, \(size) {
        mean_ttr = ttr_at_size(tokens, size, rep=rep)
        return( c(size, mean_ttr) )
    })

    dat = data.frame(N = ntk_ttr[1,], ttr = ntk_ttr[2,])
    fit = fit_vocd_curve(dat)
    return(extract_nls_fit(fit))
}


extract_nls_fit = function(fit) {
    if (class(fit) == "nls")
        return(coef(fit))
    return(NA)
}


fit_vocd_curve = function(ntk_ttr) {
    f = function(N, D) (D/N) * (sqrt(1 + 2 * N / D) - 1)
    results = tryCatch({
        nls(ttr ~ f(N, D), data=ntk_ttr, start=list(D=35))
    }, error = function(e) {
        return(NA)
    })
    return(results)
}


ttr_at_size = function(tokens, size, rep) {
    n_types = sapply(1:rep, \(i) {
        s = sample(tokens, size, replace = F)
        n = length(unique(s))
        return(n)
    })
    return( sum(n_types) / size / rep )
}








