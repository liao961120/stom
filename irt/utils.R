inv_logit = function(x) 1 / (1 + exp(-x))  # Real -> P
logistic = inv_logit

logit = function(p) log( p / (1 - p) )

rbern = function(prob, n=length(prob)) rbinom( n=n, size=1, prob=prob )

rordlogit = function (n, cutpoints, phi) {
  # Aggregate cutpoints and phi to cumulative prob
  cutpoints = c( cutpoints, Inf )
  p_cum = inv_logit( cutpoints - phi )
  
  # Convert cumulative prob to prob
  p_shift =  c( 0, p_cum )[1:length(p_cum)]
  p = p_cum - p_shift
  
  # Sample according to prob of each point
  sample( 1:length(cutpoints), size=n, replace=T, prob=p )
}

simple_hist = function( x, ylab="Count", lwd=5, col=2, ... ) {
  counts = table(x)
  if ( tolower(ylab) == "prob" )
    counts = counts / length(x)
  plot( counts, ylab=ylab, lwd=lwd, col=col, ... )
}




#' Posterior sample summaries
#'
#' Summarize posterior samples given by rtan::stan() with the shape `(n_sample,
#' n_params)`.
#' 
#' @examples
#' library(rstan)
#' m = "a stanfit object"
#' post = extract( m )
#' sa = post_summ( post$A )
#' str(sa)
post_summ = function( post, ci=c(.055, .945) ) 
  apply( post, 2, function(col) c( mean=mean(col), quantile(col, probs=ci) ) )


#' Function stolen from `rethinking::col.alpha()`
col.alpha = function (acol, alpha = 0.5) {
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255, acol[2]/255, acol[3]/255, alpha)
  acol
}


#' Functions for plotting posterior samples and summaries from `post_summ()`
plot_post = list(
  
  ci = function( post_summary, add=T, ... ) {
    s = post_summary
    if (!add) {
      xlim = c( 0, ncol(s) )
      ylim = max( abs( c(min(s),max(s)) ) ) + .3
      plot( 1, type="n", xlim=xlim, ylim=c(-ylim, ylim), ... )
      return()
    }
    for ( i in 1:ncol(s) )
      lines( c(i,i), c(s[2,i], s[3,i]), ... )
  },
  
  mean = function( post_summary, add=T, ... ) {
    s = post_summary
    if (!add) {
      plot( s[1, ], ... )
      return()
    } 
    points( s[1, ], ... )
  },
  
  param_vsep = function( param_ns, col="grey", lty=2, ... ) {
    cutpoints = cumsum( param_ns ) + .5
    abline( v = cutpoints, col="grey", lty=2 )
  },
  
  grand_mean_ref = function( post, params, col="grey", lty=2, ... ) {
    x0 = -2
    for ( i in seq_along(params) ) {
      p = params[i]
      s = post[[p]]
      np = ncol( s )
      y = mean( s )
      x1 = max(0, x0) + np
      segments( x0=x0+.5, y0=y, x1=x1+.5, y1=y, col="grey", lty=2, ... )
      x0 = x1
    }
  },
  
  NULL
)
