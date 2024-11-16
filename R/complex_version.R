#' Title: Complex version of mediation test
#'
#' The template of the monte carlo method in this function is from
#'  https://quantpsy.org/medmc/medmc.htm.
#' This function can handle multi-level model
#'  (1-1-1 mediation with two random paths).
#'
#' @param Model a list object containing the parameters of the model
#' (from Mplus output file)
#' @param first_path the name of the first random path (should not be 'fixed')
#' @param second_path the name of the second random path (should not be 'fixed')
#' @param output the name of the output image file (.png)
#' @param rep the number of repetitions (default: 20000)
#' @param conf the critical value used to generate confidence interval
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#'
#' @examples
#' # 1-1-1 mediation with two random paths
#' model <- load_model('inst/examples/random_random')
#' simple_version(model, 's1', 's2', output='random_random')
#'
complex_version <- function(Model, first_path=NULL, second_path=NULL,
                           output='Untitled', rep=20000, conf=95) {
  s_mx = toupper(first_path)
  s_ym = toupper(second_path)
  a <- Model$parameters$unstandardized %>%
    filter(
      param == s_mx,
      paramHeader == 'Means'
    )
  a <- a$est
  b <- Model$parameters$unstandardized %>%
    filter(
      param == s_ym,
      paramHeader == 'Means'
    )
  b <- b$est
  covajbj <- Model$parameters$unstandardized %>%
    filter(
      param == s_ym,
      paramHeader == paste0(s_mx, '.WITH')
    )
  covajbj <- covajbj$est

  var_a_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, s_mx]
  var_b_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, s_ym]
  var_tau_loc <- if (is.na(Model$tech1$parameterSpecification$BETWEEN$psi[s_ym, s_mx])) {
    Model$tech1$parameterSpecification$BETWEEN$psi[s_ym, s_mx]
  } else {
    Model$tech1$parameterSpecification$BETWEEN$psi[s_mx, s_ym]
  }
  vara <- Model$tech3$paramCov[var_a_loc, var_a_loc]
  varb <- Model$tech3$paramCov[var_b_loc, var_b_loc]
  covab <- if (var_a_loc > var_b_loc) {
    Model$tech3$paramCov[var_a_loc, var_b_loc]
  } else {
    Model$tech3$paramCov[var_b_loc, var_a_loc]
  }
  varcovajbj <- Model$tech3$paramCov[var_tau_loc, var_tau_loc]

  dvec=rnorm(rep)
  avec=dvec*sqrt(vara)+a
  bvec=dvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=rnorm(rep)*sqrt(varcovajbj)+covajbj
  ab=avec*bvec+cvec
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(ab,low)
  UL=quantile(ab,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)

  png(paste0(output, '.png'), width = 800, height = 600)
  hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()
}
