#' Title: Simple version of mediation test
#'
#' The template of the monte carlo method in this function is from
#'  https://quantpsy.org/medmc/medmc.htm.
#' This function can handle single level model and multi-level model that
#'  contains at least one fixed path.
#'
#' @param Model a list object containing the parameters of the model
#' (from Mplus output file)
#' @param iv the name of the independent variable
#' @param m the name of the mediator
#' @param dv the name of the dependent variable
#' @param output the name of the output image file (.png)
#' @param rep the number of repetitions (default: 20000)
#' @param conf the critical value used to generate confidence interval
#' (default: 95)
#' @param first_path the name of the first random path, left as 'fixed'
#' when the path is not random (default: 'fixed')
#' @param second_path the name of the second random path, left as 'fixed'
#' when the path is not random (default: 'fixed')
#' @param multi_level a binary variable indicating whether the specified is
#' multi-level (default: FALSE)
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#'
#' @examples
#' # Single level
#' model <- load_model('inst/examples/example_single')
#' simple_version(model, 'age', 'income', 'pain')
#'
#' # 1-1-1 mediation with two fixed paths
#' model <- load_model('inst/examples/fixed_fixed')
#' simple_version(model, 'x', 'm', 'y')
#'
#'
simple_version <- function(Model, iv=NULL, m=NULL, dv=NULL, multi_level=FALSE,
                         first_path='fixed', second_path='fixed',
                         output='Untitled', rep=20000, conf=95) {
  if ((!multi_level) | ((first_path=='fixed') & (second_path=='fixed'))) {
    iv = toupper(iv)
    m = toupper(m)
    dv = toupper(dv)
    a <- Model$parameters$unstandardized %>%
      filter(
        param == iv,
        paramHeader == paste0(m, '.ON')
      )
    a <- a$est
    b <- Model$parameters$unstandardized %>%
      filter(
        param == m,
        paramHeader == paste0(dv, '.ON')
      )
    b <- b$est

    # Single-level model
    if (!multi_level) {
      var_a_loc <- Model$tech1$parameterSpecification$beta[m, iv]
      var_b_loc <- Model$tech1$parameterSpecification$beta[dv, m]
    } else {
    # 1-1-1 mediation with two fixed paths (multi-level)
      var_a_loc <- Model$tech1$parameterSpecification$WITHIN$beta[m, iv]
      var_b_loc <- Model$tech1$parameterSpecification$WITHIN$beta[dv, m]
    }
  } else if ((first_path=='fixed') | (second_path=='fixed')) {
    # 1-1-1 mediation with one fixed path and one random path
    if (first_path!='fixed') {
      first_path = toupper(first_path)
      a <- Model$parameters$unstandardized %>%
        filter(
          param == first_path,
          paramHeader == 'Means'
        )
      a <- a$est
      b <- Model$parameters$unstandardized %>%
        filter(
          param == m,
          paramHeader == paste0(dv, '.ON')
        )
      b <- b$est

      var_a_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, first_path]
      var_b_loc <- Model$tech1$parameterSpecification$WITHIN$beta[dv, m]
    } else {
      second_path = toupper(second_path)
      a <- Model$parameters$unstandardized %>%
        filter(
          param == iv,
          paramHeader == paste0(m, '.ON')
        )
      a <- a$est
      b <- Model$parameters$unstandardized %>%
        filter(
          param == second_path,
          paramHeader == 'Means'
        )
      b <- b$est

      var_a_loc <- Model$tech1$parameterSpecification$WITHIN$beta[m, iv]
      var_b_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, second_path]
    }
  }

  var_a <- Model$tech3$paramCov[var_a_loc, var_a_loc]
  var_b <- Model$tech3$paramCov[var_b_loc, var_b_loc]
  covab <- if (var_a_loc > var_b_loc) {
    Model$tech3$paramCov[var_a_loc, var_b_loc]
  } else {
    Model$tech3$paramCov[var_b_loc, var_a_loc]
  }

  pest=c(a,b)
  acov <- matrix(c(
    var_a, covab,
    covab, var_b
  ),2,2)
  mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
  ab <- mcmc[,1]*mcmc[,2]
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
