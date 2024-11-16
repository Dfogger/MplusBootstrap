#' Title: Simple version of mediation test for mediated moderation test
#'
#' The template of the monte carlo method in this function is from
#'  https://quantpsy.org/medmc/medmc.htm.
#' This function can handle multi-level mediated moderation model where the
#'  moderator can be specified at either level one or two.
#'
#' @param Model a list object containing the parameters of the model
#' (from Mplus output file)
#' @param iv the name of the independent variable
#' @param m the name of the mediator
#' @param dv the name of the dependent variable
#' @param level_one_mod a binary variable indicating whether the moderation
#' effect is on the first level (default: TRUE)
#' @param w the name of the moderator
#' @param xw the name of the interaction term
#' @param first_path the name of the first random path (should not be 'fixed')
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#'
#' @examples
med_mod <- function(Model, iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL,
                    first_path=NULL, level_one_mod=TRUE) {
  w = toupper(w)
  m = toupper(m)
  # First Level Moderator
  if (level_one_mod) {
    dv = toupper(dv)
    interaction = toupper(xw)
    a <- Model$parameters$unstandardized %>%
      filter(
        param == interaction,
        paramHeader == paste0(dv, '.ON')
      )
    a <- a$est
    b <- Model$parameters$unstandardized %>%
      filter(
        param == w,
        paramHeader == paste0(m, '.ON')
      )
    b <- b$est

    var_a_loc <- Model$tech1$parameterSpecification$WITHIN$beta[dv, interaction]
    var_b_loc <- Model$tech1$parameterSpecification$WITHIN$beta[m, w]

    var_a <- Model$tech3$paramCov[var_a_loc, var_a_loc]
    var_b <- Model$tech3$paramCov[var_b_loc, var_b_loc]
    covab <- if (var_a_loc > var_b_loc) {
      Model$tech3$paramCov[var_a_loc, var_b_loc]
    } else {
      Model$tech3$paramCov[var_b_loc, var_a_loc]
    }
  } else {
    # Second level moderator
    first_path = toupper(first_path)
    a <- Model$parameters$unstandardized %>%
      filter(
        param == w,
        paramHeader == paste0(m, '.ON')
      )
    a <- a$est
    b <- Model$parameters$unstandardized %>%
      filter(
        param == m,
        paramHeader == paste0(first_path, '.ON')
      )
    b <- b$est

    var_a_loc <- Model$tech1$parameterSpecification$BETWEEN$beta[m, w]
    var_b_loc <- Model$tech1$parameterSpecification$BETWEEN$beta[first_path, m]

    var_a <- Model$tech3$paramCov[var_a_loc, var_a_loc]
    var_b <- Model$tech3$paramCov[var_b_loc, var_b_loc]
    covab <- if (var_a_loc > var_b_loc) {
      Model$tech3$paramCov[var_a_loc, var_b_loc]
    } else {
      Model$tech3$paramCov[var_b_loc, var_a_loc]
    }
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
