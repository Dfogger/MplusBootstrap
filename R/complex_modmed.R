# TO_DO: moderation on the second stage
#' Title: Moderated Mediation test of multilevel model (random-random; moderator at level two)
#' This function can calculate moderated mediation effect of multi-level model that
#'  contains two random paths.
#'
#' @param Model list object containing the parameters of the model
#' (from Mplus output file)
#' @param first_stage_mod a binary variable indicating whether the moderation
#' effect is on the first path (default: TRUE)
#' @param first_path the name of the first random path (should not be 'fixed')
#' @param second_path the name of the second random path (should not be 'fixed')
#' @param w the name of the moderator
#' @param output the name of the output image file (.png)
#' @param rep the number of repetitions (default: 20000)
#' @param conf the critical value used to generate confidence interval
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#'
#' @examples
complex_modmed <- function(Model, first_stage_mod=TRUE,
                           first_path=NULL, second_path=NULL, w=NULL,
                           output='Untitled', rep=20000, conf=95) {
  # random-random
  first_path = toupper(first_path)
  second_path = toupper(second_path)
  w = toupper(w)

  a = Model$parameters$unstandardized %>% filter(
    paramHeader == "Intercepts",
    param == first_path
  )  #the effect of x on m
  a = a$est

  a_loc = Model$tech1$parameterSpecification$BETWEEN$alpha[1, first_path]
  a2 = Model$tech3$paramCov[a_loc, a_loc] #the variance of coefficient a

  b <- Model$parameters$unstandardized %>%
    filter(
      param == second_path,
      paramHeader == 'Means'
    )
  b <- b$est

  c = Model$parameters$unstandardized %>% filter(
    paramHeader == paste0(first_path, ".ON"),
    param == w
  )  #the moderation effect of w on xm
  c = c$est

  c_loc = Model$tech1$parameterSpecification$BETWEEN$beta[first_path, w]
  c2 = Model$tech3$paramCov[c_loc, c_loc]  #the variance of coefficient c

  d <- Model$parameters$unstandardized %>%
    filter(
      param == second_path,
      paramHeader == paste0(first_path, '.WITH')
    )
  d <- d$est

  wsd = Model$parameters$unstandardized %>% filter(
    paramHeader == "Variances",
    param == w
  ) #sd of the moderator
  wsd = sqrt(wsd$est)

  var_a_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, first_path]
  var_b_loc <- Model$tech1$parameterSpecification$BETWEEN$alpha[1, second_path]
  vara <- Model$tech3$paramCov[var_a_loc, var_a_loc]
  varb <- Model$tech3$paramCov[var_b_loc, var_b_loc]
  covab <- if (var_a_loc > var_b_loc) {
    Model$tech3$paramCov[var_a_loc, var_b_loc]
  } else {
    Model$tech3$paramCov[var_b_loc, var_a_loc]
  }

  c_loc = Model$tech1$parameterSpecification$BETWEEN$beta[first_path, w]
  varc = Model$tech3$paramCov[c_loc, c_loc]  #the variance of coefficient c

  var_tau_loc <- if (!is.na(Model$tech1$parameterSpecification$BETWEEN$psi[second_path, first_path])) {
    Model$tech1$parameterSpecification$BETWEEN$psi[second_path, first_path]
  } else {
    Model$tech1$parameterSpecification$BETWEEN$psi[first_path, second_path]
  }
  vard <- Model$tech3$paramCov[var_tau_loc, var_tau_loc]

  # High Level of Moderator
  nvec=rnorm(rep)
  avec=nvec*sqrt(vara)+a
  bvec=nvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=nvec*sqrt(varc)+c
  dvec=rnorm(rep)*sqrt(vard)+d
  amvec=cvec*(wsd)+avec
  ab=amvec*bvec+dvec
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(ab,low)
  UL=quantile(ab,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  png(paste0(output, '_high.png'), width = 800, height = 600)
  hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()

  # Low Level of Moderator
  nvec=rnorm(rep)
  avec=nvec*sqrt(vara)+a
  bvec=nvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=nvec*sqrt(varc)+c
  dvec=rnorm(rep)*sqrt(vard)+d
  amvec=cvec*(-wsd)+avec
  ab=amvec*bvec+dvec
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(ab,low)
  UL=quantile(ab,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  png(paste0(output, '_low.png'), width = 800, height = 600)
  hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()

  # Difference between higher and lower level of Moderator
  nvec=rnorm(rep)
  avec=nvec*sqrt(vara)+a
  bvec=nvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=nvec*sqrt(varc)+c
  dvec=rnorm(rep)*sqrt(vard)+d
  ahmvec=cvec*(wsd)+avec
  almvec=cvec*(-wsd)+avec
  abh=ahmvec*bvec+dvec
  abl=almvec*bvec+dvec
  dev=abh-abl
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(dev,low)
  UL=quantile(dev,upp)
  LL4=format(LL,digits=5)
  UL4=format(UL,digits=5)
  png(paste0(output, '_difference.png'), width = 800, height = 600)
  hist(dev,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()
}
