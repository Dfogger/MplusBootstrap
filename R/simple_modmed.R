# TO_DO: moderation on the second stage
#' Title: Moderated Mediation test of multilevel model (at least one path is fixed)
#' This function can calculate moderated mediation effect of multi-level model that
#'  contains at least one fixed path.
#'
#' @param Model a list object containing the parameters of the model
#' (from Mplus output file)
#' @param iv the name of the independent variable
#' @param m the name of the mediator
#' @param w the name of the moderator
#' @param xw the name of the interaction term
#' @param dv the name of the dependent variable
#' @param first_stage_mod a binary variable indicating whether the moderation
#' effect is on the first path (default: TRUE)
#' @param level_one_mod a binary variable indicating whether the moderation
#' effect is on the first level (default: TRUE)
#' @param first_path the name of the first random path, left as 'fixed'
#' when the path is not random (default: 'fixed')
#' @param second_path the name of the second random path, left as 'fixed'
#' when the path is not random (default: 'fixed')
#' @param output the name of the output image file (.png)
#' @param rep the number of repetitions (default: 20000)
#' @param conf the critical value used to generate confidence interval
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#'
#' @examples
simple_modmed <- function(Model, iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL,
                          first_stage_mod=TRUE, level_one_mod=TRUE,
                          first_path='fixed', second_path='fixed',
                          output='Untitled', rep=20000, conf=95) {
  # Multilevel moderated mediation_all Level 1 variables (fixed-fixed)
  w = toupper(w)
  m = toupper(m)
  dv = toupper(dv)

  b = Model$parameters$unstandardized %>% filter(
    paramHeader == paste0(dv, ".ON"),
    param == m
  )  #the effect of m on y
  b = b$est

  b_loc = Model$tech1$parameterSpecification$WITHIN$beta[dv, m]
  b2 =  Model$tech3$paramCov[b_loc, b_loc] #the variance of coefficient b

  # Moderator is at level one
  if (level_one_mod) {
    xw = toupper(xw)
    c = Model$parameters$unstandardized %>% filter(
      paramHeader == paste0(m, ".ON"),
      param == xw
    )  #the moderation effect of w on xm
    c = c$est

    c_loc = Model$tech1$parameterSpecification$WITHIN$beta[m, xw]
    c2 = Model$tech3$paramCov[c_loc, c_loc]  #the variance of coefficient c

    wsd = Model$parameters$unstandardized %>% filter(
      paramHeader == "Variances",
      param == w,
      BetweenWithin == 'Within'
    ) #sd of the moderator
    wsd = sqrt(wsd$est)

    # Mod-Med Index
    # simple_version(xw, m, dv, multilevel=TRUE, output=paste0(output, 'modmed_index'))
  } else {
    # Moderator is at level two (cross-level moderation; first path must be random)
    first_path = toupper(first_path)
    c = Model$parameters$unstandardized %>% filter(
      paramHeader == paste0(first_path, ".ON"),
      param == w
    )  #the moderation effect of w on xm
    c = c$est

    c_loc = Model$tech1$parameterSpecification$BETWEEN$beta[first_path, w]
    c2 = Model$tech3$paramCov[c_loc, c_loc]  #the variance of coefficient c

    wsd = Model$parameters$unstandardized %>% filter(
      paramHeader == "Variances",
      param == w,
      BetweenWithin == 'Between'
    ) #sd of the moderator
    wsd = sqrt(wsd$est)
  }


  if ((first_path=='fixed') & (second_path=='fixed')) {
    iv = toupper(iv)

    # Simple Slope
    a = Model$parameters$unstandardized %>% filter(
      paramHeader == paste0(m, ".ON"),
      param == iv
    )  #the effect of x on m
    a = a$est

    a_loc = Model$tech1$parameterSpecification$WITHIN$beta[m, iv]
    a2 = Model$tech3$paramCov[a_loc, a_loc] #the variance of coefficient a
  } else if (first_path!='fixed') {
    # Multilevel moderated mediation_all Level 1 variables (Random-fixed)
    first_path = toupper(first_path)

    # Mod-Med Index & Simple Slope
    a = Model$parameters$unstandardized %>% filter(
      paramHeader == "Intercepts",
      param == first_path
    )  #the effect of x on m
    a = a$est

    a_loc = Model$tech1$parameterSpecification$BETWEEN$alpha[1, first_path]
    a2 = Model$tech3$paramCov[a_loc, a_loc] #the variance of coefficient a
  }

  ab = if (a_loc > b_loc) {
    Model$tech3$paramCov[a_loc, b_loc]
  } else {
    Model$tech3$paramCov[b_loc, a_loc]
  }  #the covariance of coefficient a and coefficient b, read in tech 3
  ac = if (a_loc > c_loc) {
    Model$tech3$paramCov[a_loc, c_loc]
  } else {
    Model$tech3$paramCov[c_loc, a_loc]
  }  #the covariance of coefficient a and coefficient c, read in tech 3
  bc = if (b_loc > c_loc) {
    Model$tech3$paramCov[b_loc, c_loc]
  } else {
    Model$tech3$paramCov[c_loc, b_loc]
  }  #the covariance of coefficient b and coefficient c, read in tech 3

  pest=c(a,b,c)
  acov <- matrix(c(
    a2,ab,ac,
    ab,b2,bc,
    ac,bc,c2
  ),3,3)
  mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
  # Upper Level of Moderator
  amvec<-(wsd)*mcmc[,3]+mcmc[,1]
  abb <- amvec*mcmc[,2]
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(abb,low)
  UL=quantile(abb,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  png(paste0(output, '_high.png'), width = 800, height = 600)
  hist(abb,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()

  # Lower Level of Moderator
  amvec<-(-wsd)*mcmc[,3]+mcmc[,1]
  abb <- amvec*mcmc[,2]
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(abb,low)
  UL=quantile(abb,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  png(paste0(output, '_low.png'), width = 800, height = 600)
  hist(abb,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()

  # Difference between higher and lower level of Moderator
  ahmvec<-(wsd)*mcmc[,3]+mcmc[,1] #for low, change into (-wsd)
  almvec<-(-wsd)*mcmc[,3]+mcmc[,1]
  abh <- ahmvec*mcmc[,2]
  abl<-almvec*mcmc[,2]
  d=abh-abl
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(d,low)
  UL=quantile(d,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  png(paste0(output, '_difference.png'), width = 800, height = 600)
  hist(d,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
       main='Distribution of Indirect Effect')
  dev.off()
}
