#' Title Mplus Boostrapping
#'
#' @param Model a list object containing the parameters of the model
#' (from Mplus output file)
#' @param type should choose from "med" "mod-med" or "med-mod"
#' @param first_path the name of the first random path, left as 'fixed'
#' when the path is not random (must be specified by the user)
#' @param second_path the name of the second random path, left as 'fixed'
#' when the path is not random (must be specified by the user)
#' @param iv the name of the independent variable
#' @param m the name of the mediator
#' @param w the name of the moderator
#' @param xw the name of the interaction term
#' @param dv the name of the dependent variable
#' @param first_stage_mod a binary variable indicating whether the moderation
#' effect is on the first path
#' @param level_one_mod a binary variable indicating whether the moderation
#' effect is on the first level
#' @param output the name of the output image file (.png)
#' @param rep the number of repetitions (default: 20000)
#' @param conf the critical value used to generate confidence interval
#' @param multi_level a binary variable indicating whether the specified is
#' multi-level (must be specified by the user)
#'
#' @return An image containing the distribution of the indirect effect and
#' confidence interval
#' @export
#'
#' @examples
MplusBootstrap <- function(Model, type, first_path, second_path=NULL, multi_level,
                           iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL,
                           first_stage_mod=NULL, level_one_mod=NULL,
                           output='Untitled', rep=20000, conf=95) {
  # model selection
  if (type == 'med') {
    if ((first_path != 'fixed') & (second_path != 'fixed')) {
      complex_version(Model, first_path=first_path, second_path=second_path,
                      output=output, rep=rep, conf=conf)
    } else {
      simple_version(Model, iv=iv, m=m, dv=dv, multi_level=multi_level,
                     first_path=first_path, second_path=second_path,
                     output=output, rep=rep, conf=conf)
    }
  } else if (type == 'mod-med') {
    if ((first_path != 'fixed') & (second_path != 'fixed')) {
      complex_modmed(Model, first_stage_mod=first_stage_mod,
                     first_path=first_path, second_path=second_path, w=w,
                     output=output, rep=rep, conf=conf)
    } else {
      simple_modmed(Model, iv=iv, m=m, w=w, xw=xw, dv=dv,
                    first_stage_mod=first_stage_mod, level_one_mod=level_one_mod,
                    first_path=first_path, second_path=second_path,
                    output=output, rep=rep, conf=conf)
    }
  } else if (type == 'med-mod') {
    med_mod(Model, iv=iv, m=m, w=w, xw=xw, dv=dv,
            first_path=first_path, level_one_mod=level_one_mod,
            output=output, rep=rep, conf=conf)
  }
}
