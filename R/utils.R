#' Title: Load Model Results from Mplus output file
#'
#' This function loads model reulst from Mplus output file.
#' If these exists no output file, this function will try to run the input file
#'  and reads the output file then.
#' The function depends on MplusAutomation package.
#'  https://www.statmodel.com/usingmplusviar.shtml
#'
#' @param filename filename or path of the Mplus input or output file
#'
#' @return Model results (list object), including fit statistics and parameters.
#' @export
#'
#' @examples
#' model <- load_model('inst/examples/example_single')
#'
load_model <- function(filename) {
  if (!file.exists(paste0(file_name, ".out"))) {
    runModels(target = paste0(file_name, ".inp"))
  }
  return(readModels(target = paste0(file_name, ".out")))
}
