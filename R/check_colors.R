#' Check Manual Colors
#'
#' @param colors Colors to check.
#'
#' @return Hex code of input colors.
#' @export
#' @family rwif-plot
#' @examples
#' if (requireNamespace("unikn", quietly = TRUE))
#'   check_colors(c("red", "blue"))
check_colors = function(colors) {
  if (!requireNamespace("unikn", quietly = TRUE)) {
    install.packages("unikn")
  }
  unikn::seecol(colors)
}
