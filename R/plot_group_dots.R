#' Draw Grouped Dots
#'
#' @param data A numeric `data.frame`.
#' @param title,legend_title,pal,step,step2 Plot settings.
#'
#' @return Nothing.
#' @source Adapted from <https://mp.weixin.qq.com/s/rcnkEY_FR9BStkEEC3ms_Q>
#' @family rwif-plot
#' @export
#' @examples
#' plot_group_dots(
#'   VADeaths,
#'   title = "deaths per 1000 in 1940 Virginia stratified by age, gender, and location",
#'   legend_title = "age bins")
plot_group_dots = function(data,
                           step = 0.25,
                           step2 = step * 2,
                           title = "",
                           legend_title = "",
                           pal = hcl.colors(nrow(data), "Zissou")) {
  ys = 1
  ys[2] = ys[1] + step + step2
  for (i in seq_len(ncol(data))) {
    ys[i+2] = ys[i] + step
  }
  ys = ys[-c(1, 2)]

  plot.new()
  plot.window(xlim = range(0, data), ylim = c(1, max(ys, na.rm = TRUE) + step2))

  abline(h = ys, col = "grey", lty = "dotted", lwd = 3)
  points(data, ys[col(data)], col = pal, pch = 19, cex = 2.5)
  text(0, ys, colnames(data), adj = c(0.25, -1), col = "lightslategrey")

  axis(1, lwd = 0, font = 2)
  title(title)

  legend("top", legend = rownames(data), col = pal, pch = 19, horiz = TRUE,
         bty = "n", title = legend_title)
}
