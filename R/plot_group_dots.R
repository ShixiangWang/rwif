#' Draw Grouped Dots
#'
#' @param data A numeric `data.frame`.
#' @param title,legend_title,legend_ncol,pal,step,dot_alpha Plot settings.
#' @param small_step A small step to discrete the points with equal value
#' for better visualization.
#'
#' @return Nothing.
#' @source Adapted from <https://mp.weixin.qq.com/s/rcnkEY_FR9BStkEEC3ms_Q>
#' @family rwif-plot
#' @export
#' @examples
#' plot_group_dots(
#'   VADeaths,
#'   title = "deaths per 1000 in 1940 Virginia stratified by age, gender, and location",
#'   legend_title = "age bins", legend_ncol = 5)
plot_group_dots = function(data,
                           step = 0.25,
                           small_step = 0.01,
                           title = "",
                           dot_alpha = 0.5,
                           legend_title = "",
                           legend_ncol = 1,
                           pal = hcl.colors(nrow(data), "Zissou")) {
  # 如果存在NA，会导致颜色map出错
  ys = seq(1+step, by = step, length.out = ncol(data))

  plot.new()
  plot.window(xlim = range(0, data, na.rm = TRUE), ylim = c(1, max(ys, na.rm = TRUE) + step*2))

  abline(h = ys, col = "grey", lty = "dotted", lwd = 3)
  x = as.numeric(as.matrix(data))
  y = ys[col(data)]
  if (anyNA(x)) {
    idx = !is.na(x)
    x = x[idx]
    y = y[idx]

    dord = rep(rownames(data), ncol(data))
    dord = dord[idx]

    cmap = scales::alpha(pal, dot_alpha)
    names(cmap) = rownames(data)
    cols = as.character(cmap[dord])
  } else {
    cols = pal
  }
  same = c(FALSE, diff(x) == 0 & diff(y) == 0)
  for (i in rev(seq_along(x))) {
    if (same[i]) {
      # reduce i-1 a small step
      x[i-1] = x[i] - small_step
    }
  }

  points(x, y, col = cols, pch = 19, cex = 2.5)
  text(0, ys, colnames(data), adj = c(0.25, -1), col = "lightslategrey")

  axis(1, lwd = 0, font = 2)
  title(title)

  legend("top", legend = rownames(data),
         col = scales::alpha(pal, dot_alpha), pch = 19,
         ncol = legend_ncol,
         bty = "n", title = legend_title)
}
