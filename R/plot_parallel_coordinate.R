
plot_parallel_coordinate = function(mat, group = NULL,
                                    pal = c("cornflowerblue", "red3", "orange"),
                                    dot_line_col = "grey",
                                    dot_line_width = 5,
                                    var_name_col = "darkgrey",
                                    legend_x = 1,
                                    legend_y = 2) {
  if (!is.null(group)) {
    old_pal = palette()
    palette(pal)
    on.exit(palette(old_pal))
  }

  plot.new()
  plot.window(xlim = c(1, ncol(mat)), ylim = range(mat))
  grid(nx = NA, ny = NULL)
  abline(v = seq_len(ncol(mat)), col = dot_line_col, lwd = dot_line_width, lty = "dotted")

  matlines(t(mat), col = if (is.null(group)) pal[1] else group, lty = 1)

  axis(2, lwd = 0, las = 2)
  mtext(colnames(mat), 3, at = seq_len(ncol(mat)), line = 1, col = var_name_col)

  if (!is.null(group)) {
    legend(x = legend_x, y = legend_y, legend = unique(group), col = unique(group),
           lwd = 3, bty = 'n')
  }
}


