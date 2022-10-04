plot_dual_y_axis_dots = function() {
  par(mar = c(4,4,4,4))
  plot.new()

  plot.window(xlim = range(mtcars$disp), ylim = range(pretty(mtcars$mpg)))

  points(mtcars$disp, mtcars$mpg, col = "darkorange2", pch = 19, cex = 1.5)
  axis(2, col.axis = "darkorange2", lwd = 2, las = 2)
  mtext("miles per gallon", 2, col = "darkorange2", font = 2, line = 3)

  plot.window(xlim = range(mtcars$disp), ylim = range(pretty(mtcars$hp)))

  points(mtcars$disp, mtcars$hp, col = "forestgreen", pch = 19, cex = 1.5)
  axis(4, col.axis = "forestgreen", lwd = 2, las = 2)
  mtext("horse power", 4, col = "forestgreen", font = 2, line = 3)

  box()
  axis(1)
  mtext("displacement", 1, font = 2, line = 3)
  title("displacement VS mpg VS hp", adj = 0, cex.main = 1)
}
