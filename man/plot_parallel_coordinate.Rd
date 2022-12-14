% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_parallel_coordinate.R
\name{plot_parallel_coordinate}
\alias{plot_parallel_coordinate}
\title{Draw parallel coordinate plot}
\source{
Adapted from \url{https://mp.weixin.qq.com/s/rcnkEY_FR9BStkEEC3ms_Q}
}
\usage{
plot_parallel_coordinate(
  mat,
  group = NULL,
  pal = c("cornflowerblue", "red3", "orange"),
  dot_line_col = "grey",
  dot_line_width = 5,
  var_name_col = "darkgrey",
  legend_x = 1,
  legend_y = 2
)
}
\arguments{
\item{mat}{A \code{data.frame} with numeric columns.}

\item{group}{A group vector for classification.}

\item{pal, dot_line_col, dot_line_width, var_name_col, legend_x, legend_y}{Plot settings.}
}
\value{
Nothing.
}
\description{
Draw parallel coordinate plot
}
\examples{
plot_parallel_coordinate(iris[, -c(3, 5)], group = iris$Species, legend_x = 2, legend_y = 7)
}
\seealso{
Other rwif-plot: 
\code{\link{check_colors}()},
\code{\link{plot_group_dots}()}
}
\concept{rwif-plot}
