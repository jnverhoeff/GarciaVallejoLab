#' @title Plot the two halves of the curve with touchlines and display angle
#'
#' @description Internal function. Displays basic plot of curve and touchlines created by \code{\link{leastError}}. Of greater use when refining Inflection Point
#'
#' @param part1 Datapoints from start to kneepoint
#' @param part2 Datapoints from kneepoint to end
#' @param test1 Fitted line over part1
#' @param test2 Fitted line over part2
#' @param main Title of the plot
#' @param angle Angle between test1 and test2, obtained through \code{LinesAngles} function from \code{LearnGeom}
#' @seealso \code{\link{INFLECT}} , \code{\link{QC.to.curve}},\code{\link{leastError}}
#'
#' @return None, plot is generated using the basic R plot function.
#'

angleplot <-
  function(part1 = part1,
           part2 = part2,
           test1 = test1,
           test2 = test2,
           main = kneepoint,
           angle = angle1) {
    par(fin = c(6, 1), mar = c(0, 0, 0, 0))
    plot(part1, xlim = c(0, 300), ylim = c(50, 100))
    points(part2, col = "red")
    abline(test1)
    abline(test2, col = 'red')
    text(x = 80,
         y = 85,
         paste("Kneepoint=", main),
         cex = 0.8)
    text(x = 80,
         y = 75,
         paste("Angle=", angle),
         cex = 0.8)
  }
