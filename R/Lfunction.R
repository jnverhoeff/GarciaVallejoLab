#' @title Lfunction for determing Inflection Point
#'
#' @description Wrapper function for \code{\link{leastError}} with an option for refining the Inflection Point, by limiting the range of of points to evaluate for fitting line errors. Default is to not use the refinement.
#'
#' @param totaldataframe Dataframe with set.i and corresponding unimodality scores, resulting from \code{\link{iteration.QC}}. This can be the calculcated points collection.U or the fitted curve.
#' @param cutoff Integer. Initial Inflection Point calculated on the entire curve is multiplied by \code{cutoff} to determine the new range of the curve that is used as input for \code{\link{leastError}}
#' @seealso \code{\link{INFLECT}} , \code{\link{QC.to.curve}},\code{\link{leastError}}
#'
#' @return \code{list} with 3 items: Inflection Point, the endpoint of the second touchline, and the angle between the two touchlines
#'
#' @export

Lfunction <- function(totaldataframe, cutoff = 1000) {
  colnames(totaldataframe) <- c("x", "y")
  kneepoint <- leastError(totaldataframe[1:nrow(totaldataframe), ])

  part1 <- totaldataframe[1:kneepoint, ]
  part2 <- totaldataframe[kneepoint:nrow(totaldataframe), ]
  test1 <- lm(y ~ x, part1)
  test2 <- lm(y ~ x, part2)
  angle1 <-
    LinesAngles(rev(test1$coefficients), rev(test2$coefficients))

  newrange <- kneepoint * cutoff
  oldrange <- newrange
  angleplot(part1, part2, test1, test2, totaldataframe[kneepoint, 1], angle1)
  if (newrange > nrow(totaldataframe))
    return(list(
      "knee" = totaldataframe[kneepoint, 1],
      "range" = totaldataframe[nrow(totaldataframe), 1],
      "angle" = angle1
    ))
  else {
    repeat {
      oldangle <- angle1
      oldkneepoint <- kneepoint
      kneepoint <- leastError(totaldataframe[1:newrange, ])
      part1 <- totaldataframe[1:kneepoint, ]
      part2 <- totaldataframe[kneepoint:newrange, ]
      test1 <- lm(y ~ x, part1)
      test2 <- lm(y ~ x, part2)

      angle1 <- LinesAngles(rev(test1$coefficients), rev(test2$coefficients))
      angleplot(part1, part2, test1, test2, main = totaldataframe[kneepoint, 1], angle1)
      if (kneepoint >= oldkneepoint) {
        oldrange <- newrange
        break
      }
      oldrange <- newrange

      newrange <- kneepoint * cutoff
      if (newrange < 20) {
        newrange <- 20
      }

    }
    return(list(
      "knee" = totaldataframe[oldkneepoint, 1],
      "range" = totaldataframe[oldrange, ],
      "angle" = oldangle
    ))
  }

}
