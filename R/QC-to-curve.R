#' @title Plot diagnostic INFLECT curve and find inflection point
#'
#' @description Plots the collection.U versus i.set and uses \code{\link{Lfunction}} to determine the inflection point for optimal k.
#'
#' @param collection.U Dataframe with set.i and corresponding unimodality scores, resulting from \code{\link{iteration.QC}}
#' @param basedata Data to be used to calculate inflection point, given as a string. Options are \code{Curve} and \code{Points}
#' @param ggtitle Optional title for resulting diagnostic graph. Default \code{NULL}
#' @return A \code{list} with 4 items. First is a data.frame with the unimodality scores for each metaclustering. Second is a dataframe with the fitted curve. Third is the result of \code{\link{Lfunction}}, lastly the diagnostic plot created from the other three items.
#'
#' @seealso \code{\link{INFLECT}} , \code{\link{iteration.QC}},\code{\link{Lfunction}}
#'
#' @export
QC.to.curve <-
  function(collection.U,
           basedata,
           ggtitle=NULL) {
    df.points <- collection.U[[1]]

    drc <- drm(data = df.points,
               formula = Unimodality ~ i ,
               fct = LL.4())
    df.curve <-
      data.frame(x = 1:max(df.points$i) ,
                 y = predict(object = drc, newdata = data.frame(x = 1:max(df.points$i))))

    if (basedata == "Curve") {
      result.full <- Lfunction(df.curve[-c(1:4),], cutoff = 1000)

    }

    if (basedata == "Points") {
      result.full <- Lfunction(df.points, cutoff = 1000)
    }

    if (!basedata %in% c("Curve", "Points")) {
      stop(cat("basedata should match either \"Curve\" or \"Points\""))
    }
    setcolors <- RColorBrewer::brewer.pal(name = "Set1", n = 9)

    if (basedata == "Curve") {
      part1 <- df.curve[5:result.full$knee,]
      part2 <- df.curve[result.full$knee:result.full$range,]
      touchline1 <- lm(y ~ x, part1)
      touchline2 <- lm(y ~ x, part2)
      label <-
        data.frame(x = part2[1, 1], y = part1[1, 2], z = part2[1, 1])
      title <- ggtitle

      figure <-
        ggplot() + geom_line(data = df.curve, aes(x = x, y = y), color = setcolors[1]) + ggtitle(title) +
        geom_line(data = data.frame(x = part1[, 1], y = fitted(touchline1)), aes(x = x, y = y), color = "black") +
        geom_line(data = data.frame(x = part2[, 1], y = fitted(touchline2)), aes(x = x, y = y), linetype = 2, color = "black") +
        geom_vline(aes(xintercept = df.curve[result.full$knee, 1]), linetype = 3, color = setcolors[9]) +
        geom_point(data = df.points, aes(x = i, y = Unimodality), color = setcolors[2], alpha = 0.7) +
        theme_classic() + xlab("# of FlowSOM-metaclusters") + ylab("% unimodal distributions across clusters") +
        scale_x_continuous(breaks = seq(0, max(df.points$i), 25)) + geom_label(data = label, aes(x = x, y = y, label = z))

    }
    if (basedata == "Points") {
      part1 <- df.points[1:which(df.points$i == result.full$knee), ]
      part2 <- df.points[which(df.points$i == result.full$knee):which(df.points$i == result.full$range), ]
      touchline1 <- lm(Unimodality ~ i, part1)
      touchline2 <- lm(Unimodality ~ i, part2)
      label <-
        data.frame(x = part2[1, 1], y = part1[1, 2], z = part2[1, 1])
      title <- ggtitle

      figure <-
        ggplot() + geom_line(data = df.curve, aes(x = x, y = y), color = setcolors[1]) + ggtitle(title) +
        geom_line(data = data.frame(x = part1[, 1], y = fitted(touchline1)), aes(x = x, y = y), color = "black") +
        geom_line(data = data.frame(x = part2[, 1], y = fitted(touchline2)), aes(x = x, y = y), linetype = 2, color = "black") +
        geom_vline(aes(xintercept = df.curve[result.full$knee, 1]), linetype = 3, color = setcolors[9]) +
        geom_point(data = df.points, aes(x = i, y = Unimodality), color = setcolors[2], alpha = 0.7) +
        theme_classic() + xlab("# of FlowSOM-metaclusters") + ylab("% unimodal distributions across clusters") +
        scale_x_continuous(breaks = seq(0, max(df.points$i), 25)) + geom_label(data = label, aes(x = x, y = y, label = z))
    }

    figure
    return(
      list(
        collection.U = df.points,
        fittedcurve = df.curve ,
        lfunction = result.full,
        ggplot = figure
      )
    )

  }
