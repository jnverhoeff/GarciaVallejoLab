#' @title inflect.results class definition
#'
#' @description The inflect.results object is an S4 object containing the Inflection Point, diagnostic curve and associated datapoints for use of customization of graphs.
#'
#' @details The \code{collection.U} dataframe contains the Unimodality scores for each \code{i} in \code{set.i}.
#' The \code{fittedcurve} dataframe contains the coordinates of the curve fitted to \code{collection.U}.
#' The \code{lfunction} is a \code{list} with results from \code{\link{Lfunction}}, denoting the Inflection Point and touchline range.
#' The \code{ggplot} is the \code{ggplot2} object of the diagnostic graph.
#'
#' @slot collection.U dataframe containing the Unimodality scores for each \code{i} in \code{set.i}.
#' @slot fittedcurve dataframe containing the coordinates of the curve fitted to \code{collection.U}.
#' @slot lfunction \code{list} with results from \code{\link{Lfunction}}, denoting the Inflection Point and touchline range.
#' @slot ggplot the \code{ggplot2} object of the diagnostic graph
#' @slot metaclustering.list list containing the iterative metaclustering results according to \code{set.i}
#' @slot accuracy.sets list containing \link{FlowSOMQC} results per metaclustering
#'
#' @import FlowSOM
#'
#' @name inflect.results-class
#' @rdname inflect.results-class
#' @exportClass inflect.results
#'
inflect.results <- setClass("inflect.results",
                            slots= c(collection.U = "data.frame",
                                     fittedcurve = "data.frame",
                                     lfunction = "list",
                                     ggplot = "ANY" ,
                                     metaclustering.list = "list",
                                     accuracy.sets = "list") )
