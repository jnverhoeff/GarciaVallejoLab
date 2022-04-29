#' @title Cluster quality control using diptest and IQR check
#'
#' @description Unimodality score is calculated for cluster results. Per marker per cluster \link[diptest]{dip.test} is applied and inter-quartile range is assessed.
#'
#' @param FlowSOM.results A FlowSOM object with completed SOM clustering, either after full FlowSOM function or after BuildSOM
#' @param metaclustering Vector with metacluster codes for all SOM-clusters.
#' @param zeroes.in Should be values at and below \code{0} be included. Recommended default for mass cytometry data is \code{FALSE}
#' @param only.clustering.markers If \code{TRUE} only evaluates markers specified in FlowSOM.results$map$colsUsed
#' @param acquired_markers Vector of column names with marker data to be evaluated by INFLECT. Ignored if \code{only.clustering.markers == TRUE}
#' @param uniform.test What tests are performed per marker per cluster. Options are "both", "spread" , or "unimodality" as a string.
#' @param th.pvalue Threshold for rejecting Unimodality dip.test result. Default is \code{0.05}. For more information see \link[diptest]{dip.test}
#' @param th.IQR Threshold for rejecting marker distribution based on inter-quartile range. Default is arc-sinh transformed value of \code{2}.
#' @param verbose \code{logical} , default is \code{FALSE}
#' @param ... Additional arguments to pass to this function. Not used currently.
#'
#' @return A \code{matrix} with evaluated markers in columns and clusters in rows. Each position in the matrix is \code{logical} indicating a pass or a fail.
#' @seealso \code{\link{INFLECT}} , \code{\link{iteration.QC}}
#'
#' @export
FlowSOMQC <- function (FlowSOM.results, metaclustering, zeroes.in=FALSE, only.clustering.markers = TRUE, acquired_markers=NULL, uniform.test = "both", th.pvalue = 0.05, th.IQR = 2, verbose = TRUE, ...)
{
   if (is.null(FlowSOM.results)) {
    stop("Error in FlowSOM.QC: The 'FlowSOM.results' parameter can not be NULL")
  }else if (class(FlowSOM.results)[1] != "FlowSOM") {
    stop("Error in FlowSOM.QC: The 'FlowSOM.results' parameter required a 'FlowSOM' object")
  }
   if (is.null(metaclustering)) {
    stop("Error in FlowSOM.QC: The 'metaclustering' parameter can not be NULL")
  } else if (class(metaclustering)[1] != "integer") {
    stop("Error in FlowSOM.QC: The 'metaclustering' parameter required a 'integer' of metaclustering results")
  }
  data <- FlowSOM.results$data
	if(FlowSOM.results$scale) {
	 for (j in 1:ncol(data)){
    data[,j]<- data[,j]*FlowSOM.results$scaled.scale[j]+FlowSOM.results$scaled.center[j]
  }
	}
	colnames(data)<- FlowSOM.results$prettyColnames
	data<- cbind(data, "cluster" = metaclustering[FlowSOM.results$map$mapping[,1]])
 clusters<- seq(max(metaclustering))

  clustering.markers <- FlowSOM.results$prettyColnames[FlowSOM.results$map$colsUsed]
  if (only.clustering.markers) {
    markers <- clustering.markers
  }
  else {
    if( !is.null(acquired_markers) && all(acquired_markers %in% FlowSOM.results$prettyColnames )) { markers <- acquired_markers} else{
	stop("Error in acquired_markers: The 'acquired_markers' vector must match names in 'FlowSOM.result$prettyColnames' " )}
  }

  accuracy.matrix <- matrix(nrow = length(clusters), ncol = length(markers),
                            dimnames = list(clusters, markers))
	min <- floor(min(data, na.rm = TRUE))
  max <- ceiling(max(data, na.rm = TRUE))
  ordered.markers <- c(gtools::mixedsort(clustering.markers),
                       gtools::mixedsort(setdiff(markers, clustering.markers)))
  bold.markers <- ifelse(is.element(ordered.markers, clustering.markers),
                         "bold", "plain")
  colored.markers <- ifelse(is.element(ordered.markers, clustering.markers),
                            "blue", "black")
  count <- 0
  for (cluster in clusters) {
    if (verbose) {
      count <- count + 1
      message(paste0("Cluster: ", count, " on ", length(clusters)))
    }

	expressions <- data[data[,"cluster"]== cluster , colnames(data) %in% ordered.markers]

    for (marker in ordered.markers) {
      if (nrow(expressions) > 1) {

        if (zeroes.in == FALSE) {
          marker.expression <-
          expressions[!expressions[, marker] <= 0, marker]
          if (length(marker.expression) < 5) {
          marker.expression <-
          c(rep(0, 5 - length(marker.expression)), marker.expression)
          }
        } else
        marker.expression <- expressions[, marker]

        if (uniform.test == "unimodality" || uniform.test ==
            "both") {
          p.value <- diptest::dip.test(marker.expression,
                                       ...)$p.value
          if (p.value < th.pvalue) {
            uniform <- FALSE

          }
          else {
            uniform <- TRUE
                      }
                 }
        if (uniform.test == "spread" || uniform.test ==
            "both") {
          quantile <- quantile(marker.expression)
          IQR <- quantile[4] - quantile[2]
          pinnacle <- computemode(marker.expression)$y
          if (IQR < th.IQR) {
            uniform <- ifelse(uniform, TRUE, FALSE)

          }
          else {
            uniform <- FALSE

          }

        }

        accuracy.matrix[cluster, marker] <- uniform
      }
      else {
        accuracy.matrix[cluster, marker] <- NA
      }
    }

  }

  quality.matrix.cm <- accuracy.matrix[, colnames(accuracy.matrix) %in%
                                         clustering.markers]


  message("[END] - generating Uniform Phenotypes QC")
  invisible(accuracy.matrix)
}
