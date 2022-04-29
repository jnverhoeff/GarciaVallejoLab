#' @title Plot marker performance across metaclustering results
#'
#' @description Plots the marker performance across metaclusterings in a boxplot. Performance score is the percentage of clusters
#' where a marker passed the \link[diptest]{dip.test} and IQR test per metaclustering. Information is displayed in a boxplot,
#' color gradient denotes \code{set.i} .
#'
#' @param inflect.results A inflect.results object resulting from \code{\link{INFLECT}}.
#' @param ggtitle Optional. Character string to plot as title.
#' @param markers Which markers should be included in the plot? A vector of strings (must match FlowSOM.results$prettyColnames). If \code{NULL}, all clustering.markers are displayed.
#'
#' @return \code{list} with 2 items. First is the melted dataframe with the marker performance (\%) per marker per metaclustering. Second is a ggplot object, a boxplot with marker
#' performance on the y-axis, markers on the x-axis and the color scale denoting the amount of metaclusters evaluated.
#'
#' @examples
#' # Read in FlowSOM object from file. Downsampled clustering result of Levine32 dataset clustering.
#' # SOM-clustered to 375 clusters.
#' flowsom <- system.file("extdata", "Levine32sample.Rdata", package="INFLECT")
#' load(flowsom)
#' inflect.results<- INFLECT(FlowSOM.results= dataset, set.i= c(150,200), cores= 4, zeroes.in=FALSE)
#'
#' # Display diagnostic graph
#' inflect.results$ggplot
#'
#' # Now check marker performance for all markers
#'marker.performance(inflect.results, ggtitle= "Levine32sample", markers=NULL)
#'
#'
#' @export

marker.performance <- function(inflect.results, ggtitle = NULL, markers) {

  if (!is.null(markers)){

    # Verify whether markers match colnames in accuracy matrices
    if (class(markers) != "character" ) {
      stop( "Error in marker.performance: markers should be a vector of strings" )
    } else  {
      if(!all(markers %in% colnames(inflect.results$Accuracy.sets[[1]]))) {
        stop( "Error: markers should match colnames of matrices in inflect.results$Accuracy.sets" )
      }

    }

  } else {
    markers<- colnames(inflect.results$Accuracy.sets[[1]])
  }

  success.rate<- lapply( inflect.results$Accuracy.sets, function(x){
    x<- x[,markers]
    apply(x, 2, sum, na.rm=TRUE)*100 / apply(x, 2, length)
  })
  success.rate<- as.data.frame(do.call(rbind, success.rate))
  success.rate$i <- as.numeric(rownames(success.rate))
  success.rate <- reshape2::melt(success.rate,variable.name="Marker", value.name= "Performance", id.vars="i")


  # Dataframe is prepared, continue on to figure generation
  figure<- ggplot(success.rate, aes(y= Performance, x= Marker, color=i))+geom_boxplot(outlier.alpha=0)+geom_jitter(shape=1, alpha=0.5)+
    theme_bw() + scale_color_gradient(low = "#66C2A5", high="#E41A1C") + theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) + ylab("Marker performance (% passed within metaclusterings)")
  if(!is.null(ggtitle)){figure<- figure + ggtitle(label = ggtitle)}
  figure
  return(list("marker.dataframe"= success.rate, "plot" = figure))
}
