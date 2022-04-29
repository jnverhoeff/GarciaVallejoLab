#' @title Run iterative metaclustering on SOM-clustered FlowSOM object
#'
#' @description Metaclustering runs are determined for all cluster numbers in set.i . Hierarchical clustering is used, optionally run in parallel
#'
#' @param FlowSOM.results A FlowSOM object with completed SOM clustering, either after full FlowSOM function or after BuildSOM
#' @param set.i Vector containing either the desired iterations to be tested
#' @param multicore logical, should INFLECT be run in parallel using \link[doParallel]{registerDoParallel}
#' @param cores If \code{multicore == TRUE}, number of cores to be used. If \code{NULL} max number of cores-1 is used
#'
#' @return metaclustering.list A \code{list} of \code{arrays} with metacluster-codes for the SOM-clusters within the FlowSOM.results object.
#' @seealso \code{\link{INFLECT}}
#'
#' @export
iteration.metacluster<- function(FlowSOM.results,set.i,multicore = TRUE,cores = NULL) {
  if (multicore) {
    if (is.null(cores)) {
      cores <- parallel::detectCores()-1
    } else{
      cores <- cores
    }


    registerDoParallel(cores = cores)
    metaclustering.list <- foreach(loop = set.i ,.final = function(x) {setNames(x, set.i)}) %dopar% {
      metaClusteringhclust(FlowSOM.results$map$codes,
                           nClus = loop)
    }
  } else{

    metaclustering.list <- list()
    for (loop in set.i){
      meta.result<- metaClusteringhclust(FlowSOM.results$map$codes,
                                          nClus= loop)
      metaclustering.list[[loop]]<- meta.result
    }
    metaclustering.list <- metaclustering.list[!sapply(metaclustering.list, is.null)]
    metaclustering.list <- setNames(metaclustering.list, as.character(set.i))
  }


  metaclustering.list
}
