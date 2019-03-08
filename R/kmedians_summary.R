#' summary
#'
#' Generates a table to display the cluster labels, the coordinates of the cluster medians,
#' number of points in each cluster, the average distance within the cluster,
#' the maximum distance within the cluster and the minimum distance within the cluster.
#'
#' @param X matrix,the dataset being clustered
#' @param medians matrix, coordinates of each cluster median
#' @param labels list, array with the assignment of the cluster for each point in the dataset
#'
#' @return dataframe. Returns a dataframe with 6 columns and number of rows will be the number of clusters. The labels of the columns:
#'          Cluster labels, Median Coordinates, Number of Points in Cluster, Average Distance, Minimum Distance, Maximum Distance
#' @export
#'
#' @examples
#'
summary <- function(X, medians, labels){

  # Check that inputs are valid and as expected
  if(!is.matrix(X)) return("Input X should be a matrix!")

  if(!is.matrix(medians)) return("Input medians should be a matrix!")

  if(!is.vector(labels)) return("Input labels should be a vector!")

  if((nrow(X)+ncol(X))  < (nrow(medians)+ncol(medians))) return("Too many kmedians cluster centers!")

  if((nrow(medians)+ncol(medians)) < length(unique(labels))) return("Number of kmedians cluster centers do NOT equal to the number of clusters!")


  medians_df <- data.frame(cbind(unique(labels),medians))
  colnames(medians_df) <- c("label", "medianX", "medianY")

  summary_df <- data.frame(cbind(X,labels))
  colnames(summary_df) <- c("X","Y","label")

  summary_df <- summary_df %>%
    dplyr::right_join(medians_df, by = 'label') %>%
    dplyr::mutate(distance = (abs(X-medianX)+abs(Y-medianY))) %>%
    dplyr::select(label, medianX, medianY, distance) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(medianX = unique(medianX), medianY = unique(medianY), num = n(), avgd = mean(distance), mind = min(distance), maxd = max(distance)) %>%
    dplyr::mutate(med = paste(medianX, medianY, sep = ",")) %>%
    dplyr::arrange(label) %>%
    dplyr::select(label, med, num, avgd, mind, maxd)

  colnames(summary_df) <- c("Cluster Label","Median Coordinates",
                            "Number of Points in Cluster","Average Distance",
                            "Minimum Distance","Maximum Distance")

  return (data.frame(summary_df))

}
