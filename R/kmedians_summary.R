summary <- function(X, medians, labels){
  # Generates a table to display the cluster labels, the coordinates of the cluster medians,
  # number of points in each cluster, the average distance within the cluster,
  # the maximum distance within the cluster and the minimum distance within the cluster.
  #
  #
  # Parameters
  # ----------
  #
  # X: matrix
  # The dataset being clustered
  #
  # medians: matrix
  # Coordinates of each cluster median
  #
  # labels:  list
  # Array with the assignment of the cluster for each point in the dataset
  #
  # Returns
  # -------
  # dataframe
  # Returns a dataframe with 6 columns and number of rows will be the number of clusters. The labels of the columns:
  # Cluster labels, Median Coordinates, Number of Points in Cluster, Average Distance, Minimum Distance, Maximum Distance

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
