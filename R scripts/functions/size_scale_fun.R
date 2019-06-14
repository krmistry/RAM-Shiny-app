# Funtion to create lists of size scales for point size (different for MSY, 
# catch, and productivity)
#
# @param - split_data = dataframe with data from 1 region or taxGroup
# @param - plot_titles = vector with list of all regions or taxGroups
#
# @return - list of dataframes with 3 rows (MSY, Catch, Productivity) and 2 
# columns (min and max values)

size_scale_fun <- function(split_data, 
                           plot_titles) {
  size_scale <- vector("list", length = length(plot_titles))
  names(size_scale) <- plot_titles
  
  for (i in 1:length(plot_titles)){
    size_scale[[i]] <- data.frame(matrix(NA, nrow = 3, ncol = 2))
    dimnames(size_scale[[i]]) <- list(c("MSY", "Catch", "Productivity"), 
                                      c("min", "max"))
    # If there are any NAs in the MSY column, return NA for both min and max
    if (all(is.na(split_data[[i]]$MSY)) == TRUE) {
      size_scale[[i]][1, ] == c(NA, NA)
    } else {
      size_scale[[i]][1, ] <- c(max(min(split_data[[i]]$MSY - 100, na.rm = T), 0), 
                                max(split_data[[i]]$MSY + 100, na.rm = T))
    } 
    # if there are any NAs in the catch column, return NA for both min and max
    if (all(is.na(split_data[[i]]$Catch)) == TRUE) {
      size_scale[[i]][2, ] == c(NA, NA)
    } else {
      size_scale[[i]][2, ] <- c(max(min(split_data[[i]]$Catch - 100, na.rm = T), 0), 
                                max(split_data[[i]]$Catch + 100, na.rm = T))
    }
    # if there are any NAs in the surplus production column, return NA for both
    # min and max
    if (all(is.na(split_data[[i]]$Surprod)) == TRUE) {
      size_scale[[i]][3, ] == c(NA, NA)
    } else {
      size_scale[[i]][3, ] <- c(min(split_data[[i]]$Surprod - 100, na.rm = T), 
                                max(split_data[[i]]$Surprod + 100, na.rm = T))
    }
    
    size_scale[[i]] <- round(size_scale[[i]], digits = -2)
  }
  return(size_scale)
}

