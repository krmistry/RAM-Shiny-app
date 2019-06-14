# Function to create lists with custom colors for each region or taxGroup (so
# the ggplot legends will only show the taxGroups or regions that exist in each
# region or taxGroup)
# @param split_data = dataframe with data from 1 region or taxGroup
# @param plot_titles = 

custom_color_fun <- function(split_data,
                             plot_titles,
                             type_of_plot = c("Region", "Taxonomy Group"),
                             named_Colors) {
  named_custom_Colors <- vector("list", length = length(plot_titles))
  names(named_custom_Colors) <- plot_titles
  
  if(type_of_plot == "Region") {
    for (i in 1:length(plot_titles)) {
      named_custom_Colors[[i]] <- data.frame(matrix(NA, nrow = length(levels(split_data[[i]]$taxGroup))))
      named_custom_Colors[[i]] <- named_Colors[names(named_Colors) %in% levels(split_data[[i]]$taxGroup)]
    }
  } else {
    for (i in 1:length(plot_titles)) {
      named_custom_Colors[[i]] <- data.frame(matrix(NA, nrow = length(levels(split_data[[i]]$region))))
      named_custom_Colors[[i]] <- named_Colors[names(named_Colors) %in% levels(split_data[[i]]$region)]
    }
  }
  return(named_custom_Colors)
}


