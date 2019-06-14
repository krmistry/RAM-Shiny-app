# Function to split data into a list of dataframes, separated out by region or
# taxGroup
#
# @param - data = raw data set with surplus production data
# @param - split_col_names = column names for each dataframe
# @param - region_or_taxGroup = vector with all regions or taxGroups
# @param - unique_region_or_taxGroup = unique(data$region or data$taxGroup); 
#           unique values for region and taxGroup in the raw data set
#
# @return - list of dataframes with 14 columns

split_function <- function(data,
                           split_col_names,
                           region_or_taxGroup, 
                           unique_region_or_taxGroup) {
  split <- split(data, region_or_taxGroup)
  split <- lapply(split, setNames, split_col_names) 
  # replace any values over 3 with 3 in BoverBMSY and UoverUMSY:
  for (i in 1:length(unique_region_or_taxGroup)) {
    split[[i]]$BoverBMSY <- ifelse(split[[i]]$BoverBMSY > 3, 
                                   3, split[[i]]$BoverBMSY)
    split[[i]]$UoverUMSY <- ifelse(split[[i]]$UoverUMSY > 3, 
                                   3, split[[i]]$UoverUMSY)
    split[[i]]$taxGroup <- factor(split[[i]]$taxGroup, 
                                  levels = as.character(unique(split[[i]]$taxGroup)))
    split[[i]]$region <- factor(split[[i]]$region,
                                levels = as.character(unique(split[[i]]$region)))
  }
  return(split)
}

