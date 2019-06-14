
# Module UI function:
sizeDropdownUI <- function(id, label = "Size dropdown") {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("size"), "Choose variable for point size:",
                c("MSY" = "MSY",
                  "Catch" = "Catch",
                  "Productivity" = "Surprod")) 
  )
}

sizeDropdown <- function(input, output, session, 
                         size_scale_list, dataframe_name) {
  # Isolate min, max and legend titles for size scale in ggplot:
  min_size <- reactive({
    size_scale <- size_scale_list[[dataframe_name()]]
    if (input$size == "MSY") {
      size_scale[1, 1]
    } else if (input$size == "Catch") {
      size_scale[2, 1]
    } else {
      size_scale[3, 1]
    }
  })
  max_size <- reactive({
    size_scale <- size_scale_list[[dataframe_name()]]
    if (input$size == "MSY") {
      size_scale[1, 2]
    } else if (input$size == "Catch") {
      size_scale[2, 2]
    }  else {
      size_scale[3, 2]
    }
  })
  size_legend_title <- reactive({
    size_scale <- size_scale_list[[dataframe_name()]]
    if (input$size == "MSY") {
      rownames(size_scale)[1]
    } else if (input$size == "Catch") {
      rownames(size_scale)[2]
    }  else {
      rownames(size_scale)[3]
    }
  })
  # returns either "MSY", "Catch", or "Productivity" and the accompanying min,
  # max and size_legend_title values
  return(list(mySize = reactive({input$size}), 
              min_size = min_size, 
              max_size = max_size, 
              size_legend_title = size_legend_title))
}

