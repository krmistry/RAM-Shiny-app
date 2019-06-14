
# Module UI function:
yearSliderUI <- function(id, 
                         label = "Year", 
                         all_data, 
                         segment_data) {
  ns <- NS(id)
    sliderInput(ns("Year"), label = label, 
              min = min(all_data$Year), 
              max = max(all_data$Year), 
              value = max(min(segment_data[, 2]), min(all_data$Year)),
              step = 1,
              sep = "",
              animate = animationOptions(interval = 1000))
}

#updateyearSlider <- function(session, )

# Module server function:
yearSlider <- function(session, input, output, all_data, segment_data) {
  ns <- session$ns
  observe({
    all_data <- all_data()
    segment_data <- segment_data()
    
    updateSliderInput(session, ns("Year"),
                      min = min(all_data[, 2]),
                      max = max(all_data[, 2]),
                      value = max(min(segment_data[, 2]),
                                  min(all_data[, 2])))
  })
  return(myYear = reactive({input$Year}))
}

