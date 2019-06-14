# **** Includes tab for taxonomy group

# Interactive elements: 
# - Region data selection 
# - slider with animation for years
# - check boxes (multiple choices possible) for color, representing taxGroup
# - drop down menu for size (options to choose: MSY, catch, surplus production)
# - check boxes for stock ID to include (default is all)
# - hover text for each point with stock name


require(shiny) 
library(ggplot2) 
library(RColorBrewer) 
library(tidyr) 
library(dplyr) 
library(httr) 
library(shinyBS)
library(stringi)

#---------------- Functions ----------------------------

# Function to split the data into lists of dataframes organized by either 
# region or taxGroup, and set custom factor levels for each region or taxGroup
split_function <- function(data, 
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
    # Create custom factor levels for regions and taxGroup in each dataframe
    split[[i]]$taxGroup <- factor(split[[i]]$taxGroup, 
                                  levels = as.character(unique(split[[i]]$taxGroup)))
    split[[i]]$region <- factor(split[[i]]$region,
                                levels = as.character(unique(split[[i]]$region)))
  }
  return(split)
}

# Function to create lists of size scales for point size (different for MSY, 
# catch, and productivity) with custom min and max values (with a buffer to 
# ensure nothing is cut off when rounded):
size_scale_fun <- function(split_data, plot_titles) {
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
    # Round all values to the nearest 
    size_scale[[i]] <- round(size_scale[[i]], digits = -2)
  }
  return(size_scale)
}

# Function to create lists with custom colors for each region or taxGroup (so
# the ggplot legends will only show the taxGroups or regions that exist in each
# region or taxGroup):
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

# Function to produce Kobe plot for either region or taxGroup separated data:
kobe_plot_fun <- function(data_input,
                          size_variable_input,
                          size_legend_title,
                          min_size,
                          max_size,
                          color_legend_title,
                          custom_colors) {
  ggplot(data_input, aes(x = BoverBMSY,
                       y = UoverUMSY,
                       color = taxGroup)) +
    geom_point(aes(size = !!as.symbol(size_variable_input)),
               alpha = 0.8) +
    # annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf,
    #          fill = "firebrick2", alpha = 0.1) +
    # annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1,
    #          fill = "yellowgreen", alpha = 0.1) +
    geom_hline(yintercept = 1) +
    geom_vline(xintercept = 1) +
    scale_size_continuous(name = size_legend_title,
                          limits = c(min_size, max_size),
                          labels = scales::comma) +
    ylim(0, 3) +
    xlim(0, 3) +
    scale_color_manual(name = color_legend_title, 
                       values = custom_colors,
                       drop = F) +
    theme_light() +
    labs(x = "B/BMSY", y = "U/UMSY") +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18)) +
    guides(color = guide_legend(override.aes = list(size = 5)))
}


#------------------- Data and Variables -----------------------
myFile <- "https://raw.githubusercontent.com/krmistry/dynamic_kobe/master/dss.csv"
data <- read.csv(myFile)
data$Region <- data$Region %>% 
  gsub("Russia Japan", "Northwest Pacific", .) %>%
  gsub("Europe non EU", "Norway, Iceland, Faroe Islands", .) %>%
  gsub("European Union", "European Union (non Mediterranean)", .) # region names as they appear on website graphics

data1 <- data[which(data$Year >= 1950), ] 
size_NA_ind <- which(is.na(data1$MSY) & is.na(data1$C) & is.na(data1$P))
data1 <- data1[-size_NA_ind, ]
split_col_names <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                     'Surprod','Biomass','u','BMSY','UMSY','MSY',
                     'stockid','taxGroup','region')
region_split <- split_function(data1, 
                        data1$Region,
                        unique(data1$Region)) # includes Antarctic right now

region_split <- subset(region_split, 
                       names(region_split) != "Antarctic" & 
                         names(region_split) != "Other") # these regions are currently excluded from website

region_plot_titles <- names(region_split)

# For custom colors for taxGroup type in ggplot 
# (keeps colors and legend consistent throughout animation)
region_legend_order <- c("gadids", "pleuronectids", "sebastids", "other scorpaenids", "forage fish"
                         , "carangids-mackerels", "tuna-billfish", "elasmobranchs", "other marine percoidids",
                         "other marine fish", "salmonids", "eels", "crabs-lobsters", "shrimps", 
                         "bivalves-gastropods", "cephalopods", "echinoderms")

named_region_Colors <- c("yellowgreen", "palegreen", "tomato", "pink", "darkorange", "steelblue2", "violet", "mediumpurple", 
                           "burlywood", "slategray1", "firebrick3", "khaki", "gold", "gray91", 
                           "gray", "gray42", "darksalmon")
names(named_region_Colors) <- region_legend_order


# Custom colors for taxGroups in each region:
named_custom_region_Colors <- custom_color_fun(region_split, 
                                                 region_plot_titles, 
                                                 "Region",
                                                 named_region_Colors)

# Custom size scale for MSY, catch and surplus production in each region:
size_scale_per_region <- size_scale_fun(region_split, region_plot_titles)

# TaxGroup versions of the above:
taxGroup_split <- split_function(data1, 
                               data1$taxGroup,
                               unique(data1$taxGroup))
taxGroup_split <- subset(taxGroup_split,
                         names(taxGroup_split) != "eels" &
                           names(taxGroup_split) != "echinoderms" &
                           names(taxGroup_split) != "salmonids")
taxGroup_plot_titles <- names(taxGroup_split)
named_taxGroup_Colors <- c("yellow2", "violetred1", "turquoise3", "tomato3", 
                       "steelblue3", "springgreen3", "slateblue3", "cyan",
                       "firebrick3", "plum3", "orangered1", "darkorchid3", "lightskyblue3",
                       "gold3", "darkseagreen3", "chartreuse3", "azure3", "azure4",
                       "darkseagreen3", "darksalmon")
names(named_taxGroup_Colors) <- region_plot_titles
named_custom_taxGroup_Colors <- custom_color_fun(taxGroup_split, 
                                               taxGroup_plot_titles, 
                                               "Taxonomy Group",
                                               named_taxGroup_Colors)
size_scale_per_taxGroup <- size_scale_fun(taxGroup_split, taxGroup_plot_titles)

#------------------------ Shiny App -----------------------------------------------------------

require(shiny)

ui <- fluidPage(
  tags$head(tags$style('
                       #my_tooltip {
                       position: absolute;
                       width: 300px;
                       z-index: 100;
                       }
                       ')),
  tags$script('
              $(document).ready(function(){
              // id of the plot
              $("#plot").mousemove(function(e){ 
              
              // ID of uiOutput
              $("#my_tooltip").show();         
              $("#my_tooltip").css({             
              top: (e.pageY + 5) + "px",             
              left: (e.pageX + 5) + "px"         
              });     
              });     
              });
              '),
  navbarPage("Dynamic Kobe Plot",
    tabPanel("By Region", fluid = TRUE,
             mainPanel(
               plotOutput("region_plot", 
                          click = "plot_click", 
                          height = "600px"),
               uiOutput("my_tooltip")
             ),
             sidebarPanel(
               wellPanel(
                 selectInput("region", "Region", region_plot_titles)
               ),
               wellPanel(
                 sliderInput("tab1_Year", "Year", 
                             min = min(data1$Year), 
                             max = max(data1$Year), 
                             value = min(region_split[[1]]$Year),
                             step = 1,
                             sep = "",
                             animate = animationOptions(interval = 1000)), 
                 selectInput("tab1_size", "Choose variable for point size:",
                             c("MSY" = "MSY",
                               "Catch" = "Catch",
                               "Productivity" = "Surprod")),
                 selectInput("tab1_stock", "Select stock to display:",
                             c("All",
                               as.character(unique(region_split[[1]]$ID)))),
                 checkboxGroupInput("tab1_taxGroup", "Taxonomy Group:",
                                    c(as.character(unique(region_split[[1]]$taxGroup))),
                                    selected = c(as.character(unique(region_split[[1]]$taxGroup))))
               )
             )
    ),
    tabPanel("By Taxonomy Group", fluid = TRUE,
             mainPanel(
               plotOutput("taxGroup_plot", 
                          click = "taxGroup_plot_click", 
                          height = "600px"),
               uiOutput("my_tooltip")
             ),
             sidebarPanel(
               wellPanel(
                 selectInput("taxGroup", "Taxonomy Group", 
                             taxGroup_plot_titles)
               ),
               wellPanel(
                 sliderInput("tab2_Year", "Year", 
                             min = min(data1$Year), 
                             max = max(data1$Year), 
                             value = min(taxGroup_split[[1]]$Year),
                             step = 1,
                             sep = "",
                             animate = animationOptions(interval = 1000)), 
                 selectInput("tab2_size", "Choose variable for point size:",
                             c("MSY" = "MSY",
                               "Catch" = "Catch",
                               "Productivity" = "Surprod")),
                 selectInput("tab2_stock", "Select stock to display:",
                             c("All",
                               as.character(unique(taxGroup_split[[1]]$ID)))),
                 checkboxGroupInput("tab2_region", "Region:",
                                    c(as.character(unique(taxGroup_split[[1]]$region))),
                                    selected = c(as.character(unique(taxGroup_split[[1]]$region))))
               )
             )
    )
  )
)

require(shiny)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

server <- function(input, output, session) {
  myRegion <- reactive({
    input$region
  })
  
  mytaxGroup <- reactive({
    input$tab1_taxGroup
  })
  
  observe({
    # Isolate and format region data:
    myRegion <- input$region
    region_data <- region_split[[myRegion]]
    region_data <- data.frame(region_data)
    colnames(region_data) <- split_col_names
    
    # Update stock and taxGroup inputs based on regional data:
    updateSelectInput(session, "tab1_stock", "Select stock to display:",
                      c("All",
                        as.character(unique(region_data[, 1]))))
    
    updateCheckboxGroupInput(session, "tab1_taxGroup", "Taxonomy Group:",
                             c(as.character(unique(region_data[, 13]))),
                             selected = c(as.character(unique(region_data[, 13]))))
    
    # Update size scale options based on if data is available in each region:
    if (all(is.na(region_data$MSY)) == TRUE) {
      updateSelectInput(session, "tab1_size", "Choose variable for point size:",
                        c("Catch" = "Catch",
                          "Productivity" = "Surprod"))
      } else if (all(is.na(region_data$Catch)) == TRUE) {
        updateSelectInput(session, "tab1_size", "Choose variable for point size:",
                          c("MSY" = "MSY",
                            "Productivity" = "Surprod"))
      } else if (all(is.na(region_data$Surprod)) == TRUE) {
        updateSelectInput(session, "tab1_size", "Choose variable for point size:",
                          c("MSY" = "MSY",
                            "Catch" = "Catch"))
    } else { #at the moment, didn't include options if more than 1 size scale is missing (may need to add in later)
      updateSelectInput(session, "tab1_size", "Choose variable for point size:",
                        c("MSY" = "MSY",
                          "Catch" = "Catch",
                          "Productivity" = "Surprod"))
    }
    
    # Update year slider with 1st year with data in selected region:
    updateSliderInput(session, "tab1_Year","Year", 
                      min = min(data1$Year), 
                      max = max(data1$Year), 
                      value = max(min(region_data[, 2]), min(data1$Year)))
  })
  
  # When fewer than all taxGroup types are selected, restrict stock options and adjust year slider:
  observe({
    mytaxGroup <- input$tab1_taxGroup
    myRegion <- input$region
    region_data <- region_split[[myRegion]]
    region_data <- data.frame(region_data)
    colnames(region_data) <- split_col_names
    
    if (identical(mytaxGroup, as.character(unique(region_data$taxGroup))) == FALSE) {
      taxGroup_region_data <- filter(region_data,
                                     taxGroup %in% mytaxGroup)
      
      # Jump to 1st year with data available for that taxGroup type(s):
      updateSliderInput(session, "tab1_Year","Year",
                        min = min(data1$Year),
                        max = max(data1$Year),
                        value = max(min(taxGroup_region_data[, 2]), min(data1$Year)))
      
      # Update stock selection options to only those in this taxGroup(s):
      updateSelectInput(session, "tab1_stock", "Select stock to display:",
                        c("All",
                          as.character(unique(taxGroup_region_data[, 1]))))
    } else {
      updateSliderInput(session, "tab1)Year","Year", 
                        min = min(data1$Year), 
                        max = max(data1$Year), 
                        value = max(min(region_data[, 2]), min(data1$Year)))
      updateSelectInput(session, "tab1_stock", "Select stock to display:",
                        c("All",
                          as.character(unique(region_data[, 1]))))
    }
    # myStock <- input$stock
    # FT_stock_region_data <- filter(fishery_type_region_data,
    #                                ID == myStock)
    # }
    # if (myStock != paste("All", myFisherytype, sep = " ")) {
    # myRegion <- input$region
    # region_data <- split[[myRegion]]
    # region_data <- data.frame(region_data)
    #
    # colnames(region_data) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
    #                            'Surprod','Biomass','u','BMSY','UMSY','MSY',
    #                            'stockid','Fisherytype','region')
    
    # Jump to 1st year with data available for that stock:
    #     updateSliderInput(session, "Year","Year",
    #                       min = min(data1$Year),
    #                       max = max(data1$Year),
    #                       value = max(min(FT_stock_region_data[, 2]), min(data1$Year)))
    #   }
    #   
    #   # Update size scale options based on what data is available for each stock:
    #   if (all(is.na(FT_stock_region_data$MSY)) == TRUE) {
    #     updateSelectInput(session, "size", "Choose variable for point size:",
    #                       c("Catch" = "Catch",
    #                         "Productivity" = "Surprod"))
    #   } else if (all(is.na(FT_stock_region_data$Catch)) == TRUE) {
    #     updateSelectInput(session, "size", "Choose variable for point size:",
    #                       c("MSY" = "MSY",
    #                         "Productivity" = "Surprod"))
    #   } else if (all(is.na(FT_stock_region_data$Surprod)) == TRUE) {
    #     updateSelectInput(session, "size", "Choose variable for point size:",
    #                       c("MSY" = "MSY",
    #                         "Catch" = "Catch"))
    #   } else { #at the moment, didn't include options if more than 1 size scale is missing
    #     updateSelectInput(session, "size", "Choose variable for point size:",
    #                       c("MSY" = "MSY",
    #                         "Catch" = "Catch",
    #                         "Productivity" = "Surprod"))
    #   }
  })
  
  # When a single stock is selected, update size options and year slider:
  observe({
    myStock <- input$tab1_stock
    if (myStock != "All") {
      myRegion <- input$region
      region_data <- region_split[[myRegion]]
      region_data <- data.frame(region_data)
      colnames(region_data) <- split_col_names
      
      stock_region_data <- filter(region_data,
                                  ID == myStock)
      
      # Jump to 1st year with data available for that stock:
      updateSliderInput(session, "tab1_Year","Year",
                        min = min(data1$Year),
                        max = max(data1$Year),
                        value = min(stock_region_data[, 2]))
    }
    
    #    # Update size scale options based on what data is available for each stock:
    #    if (all(is.na(stock_region_data$MSY)) == TRUE) {
    #      updateSelectInput(session, "size", "Choose variable for point size:",
    #                        c("Catch" = "Catch",
    #                          "Productivity" = "Surprod"))
    #    } else if (all(is.na(stock_region_data$Catch)) == TRUE) {
    #      updateSelectInput(session, "size", "Choose variable for point size:",
    #                        c("MSY" = "MSY",
    #                          "Productivity" = "Surprod"))
    #    } else if (all(is.na(stock_region_data$Surprod)) == TRUE) {
    #      updateSelectInput(session, "size", "Choose variable for point size:",
    #                        c("MSY" = "MSY",
    #                          "Catch" = "Catch"))
    #    } else { #at the moment, didn't include options if more than 1 size scale is missing
    #      updateSelectInput(session, "size", "Choose variable for point size:",
    #                        c("MSY" = "MSY",
    #                          "Catch" = "Catch",
    #                          "Productivity" = "Surprod"))
    #    }
  })
  
  
  
  # Reactive variables:
  myYear <- reactive({
    input$tab1_Year
  })
  mytaxGroup <- reactive({
    input$tab1_taxGroup
  })
  mySize <- reactive({
    input$tab1_size
  })
  myStock <- reactive({
    input$tab1_stock
  })
  
  # Isolate min, max and legend titles for size scale in ggplot for selected region:
  region_min_size <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$tab1_size == "MSY") {
      size_scale[1, 1]
    } else if (input$tab1_size == "Catch") {
      size_scale[2, 1]
    } else {
      size_scale[3, 1]
    }
  })
  region_max_size <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$tab1_size == "MSY") {
      size_scale[1, 2]
    } else if (input$tab1_size == "Catch") {
      size_scale[2, 2]
    }  else {
      size_scale[3, 2]
    }
  })
  region_size_legend_title <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$tab1_size == "MSY") {
      rownames(size_scale)[1]
    } else if (input$tab1_size == "Catch") {
      rownames(size_scale)[2]
    }  else {
      rownames(size_scale)[3]
    }
  })
  
  # Select list of taxGroup type colors for the selected region:
  region_custom_colors <- reactive({
    named_custom_region_Colors[[myRegion()]]
  })
  
  # Create dataframe for selected region and stock, to pass to all outputs:
  region_myData <- reactive({
    region_subset <- region_split[[myRegion()]]
    if (myStock() == "All") {
      data <- filter(region_subset,
                     Year > myYear() - 1,
                     Year < myYear() + 1,
                     taxGroup %in% mytaxGroup()
      )
    } else {
      data <- filter(region_subset,
                     Year > myYear() - 1,
                     Year < myYear() + 1,
                     taxGroup %in% mytaxGroup(),
                     ID %in% myStock())
    }
    data <- as.data.frame(data)
    data
  })
  
  output$region_plot <- renderPlot({
  #   p <- kobe_plot_fun(region_myData(),
  #                 region_mySize(),
  #                 region_size_legend_title(),
  #                 region_min_size(),
  #                 region_max_size(),
  #                 "Taxonomy Group",
  #                 region_custom_colors())
  #   p
  # })
    ggplot(myData(), aes(x = BoverBMSY,
                         y = UoverUMSY,
                         color = taxGroup)) +
      geom_point(aes(size = !!as.symbol(mySize())),
                 alpha = 0.8) +
      # annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf,
      #          fill = "firebrick2", alpha = 0.1) +
      # annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1,
      #          fill = "yellowgreen", alpha = 0.1) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 1) +
      scale_size_continuous(name = region_size_legend_title(),
                            limits = c(region_min_size(), region_max_size()),
                            labels = scales::comma) +
      ylim(0, 3) +
      xlim(0, 3) +
      scale_color_manual(name = "Taxonomy Group",
                         values = region_custom_colors(),
                         drop = F) +
      theme_light() +
      labs(x = "B/BMSY", y = "U/UMSY") +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 18)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })
  
  output$my_tooltip <- renderUI({
    click <- input$plot_click 
    y <- nearPoints(myData(), input$plot_click)
    req(nrow(y) != 0)
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderText({
    click <- input$plot_click 
    y <- nearPoints(myData(), input$plot_click)[, c("ID", "Year", "MSY", "Catch", "Surprod")]
    req(nrow(y) != 0)
    if (nrow(y) == 1) {
      paste("Stock Name: ", y$ID, "\nYear: ", y$Year, "\nMSY: ", y$MSY, "\nCatch: ", 
            y$Catch, "\nProductivity: ", y$Surprod)
    } else {
      paste("\nStock Name: ", y$ID, "\nYear: ", y$Year, "\nMSY: ", y$MSY, "\nCatch: ", 
            y$Catch, "\nProductivity: ", y$Surprod) 
    }
  })
}

shinyApp(ui = ui, server = server)