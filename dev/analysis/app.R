# library(shiny)
# library(dplyr)
# # Define UI
# ui <- fluidPage(
#   selectInput("variable", "Select Variable", choices = colnames(network_dgo)),
#   numericInput("threshold1", "Threshold Value for Class 1", value = 0),
#   numericInput("threshold2", "Threshold Value for Class 2", value = 0),
#   numericInput("threshold3", "Threshold Value for Class 3", value = 0),
#   numericInput("threshold4", "Threshold Value for Class 4", value = 0),
#   numericInput("threshold5", "Threshold Value for Class 5", value = 0),
#   numericInput("threshold6", "Threshold Value for Class 6", value = 0),
#   textInput("class_name1", "Name for Class 1", value = "Class 1"),
#   textInput("class_name2", "Name for Class 2", value = "Class 2"),
#   textInput("class_name3", "Name for Class 3", value = "Class 3"),
#   textInput("class_name4", "Name for Class 4", value = "Class 4"),
#   textInput("class_name5", "Name for Class 5", value = "Class 5"),
#   textInput("class_name6", "Name for Class 6", value = "Class 6"),
#   actionButton("generate_classes", "Generate Classes"),
#   tableOutput("class_table")
# )
#
# # Define Server logic
# server <- function(input, output, session) {
#
#   # Reactive expression to generate class definitions
#   class_definitions <- eventReactive(input$generate_classes, {
#     variable <- input$variable
#     thresholds <- c(
#       input$threshold1, input$threshold2, input$threshold3,
#       input$threshold4, input$threshold5, input$threshold6
#     )
#     class_names <- c(
#       input$class_name1, input$class_name2, input$class_name3,
#       input$class_name4, input$class_name5, input$class_name6
#     )
#
#     classes <- data.frame(
#       Class = class_names,
#       Threshold = thresholds,
#       stringsAsFactors = FALSE
#     )
#
#     # Filter out classes with missing names or thresholds
#     classes <- classes[!is.na(classes$Class) & !is.na(classes$Threshold), ]
#
#     # Generate expressions based on user inputs
#     expressions <- lapply(classes$Threshold, function(threshold) {
#       expr <- sym(variable) > threshold
#       as.character(expr)
#     })
#
#     # Add expressions to the class definitions
#     classes$Expression <- expressions
#
#     return(classes)
#   })
#
#   # Render class definitions in a table
#   output$class_table <- renderTable({
#     req(input$generate_classes)
#     class_definitions()
#   })
#
# }
#
# # Run the application
# shinyApp(ui, server)




# rhandsontable UI and server -----------------------------------------------------------
# https://rpubs.com/jrowen/intro_rhandsontable
# https://jrowen.github.io/rhandsontable/#Types



library(leaflet)
library(shiny)
library(shinycssloaders)
library(dplyr)
require(rhandsontable)
library(RColorBrewer)
load("~/repositories/mapdoAnalysis/data/network_dgo.rda")



#' ediTable UI function
#'
#' @param id
#'
#' @return
#' @importFrom shiny NS
#' @importFrom rhandsontable rHandsontableOutput
#'
ediTable <- function(id) {
  ns <- NS(id)
  rHandsontableOutput(outputId = ns("hot"))

}

#' ediTable server logic
#'
#' @noRd
#'
#' @import shiny
#' @importFrom rhandsontable renderRHandsontable hot_col hot_context_menu hot_to_r
#'
ediTable_server <-
  function(id, rd) {
    moduleServer(id,
                 function(input, output, session) {
                   output$hot <- renderRHandsontable({
                     tmp <- isolate(rd())# Gotta isolate it or it'll cause infinite loop
                     # Necessary to avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                     rownames(tmp) <- NULL
                     rhandsontable(
                       tmp,
                       rowHeaders = NULL
                     ) %>%
                       hot_col("variable", readOnly = TRUE, copyable = TRUE) %>%
                       hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                   })

                   #Update the reactive values for this user-manipulated data to pass back to main environment
                   observeEvent(input$hot, {
                     tmp <- hot_to_r(input$hot)
                     rd(tmp)
                   })
                 })
  }


# UI ----------------------------------------------------------------------

# Define UI for the main application
ui <- function(id){

  ns <- NS(id)
  tagList(# Leave this function for adding external resources
    fluidPage(

      fluidRow(
        column(width = 3,
               uiOutput(ns("manual_groupingUI"))
        ),
        column(
          width = 7,
          withSpinner(
            leafletOutput(ns(
              "analysemap"
            )),
            type = 6)
        )
      )
    )
  )
}

#' Title
#'
#' @param axis_data
#' @param variable_name
#' @param no_classes
#' @param quantile
#'
#' @return
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
create_df_input <- function(axis_data, variable_name, no_classes, quantile = 95){

  # set upper and lower boundaries of quantile interval
  q_low <- (1 - quantile/100)/2
  q_high <- 1 - q_low

  # calculate quantile values (max, min) and steps
  q_values <- quantile(axis_data[[variable_name]], probs = c(q_low, q_high), na.rm = TRUE)
  q_steps <- (q_values[[2]] - q_values[[1]])/no_classes

  # empty dataframe to store class thresholds
  classes <- rep(0, no_classes)

  # set threshold values of all classes
  for (i in 1:no_classes) {
    classes[i] <- q_steps*(no_classes-i)
  }

  # create dataframe
  df <- data.frame(class = LETTERS[1:no_classes],
                   variable = variable_name,
                   greaterthan = classes,
                   color = brewer.pal(no_classes, "RdBu"),
                   stringsAsFactors = FALSE)
}

#' Assign classes to network dgos
#'
#' @param db
#' @param variables
#' @param greater_thans
#' @param class_names
#'
#' @return
#' @export
#'
#' @examples
assign_classes <- function(db, variables, greater_thans, class_names){
  db %>%
    mutate(
      class_name = case_when(
        !!!rlang::parse_exprs(
          paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        )
      )
    )}



# Server ------------------------------------------------------------------


# Define server logic for the main application
server <- <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  # REACTIVE VALUES ---------------------------------------------------------
  r_val <- reactiveValues(
    ui_grouping = "manuel", # SelectInput of grouping

    ui_variable = NULL,
    ui_quantile = NULL,
    ui_no_classes = NULL,
    grouping_table_data = NULL,
    ui_ediTable = NULL,

    ui_apply_groups = NULL
  )



  # reactive_grouping <-  reactiveVal(init_grouping)
  #
  # ediTable_server(id = "tab", rd = reactive_grouping)

  output$analysemap <-
    renderLeaflet({
      leaflet() %>%
        setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
        addScaleBar(position = "bottomleft",
                    scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })

  # create dataframe with automatically calculated class values
  reactive_grouping <- reactive({
    init_grouping <- create_df_input(
      axis_data = network_dgo,
      variable_name = input$variable,
      no_classes = input$no_classes,
      quantile = input$quantile)
    init_grouping
  })

  init <- reactiveVal(reactive_grouping)

  # create editable table
  ediTable_server(id = "tab", rd = init)

  # # EVENT variable or quantile inputs changed
  # observeEvent(list(input$variable, input$quantile, input$no_classes), {
  #
  #   if (input$variable != "") {
  #
  #   }
  # })

  # EVENT table changed
  observeEvent(input$do, {

    tmp <- reactive_grouping()

    output$data1 <- tmp %>% renderTable()

    # Apply the mutation to create class_1
    grouped_network <-
      network_dgo |>
      assign_classes(
        variables = as.character(tmp$variable),
        greater_thans = tmp$greaterthan,
        class_names = tmp$class
      ) |>
      rowwise() |>
      mutate(
        color = tmp |> filter(class == class_name) |> pull(color)
      )

    leafletProxy("analysemap") %>%
      addPolylines(data = grouped_network,
                   # layerId = ~class,
                   weight = 5,
                   color = ~color,
                   opacity = 1,
                   label = ~grouped_network[[input$variable]] %>%
                     sf::st_drop_geometry() %>%
                     round(2),
                   highlightOptions = highlightOptions(
                     color = "red",
                     bringToFront = TRUE
                   ))
  })
  })
}


# RUN APP -----------------------------------------------------------------



# Run the application
shinyApp(ui = ui, server = server)
