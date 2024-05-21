

library(leaflet)
library(shiny)
library(shinycssloaders)
library(dplyr)
require(rhandsontable)
library(RColorBrewer)
load("~/repositories/mapdoAnalysis/data/network_dgo.rda")

# -------------------------------------------------------------------------

# rhandsontable UI and server -----------------------------------------------------------
# https://rpubs.com/jrowen/intro_rhandsontable
# https://jrowen.github.io/rhandsontable/#Types


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
                   color = {if(no_classes == 2){c("#B2182B", "#2166AC")} else{brewer.pal(no_classes, "RdBu")}},
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


# UI ----------------------------------------------------------------------

# Define UI for the main application
ui <- fluidPage(

  fluidRow(
    column(width = 3,
           uiOutput("manual_groupingUI")
    ),
    column(
      width = 7,
      withSpinner(
        leafletOutput("analysemap"),
        type = 6)
    )
  )
)




# Server ------------------------------------------------------------------


# Define server logic for the main application
server <- function(input, output) {


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


  # INIT map ----------------------------------------------------------------


  output$analysemap <-
    renderLeaflet({
      leaflet() %>%
        setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
        addScaleBar(position = "bottomleft",
                    scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })


  observeEvent(r_val$ui_grouping, {

    # GROUPING == MANUEL ------------------------------------------------------

    if (r_val$ui_grouping == "manuel") {


      # render grouping UI ------------------------------------------------------
      output$manual_groupingUI <- renderUI({
        tagList(
          p("Sélectionnez une variable et son quantile pour la définition des classes :"),
          selectInput("variable", "Variable",
                      choices = c("",names(network_dgo)[6:44]),
                      selected = "built_environment_pc"),

          fluidRow(
            column(width = 6, numericInput("quantile", "Quantile [%]", value = 95, min = 0, max = 100)),
            column(width = 4, numericInput("no_classes", "Nbre classes", value = 4, min = 2, max = 10, step = 1))
          ),

          rHandsontableOutput("hot"),
          # ediTable("tab"),
          actionButton("do", "Appliquez")
        )
      })

      # EVENTS ------------------------------------------------------------------

      # check when classes can be defined
      observeEvent(list(input$variable, input$quantile, input$no_classes),{

        req(input$variable, input$quantile, input$no_classes)

        # create classes-table
        r_val$grouping_table_data <- create_df_input(
          axis_data = network_dgo,
          variable_name = input$variable,
          no_classes = input$no_classes,
          quantile = input$quantile
        )

        print(r_val$grouping_table_data)
      })


      # update table when values are edited (either via editing the table or setting the variables in the UI)
      observeEvent(r_val$grouping_table_data, {

        output$hot <- renderRHandsontable({
          tmp <- isolate(r_val$grouping_table_data)# Gotta isolate it or it'll cause infinite loop, see https://github.com/jrowen/rhandsontable/issues/166
          rownames(tmp) <- NULL
          rhandsontable(
            tmp,
            rowHeaders = NULL
          ) %>%
            hot_col("variable", readOnly = TRUE, copyable = TRUE) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
      })

      # Update the reactive values when user edits table in the UI
      observeEvent(input$hot, {
        r_val$grouping_table_data <- hot_to_r(input$hot)
      })


      # when click apply groups to map
      observeEvent(input$do,{
        req(r_val$grouping_table_data)

        # Create classified network by adding the classes and colors
        classified_network <- network_dgo %>%
          assign_classes(variables = as.character(r_val$grouping_table_data$variable),
                         greater_thans = r_val$grouping_table_data$greaterthan,
                         class_names = r_val$grouping_table_data$class) %>%
          rowwise() %>%
          mutate(color = r_val$grouping_table_data %>% filter(class == class_name) %>% pull(color))

        # add classified network to map
        leafletProxy("analysemap") %>%
          addPolylines(data = classified_network,
                       # layerId = ~class,
                       weight = 5,
                       color = ~color,
                       opacity = 1,
                       label = ~classified_network[[input$variable]] %>%
                         sf::st_drop_geometry() %>%
                         round(2),
                       highlightOptions = highlightOptions(
                         color = "red",
                         bringToFront = TRUE
                       ))
      })
    }
  })
}


# RUN APP -----------------------------------------------------------------



# Run the application
shinyApp(ui = ui, server = server)
