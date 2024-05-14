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
library(shiny)
library(dplyr)
require(rhandsontable)



#------

#Definition of UI part of ediTable module

ediTable <- function(id, ...) {
  ns <- NS(id)
  rHandsontableOutput(outputId = ns("hot"), ...)

}

#Server logic for the module

ediTable_server <-
  function(id,
           rd,
           allowRowEdit = TRUE,
           allowColumnEdit = FALSE,
           manualRowMove = TRUE,
           ...) {
    moduleServer(id,
                 function(input, output, session) {
                   output$hot <- renderRHandsontable({
                     tmp <- isolate(rd())#Gotta isolate it or it'll cause infinite loop
                     #Necessary to avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                     rownames(tmp) <- NULL
                     rhandsontable(
                       tmp,
                       allowRowEdit = allowRowEdit,
                       allowColumnEdit = allowColumnEdit,
                       manualRowMove = manualRowMove,
                       ...
                     )

                   })

                   #Update the reactive values for this user-manipulated data to pass back to main environment
                   observeEvent(input$hot, {

                     tmp <- rhandsontable::hot_to_r(input$hot)

                     rd(tmp)


                   })

                 })
  }



# Define UI for the main application
ui <- fluidPage(
  # Application title
  titlePanel("ediTable: editable table widget"),

    # Show a plot of the generated distribution
    mainPanel(
      h1("Table 1: a reactive module"),
      p("Double click a cell to edit."),
      p("Right click to add or remove rows; click and drag to move."),
      ediTable(id = "tab"),
      h3("Outputting the edited data for Table 1"),
      tableOutput("data1")
    )
)

# Define server logic for the main application
server <- function(input, output) {
  # init_data <- head(mtcars)
  init_data <- data.frame(groupname = LETTERS[1:6],
                          variable = factor(rep(names(network_dgo)[1],6), levels = names(network_dgo),
                                            ordered = TRUE),
                               threshold = rep(0, 6),
                               stringsAsFactors = FALSE)

  reactive_data1 <-  reactiveVal(init_data)

  ediTable_server(id = "tab", rd = reactive_data1)

  observe({
    tmp <- reactive_data1()
    output$data1 <-
      tmp %>% renderTable()

  })
}

# Run the application
shinyApp(ui = ui, server = server)


