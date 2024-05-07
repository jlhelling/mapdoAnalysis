#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_analysis
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#'
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),

    fluidPage(
      useShinyjs(),

      tags$head(tags$style(
        HTML(".form-group{margin-bottom: 10px}") # less space below selectInput metric_type
      )),

      fluidRow(
        column(width = 3, uiOutput(ns("groupingUI"))),
        # column
        column(width = 7, withSpinner(leafletOutput(ns(
          "analysemap"
        )), type = 6)),
        # column
        column(width = 2, uiOutput(ns(
          "groupOverviewUI"
        ))) # column
      ) # fluidRow

    ) # fluidPage)
  )# taglist
} # function mod_analysis_ui





#' analysis Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom shinyjs onclick runjs
#'
mod_analysis_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    ### REACTIVE VALUES ####
    r_val <- reactiveValues(
      ui_grouping = NULL, # selectInput of grouping

      region_already_clicked = FALSE, # check if region already clicked to show grouping selection
      profile_display = FALSE, # controle if metric and axis is selected = display the profile

      # map
      opacity = list(clickable = 0.01, not_clickable = 0.10), # opacity value to inform the user about available bassins and regions

      ### metric selected by user
      selected_metric = NULL, # select main metric column name
      selected_metric_name = NULL, # select main metric name to display for user
      selected_metric_type = NULL, # select main metric type name to display for user

      ### geoserver controler
      cql_filter = NULL, # WMS filter
      sld_body = NULL, # WMS SLD symbology

      # data
      bassins = NULL, # bassins data
      regions_in_bassin = NULL, # all the regions in selected bassin
      network_region_axis = NULL, # all the axis in the selected region
      selected_region_feature = NULL, # region data clicked
      region_click = NULL, # region clicked information list
      axis_click = NULL, # axis clicked information list
      dgo_axis = NULL, # all DGO in selected axis
      axis_start_end = NULL, # start / end df coordinates to add pin on map
      strahler = NULL, # min and max strahler values to set strahler filter UI
      min_max_metric = NULL, # min and max metric values to set metric filter UI
      selected_axis_df = NULL, # DGO in axis dataframe to plot longitudinal profile
      data_section = NULL, # DGO elevation data for section profile
      roe_region = NULL, # ROE data in selected region
      roe_axis = NULL, # ROE data in selected axis
      hydro_sites_region = NULL, # hydro sites data in selected region
      data_dgo_clicked = NULL, # DGO clicked by user for cross section profile

    )


    ### INIT MAP ####

    output$analysemap <- renderLeaflet({
      r_val$bassins = data_get_bassins(opacity = r_val$opacity, con = con)
      map_init_bassins(bassins_data = r_val$bassins)
    })


    ### INIT Grouping class ####

    # UI create choose group
    output$groupingUI <- renderUI({
      if (!is.null(r_val$ui_grouping)) {
        div(style = "display: flex; align-items: center; margin-bottom: 0px", r_val$ui_grouping, )
      } else {
        HTML(
          '<label class="control-label" id="wait-metric-label">
             Cliquez sur une région hydrographique pour afficher la sélection des groupages</label>'
        )
      }
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$analysemap_shape_click, {

      #### bassin clicked ####
      if (input$analysemap_shape_click$group == params_map_group()[["bassin"]]){
        # disable the click interactivity for the bassin selected
        r_val$bassins = r_val$bassins %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & cdbh == input$analysemap_shape_click$id,
                                 FALSE,
                                 click))
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$analysemap_shape_click$id,
                                                             opacity = r_val$opacity,
                                                             con = con)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("analysemap") %>%
          map_add_regions_in_bassin(bassin_click = input$analysemap_shape_click,
                                    regions_data = r_val$regions_in_bassin,
                                    bassins_data = r_val$bassins)
      }


      ### region clicked ####

      if (input$analysemap_shape_click$group == params_map_group()$region){
        # store the region click values
        r_val$region_click = input$analysemap_shape_click
        # disable the click interactivity for the bassin selected
        r_val$regions_in_bassin = r_val$regions_in_bassin %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & gid == r_val$region_click$id,
                                 FALSE,
                                 click))

        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id,
                                                        con = con)
        # set region name to download
        r_val$region_name = utils_normalize_string(r_val$selected_region_feature$lbregionhy)
        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con)
        # get ROE in region
        r_val$roe_region = data_get_roe_in_region(r_val$region_click$id,
                                                  con = con)
        # get hydro sites in region
        r_val$hydro_sites_region = data_get_hydro_sites(r_val$region_click$id,
                                                        con = con)
        # get strahler data
        r_val$strahler = isolate(data_get_min_max_strahler(selected_region_id = r_val$region_click$id,
                                                           con = con))


        # map region clicked with region clicked and overlayers
        leafletProxy("analysemap") %>%
          map_region_clicked(region_click = input$analysemap_shape_click,
                             selected_region_feature = r_val$selected_region_feature,
                             regions_data = r_val$regions_in_bassin,
                             roe_region = r_val$roe_region,
                             hydro_sites_region = r_val$hydro_sites_region)


        # run only once, control with region_already_clicked
        if (r_val$region_already_clicked == FALSE) {

          # build grouping selectInput
          r_val$ui_grouping =
            selectInput(
              ns("grouping"),
              "Sélectionnez un groupage :",
              choices = c("un", "deux", "trois"),
              selected  = c("un", "deux", "trois")[1]
            )

          r_val$region_already_clicked = TRUE
        }
      }

      ### axis clicked ####

      if (input$analysemap_shape_click$group == params_map_group()$axis) {
        # save the clicked axis values
        r_val$axis_click = input$analysemap_shape_click
        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con) %>%
          filter(axis != r_val$axis_click$id)
        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id,
                                               con = con) %>%
          mutate(measure = measure/1000)
        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)
        # get ROE in axis clicked
        r_val$roe_axis = r_val$roe_region %>%
          filter(axis == r_val$axis_click$id)

        # map dgo axis when axis clicked and metric selected
        leafletProxy("analysemap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                       main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric) %>%
          map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)

        # create or update profile dataset with new axis
        r_val$selected_axis_df = r_val$dgo_axis %>%
          as.data.frame()

      }

      ### dgo clicked ####

      if (input$analysemap_shape_click$group == params_map_group()$dgo_axis) {
        # get data with dgo id
        r_val$data_section = data_get_elevation_profiles(selected_dgo_fid = input$analysemap_shape_click$id,
                                                         con = con)
        # get dgo clicked feature
        r_val$data_dgo_clicked = r_val$dgo_axis %>%
          filter(fid == input$analysemap_shape_click$id)

        # Highlight clicked DGO
        leafletProxy("analysemap") %>%
          map_dgo_cross_section(selected_dgo = r_val$data_dgo_clicked)
      }

    })
  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
