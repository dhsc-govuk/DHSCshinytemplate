#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  shiny::tagList(
    # Call the function that brings in external resources
    golem_add_external_resources(),

    # Application UI logic
    shiny::fluidPage(
     
      # GOV.UK style header
      # Main and secondary text remain unchanged from template
      shinyGovstyle::header(
        org_name = "Dashboard template",
        service_name = "",
        logo = "www/DHSC_logo.svg"
      ),
      # Skip link (for screen readers)
      tags$a(
        href = "#main-content",
        class = "skip-link",
        "Skip to main content"
      ),
      
      # Enable shinyjs helpers
      shinyjs::useShinyjs(),
      
      # Add top navigation below the header and skip link
      top_nav_links(),
      
      ######## Main content area - your content goes here #################
      # Main content area using the existing hidden tabset
      tags$main(
        id = "main-content",
        tabindex = "-1",
        class = "govuk-main-wrapper govuk-width-container",
          
          # Tabset panel for all the pages
          tabsetPanel(
            type = "hidden",
            id = "tab-container",
            # Landing page ------------------------------------------------
            tabPanel(
              "Landing page",
              value = "landing",
              shinyGovstyle::gov_layout(
                size = "full",
                shinyGovstyle::heading_text("Landing page", "l"),
                tags$div("This dashboard provides an example of what 
                       tables and charts can be provided in Shiny within
                       DHSC. It uses the DfE shinyGovStyle and 
                       the afcharts package and conforms closely 
                       to GDS standards on accessibility and branding."),
                br(),
                tags$div("The data comes from the gapminder package in R,
                           so this demonstration can be run by anyone with 
                         access to R."),
                br()
                )
              ),

            tabPanel(
              "Life expectancy vs GDP per capita",
              value = "life_exp_vs_gdpc",
              shinyGovstyle::gov_layout(
                size = "full",
                shinyGovstyle::heading_text("Life expectancy", "l"),
                tags$div(
                  class = "filter-card",
                  tags$div(
                    class = "filter-card__head",
                    tags$h3("Filter")
                  ),
                  tags$div(
                    class = "filter-card__body",
                    selectizeInput(
                      "scatter_year", "Year",
                      choices = .years,
                      selected = max(.years),
                      multiple = TRUE,
                      options = list(maxItems = 1, plugins = list("remove_button"))
                    ),
                    selectizeInput(
                      "scatter_continent", "Continent",
                      choices = c("All" = "All", stats::setNames(as.character(.continents),
                                                                 as.character(.continents))),
                      selected = "All",
                      multiple = TRUE,
                      options = list(maxItems = 1, plugins = list("remove_button"))
                    )
                  )
                ),

                  # Tabs
                  tabsetPanel(
                    id = "life_expect_tabs",

                    tabPanel(
                      "Plot",
                      tags$h3(class = "govuk-heading-m",
                              "GDP per capita and life expectancy"),
                      textOutput("life_exp_gdp_subtitle"),
                      plotly::plotlyOutput("life_exp_scatter_plot", height = 520),
                      tags$p("Data source: Gapminder")
                    ),

                    tabPanel(
                      "Data table",
                      div(
                        class = "govuk-tabs__panel",
                        tags$h3(
                          class = "govuk-heading-m",
                          "Life expectancy vs GDP per capita"
                        ),
                        textOutput("year_life_exp_gdp"),
                        # Table
                        uiOutput("life_exp_gdp_table"),
                        tags$p("Data source: Gapminder")
                      )
                    )
                  )
                )
              )

              )
        ),
      shinyGovstyle::footer(full = TRUE)

    )
  )
  
} # end of main UI


### Add styling - do not change #####################################
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app", "www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app", "www"),
      app_title = "dhscshinytemplate"
    ),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(src = "www/app.js")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
