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
      tags$a(
        href = "#main-content",
        class = "skip-link",
        "Skip to main content"
      ),
      
      # Enable shinyjs helpers
      shinyjs::useShinyjs(),

      
      # Two column layout:
      # - Left: navigation accordion
      # - Right: hidden tabset controlled via navigation links
      
      # Add top navigation below the header and skip link
      top_nav_links(),
      
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
            # Summary page ---------------------------------------------------
            tabPanel(
              "Summary Page",
              value = "summary",
              shinyGovstyle::gov_layout(
                size = "full",
                shinyGovstyle::heading_text("Summary", "l"),
                tags$div("This page shows a line chart and associated data table,
                         as well as a report that can be downloaded
                         by the user."),
                br(),
                tags$div(
                  class = "filter-card",
                  tags$div(
                    class = "filter-card__head",
                    tags$h2("Filter")
                  ),
                  tags$div(
                    class = "filter-card__body",
                    tags$div(
                      class = "filter-top-row",
                      tags$p(class = "govuk-body govuk-!-font-weight-bold", "Selected filters"),
                      tags$div(
                        class = "filter-clear",
                        actionButton(
                          "clear_filters",
                          "Clear filters",
                          class = "govuk-button govuk-button--secondary"
                        )
                      )
                    ),
  
                  selectizeInput(
                    "indicator", "Metric",
                    # Note try to avoid hard-coding choices where possible
                    choices = c(
                      "Life expectancy" = "lifeExp",
                      "GDP per capita" = "gdpPercap",
                      "Population" = "pop"
                    ),
                    selected = "lifeExp",
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  ),
                  selectizeInput(
                    "continent", "Continent",
                    choices = c("All" = "All", as.character(.continents)),
                    selected = "All",
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  ),
                  selectizeInput(
                    "country_focus", "Country focus",
                    choices = c("All selected" = "Total", "United Kingdom only" = "UK"),
                    selected = "Total",
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  ),
                  selectizeInput(
                    "countries", "Countries",
                    choices = .countries,
                    selected = c("United Kingdom", "France", "Germany", "Canada", "Australia"),
                    multiple = TRUE,
                    # AF main6 palette supports up to 6 distinct categorical colours
                    options = list(maxItems = 6, plugins = list("remove_button"))
                  )
                  )
                ),
                tags$h2(
                  class = "govuk-heading-m",
                  "Example dynamic text"
                ),
                textOutput("summary_dynamic_text"),
                br(),
                tabsetPanel(
                  tabPanel(
                    "Line Chart",
                    tags$h2(
                      class = "govuk-heading-m",
                      "Published MI"
                    ),
  
                    # Wrap plot in spinner, so that it shows a spinner
                    # when calculating/recalculating them
                    # rather than keeping them visible but greyed out
                    shinycssloaders::withSpinner(
                      plotOutput("line_chart", height = 420)
                    ),
                    tags$p("Data source: Gapminder"),
                    downloadButton("export_csv",
                      "Export CSV",
                      class = "govuk-button",
                      icon = NULL
                    ),
                    # Download button for PNG
                    downloadButton(
                      "download_line_chart_png",
                      "Download chart as PNG",
                      class = "govuk-button govuk-button--secondary",
                      icon = NULL
                    ),
                    br(),
                    br()
                  ),
                  tabPanel(
                    "Table",
                    tags$h3(
                      class = "govuk-heading-m",
                      "Chosen metric for selected countries over time"
                    ),
                    textOutput("chosen_metric"),
                    br(),
                    DT::DTOutput("tbl_summary_page"),
                    br(),
                    downloadButton("export_csv_summary_table",
                                   "Export CSV",
                                   class = "govuk-button",
                                   icon = NULL)
                  ),
                  tabPanel(
                    "Report",
                    tags$h2(
                      class = "govuk-heading-m",
                      "Customised report"
                    ),
                    tags$p("Download a customised report on a continent of your choice"),
                    selectizeInput(
                      "report_continent", "Continent",
                      choices = c("All" = "All", as.character(.continents)),
                      selected = "All",
                      multiple = TRUE,
                      options = list(maxItems = 1,
                                     plugins = list("remove_button"),
                                     dropdownParent = "body")
                    ),
                    downloadButton("export_report", "Export Report",
                                   class = "govuk-button",
                                   icon = NULL
                    ),
                    br(),
                    br()
                  )
                )
              )
            ),
            # Life expectancy page ----------------------------------------
            ## Life expectancy comparison ----------------------------------
            tabPanel(
              "Life expectancy",
              value = "life_expectancy",
              
              
                  
                  shiny::tags$h1(
                    class = "govuk-heading-xl",
                    "Life expectancy"
                  ),
                  
                  # Life expectancy comparison section 
                  shiny::tags$section(
                    id = "life-exp-comparison",
                    class = "app-page-section",
                    
                    shiny::tags$h2(
                      class = "govuk-heading-l",
                      "Comparison"
                    ),
                    
                  # Filter section
                  tags$div(
                    class = "filter-card",
                    tags$div(
                      class = "filter-card__head",
                      tags$h3("Filter")
                    ),
                    tags$div(
                      class = "filter-card__body",
                      selectInput("lexp_continent",
                                  "Continent",
                                  choices = .continents,
                                  selected = "Europe"
                      ),
                      selectInput("lexp_year_left",
                                  "Left year (baseline)",
                                  choices = .years,
                                  selected = min(.years)
                      ),
                      selectInput("lexp_year_right",
                                  "Right year (comparison)",
                                  choices = .years,
                                  selected = max(.years)
                      ),
                      numericInput("lexp_topn",
                                   "Top N countries (by largest change)",
                                   value = 10,
                                   min = 3,
                                   max = 30,
                                   step = 1
                      )
                    )
                  ),
                    # Tabs for life expectancy comparison 
                    tabsetPanel(
                      id = "pop_tabs",
                      
                      tabPanel(
                        "Chart",
                        div(
                          class = "govuk-tabs__panel",
                          textOutput(
                            "life_exp_title",
                            container = function(...) tags$h3(class = "govuk-heading-m",
                                                              ...)
                          ),
                          tags$p("Mirrored bars show baseline (left) and comparison (right) for the same countries"),
                          plotOutput("lifeexp_mirror_plot", height = 520)
                        )
                      ),
                      
                      tabPanel(
                        "Data table",
                        div(
                          class = "govuk-tabs__panel",
                          tags$h3(
                            class = "govuk-heading-m",
                            textOutput("life_exp_table_title")
                          ),
                      uiOutput("population_table"),
                      br(),
                      downloadButton("export_csv_pop_table",
                                     "Export CSV",
                                     class = "govuk-button",
                                     icon = NULL)

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
  
}

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
