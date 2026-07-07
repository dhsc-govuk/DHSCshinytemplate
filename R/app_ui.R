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
      # Skip to main content
      shinyGovstyle::skip_to_main(),

      # GOV.UK style header
      # Main and secondary text remain unchanged from template
      shinyGovstyle::header(
        org_name = "Dashboard template",
        service_name = "",
        logo = "www/DHSC_logo.svg"
      ),

      # Enable shinyjs helpers
      shinyjs::useShinyjs(),


      # Two column layout:
      # - Left: navigation accordion
      # - Right: hidden tabset controlled via navigation links

      # Add top navigation below the header and skip link
      top_nav_links(),

      # Main content area using the existing hidden tabset
      shinyGovstyle::gov_main_layout(

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
                shinyGovstyle::heading_text("Landing page", "xl"),
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
                shinyGovstyle::heading_text("Summary", "xl"),
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

              shiny::tags$div(
                class = "app-page-with-side-nav",

                # Side navigation for page sections
                page_side_nav(
                  c(
                    "Comparison" = "life-exp-comparison",
                    "Distribution" = "life-exp-distribution",
                    "GDP relationship" = "life-exp-gdp",
                    "Trends" = "life-exp-trends"
                  ),
                  header_name = "Life expectancy pages"
                ),

                # Main content for the Life expectancy page
                shiny::tags$div(
                  class = "app-page-with-side-nav__content",

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

                        uiOutput("population_table"),
                        br(),
                        downloadButton("export_csv_pop_table",
                                       "Export CSV",
                                       class = "govuk-button",
                                       icon = NULL)
                      )
                    )
                  )
                ),
          ## Life expectancy distribution -----------------------------------
          shiny::tags$section(
            id = "life-exp-distribution",
            class = "app-page-section",
            shiny::tags$h2(
              class = "govuk-heading-l",
              "Distribution"
            ),
              tags$div(
                class = "filter-card",
                tags$div(
                  class = "filter-card__head",
                  tags$h3("Filter")
                ),
                tags$div(
                  class = "filter-card__body",
                  selectizeInput(
                    "dist_year", "Year",
                    choices = .years,
                    selected = max(.years),
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  ),
                  selectizeInput(
                    "dist_continent", "Continent",
                    choices = .continents,
                    selected = "Europe",
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  ),
                  selectizeInput(
                    "dist_topn", "Countries shown",
                    choices = c(10, 20, 30),
                    selected = 20,
                    multiple = TRUE,
                    options = list(maxItems = 1, plugins = list("remove_button"))
                  )
                )
              ),
                  tabsetPanel(
                    id = "pop_dist_tabs",
                    tabPanel(
                      "Inequalities chart",
                      div(
                        class = "ineq-chart-block",
                        textOutput(
                          "life_exp_dist_title",
                          container = function(...) tags$h3(class = "govuk-heading-m",
                                                            ...)
                        ),
                        textOutput("life_exp_dist_subtitle"),
                        tags$div(
                          class = "govuk-checkboxes__item",
                          tags$input(
                            class = "govuk-checkboxes__input",
                            id = "mort_ci_toggle",
                            type = "checkbox",
                            onchange = "Shiny.setInputValue('mort_show_ci', this.checked, {priority: 'event'});"
                          ),
                          tags$label(
                            class = "govuk-label govuk-checkboxes__label",
                            `for` = "mort_ci_toggle",
                            "Show confidence intervals"
                          )
                        )
                      ),
                      plotOutput("population_ineq_plot", height = 640)
                    ),
                    tabPanel(
                      "Data table",
                      div(
                        class = "govuk-tabs__panel",
                        textOutput(
                          "life_exp_dist_title_table",
                          container = function(...) tags$h3(class = "govuk-heading-m", ...)
                        ),
                        textOutput("life_exp_dist_subtitle_table"),
                        tableOutput("population_ineq_table"),
                        tags$p("Data source: Gapminder")
                      )
                    )
                  )
              ),
            ## Life expectancy vs GDP per capita ----------------------------
          shiny::tags$section(
            id = "life-exp-gdp",
            class = "app-page-section",

            shiny::tags$h2(
              class = "govuk-heading-l",
              "GDP relationship"
            ),
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
                        tableOutput("life_exp_gdp_table"),
                        tags$p("Data source: Gapminder")
                      )
                    )
                  )
                ),

              ## Life expectancy trends ------------------------------------
          shiny::tags$section(
            id = "life-exp-trends",
            class = "app-page-section",

            shiny::tags$h2(
              class = "govuk-heading-l",
              "Trends"
            ),

            tags$p("This page shows trends in life
                   expectancy using a line chart"),

            tags$h2(class = "govuk-heading-m",
                    "Outputs"),
            tabsetPanel(
              id = "life_exp_trends_tabs",
              tabPanel(
                "Chart",
                tags$h3(class = "govuk-heading-m",
                        "Life expectancy trend by continent"),
                plotOutput("continent_trend_plot", height = 520),
                # Short description of insights
                tags$p("Life expectancy has increased across all
                continents over time, with Europe and Oceania
                consistently having the highest values."),
                br(),
                tags$p("Data source: Gapminder")
          ),
          tabPanel("Table",
                   tags$h3(class = "govuk-heading-m",
                           "Trends in life expectancy by continent"),
                   tableOutput("continent_life_trends_table"),
                   tags$p("Data source: Gapminder")
                   )
                   )
                  )
                )
              )
            ),
            # Current population --------------------------------------------
            tabPanel(
              "Other charts",
              value = "other_charts",
              shinyGovstyle::gov_layout(
                size = "full",
                shinyGovstyle::heading_text("Other charts", "xl"),
                shinyGovstyle::heading_text("Current population", "l"),
                tags$p("Illustrative snapshot from Gapminder: population by continent."),
                tags$div(
                  class = "filter-card",
                  tags$div(
                    class = "filter-card__head",
                    tags$h2("Filter")
                  ),
                  tags$div(
                    class = "filter-card__body",
                    selectizeInput(
                      "cont_pop_year", "Year",
                      choices = .years,
                      selected = max(.years),
                      multiple = TRUE,
                      options = list(maxItems = 1, plugins = list("remove_button"))
                    )
                  )
                ),
                  tabsetPanel(
                    id = "current_pop_tabs",
                    tabPanel(
                      "Chart",
                      tags$h3(class = "govuk-heading-m",
                              "Population by continent"),
                      textOutput("current_pop_year_plot"),
                      plotOutput("current_pop_plot", height = 520),
                      tags$p("Data source: Gapminder")
                    ),
                    tabPanel(
                      "Table",
                      value = "current_pop_table",
                      shinyGovstyle::gov_layout(
                        size = "full",
                        tags$h3(
                          class = "govuk-heading-m",
                          "Current population"
                        ),
                        textOutput("current_pop_year"),
                        br(),
                        tableOutput("current_pop_table"),
                        tags$p("Data source: Gapminder")
                      )
                    )
                  )
                ),
            # GDP per capita -----------------------------------------------
                shinyGovstyle::heading_text("GDP per capita", "l"),
                tags$p("Illustrative distribution from Gapminder: GDP per capita by continent."),
                tags$div(
                  class = "filter-card",
                  tags$div(
                    class = "filter-card__head",
                    tags$h2("Filter")
                  ),
                  tags$div(
                    class = "filter-card__body",
                    selectizeInput(
                      "gdp_year", "Year",
                      choices = .years,
                      selected = max(.years),
                      multiple = TRUE,
                      options = list(maxItems = 1, plugins = list("remove_button"))
                    )
                  )
                ),
                tabsetPanel(
                  id = "gdp_by_cap_tabs",
                    # Tab for chart
                    tabPanel(
                      "Chart",
                      tags$h3(class = "govuk-heading-m",
                              "GDP per capita by continent"),
                      textOutput("gdp_per_cap_yr_plot"),
                      plotOutput("gdp_boxplot", height = 520),
                      tags$p("Data source: Gapminder")
                    ),
                    # Tab for data table
                    tabPanel(
                      "Table",
                      tags$h3(class = "govuk-heading-m",
                              "GDP per capita by continent"),
                      textOutput("gdp_per_cap_yr"),
                      tableOutput("gdp_table"),
                      tags$p("Data source: Gapminder")
                    )
                  )

            ),
              # Definitions ----------------------------------------------
              tabPanel(
                "Definitions", value = "definition",
                shinyGovstyle::gov_layout(
                  size = "full",
                  shinyGovstyle::heading_text("Definitions", "xl"),
                  tags$p("This dashboard uses the Gapminder dataset from the R package \"gapminder\"."),
                  tags$h2(class = "govuk-heading-m",
                          "Variables"),
                  tableOutput("definition_table"),
                  tags$h2(class = "govuk-heading-m",
                          "Dataset coverage"),
                  tabsetPanel(
                    id = "defn_tabs",
                    tabPanel(
                      "Chart",
                      tags$h3(class = "govuk-heading-m",
                              "Countries covered in the dataset by continent"),
                      plotOutput("coverage_plot", height = 420),
                      tags$p("Data source: Gapminder")
                    ),
                    tabPanel(
                      "Table",
                      tags$h3(class = "govuk-heading-m",
                              "Countries covered in the dataset by continent"),
                      tableOutput("coverage_tbl"),
                      tags$p("Data source: Gapminder")
                    )
                  )
                )
              ),
            # Example map ----------------------------------------------
            tabPanel(
              "Example map",
              value = "uk_map",
              shinyGovstyle::gov_layout(
                size = "full",
                shinyGovstyle::heading_text("Population by Local Authority", "xl"),
                tags$div(
                  class = "filter-card",
                  tags$div(
                    class = "filter-card__head",
                    tags$h2("Filter")
                  ),
                  tags$div(
                    class = "filter-card__body",
                    selectInput(
                      "selected_la",
                      "Select a local authority",
                      choices = NULL,
                      selected = NULL
                    ),
                    checkboxInput(
                      "reverse_palette",
                      "Reverse colour scale",
                      value = FALSE
                    )
                  )
                ),
                tags$h2(class = "govuk-heading-m", "Example dynamic text"),
                textOutput("uk_map_summary_text"),
                tabsetPanel(
                  tabPanel(
                    "Map",
                    leaflet::leafletOutput("la_map", height = 700),
                    tags$p("Data source: Nomis / ONS"),
                    downloadButton(
                      "map_export_csv",
                      "Export CSV",
                      class = "govuk-button",
                      icon = NULL
                    )
                  ),
                  tabPanel(
                    "Table",
                    tableOutput("map_tbl")
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
