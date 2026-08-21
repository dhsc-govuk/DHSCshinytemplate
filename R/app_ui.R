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

        # Global two-column application layout
        tags$div(
          id = "app-shell",
          class = "app-shell app-shell--no-navigation",

          # Sticky column containing the button and page navigation
          tags$div(
            id = "app-nav-column",
            class = "app-shell__nav-column",

            # Control for hiding or showing the page navigation
            tags$button(
              id = "nav-toggle",
              type = "button",
              class = "govuk-button govuk-button--secondary nav-toggle",
              `aria-expanded` = "true",
              `aria-controls` = "app-sidebar",
              "Hide navigation"
            ),

            # Dynamically populated page navigation
            tags$aside(
              id = "app-sidebar",
              class = "app-shell__sidebar",
              uiOutput("page_side_nav")
            )
          ),

          # Active top-level page
          tags$div(
            class = "app-shell__content",


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

              )
            )

          )
        ),
        shinyGovstyle::footer(full = TRUE)

      )
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

  add_resource_path("www", app_sys("app", "www"))

  # Select organisation
  organisation <- "department-of-health-social-care"

  # Find org colour from list
  # List taken from gov frontend
  org_colour <- govuk_org_colour(organisation)

  tags$head(
    favicon(),
    tags$style(
      HTML(
        sprintf(
          ":root {
           --department-colour: %s;
         }",
          org_colour
        )
      )
    ),
    bundle_resources(
      path = app_sys("app", "www"),
      app_title = "dhscshinytemplate"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
