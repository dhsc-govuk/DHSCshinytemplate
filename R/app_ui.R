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
