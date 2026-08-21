#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ############# Styles, observers (do not change) ##########################
  # Apply afcharts theme and scale defaults globally
  # Ensures consistent styling across all plots
  afcharts::use_afcharts()

  # Load Gapminder dataset into local object
  # Used as the single data source across all tabs
  gap <- gapminder::gapminder


  ############# Main content (your content goes here) ##########################
  # Life expectancy and GDP ------------------------------
  # Healthcare Use: scatter of GDP per capita vs life expectancy
  life_exp_gdp <- reactive({
    req(input$scatter_year)
    yr <- as.integer(input$scatter_year[[1]])
    cont <- input$scatter_continent[[1]]

    d <- gap |>
      dplyr::filter(year == yr)

    if (!is.null(cont) && cont != "All") {
      d <- d |>
        dplyr::filter(continent == cont)
    }
    d
  })

  # Create subtitle
  output$life_exp_gdp_subtitle <- renderText({
    yr <- as.integer(input$scatter_year[[1]])

    paste0("Year: ", yr)
  })


  output$year_life_exp_gdp <- renderText({
    yr <- as.integer(input$scatter_year[[1]])
    paste0("Year: ", yr)
  })

  output$life_exp_scatter_plot <- plotly::renderPlotly({
    d <- life_exp_gdp()
    print(d)

    # Check there is data
    validate(need(nrow(d) > 0, "No data for selected filters."))
    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = gdpPercap,
                   y = lifeExp,
                   colour = continent,
                   shape = continent,
                   text = paste(
                     "Country:", country,
                     "<br>Continent:", continent,
                     "<br>Life expectancy:", round(lifeExp, 1),
                     "<br>GDP per capita:", scales::dollar(gdpPercap,
                                                           accuracy = 1)
                   ))) +
      ggplot2::geom_point(alpha = 0.85, size = 3) +
      ggplot2::scale_x_log10(labels = scales::label_dollar(accuracy = 1)) +
      afcharts::scale_colour_discrete_af("main6") +
      ggplot2::labs(
        x = "GDP per capita (log scale)",
        y = "Life expectancy (years)",
        colour = "",
        shape = ""
      ) +
      ggplot2::theme(legend.position = "top")


    plotly::ggplotly(p, tooltip = "text")
  })

  output$life_exp_gdp_table <- renderTable({
    d <- life_exp_gdp()


    d |>
      dplyr::select(country,
                    continent,
                    lifeExp,
                    gdpPercap) |>
      dplyr::rename(Country = country,
                    Continent = continent,
                    `Life Expectancy` = lifeExp,
                    `GDP per capita` = gdpPercap)
  })


  # Observers -----------------------------------------

  # Map service navigation input IDs to hidden tab values
  nav_map <- c(
    sn_landing = "landing",
    sn_life_exp_vs_gdpc = "life_exp_vs_gdpc"
  )

  # Create one observer per service navigation item
  purrr::iwalk(nav_map, function(tab_value, input_id) {
    shiny::observeEvent(
      input[[input_id]],
      {
        shiny::updateTabsetPanel(
          session = session,
          inputId = "tab-container",
          selected = tab_value
        )
      },
      ignoreInit = TRUE
    )
  })

  # Update the hidden tabset when the top navigation value changes
  observeEvent(input$`service_navigation`, {
    print("Updating tabset panel")
    shiny::updateTabsetPanel(
      session = session,
      inputId = "tab-container",
      selected = input$`service_navigation`
    )
  })

  # tab selection -> sidebar highlight
  observe({
    req(input$`tab-container`)

    session$sendCustomMessage(
      "set-active-contents-link",
      list(value = input$`tab-container`)
    )
  })

  # Keep country choices in sync with continent selection on Summary
  observeEvent(input$continent,
               {
                 sel_cont <- input$continent[[1]]

                 available <- if (!is.null(sel_cont) && sel_cont != "All") {
                   sort(unique(gap$country[gap$continent == sel_cont]))
                 } else {
                   countries_all
                 }

                 # Preserve any previously selected countries that are still valid
                 # after the continent filter changes.
                 current <- input$countries %||% character(0)
                 keep <- intersect(current, available)

                 # Fall back to a sensible default selection so the chart and table
                 # do not render empty after changing continent.
                 if (length(keep) == 0) {
                   keep <- intersect(c("United Kingdom", "France", "Germany", "Canada", "Australia"), available)
                   if (length(keep) == 0) keep <- head(available, 5)
                 }

                 updateSelectizeInput(session, "countries", choices = available, selected = keep, server = TRUE)
               },
               ignoreInit = FALSE
  )

} # end of server function - do not delete!
