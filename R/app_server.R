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
  
  # Set the initial active navigation item after the UI has finished rendering.
  session$onFlushed(function() {
    session$sendCustomMessage(
      "set-active-contents-link",
      list(value = "landing")
    )
  }, once = TRUE)

  
  
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

  
  output$life_exp_gdp_table <- render_gov_table(
    input_id = "population_table_gov",
    caption = "",
    data_expr = function() {
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
    }
  )
  
  


} # end of server function - do not delete!
