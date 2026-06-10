#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Apply afcharts theme and scale defaults globally
  # Ensures consistent styling across all plots
  afcharts::use_afcharts()

  # Load Gapminder dataset into local object
  # Used as the single data source across all tabs
  gap <- gapminder::gapminder

  # Pre-compute common choice lists so controls can be populated consistently
  # without recalculating unique values in multiple reactives.
  years <- sort(unique(gap$year))
  continents <- sort(unique(gap$continent))
  countries_all <- sort(unique(gap$country))

  # Map service navigation input IDs to hidden tab values
  nav_map <- c(
    sn_landing = "landing",
    sn_summary = "summary",
    sn_life_expectancy = "life_expectancy",
    sn_other_charts = "other_charts",
    sn_uk_map = "uk_map",
    sn_definition = "definition"
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

  # Summary page --------------------------------------------------
  
  # Reactive: filtered_summary()
  # Filters Gapminder data based on selected:
  # - metric
  # - continent
  # - countries
  # Returns long-format dataset for plotting and table
  filtered_summary <- reactive({
    req(input$indicator, input$continent, input$country_focus)

    metric <- input$indicator[[1]]
    cont <- input$continent[[1]]
    scope <- input$country_focus[[1]]

    d <- gap

    if (!is.null(cont) && cont != "All") {
      d <- d |> dplyr::filter(continent == cont)
    }

    # Override country selections when the UK-only focus option is selected.
    if (identical(scope, "UK")) {
      d <- d |> dplyr::filter(country == "United Kingdom")
    } else {
      if (!is.null(input$countries) && length(input$countries) > 0) {
        # Keep to 6 countries to align with the afcharts main6 palette
        d <- d |> dplyr::filter(country %in% head(input$countries, 6))
      }
    }

    # Standardise the selected metric into a common Value column so downstream
    # plots and tables can reuse the same code.
    d |>
      dplyr::mutate(
        Value = .data[[metric]]
      ) |>
      dplyr::rename(
        Year = year,
        Country = country,
        Continent = continent,
        Metric = metric
      ) |>
      dplyr::select(
        c(Year,
        Country,
        Continent,
        Metric,
        Value)
      ) |>

      dplyr::arrange(Country, Year)
  })

  # Create dynamic text using the filtered summary dataset
  output$summary_dynamic_text <- renderText({
    d <- filtered_summary()
    req(nrow(d) > 0)

    # Calculate summary values from the filtered data
    n_countries <- dplyr::n_distinct(d$Country)
    min_year <- min(d$Year, na.rm = TRUE)
    max_year <- max(d$Year, na.rm = TRUE)
    metric <- unique(d$Metric)[1]

    # Convert metric code into a readable label
    metric_label <- dplyr::case_when(
      metric == "lifeExp" ~ "life expectancy",
      metric == "gdpPercap" ~ "GDP per capita",
      metric == "pop" ~ "population",
      TRUE ~ "value"
    )

    paste0(
      "This view shows ",
      metric_label,
      " for ",
      n_countries,
      " countries from ",
      min_year,
      " to ",
      max_year,
      "."
    )
  })

  # Time series plot
  # Colour mapped to country
  # Guarded to prevent rendering during reactive instability
  make_line_chart <- function() {
    d <- filtered_summary()
    
    # Require these things before rendering
    req(
      input$countries,
      length(input$countries) > 0,
      # Limit the number of series to match the 
      # available six-colour chart palette.
      length(unique(d$Country)) <= 6
    )
    
    # If there is no data, show warning
    validate(need(nrow(d) > 0, "No data for selected filters."))
    
    metric <- unique(d$Metric)[1]
    y_lab <- dplyr::case_when(
      metric == "lifeExp" ~ "Life expectancy (years)",
      metric == "gdpPercap" ~ "US dollars",
      metric == "pop" ~ "People",
      TRUE ~ "Value"
    )
    
    ggplot2::ggplot(d, ggplot2::aes(x = Year, y = Value, group = Country, colour = Country)) +
      ggplot2::geom_line(linewidth = 0.9) +
      # 'main' only has 4 colours; use 'main6' for multi-series charts
      afcharts::scale_colour_discrete_af("main6") +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma()
      ) +
      ggplot2::labs(
        x = "",
        y = y_lab,
        colour = ""
      ) +
      ggplot2::theme(legend.position = "right")
  }
  
  
  output$line_chart <- renderPlot({
    make_line_chart()
  },
  alt = "A plot of the chosen metric for selected countries over time")
  
  output$chosen_metric <- renderText({
    req(input$indicator)

    metric <- input$indicator[[1]]

    full_metric <- dplyr::case_when(
      metric == "lifeExp" ~ "Life Expectancy",
      metric == "gdpPercap" ~ "GDP per capita",
      metric == "pop" ~ "Population",
      TRUE ~ "Value"
    )

    paste(
      "Metric:",
      full_metric
    )
  })
  
  # Give an example of a Datatable table
  # (as opposed to a basic shiny table)
  output$tbl_summary_page <- DT::renderDT(
    {
      d <- filtered_summary()

      d |>
        dplyr::select(
          Year,
          Country,
          Continent,
          Value
        )
    }, options = list(
      pageLength = 25,
      dom = "tip",       # removes search box & extra controls
      ordering = TRUE
    ))

  # Clear filters
  observeEvent(input$clear_filters,
    ignoreInit = TRUE,
    {
      updateSelectizeInput(session, "indicator", selected = "lifeExp")
      updateSelectizeInput(session, "continent", selected = "All")
      updateSelectizeInput(session, "country_focus", selected = "Total")
      updateSelectizeInput(
        session,
        "countries",
        selected = c("United Kingdom", "France", "Germany", "Canada", "Australia")
      )
    }
  )

  # Download for csv 
  # (Shared helper used by both the chart and summary table exports)
  export_handler_summary <- function(filename_prefix) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_filtered_", Sys.Date(), ".csv"),
      content = function(file) write.csv(filtered_summary(), file, row.names = FALSE)
    )
  }
  output$export_csv <- export_handler_summary("gapminder")
  output$export_csv_summary_table <- export_handler_summary("gapminder")
  
  # Create a PNG file of the chart when the user clicks download
  output$download_line_chart_png <- downloadHandler(
    filename = function() {
      paste0("line-chart-", Sys.Date(), ".png")
    },
    content = function(file) {
      png(filename = file, width = 1600, height = 900, res = 150)
      
      p <- make_line_chart()
      print(p)
      dev.off()
    }
  )
  
  
  ## Report -----------------------------------------------
  # Create a customised report, using the code in inst/app/www/report.qmd
  output$export_report <- downloadHandler(
    filename = reactive(stringr::str_glue("report_{input$report_continent}.html")),
    content = function(filename) {
      shinybusy::show_modal_spinner(
        text = "Generating HTML report. This can take up to 30 seconds."
      )
      
      # Always remove the spinner, even if the report render fails.
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      # quarto is picky over rendering location, so generate then copy
      quarto::quarto_render(
        input = here::here("inst", "app", "www", "report.qmd"),
        execute_params = list(continent = input$report_continent)
      )
      file.copy(here::here("inst", "app", "www", "report.html"), filename)
    }
  )

  # Life expectancy comparison -------------------------------------
  lifeexp_mirror_df <- reactive({
    req(input$lexp_continent, input$lexp_year_left, input$lexp_year_right, input$lexp_topn)

    cont <- input$lexp_continent[[1]]
    y_left <- as.integer(input$lexp_year_left[[1]])
    y_right <- as.integer(input$lexp_year_right[[1]])
    topn <- as.integer(input$lexp_topn[[1]])

    validate(
      need(y_left != y_right, "Choose two different years."),
      need(y_left < y_right, "Baseline year must be earlier than comparison year.")
    )

    d <- gap |>
      dplyr::filter(continent == cont, year %in% c(y_left, y_right)) |>
      dplyr::select(country, year, lifeExp) |>
      tidyr::pivot_wider(names_from = year, values_from = lifeExp)

    # Handle countries missing in either year
    validate(need(nrow(d) > 0, "No data for that selection."))

    left_col <- as.character(y_left)
    right_col <- as.character(y_right)

    d2 <- d |>
      dplyr::filter(!is.na(.data[[left_col]]), !is.na(.data[[right_col]])) |>
      dplyr::mutate(
        change = .data[[right_col]] - .data[[left_col]],
        abs_change = abs(change)
      ) |>
      dplyr::slice_max(order_by = abs_change, n = topn, with_ties = FALSE)

    validate(need(nrow(d2) > 0, "No complete country pairs for the two years."))

    # Build mirrored dataset: left year negative, right year positive
    d_left <- d2 |> 
      dplyr::mutate(
        Year = y_left,
        Side = paste0("Life expectancy ", y_left),
        LifeExp = -.data[[left_col]]
      ) |>
      dplyr::rename(
        Country = country
      ) |>
      dplyr::select(Country, Year, Side, LifeExp, change, abs_change)

    d_right <- d2 |>
      dplyr::mutate(
        Year = y_right,
        Side = paste0("Life expectancy ", y_right),
        LifeExp = .data[[right_col]]
      ) |>
      dplyr::rename(
        Country = country
      ) |>
      dplyr::select(Country, Year, Side, LifeExp, change, abs_change)

    out <- dplyr::bind_rows(d_left, d_right) |>
      dplyr::mutate(
        Country = stats::reorder(Country, abs(LifeExp))
      )

    out
  })

  output$lifeexp_mirror_plot <- renderPlot({
    d <- lifeexp_mirror_df()

    max_x <- max(abs(d$LifeExp), na.rm = TRUE)
    
    # Add in gridlines 
    breaks_y <- scales::pretty_breaks(n = 6)(c(-max_x, max_x))
    breaks_y <- breaks_y[breaks_y >= - max_x & breaks_y <= max_x]
    breaks_grid <- setdiff(breaks_y, 0)

    ggplot2::ggplot(d, ggplot2::aes(x = Country,
                                    y = LifeExp,
                                    fill = Side)) +
      ggplot2::geom_hline(yintercept = breaks_grid,
                          colour = "grey80",
                          linewidth = 0.3) +
      ggplot2::geom_col(width = 0.8) +
      ggplot2::coord_flip() +
      afcharts::scale_fill_discrete_af() +
      ggplot2::scale_y_continuous(
        limits = c(-max_x,
                   max_x),
        labels = function(x) sprintf("%s", abs(x)),
        breaks = breaks_y
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Life expectancy (years)",
        fill = "",
        caption = "Source: Gapminder"
      ) +
      ggplot2::theme(legend.position = "top",
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 14),
                     axis.text.x = ggplot2::element_text(size = 14),
                     axis.title = ggplot2::element_text(size = 15)
      )
  },
  alt = "A plot of life expectancy change in the selected continent")
  
  output$life_exp_title <- renderText({
    y_left <- as.integer(input$lexp_year_left[[1]])
    y_right <- as.integer(input$lexp_year_right[[1]])
    cont <- input$lexp_continent[[1]]
    
    paste0("Life expectancy change in ",
           cont,
           " (",
           y_left,
           " vs ",
           y_right,
           ")")
  })
  
  output$life_exp_table_title <- renderText({
    y_left <- as.integer(input$lexp_year_left[[1]])
    y_right <- as.integer(input$lexp_year_right[[1]])
    cont <- input$lexp_continent[[1]]
    
    paste0("Life expectancy change in ",
           cont,
           " (",
           y_left,
           " vs ",
           y_right,
           ")")
  })
  
  life_exp_table_title <- reactive({
    y_left <- as.integer(input$lexp_year_left[[1]])
    y_right <- as.integer(input$lexp_year_right[[1]])
    cont <- input$lexp_continent[[1]]
    
    paste0("Life expectancy change in ",
           cont,
           " (",
           y_left,
           " vs ",
           y_right,
           ")")
  })

  
  output$population_table <- render_gov_table(
    input_id = "population_table_gov",
    caption = life_exp_table_title(),
    data_expr = function() {
      lifeexp_mirror_df() |>
        dplyr::select(
          Country,
          Year,
          LifeExp
        ) |>
        dplyr::arrange(
          Country,
          Year
        ) |>
        dplyr::rename(
          `Life expectancy` = LifeExp
        )
    }
  )
  
  # Download for csv
  export_handler_pop <- function(filename_prefix) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_filtered_", Sys.Date(), ".csv"),
      content = function(file) write.csv(lifeexp_mirror_df(), file, row.names = FALSE)
    )
  }
  output$export_csv_pop_table <- export_handler_pop("gapminder_pop")

  # Life expectancy distribution
  # Reactive: dist_df()
  # Computes:
  # - Life expectancy for selected year
  # - Continental average
  # - Derived 2.5%–97.5% band across years
  # - Status classification (Better / Similar / Worse)
  dist_df <- reactive({
    req(input$dist_year, input$dist_continent, input$dist_topn)

    yr <- as.integer(input$dist_year[[1]])
    cont <- input$dist_continent[[1]]
    topn <- as.integer(input$dist_topn[[1]])

    current <- gap |>
      dplyr::filter(year == yr, continent == cont) |>
      dplyr::select(country, lifeExp)

    band <- gap |>
      dplyr::filter(continent == cont) |>
      dplyr::group_by(country) |>
      dplyr::summarise(
        lo = quantile(lifeExp, 0.025, na.rm = TRUE),
        hi = quantile(lifeExp, 0.975, na.rm = TRUE),
        .groups = "drop"
      )

    d <- current |> dplyr::inner_join(band, by = "country")
    avg <- mean(d$lifeExp, na.rm = TRUE)

    d |>
      dplyr::mutate(
        avg = avg,
        status = dplyr::case_when(
          lifeExp >= avg * 1.05 ~ "Better",
          lifeExp <= avg * 0.95 ~ "Worse",
          TRUE ~ "Similar"
        )
      ) |>
      dplyr::slice_max(order_by = abs(lifeExp - avg), n = topn, with_ties = FALSE) |>
      dplyr::mutate(
        country = reorder(country, lifeExp),
        status = factor(status, levels = c("Better", "Similar", "Worse"))
      )
  })
  
  output$life_exp_dist_title <- renderText({
    cont <- input$dist_continent[[1]]
    
    paste0("Life expectancy by country (",
           cont,
           ")")
    })

  output$life_exp_dist_subtitle <- renderText({
    show_ci <- isTRUE(input$mort_show_ci)
    cont <- input$dist_continent[[1]]
    yr <- input$dist_year[[1]]
    
    paste0(
      "Year: ",
      yr,
      if (show_ci) " (band is 2.5% to 97.5% across years)" else ""
    )
  })
  
  # Same title & subtitle for table (note output IDs need to be unique)
  output$life_exp_dist_title_table <- renderText({
    cont <- input$dist_continent[[1]]
    
    paste0("Life expectancy by country (",
           cont,
           ")")
  })
  
  output$life_exp_dist_subtitle_table <- renderText({
    show_ci <- isTRUE(input$mort_show_ci)
    cont <- input$dist_continent[[1]]
    yr <- input$dist_year[[1]]
    
    paste0(
      "Year: ",
      yr,
      if (show_ci) " (band is 2.5% to 97.5% across years)" else ""
    )
  })
  
  output$population_ineq_plot <- renderPlot({
    d <- dist_df()
    validate(need(nrow(d) > 0, "No data for selected filters."))

    # Conditional error bars added only when checkbox is TRUE
    # Prevents unnecessary rendering overhead
    show_ci <- isTRUE(input$mort_show_ci)
    cont <- input$dist_continent[[1]]
    yr <- input$dist_year[[1]]
    
    avg_val <- unique(d$avg)
    
    # Get the number of plotted countries
    n_countries <- nrow(d)
    

    ggplot2::ggplot(d, ggplot2::aes(x = country, y = lifeExp, fill = status)) +
      ggplot2::geom_col(width = 0.8) +
      {
        if (show_ci) ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2)
      } +
      # Add average line
      ggplot2::geom_hline(yintercept = avg_val, linewidth = 0.8) +
      # Add text to line
      ggplot2::annotate(
        "text",
        x = n_countries + 1.2,
        y = avg_val,
        label = "Average for this\ncontinent",
        size = 16 / 3, # Font size 16
        vjust = 0
      ) +
      ggplot2::coord_flip(clip = "off") +
      afcharts::scale_fill_discrete_af() +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Life expectancy (years)",
        fill = ""
      ) +
      ggplot2::theme(legend.position = "top",
                     plot.margin = ggplot2::margin(30, 10, 5.5, 5.5))
  },
  alt = "A plot of life expectancy by country in the selected continent")
  
  output$population_ineq_table <- renderTable({
    dist_df() |>
      dplyr::rename(
        Country = country,
        `Life expectancy` = lifeExp,
        `Low band` = lo,
        `High band` = hi,
        Status = status
      )
  }, striped = FALSE, bordered = FALSE, spacing = "s")
  
  observeEvent(TRUE, {
    if (is.null(input$mort_show_ci)) {
      session$sendInputMessage("mort_show_ci", list(value = FALSE))
    }
  }, once = TRUE)
  
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
    #,
  #alt = "A plot of GDP per capita and life expectancy"
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
  }, striped = FALSE, bordered = FALSE, spacing = "s")
  
  # Life expectancy: continent trends
  continent_trends_life <- reactive({
    d <- gap |>
      dplyr::group_by(continent, year) |>
      dplyr::summarise(lifeExp = mean(lifeExp, na.rm = TRUE), .groups = "drop")
  })

  output$continent_trend_plot <- renderPlot({
    d <- continent_trends_life()

    ggplot2::ggplot(d, ggplot2::aes(x = year, y = lifeExp, colour = continent)) +
      ggplot2::geom_line(linewidth = 1) +
      afcharts::scale_colour_discrete_af("main6") +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      ggplot2::labs(
        x = "Year",
        y = "Mean life expectancy (years)",
        colour = ""
      ) +
      ggplot2::theme(legend.position = "top")
  },
  alt = "A plot of life expectancy trend by continent")


  output$continent_life_trends_table <- renderTable(
    {
      d <- continent_trends_life()

      d |>
        dplyr::rename(
          Continent = continent,
          Year = year,
          `Life Expectancy` = lifeExp
        )
    },
    striped = FALSE,
    bordered = FALSE,
    spacing = "s"
  )

  # Population by continent -----------------------------------
  current_pop <-
    reactive({
      req(input$cont_pop_year)
      yr <- as.integer(input$cont_pop_year[[1]])

      d <- gap |>
        dplyr::filter(year == yr) |>
        dplyr::group_by(continent) |>
        dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(pop_m = pop / 1e6, continent = reorder(continent, pop_m))
    })

  output$current_pop_plot <- renderPlot({
    d <- current_pop()

    yr <- as.integer(input$cont_pop_year[[1]])

    ggplot2::ggplot(d, ggplot2::aes(x = continent, y = pop_m, fill = "All")) +
      ggplot2::geom_col(width = 0.8, show.legend = FALSE) +
      ggplot2::coord_flip() +
      afcharts::scale_fill_discrete_af("main") +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(),
        # X-axis labels cut-off if this is not included
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        x = NULL,
        y = "Population (millions)"
      ) +
      ggplot2::theme(
        legend.position = "top",
        plot.margin = ggplot2::margin(40, 10, 5.5, 5.5),
        # Add gridlines on x-axis
        panel.grid.major.x = ggplot2::element_line(colour = "grey85",
                                                   linewidth = 0.4),
        panel.grid.major.y = ggplot2::element_blank()
      )
  },
  alt = "A plot of current life expectancy by continent")
  
  output$current_pop_table <- renderTable({
    d <- 
      current_pop() |>
        dplyr::rename(Continent = continent,
                      Population= pop,
                      `Population (millions)` = pop_m)
  }, striped = FALSE, bordered = FALSE, spacing = "s")
  
  output$current_pop_year_plot <- renderText({
    yr <- as.integer(input$cont_pop_year[[1]])
    
    paste("Selected year:",
          yr)
  })
  
  output$current_pop_year <- renderText({
    yr <- as.integer(input$cont_pop_year[[1]])
    
    paste("Selected year:",
          yr)
  })

  # GDP per capita ------------------------------------------------------
  # GDP per capita distribution
  gdp_per_cap_dist <- reactive({
    req(input$gdp_year)
    yr <- as.integer(input$gdp_year[[1]])

    d <- gap |>
      dplyr::filter(year == yr) |>
      dplyr::mutate(continent = factor(continent, levels = continents))
  })
  output$gdp_boxplot <- renderPlot({
    req(input$gdp_year)
    yr <- as.integer(input$gdp_year[[1]])

    d <- gdp_per_cap_dist()

    ggplot2::ggplot(d, ggplot2::aes(x = continent,
                                    y = gdpPercap, 
                                    fill = continent)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.3,
                            show.legend = FALSE) +
      ggplot2::scale_y_log10(labels = scales::label_dollar(accuracy = 1)) +
      ggplot2::labs(
        x = NULL,
        y = "GDP per capita (log scale)",
      ) +
    ggplot2::scale_fill_manual(
      values = rep("#D9D9D9",
                   length(unique(d$continent)))
    )
  },
  alt = "A plot of GDP per capita by continent")
  
  output$gdp_per_cap_yr_plot <- renderText({
    yr <- as.integer(input$gdp_year[[1]])
    
    paste("Year:",
          yr)
  })

  output$gdp_per_cap_yr <- renderText({
    yr <- as.integer(input$gdp_year[[1]])

    paste(
      "Year:",
      yr
    )
  })

  summary_by_continent <- reactive({
    d <- gdp_per_cap_dist()

    d |>
      dplyr::group_by(continent) |>
      dplyr::summarise(
        `Number of countries` = dplyr::n(),
        Mininum = min(gdpPercap, na.rm = TRUE),
        `Lower quartile` = quantile(gdpPercap, 0.25, na.rm = TRUE, names = FALSE),
        Median = median(gdpPercap, na.rm = TRUE),
        `Upper quartile` = quantile(gdpPercap, 0.75, na.rm = TRUE, names = FALSE),
        Maximum = max(gdpPercap, na.rm = TRUE),
        `Interquartile range` = `Upper quartile` - `Lower quartile`,
        .groups = "drop"
      ) |>
      dplyr::rename(Continent = continent) |>
      dplyr::mutate(across(where(is.numeric), ~ round(.x, 0)))
  })

  output$gdp_table <- renderTable(
    {
      summary_by_continent()
    },
    striped = FALSE,
    bordered = FALSE,
    spacing = "s"
  )

  

  # Logic for map --------------------------------------------------------
  # Load mapped LA data once at startup (takes 0.2s)
  map_data <- build_map_data() |>
    dplyr::mutate(
      Value = as.numeric(unlist(Value))
    )

  non_zero_map_data <-
    map_data |>
    dplyr::filter(!is.na(Value))

  # Populate the local authority dropdown
  observe({
    updateSelectInput(
      session,
      "selected_la",
      choices = sort(unique(non_zero_map_data$la_name)),
      selected = sort(unique(non_zero_map_data$la_name))[1]
    )
  })

  # Return the selected local authority row
  selected_la_data <- reactive({
    req(input$selected_la)

    map_data |>
      dplyr::filter(la_name == input$selected_la)
  })

  # Add ranking across all local authorities
  ranked_map_data <- reactive({
    map_data |>
      dplyr::mutate(
        rank_desc = dplyr::min_rank(dplyr::desc(Value))
      )
  })

  # Summary text for selected local authority
  output$uk_map_summary_text <- renderText({
    req(input$selected_la)

    x <- ranked_map_data() |>
      dplyr::filter(la_name == input$selected_la)

    req(nrow(x) == 1)

    stringr::str_glue(
      "{x$la_name} has a population of {format(round(x$Value))}, and ranks {x$rank_desc} out of {nrow(ranked_map_data())} local authorities."
    )
  })

  # Interactive map with highlighted selected LA
  output$la_map <- leaflet::renderLeaflet({
    req(input$selected_la)

    x <- ranked_map_data() |>
      dplyr::mutate(
        is_selected = la_name == input$selected_la
      )

    af_cols <- afcharts::af_colour_palettes$main6
    
    pal <- leaflet::colorNumeric(
      palette = colorRampPalette(af_cols)(100),
      domain = x$Value,
      reverse = isTRUE(input$reverse_palette)
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>Value: %s",
      x$la_name,
      scales::label_comma()(x$Value)
    ) |>
      lapply(htmltools::HTML)

    leaflet::leaflet(x) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor = ~ pal(Value),
        fillOpacity = ~ ifelse(is_selected, 0.95, 0.35),
        color = ~ ifelse(is_selected, "#000000", "#FFFFFF"),
        weight = ~ ifelse(is_selected, 3, 1),
        smoothFactor = 0.2,
        label = labels,
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#333333",
          bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        pal = pal,
        values = ~Value,
        title = "Value",
        position = "bottomright"
      )
  })

  # Table for tabular view
  output$map_tbl <- renderTable(
    {
      ranked_map_data() |>
        sf::st_drop_geometry() |>
        dplyr::select(
          `LA code` = la_code,
          `LA name` = la_name,
          Population = Value
        ) |>
        dplyr::arrange(dplyr::desc(Population)) |>
        dplyr::mutate(Population = format(round(Population)))
    },
    striped = FALSE,
    bordered = FALSE,
    spacing = "s"
  )

  # CSV export
  output$map_export_csv <- downloadHandler(
    filename = function() paste0("population_count_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(
        ranked_map_data() |>
          sf::st_drop_geometry(),
        file
      )
    }
  )
  
  # Definitions ---------------------------------------------
  output$definition_table <- renderTable(
    {
      data.frame(
        Variable = c("country", "continent", "year", "lifeExp", "pop", "gdpPercap"),
        Description = c(
          "Country name",
          "Continent",
          "Year",
          "Life expectancy at birth (years)",
          "Population",
          "GDP per capita (US dollars)"
        ),
        stringsAsFactors = FALSE
      )
    },
    striped = FALSE,
    bordered = FALSE,
    spacing = "s"
  )
  
  country_coverage <- reactive({
    d <- gap |>
      dplyr::group_by(continent) |>
      dplyr::summarise(
        countries = dplyr::n_distinct(country),
        years = dplyr::n_distinct(year),
        .groups = "drop"
      ) |>
      dplyr::mutate(continent = reorder(continent, countries))
  })
  
  output$coverage_plot <- renderPlot({
    d <- country_coverage()
    
    ggplot2::ggplot(d, ggplot2::aes(x = continent, y = countries, fill = continent)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      afcharts::scale_fill_discrete_af("main6") +
      ggplot2::labs(
        x = NULL,
        y = "Number of countries"
      )
  },
  alt = "A plot of countries covered in the dataset by continent")
  
  output$coverage_tbl <- renderTable({
    d <- country_coverage()
    
    d |>
      dplyr::rename(
        Continent = continent,
        Countries = countries,
        Years = years
      )
  })

  
} # end of server function - do not delete!
