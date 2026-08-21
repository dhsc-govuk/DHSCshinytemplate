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
    },
    # Set which columns are numeric (and therefore aligned right)
    num_col = c(3)
  )

  # Download for csv
  export_handler_pop <- function(filename_prefix) {
    downloadHandler(
      filename = function() paste0(filename_prefix, "_filtered_", Sys.Date(), ".csv"),
      content = function(file) write.csv(lifeexp_mirror_df(), file, row.names = FALSE)
    )
  }
  output$export_csv_pop_table <- export_handler_pop("gapminder_pop")



  # Observers -----------------------------------------

  # Map service navigation input IDs to hidden tab values
  nav_map <- c(
    sn_landing = "landing",
    sn_summary = "summary"
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


} # end of server function - do not delete this bracket!
