# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Locate a file inside inst/extdata in a golem package
app_file <- function(...) {
  system.file(..., package = "dhscshinytemplate")
}

# Load LA boundaries from the RDS bundled with the app
get_la_boundaries <- function() {
  # Locate the RDS file inside the installed package
  path <- app_file("extdata", "la_boundaries.rds")

  # Read the sf object
  la_sf <- readRDS(path)

  # Standardise column names used by the app
  la_sf |>
    dplyr::rename(
      la_code = LAD24CD,
      la_name = LAD24NM
    ) |>
    dplyr::select(la_code, la_name, geometry)
}

# Load local authority population data bundled with the app
get_nomis_la_data <- function() {
  readRDS(
    app_file("extdata", "nomis_lad_population.rds")
  ) |>
    dplyr::rename(
      la_code = Geography_code,
      la_name_data = Geography_name
    ) |>
    dplyr::mutate(
      Value = as.numeric(Value)
    ) |>
    dplyr::distinct(la_code, .keep_all = TRUE)
}

# Join boundary data to bundled Nomis data
build_map_data <- function() {
  lad_sf <- get_la_boundaries()
  nomis_df <- get_nomis_la_data()

  lad_sf |>
    dplyr::left_join(nomis_df, by = "la_code") |>
    sf::st_transform("EPSG:4326")
}



 
#' Create a reusable function for shinyGovstyle tables
#'
#' @param input_id 
#' @param data_expr 
#' @param caption 
#' @param num_col 
#'
#' @returns
#' @export
#'
#' @examples
render_gov_table <- function(input_id,
                             data_expr,
                             caption,
                             caption_size = "m",
                             num_col = NULL,
                             width_overwrite = NULL) {
  
  shiny::renderUI({
    # Evaluate the table data reactively
    df <- data_expr()
    
    # Stop rendering if there is no data
    shiny::validate(
      shiny::need(nrow(df) > 0, "No data available for this table.")
    )
    
    # Return a GOV.UK styled table
    shinyGovstyle::govTable(
      inputId = input_id,
      df = df,
      caption = caption,
      caption_size = "m",
      num_col = num_col,
      width_overwrite = width_overwrite
    )
  })
}
