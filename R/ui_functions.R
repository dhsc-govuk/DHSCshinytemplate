# ---- UI helpers ----
top_nav_links <- function() {
  shiny::tags$nav(
    class = "app-top-nav",
    `aria-label` = "Service navigation",
    
    shiny::tags$div(
      class = "govuk-width-container",
      
      shinyGovstyle::service_navigation(
        links = c("Landing Page" = "sn_landing",
                  "Summary Page" = "sn_summary",
                  "Life expectancy comparison" = "sn_life_expectancy"
        )
      )
    )
  )
}
