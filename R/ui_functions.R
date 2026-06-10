# ---- UI helpers ----
# Top navigation --------------------------------------------
top_nav_links <- function() {
  shiny::tags$nav(
    class = "app-top-nav",
    `aria-label` = "Service navigation",
    
    shiny::tags$div(
      class = "govuk-width-container",
      
      shinyGovstyle::service_navigation(
        links = c(
          "Landing Page" = "sn_landing",
          "Life Expectancy vs GDP" = "sn_life_exp_vs_gdpc"
        )
      )
    )
  )
}

# Side navigation ---------------------------------------------------
# Create a side navigation for sections within a page
page_side_nav <- function(links, 
                          header_name = "Contents") {
  shiny::tags$nav(
    class = "app-page-side-nav",
    `aria-label` = "Page sections",
    
    shiny::tags$h2(
      class = "govuk-heading-s app-page-side-nav__heading",
      header_name
    ),
    
    shiny::tags$ul(
      class = "app-page-side-nav__list",
      
      # Create a list item for each named anchor
      lapply(names(links), function(label) {
        shiny::tags$li(
          class = "app-page-side-nav__item",
          shiny::tags$a(
            class = "app-page-side-nav__link",
            href = paste0("#", links[[label]]),
            label
          )
        )
      })
    )
  )
}
