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


# Colours ----------------------------------------------

govuk_org_colour <- function(org) {

  colours <- c(
    "attorney-generals-office"= "#a91c8e",
    "cabinet-office"= "#0056b8",
    "civil-service"= "#b2292e",
    "department-for-business-trade"= "#e52d13",
    "department-for-culture-media-sport"= "#ed1588",
    "department-for-education"= "#003764",
    "department-for-energy-security-net-zero"= "#00a33b",
    "department-for-environment-food-rural-affairs"= "#00a33b",
    "department-for-science-innovation-technology"= "#00f8f8",
    "department-for-transport"= "#006853",
    "department-for-work-pensions"= "#00bcb5",
    "department-of-health-social-care"= "#00a990",
    "foreign-commonwealth-development-office"= "#012069",
    "hm-government"= "#266ebc",
    "hm-revenue-customs"= "#008670",
    "hm-treasury"= "#b2292e",
    "home-office"= "#732282",
    "ministry-of-defence"= "#532a45",
    "ministry-of-housing-communities-local-government"= "#00625e",
    "ministry-of-justice"= "#000000",
    "northern-ireland-office"= "#00205c",
    "office-of-the-advocate-general-for-scotland"= "#00205c",
    "office-of-the-leader-of-the-house-of-commons"= "#497629",
    "office-of-the-leader-of-the-house-of-lords"= "#9c182f",
    "prime-ministers-office-10-downing-street"= "#0b0c0c",
    "scotland-office"= "#00205c",
    "serious-fraud-office"= "#82368c",
    "uk-export-finance"= "#cf102d",
    "wales-office"= "#a33038"
  )

  colours[[org]]
}
