# ---- UI helpers ----
top_nav_links <- function() {
  shiny::tags$nav(
    class = "app-top-nav",
    `aria-label` = "Service navigation",

    shiny::tags$div(
      class = "govuk-width-container",

      shinyGovstyle::service_navigation(
        links = c("Landing Page" = "sn_landing",
                  "Summary Page" = "sn_summary"
        )
      )
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

