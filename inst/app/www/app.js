// Tell Shiny plots and widgets to recalculate after layout changes
function triggerResponsiveResize() {
  // Trigger once after the CSS layout starts changing
  setTimeout(function () {
    window.dispatchEvent(new Event("resize"));
  }, 50);

  // Trigger again after the new layout has settled
  setTimeout(function () {
    window.dispatchEvent(new Event("resize"));
  }, 200);
}


// Set up the global sidebar hide/show control
function initialiseSidebarToggle() {
  // Find the new layout elements
  const toggle = document.getElementById("nav-toggle");
  const shell = document.querySelector(".app-shell");
  const sidebar = document.getElementById("app-sidebar");

  // Stop if this page does not contain the sidebar control
  if (!toggle || !shell || !sidebar) {
    return;
  }

  // Handle hide/show clicks
  toggle.addEventListener("click", function () {
    // Check whether the sidebar is currently expanded
    const expanded = toggle.getAttribute("aria-expanded") === "true";

    // Apply or remove the collapsed layout
    shell.classList.toggle(
      "app-shell--sidebar-collapsed",
      expanded
    );

    // Update the accessible state
    toggle.setAttribute(
      "aria-expanded",
      expanded ? "false" : "true"
    );

    // Update the visible button text
    toggle.textContent = expanded
      ? "Show navigation"
      : "Hide navigation";

    // Resize plots to use their new available width
    triggerResponsiveResize();
  });
}


// Remove persistent mouse focus from sidebar links
function initialiseNavFocusFix() {
  // Use event delegation because the page navigation is rendered dynamically
  document.addEventListener("mouseup", function (event) {
    // Find the nearest page-navigation link
    const link = event.target.closest(".app-page-side-nav__link");

    // Remove mouse focus when a navigation link was clicked
    if (link) {
      link.blur();
    }
  });
}


// Apply decorative image accessibility fixes
function initialiseImageFixes() {
  // Remove unnecessary alt text from decorative crests
  document
    .querySelectorAll('img[src="www/govuk-crest.svg"]')
    .forEach(function (img) {
      img.setAttribute("alt", "");
    });
}


// Initialise the application UI
function initialiseAppUi() {
  // Set up the global sidebar control
  initialiseSidebarToggle();

  // Set up dynamic navigation focus handling
  initialiseNavFocusFix();

  // Apply decorative image fixes
  initialiseImageFixes();
}


// Run after the initial document has loaded
if (document.readyState === "loading") {
  document.addEventListener(
    "DOMContentLoaded",
    initialiseAppUi
  );
} else {
  initialiseAppUi();
}
