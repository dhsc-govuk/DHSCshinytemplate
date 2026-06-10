// Remove persistent yellow focus after mouse clicks on navigation links
function initialiseNavFocusFix() {
  document.querySelectorAll("#nav a").forEach(function (link) {
    link.addEventListener("mouseup", function () {
      link.blur();
    });
  });
}

// Run once the document is ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", initialiseNavFocusFix);
} else {
  initialiseNavFocusFix();
}

// Tell plots and widgets to recalculate size after layout changes
function triggerResponsiveResize() {
  setTimeout(function () {
    window.dispatchEvent(new Event("resize"));
  }, 50);

  setTimeout(function () {
    window.dispatchEvent(new Event("resize"));
  }, 200);
}

// Set up click handlers
function initialiseAppUi() {
  const toggle = document.getElementById("nav-toggle");
  const nav = document.getElementById("nav");
  const grid = document.querySelector(".layout-grid");

  // Hide/show navigation
  if (toggle && nav && grid) {
    toggle.onclick = function () {
      const expanded = toggle.getAttribute("aria-expanded") === "true";

      if (expanded) {
        toggle.setAttribute("aria-expanded", "false");
        toggle.textContent = "Show navigation";
        nav.className = "nav-collapsed";
        grid.className = "layout-grid nav-collapsed";
      } else {
        toggle.setAttribute("aria-expanded", "true");
        toggle.textContent = "Hide navigation";
        nav.className = "nav-expanded";
        grid.className = "layout-grid nav-expanded";
      }
      
       // Force charts and widgets to resize to the new layout width
      triggerResponsiveResize();
    };
  }

  // Attach handlers to any links inside the sidebar navigation
  document.querySelectorAll("#nav a").forEach(function (link) {
  link.onclick = function (event) {
    event.preventDefault();

    // Use the link id to work out the tab value
    const id = link.id;
    if (!id) return;

    // Convert ids such as ic_summary to summary
    const value = id.replace(/^ic_/, "");

    // Send the selected tab value to Shiny
    if (window.Shiny && typeof Shiny.setInputValue === "function") {
      Shiny.setInputValue("sidebar_nav", value, { priority: "event" });
    }

    // Remove focus after mouse click
    link.blur();
  };
});

  // Fix decorative crest alt text
  document.querySelectorAll('img[src="www/govuk-crest.svg"]').forEach(function (img) {
    img.setAttribute("alt", "");
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", initialiseAppUi);
} else {
  initialiseAppUi();
}