# app.R — CARLAB (updated compare layout: puzzle grid, best match always visible)

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

# ---------------- Data ----------------
mtcars_data <- mtcars
mtcars_data$car <- rownames(mtcars)
rownames(mtcars_data) <- NULL

mtcars_data <- mtcars_data %>%
  mutate(
    transmission = ifelse(am == 0, "Automatic", "Manual"),
    cylinders    = factor(cyl)
  )

pref_levels <- c("Low", "Medium", "High", "Very high")

pref_to_weight <- function(x) {
  switch(
    x,
    "Low"       = 1,
    "Medium"    = 2,
    "High"      = 3,
    "Very high" = 4,
    1
  )
}

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#00e5ff",
    base_font    = font_google("Space Grotesk"),
    heading_font = font_google("Space Grotesk")
  ),
  
  tags$head(
    tags$style(HTML('
      html, body { height: 100%; }
      body {
        margin: 0;
        background: radial-gradient(circle at top,
                  #1f2933 0,
                  #020617 40%,
                  #000000 100%);
        color: #f9fafb;
        overflow-x: hidden;
        overflow-y: auto;
      }

      body::before {
        content: "";
        position: fixed;
        inset: -40%;
        background: conic-gradient(
          from 180deg,
          #00e5ff,
          #6366f1,
          #ec4899,
          #22c55e,
          #00e5ff
        );
        opacity: 0.20;
        filter: blur(80px);
        z-index: -2;
        animation: spinGlow 22s linear infinite;
      }
      @keyframes spinGlow {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      .app-shell {
        padding: 12px 24px 20px 24px;
        min-height: 100vh;
        box-sizing: border-box;
      }

      .top-nav {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 4px;
      }
      .brand-title {
        font-weight: 600;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        font-size: 11px;
        color: #9ca3af;
      }
      .nav-buttons {
        display: flex;
        gap: 8px;
      }
      .nav-pill {
        border-radius: 999px !important;
        border: 1px solid rgba(255,255,255,0.12) !important;
        padding: 5px 12px !important;
        font-size: 11px !important;
        color: #e5e7eb !important;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.9),
                  rgba(15,23,42,0.6)) !important;
        backdrop-filter: blur(16px);
        box-shadow: 0 10px 24px rgba(0,0,0,0.65);
      }
      .nav-pill:hover {
        box-shadow: 0 18px 40px rgba(0,0,0,0.85);
        border-color: rgba(56,189,248,0.7) !important;
      }

      .glass-card {
        border-radius: 20px;
        padding: 12px 14px;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.96),
                  rgba(15,23,42,0.82));
        border: 1px solid rgba(148,163,184,0.22);
        box-shadow: 0 14px 30px rgba(0,0,0,0.75);
        backdrop-filter: blur(18px);
        transition: all 0.3s ease;
      }
      .glass-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 22px 42px rgba(0,0,0,0.85);
        border-color: rgba(56,189,248,0.8);
      }

      .glass-slim {
        padding: 8px 10px;
        border-radius: 16px;
        border-color: rgba(148,163,184,0.16);
        box-shadow: 0 10px 24px rgba(0,0,0,0.7);
      }

      .section-title {
        font-size: 10px;
        text-transform: uppercase;
        letter-spacing: 0.16em;
        color: #9ca3af;
        margin-bottom: 4px;
      }

      .hero-title {
        font-size: 32px;
        font-weight: 600;
        letter-spacing: 0.06em;
      }
      .hero-highlight {
        background: linear-gradient(120deg,#22c55e,#22d3ee,#a855f7);
        -webkit-background-clip: text;
        color: transparent;
      }
      .hero-sub {
        color: #9ca3af;
        margin-top: 6px;
        max-width: 420px;
        font-size: 12px;
      }
      .hero-cta {
        margin-top: 16px;
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .hero-btn {
        border-radius: 999px;
        border: none;
        padding: 8px 18px;
        font-size: 12px;
        font-weight: 500;
        cursor: pointer;
        color: #020617;
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        box-shadow: 0 18px 40px rgba(8,47,73,0.8);
        transition: all 0.25s ease-out;
      }
      .hero-btn:hover {
        transform: translateY(-2px) scale(1.02);
        box-shadow: 0 26px 60px rgba(8,47,73,0.95);
      }
      .hero-ghost {
        border-radius: 999px;
        border: 1px solid rgba(148,163,184,0.55);
        padding: 6px 14px;
        font-size: 11px;
        color: #e5e7eb;
        background: transparent;
        backdrop-filter: blur(12px);
        cursor: pointer;
        transition: all 0.25s ease;
      }
      .hero-ghost:hover {
        border-color: rgba(56,189,248,0.85);
        background: rgba(15,23,42,0.6);
      }

      .home-shell {
        min-height: calc(100vh - 80px);
        display: flex;
        align-items: center;
      }

      .hero-orb {
        width: 220px;
        height: 220px;
        border-radius: 999px;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: floatOrb 10s ease-in-out infinite alternate;
      }
      .hero-orb-ring {
        position: relative;
        width: 190px;
        height: 190px;
        border-radius: 999px;
        background: conic-gradient(
          from 220deg,
          #22c55e,
          #22d3ee,
          #38bdf8,
          #a855f7,
          #22c55e
        );
        animation: spinRing 14s linear infinite;
        box-shadow:
          0 0 40px rgba(34,211,238,0.8),
          0 0 70px rgba(56,189,248,0.7);
        mask: radial-gradient(farthest-side,
              transparent 60%, black 62%);
      }
      .hero-orb-core {
        position: absolute;
        width: 145px;
        height: 145px;
        border-radius: 999px;
        background: radial-gradient(circle at 30% 10%,
                  rgba(15,23,42,0.7),
                  rgba(15,23,42,0.98));
        box-shadow: inset 0 0 26px rgba(15,23,42,0.95);
      }
      @keyframes floatOrb {
        from { transform: translateY(0px) translateX(0px); }
        to   { transform: translateY(-10px) translateX(4px); }
      }
      @keyframes spinRing {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      .fade-panel {
        animation: fadeSlideIn 0.45s ease-out;
      }
      @keyframes fadeSlideIn {
        from { opacity: 0; transform: translateY(10px); }
        to   { opacity: 1; transform: translateY(0px); }
      }

      /* ---------- COMPARE PAGE LAYOUT (puzzle) ---------- */
      .compare-shell {
        max-width: 1180px;
        margin: 16px auto 12px auto;
        min-height: calc(100vh - 110px);
        position: relative;
      }
      .compare-shell::before {
        content: "";
        position: absolute;
        inset: 10% 15%;
        background: radial-gradient(circle at top,
                    rgba(56,189,248,0.22),
                    transparent 60%);
        z-index: -1;
        filter: blur(40px);
        opacity: 0.85;
        animation: compareGlow 18s ease-in-out infinite alternate;
      }
      @keyframes compareGlow {
        from { transform: translateY(0) translateX(0); }
        to   { transform: translateY(-12px) translateX(10px); }
      }

      /* Puzzle grid: preferences + best match left, filters centre,
         ranked cars right. Best match visible without scroll. */
      .compare-grid {
        display: grid;
        grid-template-columns: minmax(0, 1.25fr) minmax(0, 1.6fr) 320px;
        grid-template-rows: auto auto;
        grid-template-areas:
          "pref   filter cars"
          "best   filter cars";
        grid-column-gap: 18px;
        grid-row-gap: 18px;
        align-items: start;
      }
      .compare-pref    { grid-area: pref; }
      .compare-filters { grid-area: filter; }
      .compare-mid     { grid-area: best; }
      .compare-right   { grid-area: cars; }

      .compare-mid {
        position: relative;
        overflow: hidden;
      }
      .compare-mid::before {
        content: "";
        position: absolute;
        inset: -50%;
        background: radial-gradient(circle at top left,
                    rgba(56,189,248,0.32),
                    transparent 55%);
        opacity: 0.25;
        mix-blend-mode: screen;
        pointer-events: none;
      }
      .compare-mid-inner {
        position: relative;
        transform-style: preserve-3d;
        transition: transform 0.5s ease;
      }
      .compare-mid:hover .compare-mid-inner {
        transform: translateY(-3px) translateZ(8px);
      }

      .compare-title {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.14em;
        color: #9ca3af;
        margin-bottom: 4px;
      }
      .best-name {
        font-size: 22px;
        font-weight: 600;
        letter-spacing: 0.04em;
        margin-bottom: 4px;
      }
      .best-specs {
        margin-top: 6px;
        font-size: 12px;
      }

      /* Phone-style card stack: scrollable but no visible scrollbar */
      .compare-right {
        max-height: 430px;
        overflow-y: auto;
        padding-top: 8px;
        padding-bottom: 8px;
        scrollbar-width: none;
        -ms-overflow-style: none;
        background: radial-gradient(circle at top,
                    rgba(15,23,42,0.98),
                    rgba(15,23,42,0.86));
        border-radius: 22px;
      }
      .compare-right::-webkit-scrollbar {
        width: 0;
        height: 0;
      }

      .compare-pref .form-group,
      .compare-filters .form-group {
        margin-bottom: 10px;
      }

      @media (max-width: 1100px) {
        .compare-grid {
          grid-template-columns: minmax(0, 1fr);
          grid-template-rows: auto auto auto auto;
          grid-template-areas:
            "pref"
            "filter"
            "best"
            "cars";
        }
        .compare-right {
          max-height: none;
        }
      }

      /* ---------- SELECTS: glass + subtle 3D ---------- */
      .selectize-input {
        background: rgba(15,23,42,0.78) !important;
        border-radius: 16px !important;
        border: 1px solid rgba(148,163,184,0.6) !important;
        color: #e5e7eb !important;
        box-shadow: 0 8px 20px rgba(0,0,0,0.65);
        padding-top: 6px !important;
        padding-bottom: 6px !important;
        backdrop-filter: blur(14px);
        transition: all 0.18s ease-out;
        min-height: 34px !important;
      }
      .selectize-input > input { color: #e5e7eb !important; }
      .selectize-input.input-active,
      .selectize-input.focus {
        border-color: rgba(56,189,248,0.95) !important;
        box-shadow:
          0 0 0 1px rgba(56,189,248,0.8),
          0 16px 36px rgba(8,47,73,0.9);
        transform: translateY(-1px);
      }
      .selectize-dropdown {
        background: rgba(15,23,42,0.92) !important;
        border-radius: 18px !important;
        border: 1px solid rgba(51,65,85,0.9) !important;
        box-shadow: 0 20px 45px rgba(0,0,0,0.9);
        margin-top: 6px;
        backdrop-filter: blur(18px);
        animation: dropdownFade 0.18s ease-out;
      }
      .selectize-dropdown-content .option {
        padding: 7px 11px;
        color: #e5e7eb;
        font-size: 12px;
      }
      .selectize-dropdown-content .option.active {
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        color: #020617;
      }
      @keyframes dropdownFade {
        from { opacity: 0; transform: translateY(-4px) scale(0.98); }
        to   { opacity: 1; transform: translateY(0)    scale(1);    }
      }

      /* 3D CAR CARDS */
      .car-card {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 7px 12px;
        margin-bottom: 8px;
        border-radius: 18px;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.98),
                  rgba(15,23,42,0.9));
        box-shadow: 0 10px 22px rgba(0,0,0,0.85);
        border: 1px solid rgba(30,64,175,0.7);
        transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
        cursor: pointer;
      }
      .car-card:hover {
        transform: translateY(-2px) scale(1.01);
        box-shadow: 0 18px 34px rgba(0,0,0,0.95);
        border-color: rgba(56,189,248,0.9);
      }
      .car-left {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .car-avatar {
        position: relative;
        width: 50px;
        height: 24px;
        border-radius: 14px;
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
        box-shadow:
          0 7px 14px rgba(8,47,73,0.9),
          inset 0 0 8px rgba(15,23,42,0.8);
      }
      .car-avatar::before {
        content: "";
        position: absolute;
        left: 7px;
        top: -8px;
        width: 22px;
        height: 13px;
        border-radius: 9px 9px 4px 4px;
        background: linear-gradient(135deg,#1f2937,#020617);
        box-shadow: 0 4px 8px rgba(0,0,0,0.9);
      }
      .car-avatar::after {
        content: "";
        position: absolute;
        left: 4px;
        right: 4px;
        bottom: -7px;
        height: 12px;
        background:
          radial-gradient(circle at 20% 50%,#020617 0,#020617 60%,transparent 61%),
          radial-gradient(circle at 80% 50%,#020617 0,#020617 60%,transparent 61%);
      }
      .car-avatar.cyl4 {
        background: linear-gradient(135deg,#22c55e,#16a34a);
      }
      .car-avatar.cyl6 {
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
      }
      .car-avatar.cyl8 {
        background: linear-gradient(135deg,#f97316,#ef4444);
      }

      .car-name {
        font-weight: 500;
        font-size: 13px;
      }
      .car-sub {
        font-size: 10px;
        color: #9ca3af;
      }
      .car-specs {
        font-size: 10px;
        color: #9ca3af;
        display: flex;
        flex-wrap: wrap;
        gap: 4px 8px;
        max-width: 320px;
        justify-content: flex-end;
      }
      .car-spec-pill {
        padding: 2px 7px;
        border-radius: 999px;
        background: rgba(15,23,42,0.9);
        border: 1px solid rgba(55,65,81,0.9);
      }
      .fit-pill {
        padding: 3px 9px;
        border-radius: 999px;
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        color: #020617;
        font-size: 10px;
        font-weight: 600;
        box-shadow: 0 9px 18px rgba(8,47,73,0.9);
      }

      .zoom-car-modal {
        display: flex;
        gap: 22px;
        align-items: center;
      }
      .zoom-car-3d {
        position: relative;
        width: 170px;
        height: 80px;
        border-radius: 26px;
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
        box-shadow:
          0 14px 28px rgba(8,47,73,0.95),
          inset 0 0 14px rgba(15,23,42,0.9);
      }
      .zoom-car-3d::before {
        content: "";
        position: absolute;
        left: 22px;
        top: -20px;
        width: 70px;
        height: 38px;
        border-radius: 22px 22px 10px 10px;
        background: linear-gradient(135deg,#111827,#020617);
        box-shadow: 0 10px 18px rgba(0,0,0,0.95);
      }
      .zoom-car-3d::after {
        content: "";
        position: absolute;
        left: 16px;
        right: 16px;
        bottom: -14px;
        height: 22px;
        background:
          radial-gradient(circle at 18% 50%,#020617 0,#020617 65%,transparent 66%),
          radial-gradient(circle at 82% 50%,#020617 0,#020617 65%,transparent 66%);
      }
      .zoom-car-3d.cyl4 {
        background: linear-gradient(135deg,#22c55e,#16a34a);
      }
      .zoom-car-3d.cyl6 {
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
      }
      .zoom-car-3d.cyl8 {
        background: linear-gradient(135deg,#f97316,#ef4444);
      }

      .glass-card table {
        background-color: transparent !important;
        color: #e5e7eb !important;
        font-size: 11px;
      }
      .glass-card th,
      .glass-card td {
        background-color: transparent !important;
        color: #e5e7eb !important;
        border-color: rgba(55,65,81,0.9) !important;
      }

      .irs--shiny {
        font-family: "Space Grotesk", system-ui, sans-serif;
      }
      .irs--shiny .irs-line {
        background: rgba(15,23,42,0.9);
        border-radius: 999px;
        border: 1px solid rgba(31,41,55,0.9);
        height: 4px;
      }
      .irs--shiny .irs-bar {
        background: linear-gradient(90deg,#22d3ee,#38bdf8);
        height: 4px;
      }
      .irs--shiny .irs-handle {
        top: 21px;
        width: 16px;
        height: 16px;
        border-radius: 999px;
        border: 2px solid #0f172a;
        background: #e5faff;
        box-shadow: 0 0 0 3px rgba(56,189,248,0.4);
      }
      .irs--shiny .irs-grid,
      .irs--shiny .irs-min,
      .irs--shiny .irs-max {
        display: none;
      }

      @media (max-width: 900px) {
        .app-shell { padding: 10px 12px 24px 12px; }
        .hero-title { font-size: 26px; }
        .home-shell { min-height: auto; }
      }
    '))
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "top-nav",
      div(span("CARLAB", class = "brand-title")),
      div(
        class = "nav-buttons",
        actionButton("nav_home",    "Home",    class = "nav-pill"),
        actionButton("nav_compare", "Compare", class = "nav-pill"),
        actionButton("nav_insight", "Insights", class = "nav-pill")
      )
    ),
    
    tabsetPanel(
      id       = "main_tabs",
      type     = "hidden",
      selected = "home",
      
      # ---------- HOME ----------
      tabPanel(
        title = "Home", value = "home",
        div(
          class = "fade-panel home-shell",
          fluidRow(
            column(
              7,
              div(
                class = "glass-card",
                span("Performance. Style. Trade-offs.", class = "section-title"),
                div(
                  class = "hero-title",
                  span("Design your ", class = "hero-highlight"),
                  "next car decision."
                ),
                div(
                  class = "hero-sub",
                  "Move a few sliders, let the engine do the math. ",
                  "We blend power, efficiency, weight and acceleration ",
                  "into a single score so you can compare cars in seconds."
                ),
                div(
                  class = "hero-cta",
                  actionButton("btn_start",   "Start comparing",       class = "hero-btn"),
                  actionButton("btn_insight", "See multivariate view", class = "hero-ghost")
                )
              )
            ),
            column(
              5,
              div(
                class = "hero-orb",
                div(class = "hero-orb-ring"),
                div(class = "hero-orb-core")
              )
            )
          )
        )
      ),
      
      # ---------- COMPARE ----------
      tabPanel(
        title = "Compare", value = "compare",
        div(
          class = "fade-panel",
          div(
            class = "compare-shell",
            div(
              class = "compare-grid",
              
              div(
                class = "glass-card compare-pref",
                span("Your preferences", class = "section-title"),
                selectInput(
                  "pref_mpg", "Fuel economy priority",
                  choices  = pref_levels,
                  selected = "High"
                ),
                selectInput(
                  "pref_hp", "Engine power priority",
                  choices  = pref_levels,
                  selected = "Medium"
                ),
                selectInput(
                  "pref_wt", "Lightweight priority",
                  choices  = pref_levels,
                  selected = "Medium"
                ),
                selectInput(
                  "pref_qsec", "Fast acceleration priority",
                  choices  = pref_levels,
                  selected = "High"
                )
              ),
              
              div(
                class = "glass-card compare-filters",
                span("Filters", class = "section-title"),
                selectInput(
                  "f_cyl", "Cylinders",
                  choices  = c("All", levels(mtcars_data$cylinders)),
                  selected = "All"
                ),
                selectInput(
                  "f_trans", "Transmission",
                  choices  = c("All", "Automatic", "Manual"),
                  selected = "All"
                ),
                sliderInput(
                  "f_mpg_min", "Minimum mpg",
                  min   = floor(min(mtcars_data$mpg)),
                  max   = ceiling(max(mtcars_data$mpg)),
                  value = 15, step = 1
                )
              ),
              
              div(
                class = "glass-card compare-mid",
                div(
                  class = "compare-mid-inner",
                  span("Best match for you", class = "compare-title"),
                  uiOutput("best_car_card")
                )
              ),
              
              div(
                class = "glass-card compare-right",
                span("Ranked results (tap a car)", class = "compare-title"),
                uiOutput("ranked_cards")
              )
            )
          )
        )
      ),
      
      # ---------- INSIGHTS ----------
      tabPanel(
        title = "Insights", value = "insight",
        div(
          class = "fade-panel",
          fluidRow(
            column(
              4,
              div(
                class = "glass-card",
                span("PCA configuration", class = "section-title"),
                checkboxGroupInput(
                  "pca_vars",
                  "Variables in PCA",
                  choices  = c("mpg","hp","wt","qsec","drat"),
                  selected = c("mpg","hp","wt","qsec","drat")
                ),
                sliderInput(
                  "pca_k",
                  "Clusters (k-means on PC1–PC2)",
                  min = 2, max = 6, value = 3, step = 1
                ),
                checkboxInput(
                  "pca_labels",
                  "Show car names",
                  value = FALSE
                )
              )
            ),
            column(
              8,
              div(
                class = "glass-card",
                span("Performance map (tap a car to zoom)", class = "section-title"),
                plotOutput("pca_plot", height = "320px", click = "pca_click")
              ),
              fluidRow(
                column(
                  6,
                  br(),
                  div(
                    class = "glass-card glass-slim",
                    span("Explained variance profile", class = "section-title"),
                    tableOutput("pca_variance")
                  )
                ),
                column(
                  6,
                  br(),
                  div(
                    class = "glass-card glass-slim",
                    span("Feature influence by component", class = "section-title"),
                    tableOutput("pca_loadings")
                  )
                )
              ),
              br(),
              div(
                class = "glass-card glass-slim",
                span("Cluster profiles", class = "section-title"),
                tableOutput("segment_table")
              ),
              br(),
              div(
                class = "glass-card glass-slim",
                span("Raw data", class = "section-title"),
                DTOutput("pca_data")
              )
            )
          )
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  
  observeEvent(input$nav_home,    { updateTabsetPanel(session, "main_tabs", selected = "home")    })
  observeEvent(input$nav_compare, { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$nav_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  observeEvent(input$btn_start,   { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$btn_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  show_car_modal <- function(car_row) {
    if (nrow(car_row) == 0) return(NULL)
    car_row <- car_row[1, ]
    
    cyl_class <- paste0("cyl", as.character(car_row$cylinders))
    fit_txt   <- if ("FitScore" %in% names(car_row) &&
                     !is.na(car_row$FitScore[1])) sprintf("%.3f", car_row$FitScore[1]) else "N/A"
    
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer    = modalButton("Close"),
        title     = car_row$car,
        size      = "l",
        div(
          class = "zoom-car-modal",
          div(class = paste("zoom-car-3d", cyl_class)),
          div(
            tags$p(
              tags$b("Fit score: "),
              fit_txt
            ),
            tags$ul(
              tags$li(paste("mpg:", round(car_row$mpg,1))),
              tags$li(paste("hp:", round(car_row$hp,0))),
              tags$li(paste("weight (1000 lbs):", round(car_row$wt,2))),
              tags$li(paste("0–60 style time (qsec):", round(car_row$qsec,2))),
              tags$li(paste("cylinders:", as.character(car_row$cylinders))),
              tags$li(paste("transmission:", car_row$transmission))
            )
          )
        )
      )
    )
  }
  
  scored_cars <- reactive({
    df <- mtcars_data
    
    if (!is.null(input$f_cyl) && input$f_cyl != "All") {
      df <- df %>% filter(cylinders == input$f_cyl)
    }
    if (!is.null(input$f_trans) && input$f_trans != "All") {
      df <- df %>% filter(transmission == input$f_trans)
    }
    df <- df %>% filter(mpg >= input$f_mpg_min)
    
    if (nrow(df) == 0) return(df %>% mutate(FitScore = numeric(0)))
    
    z_mpg  <- scale(df$mpg)
    z_hp   <- scale(df$hp)
    z_wt   <- scale(-df$wt)
    z_qsec <- scale(-df$qsec)
    
    w <- c(
      mpg  = pref_to_weight(input$pref_mpg),
      hp   = pref_to_weight(input$pref_hp),
      wt   = pref_to_weight(input$pref_wt),
      qsec = pref_to_weight(input$pref_qsec)
    )
    w <- w / sum(w)
    
    fit <- w["mpg"]  * as.numeric(z_mpg) +
      w["hp"]   * as.numeric(z_hp)  +
      w["wt"]   * as.numeric(z_wt)  +
      w["qsec"] * as.numeric(z_qsec)
    
    df$FitScore <- fit
    df %>% arrange(desc(FitScore))
  })
  
  output$best_car_card <- renderUI({
    df <- scored_cars()
    if (nrow(df) == 0) {
      return(div(
        h4("No cars found"),
        tags$small("Try relaxing filters or lowering minimum mpg.")
      ))
    }
    best <- df[1, ]
    div(
      class = "best-card",
      div(class = "best-name", best$car),
      tags$ul(
        class = "best-specs",
        tags$li(HTML(paste0("<b>mpg:</b> ", round(best$mpg, 1)))),
        tags$li(HTML(paste0("<b>hp:</b> ", round(best$hp, 0)))),
        tags$li(HTML(paste0("<b>weight:</b> ", round(best$wt, 2)))),
        tags$li(HTML(paste0("<b>0–60 time (qsec):</b> ", round(best$qsec, 2)))),
        tags$li(HTML(paste0("<b>cyl:</b> ", as.character(best$cylinders)))),
        tags$li(HTML(paste0("<b>trans:</b> ", best$transmission)))
      )
    )
  })
  
  output$ranked_cards <- renderUI({
    df <- scored_cars()
    if (nrow(df) == 0) {
      return(div(tags$small("No cars match these settings.")))
    }
    
    df <- df %>% mutate(FitScore = round(FitScore, 3))
    df <- head(df, 10)
    
    card_list <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      cyl_class <- paste0("cyl", as.character(r$cylinders))
      
      div(
        class   = "car-card",
        onclick = sprintf(
          "Shiny.setInputValue('zoom_car', '%s', {priority: 'event'})",
          r$car
        ),
        div(
          class = "car-left",
          div(class = paste("car-avatar", cyl_class)),
          div(
            div(class = "car-name", r$car),
            div(class = "car-sub",
                paste0(r$cylinders, " cyl \u00b7 ", r$transmission))
          )
        ),
        div(
          class = "car-specs",
          span(class = "car-spec-pill",
               paste0("mpg ", round(r$mpg, 1))),
          span(class = "car-spec-pill",
               paste0("hp ", round(r$hp, 0))),
          span(class = "car-spec-pill",
               paste0("wt ", round(r$wt, 2))),
          span(class = "car-spec-pill",
               paste0("0–60s ", round(r$qsec, 1))),
          span(class = "fit-pill",
               paste0("Fit ", sprintf("%.2f", r$FitScore)))
        )
      )
    })
    
    do.call(tagList, card_list)
  })
  
  observeEvent(input$zoom_car, {
    df <- scored_cars()
    car_row <- df[df$car == input$zoom_car, ]
    show_car_modal(car_row)
  })
  
  pca_res <- reactive({
    req(input$pca_vars)
    x <- mtcars_data[, input$pca_vars, drop = FALSE]
    prcomp(x, scale. = TRUE)
  })
  
  pca_scores <- reactive({
    sc <- as.data.frame(pca_res()$x)
    sc$car <- mtcars_data$car
    sc$cyl <- factor(mtcars_data$cyl)
    sc
  })
  
  pca_clusters <- reactive({
    sc <- pca_scores()
    set.seed(123)
    kmeans(sc[, c("PC1", "PC2")], centers = input$pca_k)
  })
  
  output$pca_plot <- renderPlot({
    sc <- pca_scores()
    cl <- pca_clusters()
    sc$cluster <- factor(cl$cluster)
    
    g <- ggplot(sc, aes(x = PC1, y = PC2,
                        color = cluster, shape = cyl)) +
      geom_text(aes(label = "\U0001F697"), size = 6, show.legend = FALSE) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = "PC1 (performance / size)",
        y = "PC2 (efficiency / gearing)",
        color = "Cluster",
        shape = "Cylinders"
      ) +
      theme_minimal(base_family = "Space Grotesk") +
      theme(
        plot.background  = element_rect(fill = "#020617", color = NA),
        panel.background = element_rect(fill = "#020617", color = NA),
        text             = element_text(color = "white", size = 10),
        axis.text        = element_text(color = "white", size = 9),
        legend.background = element_rect(fill = "#020617", color = NA),
        legend.key       = element_rect(fill = "#020617", color = NA),
        panel.grid.major = element_line(color = "#374151"),
        panel.grid.minor = element_line(color = "#1f2933")
      )
    
    if (isTRUE(input$pca_labels)) {
      g <- g + geom_text(aes(label = car),
                         vjust = -1.3,
                         size = 3,
                         color = "white",
                         show.legend = FALSE)
    }
    g
  })
  
  observeEvent(input$pca_click, {
    click <- input$pca_click
    if (is.null(click)) return()
    sc <- pca_scores()
    
    d <- (sc$PC1 - click$x)^2 + (sc$PC2 - click$y)^2
    idx <- which.min(d)
    if (!length(idx)) return()
    
    if (sqrt(min(d)) > 0.7) return()
    
    car_name <- sc$car[idx]
    car_row  <- mtcars_data[mtcars_data$car == car_name, ]
    show_car_modal(car_row)
  })
  
  output$pca_variance <- renderTable({
    sdev <- pca_res()$sdev
    eig  <- sdev^2
    prop <- eig / sum(eig)
    cum  <- cumsum(prop)
    data.frame(
      PC         = paste0("PC", seq_along(eig)),
      Eigenvalue = round(eig, 3),
      Proportion = round(prop, 3),
      Cumulative = round(cum, 3)
    )
  }, striped = TRUE, hover = TRUE)
  
  output$pca_loadings <- renderTable({
    load <- pca_res()$rotation
    round(load, 3)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)
  
  output$segment_table <- renderTable({
    df <- mtcars_data
    cl <- pca_clusters()
    df$cluster <- factor(cl$cluster)
    
    df %>%
      group_by(cluster) %>%
      summarise(
        Avg_mpg = round(mean(mpg), 1),
        Avg_hp  = round(mean(hp), 0),
        Avg_wt  = round(mean(wt), 2),
        Avg_qsec= round(mean(qsec), 2),
        Cars    = n()
      )
  }, striped = TRUE, hover = TRUE)
  
  output$pca_data <- renderDT({
    datatable(
      mtcars_data %>% select(car, everything()),
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
