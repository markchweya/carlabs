# app.R — Cinematic Car Performance Explorer (clean version)

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
  switch(x,
         "Low"       = 1,
         "Medium"    = 2,
         "High"      = 3,
         "Very high" = 4,
         1)
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
        padding: 18px 32px 32px 32px;
      }

      .top-nav {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 24px;
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
        gap: 10px;
      }
      .nav-pill {
        border-radius: 999px !important;
        border: 1px solid rgba(255,255,255,0.12) !important;
        padding: 6px 16px !important;
        font-size: 12px !important;
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
        border-radius: 24px;
        padding: 22px 24px;
        background: linear-gradient(135deg,
                  rgba(15,23,42,0.92),
                  rgba(15,23,42,0.78));
        border: 1px solid rgba(148,163,184,0.25);
        box-shadow: 0 18px 40px rgba(0,0,0,0.75);
        backdrop-filter: blur(18px);
        transition: all 0.35s ease;
      }
      .glass-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 26px 60px rgba(0,0,0,0.85);
        border-color: rgba(56,189,248,0.85);
      }

      .section-title {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.16em;
        color: #9ca3af;
        margin-bottom: 8px;
      }

      .hero-title {
        font-size: 40px;
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
      }
      .hero-cta {
        margin-top: 24px;
        display: flex;
        gap: 12px;
        align-items: center;
      }
      .hero-btn {
        border-radius: 999px;
        border: none;
        padding: 10px 22px;
        font-size: 13px;
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
        padding: 8px 18px;
        font-size: 12px;
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

      .hero-orb {
        width: 260px;
        height: 260px;
        border-radius: 999px;
        background: conic-gradient(
          from 210deg,
          #22c55e,
          #22d3ee,
          #a855f7,
          #22c55e
        );
        filter: blur(2px);
        box-shadow: 0 0 120px rgba(34,211,238,0.6);
        position: relative;
        overflow: hidden;
        animation: floatOrb 10s ease-in-out infinite alternate;
      }
      .hero-orb-inner {
        position: absolute;
        inset: 20px;
        border-radius: inherit;
        background: radial-gradient(circle at 30% 10%,
                  rgba(15,23,42,0.7),
                  rgba(15,23,42,0.96));
      }
      @keyframes floatOrb {
        from { transform: translateY(0px) translateX(0px); }
        to   { transform: translateY(-14px) translateX(6px); }
      }

      .fade-panel {
        animation: fadeSlideIn 0.45s ease-out;
      }
      @keyframes fadeSlideIn {
        from { opacity: 0; transform: translateY(10px); }
        to   { opacity: 1; transform: translateY(0px); }
      }

      table.dataTable {
        background-color: transparent !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        background-color: rgba(15,23,42,0.9);
        border-radius: 999px;
        border: 1px solid rgba(148,163,184,0.4);
        color: #e5e7eb;
      }
    '))
  ),
  
  div(
    class = "app-shell",
    
    # top nav
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
    
    # main panels controlled by nav buttons
    tabsetPanel(
      id       = "main_tabs",
      type     = "hidden",
      selected = "home",
      
      # ---------- HOME ----------
      tabPanel(
        value = "home",
        div(
          class = "fade-panel",
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
                  actionButton("btn_start", "Start comparing", class = "hero-btn"),
                  actionButton("btn_insight", "See multivariate view", class = "hero-ghost")
                )
              )
            ),
            column(
              5,
              div(
                class = "hero-orb",
                div(class = "hero-orb-inner")
              )
            )
          )
        )
      ),
      
      # ---------- COMPARE ----------
      tabPanel(
        value = "compare",
        div(
          class = "fade-panel",
          fluidRow(
            column(
              4,
              div(
                class = "glass-card",
                span("Your preferences", class = "section-title"),
                br(),
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
                ),
                tags$hr(),
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
              )
            ),
            column(
              8,
              fluidRow(
                column(
                  12,
                  div(
                    class = "glass-card",
                    span("Best match for you", class = "section-title"),
                    uiOutput("best_car_card")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  12,
                  span("Ranked results", class = "section-title"),
                  div(
                    class = "glass-card",
                    DTOutput("ranked_table")
                  )
                )
              )
            )
          )
        )
      ),
      
      # ---------- INSIGHTS ----------
      tabPanel(
        value = "insight",
        div(
          class = "fade-panel",
          fluidRow(
            column(
              4,
              div(
                class = "glass-card",
                span("PCA configuration", class = "section-title"),
                br(),
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
                  value = TRUE
                )
              )
            ),
            column(
              8,
              div(
                class = "glass-card",
                span("Performance map", class = "section-title"),
                plotOutput("pca_plot", height = "380px")
              ),
              br(),
              fluidRow(
                column(
                  6,
                  div(
                    class = "glass-card",
                    span("Variance explained", class = "section-title"),
                    tableOutput("pca_variance")
                  )
                ),
                column(
                  6,
                  div(
                    class = "glass-card",
                    span("PCA loadings", class = "section-title"),
                    tableOutput("pca_loadings")
                  )
                )
              ),
              br(),
              div(
                class = "glass-card",
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
  
  # nav buttons
  observeEvent(input$nav_home,    { updateTabsetPanel(session, "main_tabs", selected = "home")    })
  observeEvent(input$nav_compare, { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$nav_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  # hero buttons
  observeEvent(input$btn_start,   { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$btn_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  # ---------- Car comparison ----------
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
      return(div(h4("No cars found"),
                 tags$small("Try relaxing filters or lowering minimum mpg.")))
    }
    best <- df[1, ]
    div(
      h3(best$car),
      tags$ul(
        tags$li(HTML(paste0("<b>Fuel economy (mpg):</b> ", round(best$mpg, 1)))),
        tags$li(HTML(paste0("<b>Engine power (hp):</b> ", round(best$hp, 0)))),
        tags$li(HTML(paste0("<b>Weight (1000 lbs):</b> ", round(best$wt, 2)))),
        tags$li(HTML(paste0("<b>0–60 style time (qsec):</b> ", round(best$qsec, 2)))),
        tags$li(HTML(paste0("<b>Cylinders:</b> ", as.character(best$cylinders)))),
        tags$li(HTML(paste0("<b>Transmission:</b> ", best$transmission)))
      )
    )
  })
  
  output$ranked_table <- renderDT({
    df <- scored_cars()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No cars match these settings.")))
    }
    out <- df %>%
      select(
        Car       = car,
        FitScore,
        mpg,
        hp,
        wt,
        qsec,
        Cylinders    = cylinders,
        Transmission = transmission
      )
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX    = TRUE,
        order      = list(list(1, "desc"))
      )
    )
  })
  
  # ---------- PCA & clusters ----------
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
      geom_point(size = 3, alpha = 0.95) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = "PC1 (performance / size)",
        y = "PC2 (efficiency / gearing)",
        color = "Cluster",
        shape = "Cylinders"
      ) +
      theme_minimal(base_family = "Space Grotesk") +
      theme(
        plot.background  = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        text             = element_text(color = "white"),
        axis.text        = element_text(color = "white"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key       = element_rect(fill = NA, color = NA),
        panel.grid.major = element_line(color = "#374151"),
        panel.grid.minor = element_line(color = "#1f2933")
      )
    
    if (isTRUE(input$pca_labels)) {
      g <- g + geom_text(aes(label = car),
                         vjust = -1.0,
                         size = 3,
                         show.legend = FALSE)
    }
    g
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
  
  output$pca_data <- renderDT({
    datatable(
      mtcars_data %>% select(car, everything()),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
