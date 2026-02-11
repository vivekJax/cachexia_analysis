# Phase 5: Cachexia Analysis Dashboard (Shiny + shinydashboard)
# Run from Cachexia project root: shiny::runApp() or shiny::runApp(getwd())

if (!requireNamespace("shiny", quietly = TRUE)) stop("Install package 'shiny' to run the app.")
if (!requireNamespace("shinydashboard", quietly = TRUE)) stop("Install package 'shinydashboard' to run the app.")

# Load analysis code (Phases 1-4 + reporting) on every app start so code changes
# are picked up when you restart the app (no need to restart R).
app_dir <- getwd()
source_path <- file.path(app_dir, "AnalysisCode", "source_all.R")
if (!file.exists(source_path)) {
  parent_dir <- dirname(app_dir)
  source_path <- file.path(parent_dir, "AnalysisCode", "source_all.R")
  if (file.exists(source_path)) {
    setwd(parent_dir)
    app_dir <- parent_dir
  } else {
    stop("Run the app from the Cachexia project root. Expected AnalysisCode/source_all.R.")
  }
}
assign("CACHEXIA_ROOT", normalizePath(app_dir, winslash = "/"), envir = .GlobalEnv)
source(source_path, local = FALSE)

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Cachexia Analysis Dashboard"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Data Overview", tabName = "overview", icon = shiny::icon("database")),
      shinydashboard::menuItem("Human Scoring", tabName = "human", icon = shiny::icon("user-md")),
      shinydashboard::menuItem("Digital Phenotyping", tabName = "digital", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("Early Detection", tabName = "detection", icon = shiny::icon("search")),
      shinydashboard::menuItem("Export", tabName = "export", icon = shiny::icon("download"))
    ),
    shiny::hr(),
    shiny::checkboxInput("digital_load_all", "Load full digital data (all rows)", value = FALSE),
    shiny::sliderInput("digital_n_max", "Digital data rows (when not full)", min = 1e4, max = 2e5, value = 5e4, step = 1e4),
    shiny::actionButton("run_pipeline", "Load / refresh analysis", class = "btn-primary", width = "100%")
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(shiny::tags$style(shiny::HTML(".content-wrapper { overflow-x: auto; }"))),
    shinydashboard::tabItems(
      # Tab 1: Data Overview
      shinydashboard::tabItem(
        tabName = "overview",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Study design summary", width = 12, status = "primary",
            shiny::tableOutput("study_design_table"),
            shiny::p("58 animals: 40 CT-26, 18 Vehicle. Cage groupings: CT-26 Uniform 3:0, CT-26 Mixed 2:1, etc.")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Animal assignment verification", width = 6, status = "info",
            shiny::verbatimTextOutput("validation_result")
          ),
          shinydashboard::box(
            title = "Data quality report", width = 6, status = "info",
            shiny::verbatimTextOutput("data_quality")
          )
        )
      ),
      # Tab 2: Human Scoring
      shinydashboard::tabItem(
        tabName = "human",
        shiny::fluidRow(
          shinydashboard::box(
            title = "BCS trajectories", width = 12, status = "primary",
            shiny::plotOutput("bcs_trajectories_plot", height = "400px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Weight trajectories (% change from baseline)", width = 12, status = "primary",
            shiny::plotOutput("weight_trajectories_plot", height = "400px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "BCS survival (time to BCS < 3)", width = 12, status = "primary",
            shiny::plotOutput("bcs_survival_plot", height = "400px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(title = "BCS group comparison (CT-26 vs Vehicle)", width = 6,
            shiny::tableOutput("bcs_comparison_table"),
            shiny::verbatimTextOutput("bcs_test_text")
          ),
          shinydashboard::box(title = "Weight group comparison", width = 6,
            shiny::tableOutput("weight_comparison_table"),
            shiny::verbatimTextOutput("weight_test_text")
          )
        )
      ),
      # Tab 3: Digital Phenotyping
      shinydashboard::tabItem(
        tabName = "digital",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Digital feature", width = 12, status = "primary",
            shiny::selectInput("digital_metric", "Metric", choices = NULL),
            shiny::plotOutput("digital_trajectories_plot", height = "450px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Digital metric group comparison (CT-26 vs Vehicle)", width = 12,
            shiny::tableOutput("digital_comparison_table"),
            shiny::verbatimTextOutput("digital_test_text")
          )
        )
      ),
      # Tab 4: Early Detection
      shinydashboard::tabItem(
        tabName = "detection",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Manual onset (BCS & weight)", width = 6, status = "primary",
            shiny::tableOutput("manual_onset_table")
          ),
          shinydashboard::box(
            title = "Digital onset (sustained drop from baseline)", width = 6,
            shiny::tableOutput("digital_onset_table")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Lead time (manual BCS onset vs digital)", width = 6,
            shiny::tableOutput("lead_time_table"),
            shiny::verbatimTextOutput("lead_time_summary_text")
          ),
          shinydashboard::box(
            title = "Effect sizes (last BCS, weight AUC)", width = 6,
            shiny::tableOutput("effect_sizes_table")
          )
        )
      ),
      # Tab 5: Export
      shinydashboard::tabItem(
        tabName = "export",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Download processed data", width = 12, status = "primary",
            shiny::p("Export key tables as CSV. Run 'Load / refresh analysis' first."),
            shiny::downloadButton("dl_bcs_onset", "BCS onset"),
            shiny::downloadButton("dl_weight_onset", "Weight onset"),
            shiny::downloadButton("dl_weight_auc", "Weight AUC"),
            shiny::downloadButton("dl_effect_sizes", "Effect sizes"),
            shiny::downloadButton("dl_manual_onset", "Manual onset (BCS + weight)"),
            shiny::downloadButton("dl_lead_time", "Lead time comparison")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive: merged data (Excel only, fast)
  merged_base <- shiny::reactive({
    meta <- load_treatment_metadata()
    bcs <- load_bcs_data()
    weights <- load_weight_data()
    merge_all_data(meta = meta, bcs = bcs, weights = weights, digital = NULL)
  })

  # Reactive: full pipeline (with digital sample when user clicks or on init)
  pipeline <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$run_pipeline, {
    n_max <- if (isTRUE(input$digital_load_all)) NULL else as.integer(input$digital_n_max)
    if (!is.null(n_max) && n_max < 1) n_max <- 50000L
    shiny::withProgress(message = "Running pipeline...", value = 0, {
      shiny::setProgress(0.2, detail = if (is.null(n_max)) "Loading full digital data..." else "Loading digital data...")
      p <- run_full_pipeline(digital_n_max = n_max)
      shiny::setProgress(1, detail = "Done.")
      pipeline(p)
    })
  })
  # Run pipeline once on first load (with default n_max)
  shiny::observe({
    if (is.null(pipeline())) {
      shiny::withProgress(message = "Initial load...", value = 0, {
        shiny::setProgress(0.2)
        p <- run_full_pipeline(digital_n_max = 50000L)
        shiny::setProgress(1)
        pipeline(p)
      })
    }
  })

  # Tab 1: Data Overview
  output$study_design_table <- shiny::renderTable({
    m <- merged_base()$meta
    if (is.null(m) || nrow(m) == 0) return(data.frame(Message = "No metadata."))
    tab <- as.data.frame(table(m$treatment))
    names(tab) <- c("Treatment", "N")
    tab
  }, striped = TRUE, bordered = TRUE)
  output$validation_result <- shiny::renderPrint({
    meta <- merged_base()$meta
    dig <- NULL
    if (!is.null(pipeline()$merged$digital)) dig <- pipeline()$merged$digital
    val <- validate_animal_assignments(meta, digital = dig)
    cat("OK:", val$ok, "\n")
    if (length(val$message)) cat(paste(val$message, collapse = "\n"), "\n")
    if (!is.null(val$details$treatment_counts)) print(val$details$treatment_counts)
  })
  output$data_quality <- shiny::renderPrint({
    q <- summarize_data_quality(merged_base())
    print(q)
  })

  # Tab 2: Human Scoring
  output$bcs_trajectories_plot <- shiny::renderPlot({
    p <- plot_bcs_trajectories(merged_base()$bcs_with_treatment, by_treatment = TRUE, show_mean = TRUE, threshold = BCS_CACHEXIA_THRESHOLD)
    if (!is.null(p) && inherits(p, "gg")) print(p) else p
  }, res = 96)
  output$weight_trajectories_plot <- shiny::renderPlot({
    p <- plot_weight_trajectories(merged_base()$weights_with_treatment, percent_change = TRUE, by_treatment = TRUE, show_mean = TRUE)
    if (!is.null(p) && inherits(p, "gg")) print(p) else p
  }, res = 96)
  output$bcs_comparison_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$bcs_group_comparison$summary)) return(NULL)
    pl$bcs_group_comparison$summary
  }, striped = TRUE, bordered = TRUE)
  output$bcs_test_text <- shiny::renderPrint({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$bcs_group_comparison$test)) return(invisible(NULL))
    print(pl$bcs_group_comparison$test)
  })
  output$weight_comparison_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$weight_group_comparison$summary)) return(NULL)
    pl$weight_group_comparison$summary
  }, striped = TRUE, bordered = TRUE)
  output$weight_test_text <- shiny::renderPrint({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$weight_group_comparison$test)) return(invisible(NULL))
    print(pl$weight_group_comparison$test)
  })
  output$bcs_survival_plot <- shiny::renderPlot({
    if (!exists("plot_bcs_survival_curve")) return(NULL)
    plot_bcs_survival_curve(merged_base()$bcs_with_treatment, threshold = BCS_CACHEXIA_THRESHOLD)
  }, res = 96)

  # Tab 3: Digital Phenotyping
  shiny::observe({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$digital_daily)) return()
    metrics <- setdiff(names(pl$digital_daily), c("animal_id", "date", "treatment"))
    if (length(metrics)) shiny::updateSelectInput(session, "digital_metric", choices = metrics, selected = metrics[1])
  })
  output$digital_trajectories_plot <- shiny::renderPlot({
    pl <- pipeline()
    metric <- input$digital_metric
    if (is.null(pl) || is.null(pl$digital_daily) || is.null(metric) || !metric %in% names(pl$digital_daily)) return(NULL)
    p <- plot_digital_trajectories(pl$digital_daily, metric = metric, by_treatment = TRUE, show_mean = TRUE)
    if (!is.null(p) && inherits(p, "gg")) print(p) else p
  }, res = 96)
  output$digital_comparison_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$digital_group_comparison$summary)) return(NULL)
    pl$digital_group_comparison$summary
  }, striped = TRUE, bordered = TRUE)
  output$digital_test_text <- shiny::renderPrint({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$digital_group_comparison$test)) return(invisible(NULL))
    print(pl$digital_group_comparison$test)
    if (!is.null(pl$digital_group_comparison$effect_size_cohens_d))
      cat("Cohen's d:", pl$digital_group_comparison$effect_size_cohens_d, "\n")
  })

  # Tab 4: Early Detection
  output$manual_onset_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$manual_onset) || nrow(pl$manual_onset) == 0) return(NULL)
    head(pl$manual_onset, 50)
  }, striped = TRUE, bordered = TRUE)
  output$digital_onset_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$digital_onset) || nrow(pl$digital_onset) == 0) return(data.frame(Message = "No digital onset (load digital data and run pipeline)."))
    head(pl$digital_onset, 50)
  }, striped = TRUE, bordered = TRUE)
  output$lead_time_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$lead_time_comparison) || nrow(pl$lead_time_comparison) == 0) return(NULL)
    head(pl$lead_time_comparison, 30)
  }, striped = TRUE, bordered = TRUE)
  output$lead_time_summary_text <- shiny::renderPrint({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$lead_time_summary)) return(invisible(NULL))
    print(pl$lead_time_summary)
  })
  output$effect_sizes_table <- shiny::renderTable({
    pl <- pipeline()
    if (is.null(pl) || is.null(pl$effect_sizes)) return(NULL)
    pl$effect_sizes
  }, striped = TRUE, bordered = TRUE)

  # Tab 5: Export - download handlers
  output$dl_bcs_onset <- shiny::downloadHandler(
    filename = "cachexia_bcs_onset.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$bcs_onset)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$bcs_onset, file, row.names = FALSE)
    }
  )
  output$dl_weight_onset <- shiny::downloadHandler(
    filename = "cachexia_weight_onset.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$weight_onset)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$weight_onset, file, row.names = FALSE)
    }
  )
  output$dl_weight_auc <- shiny::downloadHandler(
    filename = "cachexia_weight_auc.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$weight_auc)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$weight_auc, file, row.names = FALSE)
    }
  )
  output$dl_effect_sizes <- shiny::downloadHandler(
    filename = "cachexia_effect_sizes.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$effect_sizes)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$effect_sizes, file, row.names = FALSE)
    }
  )
  output$dl_manual_onset <- shiny::downloadHandler(
    filename = "cachexia_manual_onset.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$manual_onset)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$manual_onset, file, row.names = FALSE)
    }
  )
  output$dl_lead_time <- shiny::downloadHandler(
    filename = "cachexia_lead_time_comparison.csv",
    content = function(file) {
      pl <- pipeline()
      if (is.null(pl) || is.null(pl$lead_time_comparison)) { write.csv(data.frame(), file); return() }
      utils::write.csv(pl$lead_time_comparison, file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)
