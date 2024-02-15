navbarPage(
  theme = "lumen",
  title = "grunID UI",
  # tabPanel(title = "About"),
  tabPanel("Upload Results",
           sidebarPanel(
             width = 3,
             h4("More Information"),
             tags$br(),
             actionButton("show_protocol_details", "Protocol Details", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("show_lab_details", "Lab Details", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("show_methods_details", "Genetic Method Details", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("info_performed_by", "More Info: Lab Work Performed By", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("info_run_description", "More Info: Plate Run Description", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("info_sample_type", "More Info: Sample Type", icon = icon("circle-info")),
             tags$br(),
             tags$br(),
             actionButton("info_layout_type", "More Info: Layout Type", icon = icon("circle-info")),
           ),
           column(
             width = 6,
             h3("Enter Plate Run"),
             selectInput("protocol", "Select a Protocol", choices = all_protocols$name),
             selectInput("laboratory", "Select a Laboratory", choices = all_labs$code),
             selectInput("genetic_method", "Select a Genetic Method", choices = all_gen_methods$code),
             textInput("performed_by", "Lab work performed by:"),
             textInput("run_description", "Plate Run Description:"),
             dateInput("date_run", "Date of Run"),
             fileInput("sherlock_results", "Upload Sherlock Results"),
             selectInput("sample_type", "Select a Sample Type", choices = c("fin clip", "mucus")),
             selectInput("layout_type", "Select Layout Type", choices = c("split_plate_early_late", "split_plate_late_early",
                                                                          "split_plate_spring_winter", "split_plate_winter_spring",
                                                                          "triplicate", "single_assay_ots28_early",
                                                                          "single_assay_ots28_late", "single_assay_ots16_spring",
                                                                          "single_assay_ots16_winter")),
             selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),
             checkboxInput("perform_genetics_id", label = "Run genetic calculations for samples after upload", value = TRUE),
             actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket")),
             tags$br(),
             tags$br()
           ),
           column(
             width = 3,
             textOutput("console_logs")
           ),
  ),
  tabPanel(title = "Add Samples",
           selectInput("add_sample_location_code", label = "Locaiton Code", choices = all_locations),
           selectInput("add_sample_event_number", label = "Event Number", choices = 1:20),
           textInput("add_sample_first_sample_date", label = "First Sample Date (YYYY-mm-dd)"),
           selectInput("add_sample_sample_bin_code", label = "Bin Code (A-Z)", choices = LETTERS[1:20]),
           numericInput("add_sample_min_fork_length", label = "Min Fork Length", min = 1, value = 1),
           numericInput("add_sample_max_fork_length", label = "Max Fork Length", max = 200, value = 100),
           numericInput("add_sample_number_samples", label = "Expected Number of Samples", min = 1, value = 1),

           actionButton("add_sample_submit", label = "Submit")
           ),
  tabPanel(title = "Upload Field Sheets",
           mainPanel(
             tags$h3("Process and upload completed field sheets"),
             tags$h5("Field sheets returned from the field need to be processed before being uploaded
                     to the database. Upload the file (this will refine the results into a clean table,
                     which you can view below)."),
             tags$hr(),
             fileInput("filled_field_sheets", "Process field sheets"),
             tags$h5("Press the upload button to upload the cleaned table
                     to the database."),
             actionButton("do_upload_field_sheets", "Upload field sheets to database", class = "btn-success", icon = icon("rocket")),
             tags$hr(),
             tags$h4("Preview clean field sheet data"),
             tags$br(),
             DT::dataTableOutput("field_sheet_summary") |>
               shinycssloaders::withSpinner()
           ),
  ),
  tabPanel(title = "Sample Status",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$div(
                 actionButton("sample_status_refresh", "Refresh Data", class = "btn-success", icon = icon("refresh")),
                 style = "padding-bottom: 15px;"
               ),
               selectInput("sample_status_season", "Season", choices = 2023:2024, selected = 2024),
               selectInput("sample_status_filter", "Sample Status",
                           c("All", names(sample_status_options))),
               selectInput("location_filter", "Location",
                           c("All", all_locations)),
               tags$hr(),
               tags$h3("Season Summary"),
               tableOutput("season_summary")
             ),
             mainPanel(
               shinycssloaders::withSpinner(DT::dataTableOutput("sample_status_table")))
           )
  ),
  tabPanel(title = "Query",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               selectInput("season_filter", "Season Filter",
                           available_years),
               actionButton("season_filter_description",
                            "What is a season?",
                            icon = icon("circle-info")),
               br(),
               br(),
               selectInput("dataset_type_filter", "Dataset Type",
                           c("clean", "raw", "unprocessed")),
               actionButton("dataset_type_description",
                            "What are the dataset types?",
                            icon = icon("circle-info")),
               br(),
               br(),
               checkboxInput("filter_to_heterozygotes",
                             label = "Filter results to heterozygotes",
                             value = FALSE),
               checkboxInput("filter_to_failed",
                             label = "Filter results to failed assays",
                             value = FALSE),
               tags$div(
                 actionButton("query_refresh", "Run Query", class = "btn-success"),
                 style = "padding-bottom: 15px;"
               ),
               # tags$h4("Quick plot"),
               # actionButton("show_season_plot", "Show Season Plot",
               #              icon = icon("chart-line"))
             ),
             mainPanel(
               DT::dataTableOutput("season_table") |>
                 shinycssloaders::withSpinner()
             )
           )),
  tabPanel(title = "Subsample",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               actionButton("subsample_logic",
                            "Subsampling logic",
                            icon = icon("circle-info")),
               tags$h6("This subsampling logic only applies to the 2024 season, which
                       spans 10-01-2023 through 09-30-2024"),
               tags$hr(),
               tags$h4("Summary table"),
               tags$h6("This table sums the number of samples in each subsampling scenario for each stream, event number, and bin."),
               tags$hr(),
               DT::dataTableOutput("subsample_summary_table") |>
                 shinycssloaders::withSpinner()
             ),
             mainPanel(
               selectInput("subsample_season_filter", "Season Filter",
                           2024),
               tags$h4("Full result table"),
               DT::dataTableOutput("subsample_table") |>
                 shinycssloaders::withSpinner()
             )
           ))
)

# grunID::add_new_plate_results()
