navbarPage(
  theme = "lumen",
  title = "grunID UI",
  header = tags$head(
    tags$style(HTML('
      .round-btn {
        border-radius: 50%;
      }
      .icon-offset {
        margin-left: 5px; /* Adjust the margin as needed */
      }
    '))
  ),
  # tabPanel(title = "About"),
  navbarMenu("Add Data",
             tabPanel("Upload Results",
                      # sidebarPanel(
                      #   width = 3
                      # ),
                      column(
                        # offset = 1,
                        width = 6,
                        h3("Enter Plate Run"),

                        uiOutput("ui_banner_for_failed_status"),
                        uiOutput("ui_banner_for_need_ots16_status"),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          selectInput("protocol", "Select a Protocol", choices = all_protocols$name),
                          actionButton("show_protocol_details", label = NULL, icon = icon("question"), class = "round-btn icon-offset")
                        ),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          selectInput("laboratory", "Select a Laboratory", choices = all_labs$code),
                          actionButton("show_lab_details", label = NULL, icon = icon("question"), class = "round-btn icon-offset")

                        ),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          selectInput("genetic_method", "Select a Genetic Method", choices = all_gen_methods$code),
                          actionButton("show_methods_details", label = NULL, icon = icon("question"), class = "round-btn icon-offset")

                        ),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          textInput("performed_by", "Lab work performed by:"),
                          actionButton("info_performed_by", label = NULL, icon = icon("question"), class = "round-btn icon-offset")

                        ),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          textInput("run_description", "Plate Run Description:"),
                          actionButton("info_run_description", label = NULL, icon = icon("question"), class = "round-btn icon-offset")

                        ),
                        dateInput("date_run", "Date of Run"),
                        fileInput("sherlock_results", "Upload Sherlock Results"),

                        tags$div(
                          style = "display: flex; align-items: center;",
                          selectInput("sample_type", "Select a Sample Type", choices = c("fin clip", "mucus")),
                          actionButton("info_sample_type", label = NULL, icon = icon("question"), class = "round-btn icon-offset")

                        ),
                        tags$div(
                          style = "display: flex; align-items: center;",
                          selectInput("layout_type", "Select Layout Type",
                                      choices = c("Split Plate - Early + Late"="split_plate_early_late",
                                                  "Split Plate - Late + Early"="split_plate_late_early",
                                                  "Split Plate - Spring + Winter"="split_plate_spring_winter",
                                                  "Split Plate - Winter + Spring"="split_plate_winter_spring",
                                                  "Single Assay OTS 28 Early (v5 Mapping)"="single_assay_ots28_early",
                                                  "Single Assay OTS 28 Late (v5 Mapping)"="single_assay_ots28_late",
                                                  "Single Assay OTS 16 Spring (v5 Mapping)"="single_assay_ots16_spring",
                                                  "Single Assay OTS 16 Winter (v5 Mapping)"="single_assay_ots16_winter",
                                                  "Triplicate"="triplicate"
                                      )
                          ),
                          actionButton("info_layout_type", label = NULL, icon = icon("question"), class = "round-btn icon-offset")
                        ),

                        selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),
                        checkboxInput("perform_genetics_id", label = "Run genetic calculations for samples after upload", value = TRUE),
                        actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket")),

                        tags$br()
                      ),
                      column(
                        width = 3,
                        textOutput("console_logs")
                      ),
             ),
             tabPanel("Check-in Samples",
                      uiOutput("check_in_notification"),
                      fileInput("check_in_samples_file", label = "Check-in Samples File"),
                      actionButton("check_in_samples_submit", "Submit", class = "btn-success")
                      ),
             tabPanel(title = "Sample",
                      selectInput("add_sample_location_code", label = "Locaiton Code", choices = all_locations),
                      selectInput("add_sample_event_number", label = "Event Number", choices = 1:20),
                      textInput("add_sample_first_sample_date", label = "First Sample Date (YYYY-mm-dd)"),
                      selectInput("add_sample_sample_bin_code", label = "Bin Code (A-Z)", choices = LETTERS[1:20]),
                      numericInput("add_sample_min_fork_length", label = "Min Fork Length", min = 1, value = 1),
                      numericInput("add_sample_max_fork_length", label = "Max Fork Length", max = 200, value = 100),
                      numericInput("add_sample_number_samples", label = "Expected Number of Samples", min = 1, value = 1),

                      actionButton("add_sample_submit", label = "Submit")),
             tabPanel("Protocol",
                      textInput("add_protocol_name", "Protocol Name"),
                      textInput("add_protocol_software_version", "Software Version", value = protocol_template$software_version),
                      textInput("add_protocol_reader_type", "Reader Type", value = protocol_template$reader_type),
                      textInput("add_protocol_serial_number", "Serial Number", value = protocol_template$reader_serial_number),
                      textInput("add_protocol_plate_type", "Plate Type", value = protocol_template$plate_type),
                      numericInput("add_protocol_set_point", "Set Point", value = protocol_template$set_point),
                      checkboxInput("add_protocol_preheat_before_moving", "Preheat Before Moving", value = TRUE),
                      textInput("add_protocol_runtime", "Runtime", value = protocol_template$runtime),
                      textInput("add_protocol_interval", "Interval", value = protocol_template$interval),
                      numericInput("add_protocol_read_count", "Read Count", value = protocol_template$read_count),
                      textInput("add_protocol_run_mode", "Run Mode", value = protocol_template$run_mode),
                      numericInput("add_protocol_excitation", "Excitation", value = protocol_template$excitation),
                      numericInput("add_protocol_emissions", "Emissions", value = protocol_template$emissions),
                      textInput("add_protocol_optics", "Optics", value = protocol_template$optics),
                      numericInput("add_protocol_gain", "Gain", value = protocol_template$gain),
                      textInput("add_protocol_light_source", "Light Source", value = protocol_template$light_source),
                      textInput("add_protocol_lamp_energy", "Lamp Energy", value = protocol_template$lamp_energy),
                      numericInput("add_protocol_read_height", "Read Height", value = protocol_template$read_height),


                      actionButton("add_protocol_submit", label = "Submit")
             ),
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
               tags$hr(),... =
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
               selectInput("subsample_sampling_event_filter", "Sampling Event",
                           ""),
               tags$h4("Full result table"),
               DT::dataTableOutput("subsample_table") |>
                 shinycssloaders::withSpinner(),
               hr(),
               column(
                 width = 6,
                 tags$div(
                   selectInput("subsample_plate_map_type", "Plate Map Type", c("dual_assay", "single_assay"))
                 ),
                 tags$div(
                   style = "display: flex; align-items: center;",
                   textInput("subsample_plate_map_filepath", "Name of Plate Map for Download", placeholder = "e.g. JPE24_E1-2_P1_SH"),
                   actionButton("show_subsampling_plate_map_naming_conventions", label = NULL, icon = icon("question"),
                                class = "round-btn icon-offset")
                 ),
                 tags$div(
                   downloadButton("do_generate_subsample_plate_map", "Generate Subsample Plate Map")
                 )
               )
             ),
           )),
  tabPanel(title = "Plate Validations",
           mainPanel(
             tags$h4("Flagged plate runs"),
             br(),
             checkboxInput("filter_to_active_plate_runs",
                           label = "Filter to active plate runs", value = TRUE),
             DT::dataTableOutput("flagged_table") |>
               shinycssloaders::withSpinner(),
             hr(), br(),
             # uiOutput("ui_subplate_selection"),

             # tags$h4("Validate plate run data:"),
             htmlOutput("flagged_plate_run_comment"),
             htmlOutput("ui_subplate_checkbox"),
             br(),
             DT::dataTableOutput("flagged_plate_run_table_display") |>
               shinycssloaders::withSpinner(),
             br(),
             bsModal("modal_plate_data", title = "Plate Data", trigger = "pv_view_plate_data_btn", size = "large", DT::dataTableOutput("pv_all_plate_data_tbl")),
             br(),
             actionButton("pv_view_plate_data_btn", "View Full Plate Data", class = "btn-default"),
             div(style = "display:inline-block; float:right",
                 actionButton("do_activate", "Accept Selected Subplates",
                              class = "btn-success"),
                 actionButton("do_deactivate", "Reject Selected Subplates",
                              class = "btn-warning"),
                 actionButton("do_deactivate_entire_plate", "Deactive Entire Plate",
                              class = "btn-danger")),

             br(), br(),
           )
  )
)



# grunID::add_new_plate_results()
