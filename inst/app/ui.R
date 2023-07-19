navbarPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  titlePanel(
    "Upload plate run using grunID"
  ),
  tags$style(".modal-dialog {max-width: 95vw;}"),
  #tags$style(type = 'text/css',
  #".modal-dialog {width: fit-content !important;}"),
  tabsetPanel(
    tabPanel("Upload Data",
             fluidRow(

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
                 tags$br(),
                 tags$br(),
                 actionButton("info_single_assay_type", "More Info: Single Assay Type", icon = icon("circle-info")),
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
                 selectInput("sample_type", "Select a Sample Type", choices = c("mucus", "fin clip")),
                 selectInput("layout_type", "Select Layout Type", choices = c("split_plate_early_late",
                                                                              "split_plate_spring_winter",
                                                                              "triplicate", "single_assay_type")),
                 selectInput("single_assay_type", "Select Single Assay Type", choices = c("",
                                                                                          "ots28_early",
                                                                                          "ots28_late",
                                                                                          "ots16_spring",
                                                                                          "ots16_winter"), selected = NULL),
                 selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),
                 actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket")),
                 tags$br(),
                 tags$br()
               ),
               column(
                 width = 3,
                 textOutput("console_logs")
               )
             )
          ),
    tabPanel("Sample Status Table",
             fluidRow(
               sidebarPanel(
                 # TODO add options for filtering to location, sample event
                 selectInput("sample_status_filter", "Sample Status",
                             c("All", sample_status_options))
               ),
               mainPanel(
                 DT::dataTableOutput("sample_status_table"))
             )
    )
  )
)

# grunID::add_new_plate_results()
