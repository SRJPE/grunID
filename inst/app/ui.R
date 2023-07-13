navbarPage(
  title = "grunID Data Uploader",
  tags$style(
    type = 'text/css',
    '.modal-dialog { width: fit-content !important; }'
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("protocol", "Select a Protocol", choices = all_protocols$name),
      actionButton("show_protocol_details", "More info", icon = icon("circle-info")),
      # bsModal("show_protocol_table", "Available Protocols", "show_protocol_details",
      #         size = "large", dataTableOutput("protocol_tbl")),
      tags$br(),
      tags$br(),
      selectInput("laboratory", "Select a Laboratory", choices = all_labs$code),
      actionButton("show_lab_details", "More info", icon = icon("circle-info")),
      tags$br(),
      tags$br(),
      selectInput("genetic_method", "Select a Genetic Method", choices = all_gen_methods$code),
      actionButton("show_methods_details", "More info", icon = icon("circle-info")),
      tags$br(),
      tags$br(),
      textInput("performed_by", "Lab work performed by:"),
      actionButton("info_performed_by", "", icon = icon("circle-info")),
      textInput("run_description", "Plate Run Description:"),
      actionButton("info_run_description", "", icon = icon("circle-info")),
      dateInput("date_run", "Date of Run"),
      fileInput("sherlock_results", "Upload Sherlock Results"),
      selectInput("sample_type", "Select Sample Type", choices = c("mucus", "fin clip")),
      actionButton("info_sample_type", "", icon = icon("circle-info")),
      selectInput("layout_type", "Select Layout Type", choices = c("split_plate_early_late",
                                                                   "split_plate_spring_winter",
                                                                   "triplicate", "single_assay_type")),
      actionButton("info_layout_type", "", icon = icon("circle-info")),
      selectInput("single_assay_type", "Select Single Assay Type", choices = c("",
                                                                               "ots28_early",
                                                                               "ots28_late",
                                                                               "ots16_spring",
                                                                               "ots16_winter"), selected = NULL),
      actionButton("info_single_assay_type", "", icon = icon("circle-info")),
      selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),


      actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket"))


    ),
    mainPanel()
  )
)

# grunID::add_new_plate_results()
