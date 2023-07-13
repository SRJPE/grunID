navbarPage(
  title = "grunID Data Uploader",
  sidebarLayout(
    sidebarPanel(
      selectInput("protocol", "Select a Protocol", choices = all_protocols$name),
      actionButton("show_protocol_details", "More info"),
      tags$br(),
      tags$br(),
      selectInput("laboratory", "Select a Laboratory", choices = all_labs$code),
      actionButton("show_lab_details", "More info"),
      tags$br(),
      tags$br(),
      selectInput("genetic_method", "Select a Gentic Method", choices = all_gen_methods$code),
      actionButton("show_methods_details", "More info"),
      tags$br(),
      tags$br(),
      textInput("performed_by", "Lab work performed by:"),
      textInput("run_description", "Plate Run Description:"),
      dateInput("date_run", "Date of Run"),

      fileInput("sherlock_results", "Upload Sherlock Results"),
      selectInput("sample_type", "Select Sample Type", choices = "mucus"),
      selectInput("layout_type", "Select Layout Type", choices = "split_plate_early_late"),
      selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),


      actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket"))


    ),
    mainPanel()
  )
)

# grunID::add_new_plate_results()
