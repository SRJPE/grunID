navbarPage(
  title = "grunID Data Uploader",
  sidebarLayout(
    sidebarPanel(
      selectInput("protocol", "Select a Protocol", choices = all_protocols$name),
      actionButton("show_protocol_details", "More info", icon = icon("circle-info")),
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
      helpText("Add the user peforming lab work"),
      textInput("run_description", "Plate Run Description:"),
      helpText("This field is for comments and can be left blank"),
      dateInput("date_run", "Date of Run"),

      fileInput("sherlock_results", "Upload Sherlock Results"),
      selectInput("sample_type", "Select Sample Type", choices = c("mucus", "fin clip")),
      helpText("Enter the type of sample being processed"),
      selectInput("layout_type", "Select Layout Type", choices = c("split_plate_early_late",
                                                                   "split_plate_spring_winter",
                                                                   "triplicate", "single_assay_type")),
      helpText("This refers to how the assays are organized on the plate map"),
      selectInput("single_assay_type", "Select Single Assay Type", choices = c("",
                                                                               "ots28_early",
                                                                               "ots28_late",
                                                                               "ots16_spring",
                                                                               "ots16_winter"), selected = NULL),
      helpText("If your plate map is a single assay, enter the assay type here. If your layout type is not single assay, it is ok to leave this field blank"),
      selectInput("plate_size", "Select Plate Size", choices = c(384, 96)),


      actionButton("do_upload", "Upload Results", class = "btn-success", icon = icon("rocket"))


    ),
    mainPanel()
  )
)

# grunID::add_new_plate_results()
