function(input, output, session) {

  observeEvent(input$show_protocol_details, {
    showModal(modalDialog(
      title = "Protocol Details",
      renderTableWithScrollOnX(all_protocols |>
                    filter(active == TRUE) |>
                    select(name, reader = reader_type, serial_no = reader_serial_number,
                           plate = plate_type, set_point, preheat = preheat_before_moving,
                           runtime, interval, read_count, mode = run_mode, excitation,
                           emissions, light_source, lamp = lamp_energy, height = read_height), striped = TRUE, width = "auto"),
    size = "l"))
  })

  observeEvent(input$show_lab_details, {
    showModal(modalDialog(
      title = "Laboratories Details",
      renderTableWithScrollOnX(all_labs),
      size = "l"
    ))
  })

  observeEvent(input$show_methods_details, {
    showModal(modalDialog(
      title = "Genetic Methods Details",
      renderTableWithScrollOnX(all_gen_methods),
      size = "l"
    ))
  })

  observeEvent(input$info_performed_by, {
    showModal(modalDialog(
      "You can add the user peforming lab work to be stored in the database",
      size = "l"
    ))
  })

  observeEvent(input$info_run_description, {
    showModal(modalDialog(
      "Add any lab comments associated with the plate run in this field. This can be left blank",
      size = "l"
    ))
  })

  observeEvent(input$info_sample_type, {
    showModal(modalDialog(
      "What type of sample are you processing? Current options are mucus or fin clip",
      size = "l"
    ))
  })

  observeEvent(input$info_layout_type, {
    showModal(modalDialog(
      "What plate map layout are you using? This refers to which assays are being run and in what organization on the plate.
      Current options are split_plate_early_late, split_plate_spring_winter, triplicate, or single_assay. If you
      select single_assay, you must fill out the single assay type box",
      size = "l"
    ))
  })

  observeEvent(input$info_single_assay_type, {
    showModal(modalDialog(
      "This only needs to be filled out if your layout type is `single assay`. Otherwise it can be left blank",
      size = "l"
    ))
  })


  observeEvent(input$do_upload, {
    tryCatch({
      #messages <- capture.output(
        grunID::add_new_plate_results(con, protocol_name = input$protocol,
                                           genetic_method = input$genetic_method,
                                           laboratory = input$laboratory,
                                           lab_work_performed_by = input$performed_by,
                                           description = input$run_description,
                                           date_run = input$date_run,
                                           filepath = input$sherlock_results$datapath,
                                           sample_type = input$sample_type,
                                           layout_type = input$layout_type,
                                           plate_size = input$plate_size,
                                      run_gen_id = input$perform_genetics_id)
      #)
      #shinyCatch({message(paste0(messages))}, prefix = '') # this prints out messages (only at the end of the function) to shiny
      spsComps::shinyCatch({message("Success!")}, position = "top-center")},
      error = function(e) {
          spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
      })
    }
  )

  output$sample_status_table <- DT::renderDataTable(DT::datatable({
    data <- all_sample_status

    if(input$sample_status_filter != "All") {
      data <- data |>
        dplyr::filter(status == input$sample_status_filter)
    }
    if(input$location_filter != "All") {
      data <- data |>
        dplyr::filter(stringr::str_detect(sample_id, input$location_filter))
    }
    data
  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20)) |>
    formatStyle("status",
                target = "cell",
                backgroundColor = styleEqual(
                  levels = names(sample_status_options),
                  values = as.character(sample_status_options)
                )),
  server = FALSE
)

  output$season_summary <- renderTable({
    all_sample_status |>
      group_by(status) |>
      summarise(
        total = n()
      )
  })

  output$season_table <- DT::renderDataTable(DT::datatable({

    season_filter <- input$season_filter
    dataset_filter <- input$dataset_type_filter
    data <- grunID::get_samples_by_season(con,
                                          season = season_filter,
                                          dataset = dataset_filter)
    data
  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20)),
  server = FALSE
  )

  observeEvent(input$season_filter_description, {
    showModal(modalDialog(
      "Currently the season filter will pull all samples from sampling events in the
      year provided up to September 30th, and samples from the previous year after
      October 1st.",
      size = "l"
    ))
  })

  observeEvent(input$dataset_type_description, {
    showModal(modalDialog(
      "There are two dataset 'types' currently available. The raw dataset has all the same
      variables as the clean dataset, but with additional information about assay name,
      raw fluoresence values, postive detections and the threshold values used to calculate them,
      and the plate run ID. The clean dataset has no raw data and provides a 'cleaner'
      dataset geared toward use in a probabilistic length-at-date (PLAD) model. ",
      size = "l"
    ))
  })

}
