function(input, output, session) {

  observeEvent(input$show_protocol_details, {
    showModal(modalDialog(
      title = "Protocol Details",
      renderTable(all_protocols |>
                    filter(active == TRUE) |>
                    select(name, reader = reader_type, serial_no = reader_serial_number,
                           plate = plate_type, set_point, preheat = preheat_before_moving,
                           runtime, interval, read_count, mode = run_mode, excitation,
                           emissions, light_source, lamp = lamp_energy, height = read_height))
    ))
  })

  observeEvent(input$show_lab_details, {
    showModal(modalDialog(
      title = "Laboratories Details",
      renderTable(all_labs)
    ))
  })

  observeEvent(input$show_methods_details, {
    showModal(modalDialog(
      title = "Genetic Methods Details",
      renderTable(all_gen_methods)
    ))
  })

  observeEvent(input$info_performed_by, {
    showModal(modalDialog(
      "You can add the user peforming lab work to be stored in the database"
    ))
  })

  observeEvent(input$info_run_description, {
    showModal(modalDialog(
      "Add any lab comments associated with the plate run in this field. This can be left blank"
    ))
  })

  observeEvent(input$info_sample_type, {
    showModal(modalDialog(
      "What type of sample are you processing? Current options are mucus or fin clip"
    ))
  })

  observeEvent(input$info_layout_type, {
    showModal(modalDialog(
      "What plate map layout are you using? This refers to which assays are being run and in what organization on the plate.
      Current options are split_plate_early_late, split_plate_spring_winter, triplicate, or single_assay. If you
      select single_assay, you must fill out the single assay type box"
    ))
  })

  observeEvent(input$info_single_assay_type, {
    showModal(modalDialog(
      "This only needs to be filled out if your layout type is `single assay`. Otherwise it can be left blank"
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
                                           plate_size = input$plate_size)
      #)
      #shinyCatch({message(paste0(messages))}, prefix = '') # this prints out messages (only at the end of the function) to shiny
      spsComps::shinyCatch({message("Success!")}, position = "top-center")},
      error = function(e) {
          spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
          # showModal(
          #   modalDialog(
          #     paste(e)
          #   )
          # )
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
  options = list(autoWidth = TRUE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 rownames = FALSE,
                 pageLength = 20)) |>
    formatStyle("status",
                backgroundColor = styleEqual(
                  levels = sample_status_options,
                  values = sample_status_colors
                )),
    # formatStyle("status",
    #             backgroundColor = styleEqual(
    #               levels = "analysis complete",
    #               values = "#d5ead5",
    #               default = NULL
    #             )),
  server = FALSE
)

}
