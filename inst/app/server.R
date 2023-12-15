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
      Current options are split_plate_early_late, split_plate_spring_winter, triplicate, single_assay_ots28_early,
      single_assay_ots28_late, single_assay_ots16_spring, single_assay_ots16_winter",
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
                                      .control_id = "EBK",
                                      run_gen_id = input$perform_genetics_id)
      #)
      #shinyCatch({message(paste0(messages))}, prefix = '') # this prints out messages (only at the end of the function) to shiny
      spsComps::shinyCatch({message("Success!")}, position = "top-center")},
      error = function(e) {
          spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
      })
    }
  )

  selected_all_sample_status <- reactive({
    re <- ifelse(input$sample_status_season == 2023, "\\b\\w{3}23", "\\b\\w{3}24")
    data <- all_sample_status() |> filter(str_detect(sample_id, re))

    if(input$sample_status_filter != "All") {
      data <- data |>
        dplyr::filter(status == input$sample_status_filter)
    }
    if(input$location_filter != "All") {
      data <- data |>
        dplyr::filter(stringr::str_detect(sample_id, input$location_filter))
    }

    data

  })

  output$sample_status_table <- DT::renderDataTable({

    DT::datatable(selected_all_sample_status(),
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
                  ))

  }, server = FALSE)

  output$season_summary <- renderTable({
    re <- ifelse(input$sample_status_season == 2023, "\\b\\w{3}23", "\\b\\w{3}24")

    all_sample_status() |> filter(str_detect(sample_id, re)) |>
      group_by(status) |>
      summarise(
        total = n()
      )
  })

  selected_samples_by_season <- reactive({
    grunID::get_samples_by_season(con, input$season_filter, input$dataset_type_filter,
                                  input$filter_to_heterozygotes, input$filter_to_failed)
  })

  output$season_table <- DT::renderDataTable(DT::datatable(selected_samples_by_season(),
  extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20)),
  server = FALSE
  ) |>
    shiny::bindCache(input$season_filter, input$dataset_type_filter,
                     input$filter_to_heterozygotes, input$filter_to_failed)

  observeEvent(input$season_filter_description, {
    showModal(modalDialog(
      "Currently the season filter will pull all samples from sampling events in the
      year provided up to September 30th and samples from the previous year after
      October 1st. So for season = 2022 the query will return all samples from
      10-01-2021 - 09-30-2022",
      size = "l"
    ))
  })

  observeEvent(input$dataset_type_description, {
    showModal(modalDialog(
      HTML("<h3> There are three dataset 'types' currently available. </h3>
           The <strong>clean</strong> dataset has no raw data and generates a dataset formatted
           for input to a probabilistic length-at-date (PLAD) model. </br> </br>
           The <strong>raw</strong> dataset has all the same variables as the clean dataset, but with additional information about assay name,
           raw fluoresence values, postive detections and the threshold values used to calculate them,
           and the plate run ID. </br> </br>
           The <strong>unprocessed</strong> dataset has all the same variables as the clean dataset but with additional
           information like raw fluoresence, time, sample type, assay, background value, and well location"),
      size = "l"
    ))
  })

output$season_plot <- renderPlot(
  grunID::get_samples_by_season(con, season = input$season_filter, dataset = input$dataset_type_filter) |>
    dplyr::mutate(week = lubridate::week(datetime_collected)) |>
    dplyr::filter(!is.na(week)) |>
    dplyr::group_by(week) |>
    dplyr::summarise(prop_spring_gen = sum(genetic_run_assignment == "Spring") / n(),
                     prop_spring_field = sum(field_run_assignment == "Spring") / n()) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(c(prop_spring_gen, prop_spring_field),
                        names_to = "method",
                        values_to = "prop_spring") |>
    dplyr::mutate(method = ifelse(method == "prop_spring_field", "Field assignment", "Genetic assignment")) |>
    ggplot2::ggplot(aes(x = week, y = prop_spring, color = method)) +
    geom_line() +
    xlab("Week") + xlim(c(0, 52)) +
    ylab("Proportion Spring Run") + ylim(c(0, 1)) +
    scale_color_manual(values = c("#F1BB7B", "#FD6467")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  )


  observeEvent(input$show_season_plot, {
    showModal(
      modalDialog(
        title = "Proportion spring run by weeks within season",
        plotOutput("season_plot"),
        size = "l")
      )
    })

  # read in field data, if available
  clean_field_data <- reactive({
    if(is.null(input$filled_field_sheets$datapath)) return(NULL)
    tryCatch({
      data <- grunID::process_field_sheet_samples(input$filled_field_sheets$datapath)
    }, error = function(e) {
      spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
    })
    data
  })

  # if button pressed, upload field sheet data to database
  observeEvent(input$do_upload_field_sheets, {
    tryCatch({
      grunID::update_field_sheet_samples(con, clean_field_data())
      spsComps::shinyCatch({message("Field sheets updated in database")}, position = "top-center")
    },
    error = function(e) {
      spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
    })
  })

  # display field sheets
  output$field_sheet_summary <- DT::renderDataTable(DT::datatable({
    clean_field_data()
  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 10)),
  server = FALSE
  )

  # subsample table
  output$subsample_table <- DT::renderDataTable(DT::datatable({

    grunID::generate_subsample(con, as.numeric(input$season_filter))$results

  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20)),
  server = FALSE
  ) |>
    shiny::bindCache(input$season_filter)

  # subsample logic
  observeEvent(input$subsample_logic, {
    showModal(modalDialog(
      HTML("<h3> Subsampling logic for 2024 season </h3> <br>
           This function subsamples from all samples in the 2024 season according to the following logic: <br>
           <ul>
           <li>At least 50% of samples per site per event will be sampled.</li>
           <li>If the number of samples is odd, divide that number by 2 and round the resulting number up to the nearest integer.</li>
           <li>If the total number of samples for a given site in a given event is less than 20, process all samples for that site/event.</li>
           <li>If multiple bins are represented in a set of samples for a given site and event, select 50% of the samples from each bin for processing.</li>
           <li>If the total number of samples in a bin is less than or equal to 5, process all of the samples for that bin.</li>
           <li>If this rule contradicts the “less than 20” rule (above), this rule should be prioritized. For example, if we receive a
           sample set from a given site and event where Bins A, B, C, D, and E are each represented by five samples (total sample size = 25),
           process all of the samples for that site/event.</li>
           <li>Subsampling is random.</li>"),
      size = "l"
    ))
  })

  # subsample summary table
  output$subsample_summary_table <- DT::renderDataTable(DT::datatable({

    grunID::generate_subsample(con, as.numeric(input$season_filter))$summary
  },
  rownames = FALSE))
}
