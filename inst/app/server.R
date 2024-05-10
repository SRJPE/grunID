function(input, output, session) {

  # Upload Results ----------------------------------------------------------
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

  samples_failing <- reactiveVal(nrow(check_for_status()$failed))
  samples_need_ots16 <- reactiveVal(nrow(check_for_status()$need_ots16))

  # Render the banner based on the failed samples status
  output$ui_banner_for_failed_status <- renderUI({
    if (samples_failing() == 0) {
      return(NULL)
    } else {
      total_samples_failing <- nrow(samples_failing())
      HTML(paste0('<div class="alert alert-danger" role="alert">',
                  samples_failing(), ' samples were found with a failed status, please review using query',
                  '</div>'))
    }
  })

  output$ui_banner_for_need_ots16_status <- renderUI({
    if (samples_need_ots16() == 0) {
      return(NULL)
    } else {
      total_samples_failing <- nrow(samples_need_ots16())
      HTML(paste0('<div class="alert alert-warning" role="alert">',
                  samples_need_ots16(), ' samples were found that need OTS16, please seach "need ots16" in query tab for details',
                  '</div>'))
    }
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
      tagList(
        tags$h3("Dual Assay Layout"),
        tags$p("For dual assay layouts select either 'Split Plate Early + Late' or 'Split Plate Spring + Winter'"),
        img(src = "assets/plate-mapping-2-assay.png", width = "100%"),
        tags$h3("Single Assay V5"),
        tags$p("For single assay layouts select one of the 'Single Assay' options"),
        img(src = "assets/plate-mapping-v5.png", width = "100%")
      ),
      size = "l",easyClose = TRUE)
    )
  })

  observeEvent(input$do_upload, {
    showModal(modalDialog(
      title = "Confirm data submission",
      tagList(
        h5(tags$b("Please confirm Plate Layout Selection")),
        h5("This extra check is here to ensure that the order of the layout present in the results file matches the layout selected in this upload tool."),
        h5("Layout Selected:", tags$b(glue::glue("{input$layout_type}"))),
        h5("Results filename:", tags$b(glue::glue("{input$sherlock_results$name}"))),
        h5("Is this selection correct?"),
      ),
      size = "l",
      easyClose = F,
      footer = tagList(
        actionButton("yes_upload", "Yes", class= "btn-success"),
        actionButton("no_upload", "Cancel", class = "btn-danger")
      )
    ))
  })


  observeEvent(input$yes_upload | input$no_upload, ignoreInit = TRUE, {
    if(input$yes_upload > 0){
      tryCatch({
        removeModal(session = session)
        if (input$layout_type == "custom") {
          grunID::add_new_plate_results(
            con,
            protocol_name = input$protocol,
            genetic_method = input$genetic_method,
            laboratory = input$laboratory,
            lab_work_performed_by = input$performed_by,
            description = input$run_description,
            date_run = input$date_run,
            filepath = input$sherlock_results$datapath,
            sample_type = input$sample_type,
            layout_type = input$layout_type,
            plate_size = input$plate_size,
            selection_strategy = "recent priority",
            .control_id = input$control_blank,
            run_gen_id = input$perform_genetics_id,
            samples_type = input$sample_id_type,
            custom_layout_filepath = input$custom_layout_file$datapath)
        } else {
          grunID::add_new_plate_results(
            con,
            protocol_name = input$protocol,
            genetic_method = input$genetic_method,
            laboratory = input$laboratory,
            lab_work_performed_by = input$performed_by,
            description = input$run_description,
            date_run = input$date_run,
            filepath = input$sherlock_results$datapath,
            sample_type = input$sample_type,
            layout_type = input$layout_type,
            plate_size = input$plate_size,
            selection_strategy = "recent priority",
            .control_id = input$control_blank,
            samples_type = input$sample_id_type,
            run_gen_id = input$perform_genetics_id)

        }
        spsComps::shinyCatch({message("Success!")}, position = "top-center")
      },
      error = function(e) {
        removeModal(session = session)
        if (startsWith(e$message, "Error attempting insert data")) {
          showNotification(
            ui = tags$p(paste(e$message), call. = FALSE),
            closeButton = TRUE,
            duration = 20,
            type = "error"
          )
        } else if (startsWith(e$message, "Qa/Qc Test Not Passed")){
          showNotification(
            ui = tags$p(paste(str_split(e$message, pattern = "Qa/Qc ")[[1]][-1], collapse = " ---- ")),
            closeButton = TRUE,
            duration = 20,
            type = "error"
          )
        } else {
          showNotification(
            ui = tags$p(paste(e)),
            closeButton = TRUE,
            duration = 20,
            type = "error"
          )
        }
      },
      finally = {
        samples_failing(nrow(check_for_status()$failed))
        samples_need_ots16(nrow(check_for_status()$need_ots16))
      }
      )

    } else if (input$no_upload > 0) {
      removeModal(session = session)
      print("Submission cancelled by user.")
  }}, priority = 999)


  # Sample Status ---------------------------------------------------------------------

  initial_load_sample_status <- reactiveVal(TRUE)
  observeEvent(input$sample_status_refresh, {
    initial_load_sample_status(FALSE)
  })

  latest_sample_status <- eventReactive(list(input$sample_status_refresh, initial_load_sample_status()), {
    logger::log_info("Fetching latest results using sample status query")
    DB_get_sample_status()
  })

  selected_all_sample_status <- reactive({

    re <- ifelse(input$sample_status_season == 2023, "\\b\\w{3}23", "\\b\\w{3}24")
    data <- latest_sample_status() |> filter(str_detect(sample_id, re))

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

    latest_sample_status() |> filter(str_detect(sample_id, re)) |>
      group_by(status) |>
      summarise(
        total = n()
      )
  })


  # Query -------------------------------------------------------------------

  edit_data_submissions <- reactiveValues()

  selected_samples_by_season <- eventReactive(input$query_refresh, {
    if (input$dataset_type_filter == "runid") {
      data <- DBI::dbGetQuery(con, "SELECT DISTINCT ON (gri.sample_id)
    gri.sample_id,
    rt.run_name,
    substring(gri.sample_id FROM '^[^_]+_((?:100|[1-9][0-9]?))_') AS sample_event,
    st.datetime_collected,
    st.fork_length_mm,
    st.field_run_type_id
FROM
    genetic_run_identification gri
JOIN
    public.run_type rt
ON
    rt.id = gri.run_type_id
JOIN
    public.sample st
ON st.id = gri.sample_id
ORDER BY gri.sample_id, gri.updated_at DESC;
;
;")

    } else {

      logger::log_info("Fetching latest results using grunID::get_samples_by_season()")
      data <- grunID::get_samples_by_season(con, input$season_filter, input$dataset_type_filter,
                                            input$filter_to_heterozygotes, input$filter_to_failed)
    }
    data
  })

  output$season_table <- DT::renderDT({
    validate(need(nrow(selected_samples_by_season()) > 0, "Select a dataset and run query to view data"))
    selected_samples_by_season()
  },     extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20), server = FALSE, editable = list(target = "cell", disable = list(columns = c(0, 2:5))), selection="none")


  observeEvent(input$season_table_cell_edit, {
    sample_id_to_update <- selected_samples_by_season()[input$season_table_cell_edit$row,]$sample_id
    edit_data_submissions[[sample_id_to_update]] <- input$season_table_cell_edit$value
    })

  observeEvent(input$runid_submit_edits, {
    d <- reactiveValuesToList(edit_data_submissions)
    print(str(d))
    samples_to_update <- names(d)
    run_types <- c("Fall" = "FAL", "Spring" = "SPR", "Winter" = "WIN", "Unknown" = "UNK")
    for (sample in samples_to_update) {
      new_run_type = run_types[d[[sample]]]
      print(new_run_type)
      tryCatch(
      grunID::update_genetic_run_id(con, sample_id = sample, run_type = new_run_type),
      error = function(e) {
        showNotification(tags$p(print(e$message)), type = "error")
      }
      )
    }

    selected_samples_by_season()

    # Highlight the edited row
    proxy <- dataTableProxy("season_table")
    proxy %>% selectRows(edit_data_submissions$row)
  })

  observeEvent(input$runid_cancel_edits, {
    edit_data_submissions <- reactiveValues()
    output$season_table <- DT::renderDT({
      validate(need(nrow(selected_samples_by_season()) > 0, "Select a dataset and run query to view data"))
      selected_samples_by_season()
    },     extensions = "Buttons",
    rownames = FALSE,
    options = list(autoWidth = FALSE,
                   dom = "Bfrtip",
                   buttons = c("copy", "csv", "excel"),
                   lengthChange = TRUE,
                   pageLength = 20), server = FALSE, editable = list(target = "cell", disable = list(columns = c(0))), selection="none")


  })

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
    selected_samples_by_season() |>
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
#

  # Upload Field Sheets -----------------------------------------------------

  # read in field data, if available
  clean_field_data <- reactive({
      process_field_sheet_samples2(input$filled_field_sheets$datapath)
  })

  # if button pressed, upload field sheet data to database
  observeEvent(input$do_upload_field_sheets, {
    tryCatch({
      update_field_sheet_samples(con, clean_field_data())
      spsComps::shinyCatch({message("Field sheets updated in database")}, position = "top-center")
    },
    error = function(e) {
      spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
    })
  })

  # display field sheets
  output$field_sheet_summary <- DT::renderDataTable(DT::datatable({
    req(clean_field_data())
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


  # Subsample ---------------------------------------------------------------

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



  # Plate Validations -------------------------------------------------------------------

  initial_load_qa_qc <- reactiveVal(TRUE)

  plate_run_stack <- eventReactive(list(initial_load_qa_qc()), {
    logger::log_info("Fetching latest results from database for QA/QC tab")
    data <- plate_runs_used_for_genid()
    data |> arrange(desc(created_at))
  })

  # flagged table
  output$flagged_table <- DT::renderDataTable(DT::datatable(
    plate_run_stack(),
    rownames = FALSE,
    selection = "single",
    options = list(autoWidth = FALSE,
                   lengthChange = TRUE,
                   pageLength = 5,
                   dom = "ts")),
    server = FALSE)

  plate_data_top_stack <- reactive({
    selected_plate_run_id <- plate_run_stack()[1, ]$id
    tbl(con, "assay_result") |> dplyr::filter(plate_run_id == selected_plate_run_id) |>
      collect()
  })

  output$flagged_plate_run_comment <- renderUI({
    plate_has_flags <- !is.na(plate_run_stack()[1, ]$flags)
    if (!plate_has_flags) {
      return(NULL)
    } else {
      HTML(paste0('<div class="alert alert-danger" role="alert">',
                  'This plate contains flags, please choose action for resolving',
                  '</div>'))
    }
  })




  output$flagged_plate_run_table_display <- DT::renderDataTable({
    data <- plate_data_top_stack() |>
      select(plate_run_id, sample_id, raw_fluorescence, threshold, positive_detection, sub_plate) |>
      collect() |>
      filter(str_detect(sample_id, "^EBK")) |>
      arrange(sample_id)
    DT::datatable(data,
                  rownames = FALSE,
                  selection = "none",
                  options = list(dom = 't', pageLength = 500, scrollX = TRUE, scrollY = "500px")
    )
  })

  output$pv_all_plate_data_tbl <- DT::renderDataTable({
    plate_data_top_stack() |>
      select(plate_run_id, sample_id, raw_fluorescence, threshold, positive_detection, sub_plate, active) |>
      collect() |>
      DT::datatable(options = list(scrollY="500px", pageLength = 500, dom = "t")) |>
      formatStyle("active",
                  target = "row",
                  backgroundColor = styleEqual(
                    levels = c(FALSE),
                    values = c("#a1a1a1")
                  ))
  })

  observeEvent(input$delete_selected_plate, {
    showModal(modalDialog(
      title = "Confirm data deletion",
      tagList(
        h5(tags$b("Please confirm you wish to delete the full plate run and corresponding data.")),
        h5("Deleting the plate run will delete all data uploaded from this plate, this can potentially revert any run assignments and status codes assigned to a sample.")
      ),
      size = "l",
      easyClose = F,
      footer = tagList(
        actionButton("yes_delete_full_plate", "Yes, Delete", class= "btn-default"),
        actionButton("no_delete_full_plate", "Cancel", class = "btn-danger")
      )
    ))
  })

  observeEvent(
    eventExpr = list(
      input$yes_delete_full_plate,
      input$no_delete_full_plate
    ),
    handlerExpr = {
      if (input$yes_delete_full_plate > 0) {
        removeModal()
        logger::log_info("Removing plate run: {plate_run_stack()[1, ]$id}")
        tryCatch(
        remove_plate_run(con, plate_run_stack()[1, ]$id),
        error = function(e) {
          showNotification(ui = tags$p(paste0(e$message)), type = "error")
        }

        )

        } else if (input$no_delete_full_plate > 0) {
        removeModal()
        return(NULL)
      }
    },
    ignoreInit = TRUE)

  # deactivate
  # TODO update this action
  observeEvent(input$do_deactivate, {
    tryCatch({
      plate_id_to_deactivate <- selected_flagged_table_row()$plate_run_id
      # grunID::deactivate_plate_run(con, plate_id_to_deactivate)

      subplates_to_deactivate <- as.integer(input$subplate_in_selceted_plate)

      sql_statement <- glue::glue_sql("UPDATE assay_result set active = false where plate_run_id = {plate_id_to_deactivate} and sub_plate IN ({subplates_to_deactivate*});",
                                      .con = con)

      DBI::dbExecute(con, sql_statement)
      spsComps::shinyCatch({message(paste0("Plate run ", plate_id_to_deactivate, " deactivated"))}, position = "top-center")
    },
    error = function(e) {
      spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
    })
    # refresh
    initial_load_qa_qc(FALSE)
    initial_load_qa_qc(TRUE)
  })

  # TODO update this action
  # activate
  observeEvent(input$do_activate, {
    tryCatch({
      plate_id_to_activate <- selected_flagged_table_row()$plate_run_id
      subplates_to_activate <- as.integer(input$subplate_in_selceted_plate)

      sql_statement <- glue::glue_sql("UPDATE assay_result set active = true where plate_run_id = {plate_id_to_activate} and sub_plate IN ({subplates_to_activate*});",
                                      .con = con)
      DBI::dbExecute(con, sql_statement)
      spsComps::shinyCatch({message(paste0("Plate run ", plate_id_to_activate, " activated"))}, position = "top-center")
    },
    error = function(e) {
      spsComps::shinyCatch({stop(paste(e))}, prefix = '', position = "top-center")
    })
    # refresh
    initial_load_qa_qc(FALSE)
    initial_load_qa_qc(TRUE)
  })

  # Add sample -------------------------------------

  observeEvent(input$add_sample_submit, {
    grunID::add_sample(
      con = con,
      location_code = input$add_sample_location_code,
      sample_event_number = input$add_sample_event_number,
      first_sample_date = input$add_sample_first_sample_date,
      sample_bin_code = input$add_sample_sample_bin_code,
      min_fork_length = input$add_sample_min_fork_length,
      max_fork_length = input$add_sample_max_fork_length,
      expected_number_of_samples = input$add_sample_number_samples
    )
  })

  observeEvent(input$add_protocol_submit, {
    new_protocol <- protocol_template

    new_protocol$name <- input$add_protocol_name
    new_protocol$software_version <- input$add_protocol_software_version
    new_protocol$reader_type <- input$add_protocol_reader_type
    new_protocol$reader_serial_number <- input$add_protocol_serial_number
    new_protocol$plate_type <- input$add_protocol_plate_type
    new_protocol$set_point <- as.double(input$add_protocol_set_point)
    new_protocol$preheat_before_moving <- input$add_protocol_preheat_before_moving
    new_protocol$runtime <- input$add_protocol_runtime
    new_protocol$interval <- input$add_protocol_interval
    new_protocol$read_count <- as.double(input$add_protocol_read_count)
    new_protocol$run_mode <- input$add_protocol_run_mode
    new_protocol$excitation <- as.double(input$add_protocol_excitation)
    new_protocol$emissions <- as.double(input$add_protocol_emissions)
    new_protocol$optics <- input$add_protocol_optics
    new_protocol$gain <- as.double(input$add_protocol_gain)
    new_protocol$light_source <- input$add_protocol_light_source
    new_protocol$lamp_energy <- input$add_protocol_lamp_energy
    new_protocol$read_height <- as.double(input$add_protocol_read_height)

    new_protocol |> glimpse()


    grunID::add_protocol(
      con = con,
      protocol = new_protocol
    )
  })


  # Samples Check-in --------------------------------------------------

  samples_created_from_checkin <- reactiveVal(c())


  observeEvent(input$check_in_samples_submit, {
    samples_created <- grunID::check_in_jpe_field_samples(con, input$check_in_samples_file$datapath)
    samples_created_from_checkin(samples_created)

    if (length(samples_created) == 0) {
      showNotification("no new results found in the check-in file", closeButton = TRUE, type = "warning", duration = NULL)
    } else {
      showNotification(glue::glue("{length(samples_created_from_checkin())} additional sample(s) created from check-in file! You can view the list in Rstudio Output. You can view the list in Rstudio Output."),
                       closeButton = TRUE, type = "warning", duration = NULL)
    }
  })

}
