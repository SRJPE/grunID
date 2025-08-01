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
  samples_need_gtseq <- reactiveVal(nrow(check_for_status()$need_gtseq))
  active_flagged_plate_run <- reactiveVal(check_for_status()$plate_run_flag)

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
                  samples_need_ots16(), ' samples were found that need OTS16, please seach "need ots16" in sample status tab for details',
                  '</div>'))
    }
  })

  output$ui_banner_for_need_gtseq_status <- renderUI({
    if (samples_need_gtseq() == 0) {
      return(NULL)
    } else {
      total_samples_failing <- nrow(samples_need_gtseq())
      HTML(paste0('<div class="alert alert-warning" role="alert">',
                  samples_need_gtseq(), ' samples were found that need GTSEQ, please seach "need gtseq" in sample status tab for details',
                  '</div>'))
    }
  })

  output$ui_banner_for_flagged_plate_run <- renderUI({
    if (!active_flagged_plate_run()) {
      return(NULL)
    } else {
      total_samples_failing <- nrow(active_flagged_plate_run())
      HTML(paste0('<div class="alert alert-danger" role="alert">',
                  'the latest plate run upload contains a flag, please review in the Plate Validations tab',
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

  observeEvent(input$run_description_use_filename, {
    if (input$run_description_use_filename == TRUE) {
      if (!is.null(input$sherlock_results)) {
        updateTextInput(session, "run_description", value = tools::file_path_sans_ext(input$sherlock_results[1,1]))
      } else {
        updateTextInput(session, "run_description", value = "USING FILENAME")
      }
    } else {
      updateTextInput(session, "run_description", value = "")
    }
  })

  observeEvent(input$sherlock_results, {
    if (!is.null(input$sherlock_results) && input$run_description_use_filename) {
      updateTextInput(session, "run_description", value = tools::file_path_sans_ext(input$sherlock_results[1,1]))
    }
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
            filepath = input$sherlock_results,
            sample_type = input$sample_type,
            layout_type = input$layout_type,
            plate_size = input$plate_size,
            selection_strategy = "recent priority",
            .control_id = input$control_blank,
            run_gen_id = input$perform_genetics_id,
            samples_type = input$sample_id_type,
            threshold_strategy = "twice average",
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
            filepath = input$sherlock_results,
            sample_type = input$sample_type,
            layout_type = input$layout_type,
            plate_size = input$plate_size,
            selection_strategy = "recent priority",
            .control_id = input$control_blank,
            threshold_strategy = "twice average",
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
        active_flagged_plate_run(check_for_status()$plate_run_flag)
        print(check_for_status()$plate_run_flag)

      }
      )

    } else if (input$no_upload > 0) {
      removeModal(session = session)
      print("Submission cancelled by user.")
    }}, priority = 999)

  observeEvent(input$gtseq_upload_file, {
    req(input$gtseq_upload_file)
    output$gtseq_results_preview <- renderTable(grunID::read_gtseq(input$gtseq_upload_file$datapath))
  })

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

    data <- latest_sample_status() |> filter(season == (as.numeric(input$sample_status_season) - 2000))

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

    latest_sample_status() |> filter(season == (as.numeric(input$sample_status_season) - 2000)) |>
      group_by(status) |>
      summarise(
        total = n()
      )
  })


  # Query -------------------------------------------------------------------

  edit_data_submissions <- reactiveValues()

  get_sql_statement <- function(table) {
    statement <- switch (table,
                         "Run Assignment" = {
                           "
                           SELECT
    --- gri.id AS genetic_run_id,
    gri.sample_id,
    s.event_number AS event,
    --- gri.run_type_id AS genetic_run_type_id,
    rt_genetic.run_name AS genetic_run_name,
    s.fork_length_mm,
    s.datetime_collected,
    rt_field.run_name AS field_run_name,
    gri.early_plate_id,
    gri.late_plate_id,
    gri.winter_plate_id,
    gri.spring_plate_id
    --- gri.created_at AS genetic_run_created_at
FROM (
    SELECT *,
           ROW_NUMBER() OVER (PARTITION BY sample_id ORDER BY created_at DESC) as rn
    FROM genetic_run_identification
) gri
JOIN run_type rt_genetic ON gri.run_type_id = rt_genetic.id
JOIN sample s ON gri.sample_id = s.id
LEFT JOIN run_type rt_field ON s.field_run_type_id = rt_field.id
WHERE gri.rn = 1
  AND rt_genetic.run_name IN ({run_name_filters*})
  AND (rt_field.run_name is NULL or rt_field.run_name IN ({field_run_name_filters*}))
  AND s.event_number IN ({sample_event_filters*})
  AND s.season = {season_filter}
ORDER BY gri.sample_id;
                           "
                         },
                         "Assay Results" = {
                           "SELECT * FROM assay_result;"
                         },
                         "Raw Assay Results" = {
                           "SELECT * FROM raw_assay_result;"
                         },
                         "Plate Runs" = {
                           "SELECT * FROM plate_run;"
                         },
                         "Sample Archive Plates" = {
                           "select sample_id, sample_archive_plates.arc_plate_id from sample_archive_plates join sample s on s.id = sample_archive_plates.sample_id where s.season = 25;"
                         }
    )

    return(statement)
  }

  # TODO: refactor so that code is not repeated like this
  query_results <- eventReactive(input$query_refresh, {
    if (input$query_table_select == "Run Assignment") local({
      sql_statement <- get_sql_statement(input$query_table_select)
      run_name_filters <- input$query_ra_select_run_type
      field_run_name_filters <- input$query_ra_select_field_run_type
      sample_event_filters <- input$query_ra_select_sample_event
      season_filter <- as.numeric(input$season_filter) - 2000
      stmt <- glue::glue_sql(sql_statement, .con = con)
      print(stmt)
      data <- DBI::dbGetQuery(con, stmt)
      return(data)
    }) else if (input$query_table_select == "Assay Results") local({
      sql_statement <- get_sql_statement(input$query_table_select)
      stmt <- glue::glue_sql(sql_statement, .con = con)
      data <- DBI::dbGetQuery(con, stmt)
      return(data)
    }) else if (input$query_table_select == "Raw Assay Results") local({
      sql_statement <- get_sql_statement(input$query_table_select)
      stmt <- glue::glue_sql(sql_statement, .con = con)
      data <- DBI::dbGetQuery(con, stmt)
      return(data)
    }) else if (input$query_table_select == "Plate Runs") local({
      sql_statement <- get_sql_statement(input$query_table_select)
      stmt <- glue::glue_sql(sql_statement, .con = con)
      data <- DBI::dbGetQuery(con, stmt)
      return(data)
    }) else if (input$query_table_select == "Sample Archive Plates") local({
      sql_statement <- get_sql_statement(input$query_table_select)
      stmt <- glue::glue_sql(sql_statement, .con = con)
      data <- DBI::dbGetQuery(con, stmt)
      return(data)
    })

  })

  output$season_table <- DT::renderDT({
    query_results()
  },     extensions = "Buttons",
  rownames = FALSE,
  options = list(autoWidth = FALSE,
                 dom = "Bfrtip",
                 buttons = c("copy", "csv", "excel"),
                 lengthChange = TRUE,
                 pageLength = 20),
  server = FALSE,
  editable = list(target = "cell", disable = list(columns = c(0, 1, 3:9))),
  selection="none")

  # editable = list(target = "cell", disable = list(columns = c(0, 2:5)))


  observeEvent(input$season_table_cell_edit, {
    sample_id_to_update <- query_results()[input$season_table_cell_edit$row,]$sample_id
    edit_data_submissions[[sample_id_to_update]] <- input$season_table_cell_edit$value
  })

  observeEvent(input$runid_submit_edits, {
    d <- reactiveValuesToList(edit_data_submissions)
    samples_to_update <- names(d)

    for (sample in samples_to_update) {
      new_run_type = run_name_to_code[d[[sample]]]
      tryCatch(
        grunID::update_genetic_run_id(con, sample_id = sample, run_type = new_run_type),
        error = function(e) {
          showNotification(tags$p(print(e$message)), type = "error")
        },
        finally = DT::renderDT({
          validate(need(nrow(query_results()) > 0, "Select a dataset and run query to view data"))
          query_results()
        },     extensions = "Buttons",
        rownames = FALSE,
        options = list(autoWidth = FALSE,
                       dom = "Bfrtip",
                       buttons = c("copy", "csv", "excel"),
                       lengthChange = TRUE,
                       pageLength = 20), server = FALSE, editable = list(target = "cell", disable = list(columns = c(0, 1, 3:9))), selection="none")
      )
    }

    query_results()

    # Highlight the edited row
    proxy <- dataTableProxy("season_table")
    proxy %>% selectRows(edit_data_submissions$row)
  })

  observeEvent(input$runid_cancel_edits, {
    edit_data_submissions <- reactiveValues()
    output$season_table <- DT::renderDT({
      validate(need(nrow(query_results()) > 0, "Select a dataset and run query to view data"))
      query_results()
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

  events_with_returned_from_field_samples <- reactive({
    season_code <- stringr::str_sub(as.character(get_current_season()$year), 3,4)
    get_sample_status(con, season = get_current_season()$year) |>
      filter(status_code_name == "returned from field") |>
      ungroup()
  })

  events_with_need_ots16_or_gtseq_samples <- reactive({
    get_sample_status(con, season = get_current_season()$season_code) |>
      filter(status_code_name %in% c("need ots16", "need gtseq"))
  })

  need_ots16_or_gtseq_events <- reactive({
    events_with_need_ots16_or_gtseq_samples() |>
      distinct(event_number) |>
      pull()
  })

  output$gen_arc_plate_events_UI <- renderUI({
    selectInput("gen_arc_plate_events", "Events", multiple = TRUE, choices = dplyr::distinct(events_with_returned_from_field_samples(), event_number)$event_number)
  })

  output$gen_ham_plate_events_UI <- renderUI({
    selectInput("gen_ham_plate_events", "Events", multiple = TRUE, choices = need_ots16_or_gtseq_events())
  })

  output$gen_arc_plate_samples_preview <- renderTable({
    validate(need(!is.null(input$gen_arc_plate_events), "select at least one event"))
    events_with_returned_from_field_samples() |>
      filter(event_number %in% (input$gen_arc_plate_events)) |>
      transmute(`#` = row_number(), sample_id, status_code_name, comment)
  })


  output$gen_ham_plate_samples_preview <- renderTable({
    validate(need(!is.null(input$gen_ham_plate_events), "select at least one event"))
    events_with_need_ots16_or_gtseq_samples() |>
      filter(status_code_name == if_else(input$gen_ham_destination == "sherlock", "need ots16", "need gtseq")) |>
      filter(event_number %in% input$gen_ham_plate_events) |>
      select(sample_id, status_code_name, comment)
  })


  # Generate Plates ---------------------------------------------------------------
  observeEvent(input$save_arc_plates, {
    if (length(input$gen_arc_plate_events) == 0) {
      shiny::showNotification("you must select at least one event to generate a plate",
                              type = "warning")
      return(NULL)
    }


    res <- make_archive_plate_maps_by_event(con,
                                     events = input$gen_arc_plate_events,
                                     output_dir = cfg$data_output,
                                     season = get_current_season()$year)

    if (res$success) {
      shiny::showNotification(glue::glue("file created and saved to: {cfg$data_output} see RStudio Console for details and full paths"),
                              type = "message")
    }
  })

  observeEvent(input$save_ham_plates, {
    if (length(input$gen_ham_plate_events) == 0) {
      shiny::showNotification("you must select at least one event to generate a plate",
                              type = "warning")
      return(NULL)
    }

    res <- make_sw_plate_maps(
      con,
      events = input$gen_ham_plate_events,
      destination = input$gen_ham_destination,
      output_dir = cfg$data_output,
      season = get_current_season()
    )

    if (res$success) {
      shiny::showNotification(glue::glue("file created and saved to: {cfg$data_output} see RStudio Console for details and full paths"),
                              type = "message")
    }
  })


  observeEvent(input$register_plate_submit, {
    print(input$register_plate_files)
    purrr::walk(1:nrow(input$register_plate_files), function(i) {
      this_file <- input$register_plate_files[i, ]
      logger::log_info(this_file$datapath)
      register_arc_plate(con, this_file$datapath, this_file$name)
      # upload_to_azure_storage(container, this_file$datapath, paste0("arc-plates/",this_file$name))
    }, .progress = TRUE)
  })




  # Plate Validations -------------------------------------------------------------------

  initial_load_qa_qc <- reactiveVal(TRUE)

  plate_run_stack <- eventReactive(list(initial_load_qa_qc()), {
    logger::log_info("Fetching latest results from database for QA/QC tab")
    plate_ids <- plate_runs_used_for_genid() |> pull(id)
    tbl(con, "plate_run") |>
      filter(id %in% plate_ids) |>
      dplyr::arrange(desc(created_at)) |>
      collect()
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


  assay_codes <- tbl(con, "assay") |>
    select(id, assay_name)

  plate_data_top_stack <- reactive({
    selected_plate_run_id <- plate_run_stack()[1, ]$id
    tbl(con, "assay_result") |> dplyr::filter(plate_run_id == selected_plate_run_id) |>
      left_join(assay_codes, by = c("assay_id" = "id")) |>
      collect()
  })


  output$flagged_plate_run_table_display <- DT::renderDataTable({
    data <- plate_data_top_stack() |>
      select(plate_run_id, assay_name, sample_id, raw_fluorescence, threshold, positive_detection, sub_plate) |>
      collect() |>
      filter(str_detect(sample_id, "^EBK")) |>
      dplyr::arrange(sample_id)
    DT::datatable(data |> select(-plate_run_id, -sub_plate, -positive_detection),
                  rownames = FALSE,
                  selection = "none",
                  options = list(dom = 't', pageLength = 500, scrollX = TRUE, scrollY = "500px")
    ) |>
      formatStyle(columns = "raw_fluorescence", target = "row",
                  backgroundColor = styleInterval(c(12000), c("white", "#ebb5b5")))
  })

  subplate_choices <- reactive({
    get_all_subplates_for_run(con, plate_run_id = plate_run_stack()[1, ]$id)
  })

  output$ui_subplate_checkbox <- renderUI({
    checkboxGroupInput("subplate_checkbox", "Subplate", choices = subplate_choices())
  })

  output$pv_all_plate_data_tbl <- DT::renderDataTable({
    plate_data_top_stack() |>
      select(plate_run_id, sample_id, raw_fluorescence, threshold, positive_detection, sub_plate, active) |>
      collect() |>
      dplyr::arrange(sub_plate) |>
      DT::datatable(options = list(scrollY="500px", pageLength = 500, dom = "t")) |>
      formatStyle("sub_plate",
                  target = "row",
                  backgroundColor = styleEqual(
                    levels = c(1, 2, 3, 4),
                    values = c("#ebdccc", "#d7ebcc", "#ccdaeb", "#dfcceb")
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
          # remove_plate_run(con, plate_run_stack()[1, ]$id),
          remove_subplates_from_run(con, plate_run_stack()[1, ]$id, input$subplate_checkbox),
          error = function(e) {
            showNotification(ui = tags$p(paste0(e$message)), type = "error")
          }

        )
        initial_load_qa_qc(!initial_load_qa_qc())
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
      showNotification("chek-in complete, no new samples were created from this check-in", closeButton = TRUE, type = "warning", duration = NULL)
    } else {
      showNotification(glue::glue("{length(samples_created_from_checkin())} additional sample(s) created from check-in file! You can view the list in Rstudio Output. You can view the list in Rstudio Output."),
                       closeButton = TRUE, type = "warning", duration = NULL)
    }
  })

}
