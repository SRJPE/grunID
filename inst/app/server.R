function(input, output, session) {


  observeEvent(input$show_protocol_details, {
    showModal(modalDialog(
      title = "Protocol Details",
      renderTable(all_protocols)
    ))
  })

  observeEvent(input$show_lab_details, {
    showModal(modalDialog(
      title = "Laboratories Details",
      renderTable(all_labs)
    ))
  })

  # TODO add a little question icon for some of these
  observeEvent(input$show_methods_details, {
    showModal(modalDialog(
      title = "Genetic Methods Details",
      renderTable(all_gen_methods)
    ))
  })

  # TODO show error in app / prevents app from crashing
  observeEvent(input$do_upload, {
    add_new_plate_results(con, protocol_name = input$protocol,
                          genetic_method = input$genetic_method,
                          laboratory = input$laboratory,
                          lab_work_performed_by = input$performed_by,
                          description = input$run_description,
                          date_run = input$date_run,
                          filepath = input$sherlock_results$datapath,
                          sample_type = input$sample_type,
                          layout_type = input$layout_type,
                          plate_run_id = NULL,
                          plate_size = input$plate_size)
  })
}
