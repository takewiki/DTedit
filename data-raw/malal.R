
  # Display an important message that can be dismissed only by clicking the
  # dismiss button.
  shinyApp(
    ui = basicPage(
      actionButton("show", "Show modal dialog")
    ),
    server = function(input, output) {
      observeEvent(input$show, {
        showModal(modalDialog(
          title = "Important message",
          "This is an important message!"
        ))
      })
    }
  )