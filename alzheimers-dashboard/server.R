# ---- server ----
server <- function(input, output, session) {
  observeEvent(input$go1, updateNavbarPage(session, "main_nav", selected = study_names[1]))
  observeEvent(input$go2, updateNavbarPage(session, "main_nav", selected = study_names[2]))
  observeEvent(input$go3, updateNavbarPage(session, "main_nav", selected = study_names[3]))

  study_server("s1", study_files[1])
  study_server("s2", study_files[2])
  study_server("s3", study_files[3])
}