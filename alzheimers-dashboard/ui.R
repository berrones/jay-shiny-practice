# ---- UI ----
ui <- page_navbar(
  title = "Alzheimer’s Study Analyzer",
  theme = med_theme,
  id = "main_nav",
  nav_panel(
    "Home",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(study_names[1]),
        p("Multiplexed Immunoassay Panel Identifies Novel CSF Biomarkers for Alzheimer's Disease Diagnosis and Prognosis, PLoS ONE 6(4): e18850."),
        actionButton("go1", "Open analysis", class = "btn-primary")
      ),
      card(
        card_header(study_names[2]),
        p("A case-control study of smoking and Alzheimer's disease. International Journal of Geriatric Psychiatry 12: 295-300."),
        actionButton("go2", "Open analysis", class = "btn-primary")
      ),
      card(
        card_header(study_names[3]),
        p("Pattern of brain atrophy rates in autopsy-confirmed dementia with Lewy bodies, Neurobiology of Aging 36: 452-461."),
        actionButton("go3", "Open analysis", class = "btn-primary")
      )
    )
  ),
  nav_panel(study_names[1], study_ui("s1", study_names[1])),
  nav_panel(study_names[2], study_ui("s2", study_names[2])),
  nav_panel(study_names[3], study_ui("s3", study_names[3]))
)