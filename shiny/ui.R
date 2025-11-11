navbarPage(
  "Multivariate Measurement Error Adjustment with External Calibration Data",
  tabPanel("Naive Model",
    sidebarPanel(
      numericInput("a1", "a1", 3, min = 1, max = 9),
      numericInput("a2", "a2", 2, min = 1, max = 9),
      numericInput("a3", "a3", 1, min = 1, max = 9)
    ),
    mainPanel(verbatimTextOutput("naive_summary"))
  ),
  tabPanel("rACME-MEEV",
    sidebarPanel(
      numericInput(
        "a1-calib-low",
        "a1 calibration coefficient lower boundary",
        0.5,
        min = 0,
        max = 1,
        step = 0.1
      ),
      numericInput(
        "a1-calib-high",
        "a1 calibration coefficient upper boundary",
        0.8,
        min = 0,
        max = 1,
        step = 0.1
      ),
      numericInput(
        "a2-calib-low",
        "a2 calibration coefficient lower boundary",
        0.5,
        min = 0,
        max = 1,
        step = 0.1
      ),
      numericInput(
        "a2-calib-high",
        "a2 calibration coefficient upper boundary",
        0.8,
        min = 0,
        max = 1,
        step = 0.1
      ),
      numericInput(
        "a3-calib-low",
        "a3 calibration coefficient lower boundary",
        0.5,
        min = 0,
        max = 1,
        step = 0.1
      ),
      numericInput(
        "a3-calib-high",
        "a3 calibration coefficient upper boundary",
        0.8,
        min = 0,
        max = 1,
        step = 0.1
      ),
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)
