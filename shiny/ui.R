navbarPage(
  "Multivariate Measurement Error Adjustment with External Validation Data",
  tabPanel("Naive Model",
    sidebarPanel(
      numericInput("a1", "α₁", 3, min = 1, max = 9),
      numericInput("a2", "α₂", 2, min = 1, max = 9),
      numericInput("a3", "α₃", 1, min = 1, max = 9),
      hr(),
      helpText(
        "
        Each one of the α values refers to a different coefficient on
        a new simulated dataset drawn from γ distributions. This is meant
        to mimic real world consumption data [1] that is linked to an 
        outcome that is normally distributed. The α coefficients represent
        the true relationship between the exposures and the outcome. But,
        This dataset is then 'tainted' by adding a measurement error in
        the form of normally distributed noise to each of the exposure
        estimates. Below shows the desired relationship:
        ",
        br(),
        br(),
        "y = α₁ ∗ x₁ + α₂ ∗ x₂ + α₃ ∗ x₃ + ϵ",
        br(),
        br(),
        "And here is what is observed when the measurement error is added:",
        br(),
        br(),
        "y = α₁ ∗ (x₁ + N₁) + α₂ ∗ (x₂ + N₂) + α₃ ∗ (x₃ + N₃) + ϵ",
        br(),
        br(),
        "[1] Passerelli et al. 2022",
      ),
      hr(),
      helpText(
        "All code available: https://github.com/westford14/rACME-MEEV"
      )
    ),
    mainPanel(
      verbatimTextOutput("naive_summary"),
      hr(),
      helpText(
        "
        Above shows a 'naive' model where measurement error is not
        taken into consideration. And it is clear to see how
        'measurement error' can significantly impact our view of
        the world. The estimated coefficients on `x2`, `x2`, and `x3`
        are vastly different and do not at all reflect
        the set values by the left panel. Thus, we can see how and
        why this measurement error adjustment is so important. Continue
        on to the next tab to see how we the proposed measurment error
        adjustment methodology performs on this very same data.
        "
      )
    )
  ),
  tabPanel("rACME-MEEV",
    sidebarPanel(
      numericInput("a1", "α₁", 3, min = 1, max = 9),
      numericInput("a2", "α₂", 2, min = 1, max = 9),
      numericInput("a3", "α₃", 1, min = 1, max = 9),
      sliderInput(
        inputId = "a1-calib",
        label = "α₁ validity coefficient",
        min = 0, max = 1,
        value = c(0.5, 0.8),
        step = 0.05
      ),
      sliderInput(
        inputId = "a2-calib",
        label = "α₂ validity coefficient",
        min = 0, max = 1,
        value = c(0.5, 0.8),
        step = 0.05
      ),
      sliderInput(
        inputId = "a3-calib",
        label = "α₃ validity coefficient",
        min = 0, max = 1,
        value = c(0.5, 0.8),
        step = 0.05
      ),
      hr(),
      helpText(
        "
        In the absence of the running a validation study, we can
        leverage literature basedvalidity coefficients to help
        adjust for the measurement error. Because this developed
        method is Bayesian in nature, we can instead use a
        distribution over the range of the validity coefficients
        to add more prior data. In this library the
        `generate_coefficient` function can easily do this for us.
        This performs a Fisher-Z transformation [1] on the values
        to normalize the distribution and then back transforms it
        to yield the resultant mean value of the distribution.
        ",
        br(),
        br(),
        "Remember that we are drawing from tainted data that follows:",
        br(),
        br(),
        "y = α₁ ∗ (x₁ + N₁) + α₂ ∗ (x₂ + N₂) + α₃ ∗ (x₃ + N₃) + ϵ",
        br(),
        br(),
        "
        [1] Wikipedia contributors. (2025, September 4). Fisher 
        transformation. Wikipedia.
        https://en.wikipedia.org/wiki/Fisher_transformation
        "
      ),
      hr(),
      helpText(
        "All code available: https://github.com/westford14/rACME-MEEV"
      )
    ),
    mainPanel(
      plotOutput("plot1"),
      hr(),
      plotOutput("plot2"),
      hr(),
      plotOutput("plot3"),
      hr(),
      textOutput("adjusted_x1"),
      textOutput("adjusted_x2"),
      textOutput("adjusted_x3"),
      hr(),
      helpText(
        "
        Recall the original model yielded coefficients that were
        not at all close to the 'true' simulated related from
        earlier. Looking through at the mean values of the adjusted
        coefficients, we can see how much closer to the true values
        set in the other tab. Of course, these do not exactly match
        with the known simulated α parameters, but they are much
        closer to reality than what had previously been derived
        from the tainted dataset. Even more importantly, when we look
        at the 97.5% percentile, we can see that α parameters are
        contained within each one of the adjusted covariates. This
        means that we have recovered a lot of the ground truth
        from the tainted dataset.
        "
      )
    )
  ),
  tabPanel("Data Import",
    sidebarLayout(
      sidebarPanel(
        helpText(
          "
          Here you can now try with your own dataset. Please remember that
          this is a multivariate adjust that requires at least 3 different
          exposures (inputs) to properly function. You can upload a CSV
          from this tab and then analysis it in the next tab labelled
          'User Data Model'. There you can you select the different output
          and inputs and then see the resultant analysis as well. Due to
          constraints within shiny, validity coefficients will be chosen
          automatically.
          "
        ),
        hr(),
        fileInput("file", "Upload your CSV", multiple = FALSE),
        hr(),
        h5(helpText("select the read.table parameters below")),
        checkboxInput(inputId = "header", label = "header", value = FALSE),
        checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
        radioButtons(
          inputId = "sep",
          label = "separator",
          choices = c(comma = ",", semicolon = ";", tab = "\t", space = ""),
          selected = ","
        ),
        hr(),
        helpText(
          "All code available: https://github.com/westford14/rACME-MEEV"
        )
      ),
      mainPanel(uiOutput("user_data_table"))
    )
  ),
  tabPanel(
    "User Data Model",
    sidebarLayout(
      sidebarPanel(
        helpText(
          "
          Below you will see the columns availbe from the 'Data Import'
          tab that you can select. The dropdown menu will be the output
          variable and the other buttons will be the input exposures. As
          each of the input exposures is selected a slider will populate
          that you can use to fill with the validity coefficients that
          you wish to add to each of the exposure variables. You will 
          see the model string dynamically generate up top as well as the
          naive model summary listed with its coefficients on the
          various exposures as well as the graphs of the adjusted 
          coefficients below that.
          "
        ),
        hr(),
        uiOutput("output_variable"),
        uiOutput("exposure_variables"),
        uiOutput("sliders"),
        hr(),
        helpText(
          "All code available: https://github.com/westford14/rACME-MEEV"
        )
      ),
      mainPanel(
        helpText("Your Selected variables"),
        verbatimTextOutput("other_val_show"),
        hr(),
        helpText(
          "Below shows the naive model:"
        ),
        verbatimTextOutput("user_naive_summary"),
        hr(),
        helpText(
          "Below shows the measurement error adjustment model:"
        ),
        hr(),
        uiOutput("user_model_output")
      )
    )
  )
)
