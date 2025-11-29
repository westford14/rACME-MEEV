library(rACMEMEEV)
library(glmnet)
library(caret)

seed <- 987654

simulate_gamma_mix <- function(n, a1 = 3, a2 = 2, a3 = 1) {
  set.seed(seed)
  shape1 <- 2
  rate1 <- 2
  shape2 <- 3
  rate2 <- 3
  shape3 <- 5
  rate3 <- 5

  x1 <- rgamma(n, shape1, rate1)
  x2 <- rgamma(n, shape2, rate2)
  x3 <- rgamma(n, shape3, rate3)
  output <- a1 * x1 + a2 * x2 + a3 * x3 + rnorm(n)

  frame <- data.frame(
    list(
      output = output,
      x1 = x1,
      x2 = x2,
      x3 = x3
    )
  )
  return(frame)
}

run_all <- function(df, coefs, ind, columns) {
  df <- as.data.frame(df)
  output <- acme_model(df, columns, seed = seed)
  lambda <- attenuation_matrix(output, columns, coefs)
  form <- sprintf(
    "%s~%s",
    ind,
    paste0(columns, collapse = " + ")
  )
  model_output <- multivariate_model(
    form,
    data = df,
    columns = columns,
    a_c_matrix = lambda$matrix,
    sds = lambda$sds,
    variances = lambda$variances,
    univariate = TRUE,
    seed = seed
  )
  model_output
}

function(input, output, session) {
  selected_data <- reactive({
    simulate_gamma_mix(100, input$a1, input$a2, input$a3)
  })
  tainted_data <- reactive({
    tainted_df <- selected_data()
    tainted_df$x1 <- tainted_df$x1 + rnorm(100, mean = 2)
    tainted_df$x2 <- tainted_df$x2 + rnorm(100, mean = 2)
    tainted_df$x3 <- tainted_df$x3 + rnorm(100, mean = 2)
    return(tainted_df)
  })

  a1_c <- reactive({
    generate_coefficient(
      1000,
      input[["a1-calib"]][1],
      input[["a1-calib"]][2],
      0.95
    )
  })
  a2_c <- reactive({
    generate_coefficient(
      1000,
      input[["a2-calib"]][1],
      input[["a2-calib"]][2],
      0.95
    )
  })
  a3_c <- reactive({
    generate_coefficient(
      1000,
      input[["a3-calib"]][1],
      input[["a3-calib"]][2],
      0.95
    )
  })

  model_output <- reactive({
    run_all(
      tainted_data(),
      c(a1_c(), a2_c(), a3_c()),
      "output",
      c("x1", "x2", "x3")
    )
  })

  plots <- reactive({
    plot_covariates(model_output(), c("x1", "x2", "x3"))
  })

  output$plot1 <- renderPlot({
    plot(plots()$x1)
  })

  output$plot2 <- renderPlot({
    plot(plots()$x2)
  })

  output$plot3 <- renderPlot({
    plot(plots()$x3)
  })

  output$adjusted_x1 <- renderText({
    x <- quantile(model_output()$multivariate$x1, 0.975)
    print(
      paste(
        "x1 97.5%: ", sprintf("%.3f", x)
      )
    )
  })

  output$adjusted_x2 <- renderText({
    x2 <- quantile(model_output()$multivariate$x2, 0.975)
    print(
      paste(
        "x2 97.5%: ", sprintf("%.3f", x2)
      )
    )
  })

  output$adjusted_x3 <- renderText({
    x3 <- quantile(model_output()$multivariate$x3, 0.975)
    print(
      paste(
        "x3 97.5%: ", sprintf("%.3f", x3)
      )
    )
  })

  output$naive_summary <- renderPrint({
    tainted_model <- glm("output ~ x1 + x2 + x3", data = tainted_data())
    print(summary(tainted_model))
  })

  output$true_summary <- renderPrint({
    true_model <- glm("output ~ x1 + x2 + x3", data = selected_data())
    print(summary(true_model))
  })

  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    read.table(
      file = file1$datapath,
      sep = input$sep,
      header = input$header,
      stringsAsFactors = input$stringAsFactors
    )
  })

  output$sliders <- renderUI({
    req(input$other_var_select)

    sliders <- lapply(input$other_var_select, function(varname) {
      sliderInput(
        inputId = paste0("calib_", varname),
        label = paste(varname, ": ", "calibration coefficient range", sep = ""),
        min = 0, max = 1,
        value = c(0.5, 0.8),
        step = 0.05
      )
    })
    do.call(tagList, sliders)
  })

  output$table <- renderTable({
    if (is.null(data())) {
      return()
    }
    head(data(), 25)
  })
  output$user_data_table <- renderUI({
    tableOutput("table")
  })

  output$output_variable <- renderUI({
    selectInput(
      "ind_var_select",
      "Select dependent variable",
      choices = as.list(names(data())),
      multiple = FALSE
    )
  })
  output$exposure_variables <- renderUI({
    checkboxGroupInput(
      "other_var_select",
      "Select exposures",
      choices = as.list(names(data()))
    )
  })

  output$other_val_show <- renderPrint({
    input$other_var_select
    input$ind_var_select
    f <- data()

    form <- sprintf(
      "%s~%s",
      input$ind_var_select,
      paste0(input$other_var_select, collapse = " + ")
    )
    print(form)
  })

  output$user_naive_summary <- renderPrint({
    form <- sprintf(
      "%s~%s",
      input$ind_var_select,
      paste0(input$other_var_select, collapse = " + ")
    )
    tainted_model <- glm(form, data = data())
    print(summary(tainted_model))
  })

  output$user_model_output <- renderUI({
    plot_output_list <- lapply(
      input$other_var_select,
      function(i) {
        plotname <- paste("plot", i, sep = "_")
        plotOutput(plotname)
      }
    )
    do.call(tagList, plot_output_list)
  })

  observe({
    req(input$other_var_select)
    if (length(input$other_var_select) < 3) return()

    coeffs <- lapply(input$other_var_select, function(varname) {
      bounds <- input[[paste0("calib_", varname)]]
      if (is.null(bounds)) {
        return()
      }
      generate_coefficient(
        1000,
        bounds[1],
        bounds[2],
        0.95
      )
    })

    coefs <- unlist(coeffs, use.names = FALSE)

    safe_model <- tryCatch(
      {
        run_all(
          data(),
          coefs,
          input$ind_var_select,
          input$other_var_select
        )
      },
      error = function(e) {
        message("Model error: ", e$message)
        NULL
      }
    )

    if (is.null(safe_model)) return()

    safe_plots <- tryCatch(
      {
        plot_covariates(safe_model, input$other_var_select)
      },
      error = function(e) {
        message("Plot generation error: ", e$message)
        NULL
      }
    )

    if (is.null(safe_plots)) return()


    for (i in seq_along(input$other_var_select)) {
      local({
        var <- input$other_var_select[i]
        plotname <- paste("plot", var, sep = "_")

        output[[plotname]] <- renderPlot({
          safe_plots[[var]]
        })
      })
    }
  })
}
