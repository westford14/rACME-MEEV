library(rACMEMEEV)
library(glmnet)

set.seed(987654)

simulate_gamma_mix <- function(n, a1 = 3, a2 = 4, a3 = 5) {
  shape1 <- 2
  rate1 <- 2
  shape2 <- 3
  rate2 <- 3
  shape3 <- 5
  rate3 <- 5

  x <- rgamma(n, shape1, rate1)
  y <- rgamma(n, shape2, rate2)
  z <- rgamma(n, shape3, rate3)
  output <- a1 * x + a2 * y + a3 * z + rnorm(n)

  frame <- data.frame(
    list(
      output = output,
      x = x,
      y = y,
      z = z
    )
  )
  return(frame)
}

run_all <- function(df, coefs) {
  df <- as.data.frame(df)
  output <- acme_model(df, c("x", "y", "z"))
  lambda <- attenuation_matrix(output, c("x", "y", "z"), coefs)
  model_output <- multivariate_model(
    "output ~ x + y + z",
    data = df,
    columns = c("x", "y", "z"),
    a_c_matrix = lambda$matrix,
    sds = lambda$sds,
    variances = lambda$variances,
    univariate = TRUE
  )
  model_output
}

function(input, output, session) {
  selected_data <- reactive({
    simulate_gamma_mix(100, input$a1, input$a2, input$a3)
  })
  tainted_data <- reactive({
    tainted_df <- selected_data()
    tainted_df$x <- tainted_df$x + rnorm(100, mean = 2)
    tainted_df$y <- tainted_df$y + rnorm(100, mean = 2)
    tainted_df$z <- tainted_df$z + rnorm(100, mean = 2)
    return(tainted_df)
  })

  a1_c <- reactive({
    generate_coefficient(
      1000,
      input[["a1-calib-low"]],
      input[["a1-calib-high"]],
      0.95
    )
  })
  a2_c <- reactive({
    generate_coefficient(
      1000,
      input[["a2-calib-low"]],
      input[["a2-calib-high"]],
      0.95
    )
  })
  a3_c <- reactive({
    generate_coefficient(
      1000,
      input[["a3-calib-low"]],
      input[["a3-calib-high"]],
      0.95
    )
  })

  model_output <- reactive({
    run_all(
      tainted_data(),
      c(a1_c(), a2_c(), a3_c())
    )
  })

  plots <- reactive({
    plot_covariates(model_output(), c("x", "y", "z"))
  })

  output$plot1 <- renderPlot({
    palette(
      c(
        "#E41A1C",
        "#377EB8",
        "#4DAF4A",
        "#984EA3",
        "#FF7F00",
        "#FFFF33",
        "#A65628",
        "#F781BF",
        "#999999"
      )
    )

    plot(plots()$x)
  })

  output$plot2 <- renderPlot({
    palette(
      c(
        "#E41A1C",
        "#377EB8",
        "#4DAF4A",
        "#984EA3",
        "#FF7F00",
        "#FFFF33",
        "#A65628",
        "#F781BF",
        "#999999"
      )
    )

    plot(plots()$y)
  })

  output$plot3 <- renderPlot({
    palette(
      c(
        "#E41A1C",
        "#377EB8",
        "#4DAF4A",
        "#984EA3",
        "#FF7F00",
        "#FFFF33",
        "#A65628",
        "#F781BF",
        "#999999"
      )
    )

    plot(plots()$z)
  })

  output$naive_summary <- renderPrint({
    tainted_model <- glm("output ~ x + y + z", data = tainted_data())
    print(summary(tainted_model))
  })
}