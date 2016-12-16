
library(shiny)
library(igraph)

ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("rTRM+ simulator"),
    sidebarPanel(
      h3("General"),
      sliderInput("nsample", "nsample", min = 100, max = 1000, value = 100, step = 100),
      numericInput("seed", "seed", value = 123),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Regulators",
          h3("Regulators"),
          inputPanel(
            column(12,
                   h5("R1"),
                   sliderInput("r1mean", "mean", value = 1, min = 0, max = 5),
                   sliderInput("r1sd", "sd", value = 1, min = 0, max = 5)
            ),
            column(12,
                   h5("R2"),
                   sliderInput("r2mean", "mean", value = 1, min = 0, max = 5),
                   sliderInput("r2sd", "sd", value = 1, min = 0, max = 5)
            ),
            column(12,
                   h5("R3"),
                   sliderInput("r3mean", "mean", value = 1, min = 0, max = 5),
                   sliderInput("r3sd", "sd", value = 1, min = 0, max = 5)
            )
          ),
          plotOutput("regulators")
        ),
        tabPanel(
          title = "Network",
          h3("Weights"),
          inputPanel(
            column(12,
                   h5("G1"),
                   sliderInput("g1r1w", "R1 weight", value = 0, min = 0, max = 5),
                   sliderInput("g1r2w", "R2 weight", value = 0, min = 0, max = 5),
                   sliderInput("g1r3w", "R3 weight", value = 0, min = 0, max = 5)
            ),
            column(12,
                   h5("G2"),
                   sliderInput("g2r1w", "R1 weight", value = 0, min = 0, max = 5),
                   sliderInput("g2r2w", "R2 weight", value = 0, min = 0, max = 5),
                   sliderInput("g2r3w", "R3 weight", value = 0, min = 0, max = 5)
            ),
            column(12,
                   h5("G3"),
                   sliderInput("g3r1w", "R1 weight", value = 0, min = 0, max = 5),
                   sliderInput("g3r2w", "R2 weight", value = 0, min = 0, max = 5),
                   sliderInput("g3r3w", "R3 weight", value = 0, min = 0, max = 5)
            )
          ),
          plotOutput("graphplot"),
          plotOutput("correlation")
        ),
        tabPanel(
          title = "Matrix",
          dataTableOutput("matrix")
        ),
        tabPanel(
          title = "Expression",
          plotOutput("expression")
        ),
        tabPanel(
          title = "Correlation"
        )
      )
    )
  )
})

server <- shinyServer(function(input, output, session) {
  m <- matrix(0, nrow = 3, ncol = 3)
  colnames(m) <- c("G1", "G2", "G3")
  rownames(m) <- c("R1", "R2", "R3")

  data <- reactiveValues(df = NULL, model = m)

  observeEvent(input$update, {
    set.seed(input$seed)

    # regulators.
    r1 <- rnorm(input$nsample, mean = input$r1mean, sd = input$r1sd)
    r2 <- rnorm(input$nsample, mean = input$r2mean, sd = input$r2sd)
    r3 <- rnorm(input$nsample, mean = input$r3mean, sd = input$r3sd)

    # model.
    data$model[, "G1"] <- c(input$g1r1w, input$g1r2w, input$g1r3w)
    data$model[, "G2"] <- c(input$g1r1w, input$g1r2w, input$g1r3w)
    data$model[, "G3"] <- c(input$g1r1w, input$g1r2w, input$g1r3w)

    # network
    g1 <- r1 * input$g1r1w + r2 * input$g1r2w + r3 * input$g1r3w
    g2 <- r1 * input$g2r1w + r2 * input$g2r2w + r3 * input$g2r3w
    g3 <- r1 * input$g3r1w + r2 * input$g3r2w + r3 * input$g3r3w

    data$df <- data.frame(
      r1 = r1,
      r2 = r2,
      r3 = r3,
      g1 = g1,
      g2 = g2,
      g3 = g3
    )
  })

  output$graphplot <- renderPlot({
    g <- graph_from_incidence_matrix(data$model, directed = TRUE, multiple = FALSE, mode = "out", weighted = TRUE)
    V(g)$color <- "steelblue2"
    V(g)[ type ]$color <- "grey"
    V(g)$shape <- "square"
    V(g)[ type ]$shape <- "circle"
    l <- layout.bipartite(g)
    if(ecount(g)>0)
      E(g)$width <- E(g)$weight
    plot(g, layout = l, vertex.size = 25, vertex.label.color = "black", vertex.label.family = "sans")
  })

  output$matrix <- renderDataTable({
    data$df
  })

  output$expression <- renderPlot({
    if (!is.null(data$df)) {
      dd <- data$df %>% gather("gene", "value")
      ggplot(dd, aes(x = value)) + geom_histogram() + facet_wrap(~gene)
    }
  })

  output$regulators <- renderPlot({
    if (!is.null(data$df)) {
      dd <- data$df %>% gather("gene", "value") %>% mutate(type = sub(".$", "", gene)) %>% filter(type == "r")
      ggplot(dd, aes(x = value, fill = gene)) + geom_histogram() + facet_wrap(~gene, ncol = 1)
    }
  })

  output$correlation <- renderPlot({
    if (!is.null(data$df))
      pairs(data$df)
  })
})

shinyApp(ui, server)
