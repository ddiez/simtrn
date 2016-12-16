
library(shiny)
library(igraph)

ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("rTRM+ simulator"),
    sidebarPanel(
      h3("General"),
      sliderInput("nsample", "nsample", min = 1, max = 1000, value = 100),
      actionButton("updateregulator", "Update"),
      #numericInput("seed", "seed", value = 123),
      conditionalPanel(
        condition = "input.tabs == 'Expression'",
        h3("Regulators"),
        h5("R1"),
        sliderInput("r1mean", "mean", value = 1, min = 0, max = 10),
        sliderInput("r1sd", "sd", value = 1, min = 0, max = 10),
        h5("R2"),
        sliderInput("r2mean", "mean", value = 1, min = 0, max = 10),
        sliderInput("r2sd", "sd", value = 1, min = 0, max = 10),
        h5("R3"),
        sliderInput("r3mean", "mean", value = 1, min = 0, max = 10),
        sliderInput("r3sd", "sd", value = 1, min = 0, max = 10)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Graph'",
        h3("Genes"),
        h5("G1"),
        sliderInput("g1r1w", "R1 weight", value = 0, min = 0, max = 10),
        sliderInput("g1r2w", "R2 weight", value = 0, min = 0, max = 10),
        sliderInput("g1r3w", "R3 weight", value = 0, min = 0, max = 10),
        h5("G2"),
        sliderInput("g2r1w", "R1 weight", value = 0, min = 0, max = 10),
        sliderInput("g2r2w", "R2 weight", value = 0, min = 0, max = 10),
        sliderInput("g2r3w", "R3 weight", value = 0, min = 0, max = 10),
        h5("G3"),
        sliderInput("g3r1w", "R1 weight", value = 0, min = 0, max = 10),
        sliderInput("g3r2w", "R2 weight", value = 0, min = 0, max = 10),
        sliderInput("g3r3w", "R3 weight", value = 0, min = 0, max = 10)
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Graph",
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
  r1 <- reactive({
    input$updateregulator
    isolate({
      rnorm(input$nsample, mean = input$r1mean, sd = input$r1sd)
    })
  })

  r2 <- reactive({
    input$updateregulator
    isolate({
      rnorm(input$nsample, mean = input$r2mean, sd = input$r2sd)
    })
  })

  r3 <- reactive({
    input$updateregulator
    isolate({
      rnorm(input$nsample, mean = input$r3mean, sd = input$r3sd)
    })
  })

  g1 <- reactive({
    r1() * input$g1r1w + r2() * input$g1r2w + r3() * input$g1r3w
  })

  g2 <- reactive({
    r1() * input$g2r1w + r2() * input$g2r2w + r3() * input$g2r3w
  })

  g3 <- reactive({
    r1() * input$g3r1w + r2() * input$g3r2w + r3() * input$g3r3w
  })

  imatrix <- reactive({
    data.frame(
      r1 = r1(),
      r2 = r2(),
      r3 = r3(),
      g1 = g1(),
      g2 = g2(),
      g3 = g3()
    )
  })

  model <- reactive({
    m <- matrix(
      c(
        c(input$g1r1w, input$g1r2w, input$g1r3w),
        c(input$g2r1w, input$g2r2w, input$g2r3w),
        c(input$g3r1w, input$g3r2w, input$g3r3w)
      ),
      ncol = 3
    )
    colnames(m) <- c("G1", "G2", "G3")
    rownames(m) <- c("R1", "R2", "R3")
    m
  })

  output$graphplot <- renderPlot({
    g <- graph_from_incidence_matrix(model(), directed = TRUE, multiple = FALSE, mode = "out", weighted = TRUE)
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
    imatrix()
  })

  output$expression <- renderPlot({
    dd <- imatrix() %>% gather("gene", "value")
    ggplot(dd,aes(x=value)) + geom_histogram() + facet_wrap(~gene)
  })

  output$correlation <- renderPlot({
    pairs(imatrix())
  })
})

shinyApp(ui, server)
