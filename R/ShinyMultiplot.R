library(shiny)
library(shinydashboard)
library(knitr)
library(tibble)
library(ggplot2)

source("viz_helper_funcs.R")

header <- dashboardHeader(
  title = "Data Explorer"
)

body <- dashboardBody(
  fluidRow(
    column(
      width = 9,
      box(
        width = NULL,
        status = "info",
        plotOutput("boxplot")
      )
    ),
    column(
      width = 3,
      box(
        width = NULL,
        status = "primary",
        selectInput("in.data",
                    "Dataset to Load:",
                    choices = c(
                      "iris" = "iris",
                      "mtcars" = "mtcars"
                    ),
                    selected = "mtcars"
        ),
        uiOutput("in.fields"),
        sliderInput("sample.size",
                    "Sample Size:",
                    min = 1,
                    max = 500,
                    value = 200)
      )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
  )


ui_old <- shinyUI(
  # Layout Guide for Grid:
  # https://shiny.rstudio.com/articles/layout-guide.html
  fluidPage(
    # Application title
    titlePanel("Sampling from the Normal Distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("sample.size",
                    "Sample Size:",
                    min = 1,
                    max = 500,
                    value = 200)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("boxplot")
      )
    )

  )
)

server <- function(input, output) {
  # List fields of the selected dataset
  output$in.fields <-renderUI({
    selectInput("in.field",
                "Field to Explore:",
                choices = colnames(get(input$in.data)),
                selected = "mtcars"
    )
  })

  # Display the BoxPlotIntro R Markdown
  output$boxplot <- renderPlot({
    set.seed(123)

    sample.size <- input$sample.size

    sim.norm <- as.tibble(cbind(Y = rnorm(sample.size)))
    sim.norm$X <- as.factor("Normal")



    sim.norm <-
      as.tibble(
        cbind(
          Y =
            get(input$in.data)[[input$in.field]]
        )
      )

    sim.norm$X <- as.factor(input$in.field)

    plot.sim.norm <-
      ggplot(sim.norm, aes(y = Y, x = X)) +
      #ggplot(get(paste(input$in.data,"$",input$in.field,sep="")), aes(y = Y, x = X)) +
      geom_boxplot(fatten = 1) +
      # Add mean as dashed line
      # stat_summary(fun.y = mean,
      #              fun.args = list(trim = 0.25),
      #              geom = "errorbar",
      #              width = 0.75,
      #              linetype = "dashed",
      #              color = "#ff0000",
      #              size = 1) +
      labs(title = input$in.data,
           x = input$in.field,
           y = "") +
      #scale_y_continuous(breaks = seq(-3,3,1),
      #                   limits = c(-3.25, 3.25)) +
      scale_x_discrete(expand = c(0,0)) +
      theme_project() +
      theme(legend.position = "none") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())

    #print(plot.sim.norm)

    norm.scatter <- as.tibble(cbind(X = seq(-3.25, 3.25, 0.01)))
    norm.scatter$Y <- dnorm(norm.scatter$X)

    plot.norm.scatter <-
      ggplot(norm.scatter, aes(y = Y, x = X)) +
      geom_line (size = 1.25) +
      geom_vline(xintercept = qnorm(1/4),
                 color = "#666666",
                 linetype = "solid",
                 size = 1.25) +
      geom_vline(xintercept = qnorm(3/4),
                 color = "#666666",
                 linetype = "solid",
                 size = 1.25) +
      labs(title = "Normal PDF",
           x = "Data Values",
           y = "Frequency") +
      scale_x_continuous(breaks = seq(-3,3,1),
                         limits = c(-3.25,3.25)) +
      scale_y_continuous(expand = c(0,0), limits = c(0,0.42)) +
      theme_project() +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
      coord_flip()

    multiplot(plot.norm.scatter, plot.sim.norm, cols = 2)
  })
}

shinyApp(ui, server)
