library(dplyr)
library(shiny)
library(shinydashboard)
library(knitr)
library(tibble)
library(ggplot2)
library(GGally)

source("viz_helper_funcs.R")

header <- dashboardHeader(
  title = "Data Explorer"
)

body <- dashboardBody(
  fluidRow(
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
        uiOutput("color.field"),
        actionButton("update.plot","Update")
      )
    ),
    column(
      width = 9,
      box(
        width = NULL,
        status = "primary",
        plotOutput("pairplot")
      )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
  )


server <- function(input, output) {
  # List fields of the selected dataset
  output$in.fields <- renderUI({
    checkboxGroupInput("in.field.list",
                "Field to Explore:",
                choices = colnames(get(input$in.data))
    )
  })

  output$color.field <- renderUI({
    selectInput("in.color.field",
                "Color Field:",
                choices = colnames(get(input$in.data))
    )
  })

  # Display the Pairs Plot
  get.plot.colnames <- eventReactive(input$update.plot, {
    input$in.field.list
  })

  get.color.field <- eventReactive(input$update.plot, {
    input$in.color.field
  })

  dataset.name <- eventReactive(input$update.plot, {
    input$in.data
  })

  get.list <- eventReactive(input$update.plot, {
    get(dataset.name()) %>%
      select(one_of(c(get.plot.colnames(),get.color.field())))
  })

  output$pairplot <- renderPlot({
    showpairs <- get.list()

    for (field in colnames(showpairs)) {
      if (is.numeric(showpairs[[field]])) {
        field.size <- length(showpairs[[field]])
        field.unique.size <-
          length(as.factor(sort(unique(showpairs[[field]]))))
        if (field.unique.size < 10 &&
            field.size / field.unique.size >= 2.0) {
          showpairs[[field]] <- as.factor(showpairs[[field]])
        } else {
          showpairs[[field]] <-
            cut(showpairs[[field]],
                min(field.unique.size, 9)
            )
        }
      }
    }
    #get.color.field
    ggpairs(showpairs,
            columns = get.plot.colnames(),
            aes_string(colour = get.color.field()),
            #aes(colour = NULL),
            title = paste("Dataset:",
                          dataset.name(),
                          "; Fields:",
                          # TODO: make the colnames show up as a list
                          #       currently just shows first item
                          get.plot.colnames(),
                          sep=" "))
            #title = input$in.data)
  })
}

shinyApp(ui, server)
