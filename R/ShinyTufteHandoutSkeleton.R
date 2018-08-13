library(shiny)
library(knitr)

ui <- shinyUI(
  # Layout Guide for Grid:
  # https://shiny.rstudio.com/articles/layout-guide.html
  fluidPage(
    uiOutput('markdown')
  )
)

server <- function(input, output) {
  # Display the TufteHandoutSkeleton R Markdown
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('TufteHandoutSkeleton.Rmd', quiet = TRUE)))
  })
}

shinyApp(ui, server)
