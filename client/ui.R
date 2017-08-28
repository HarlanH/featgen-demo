

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title="Acme Chain Model"),
    dashboardSidebar(
      selectInput("store", "Store", sort(unique(names(dat))))
    ),
    dashboardBody(
      fluidRow(
        valueBoxOutput("score")
      ),
      fluidRow(
        textOutput("json")
      )
      
    )
  )
)
