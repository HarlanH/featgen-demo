

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title="Acme Chain Model"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Score", tabName="score", icon=icon("bullseye")),
        menuItem("Model", tabName="model", icon=icon("bar-chart"))
      ),
      selectInput("store", "Store", sort(unique(names(dat))))
    ),
    dashboardBody(
      tabItems(
        tabItem("score",
                fluidRow(
                  valueBoxOutput("score")
                ),
                fluidRow(
                  box(
                    title="Object being predicted",
                    textOutput("json")
                  )
                )
              ),
        tabItem("model",
                fluidRow(
                  box(
                    tableOutput("model_stats")
                  )
                ))
      )
      
      
    )
  )
)
