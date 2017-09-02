
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  obj_json <- reactive({
    toJSON(dat[[input$store]], pretty=TRUE, auto_unbox = TRUE)
  })
  
  output$json <- renderText({obj_json()})
  
  score <- reactive({
    resp <- POST("http://localhost:8000/predict", body=obj_json())
    content(resp)$current_sales
  })
  
  info <- reactive({
    resp <- GET("http://localhost:8000/info")
    content(resp)
  })
  
  output$score <- renderValueBox({
    valueBox(
      format(as.numeric(score()), nsmall=2),
      "Predicted Sales",
      icon=icon("usd")
    )
  })
  
  output$model_stats <- renderTable({
    info() %>%
      as_data_frame() %>%
      gather(stat, value)
  })
})
