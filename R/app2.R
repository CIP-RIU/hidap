library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(
        12,
        h1("BrAPI connections"),
        box(
          title = "BrAPI test", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
          h3("1. Check library"),
          actionButton("checkLib", "Check lib"),
          fluidRow(
            column(
              6,
              h3("2. Select database"),
              actionButton("showDb", "Show db"),
              DTOutput('tbl')
            ),
            column(
              6,
              h3("3. Get Address:Port"),
              uiOutput('info')
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  listdb <- function() {
    white_list <- brapi::ba_db()
    white_list2 <- as.data.frame(names(white_list), stringsAsFactors = FALSE)
  }
  
  observeEvent(input$showDb, {
    output$tbl = renderDT(
      listdb(), options = list(lengthChange = FALSE), selection = 'single'
    )
  })
  
  output$info <- renderPrint({
    row_count <- input$tbl_rows_selected
    #print(row_count)
    #data <- Product()[row_count, ]
    cat('Row Selected: ')
    #cat(row_count) #display the selected row 1st col value
    print(input$tbl[1,row_count])


  })
  
  # observe({
  #   row_count <- input$tbl_rows_selected
  #   print(input$tbl[row_count,])
  # })
  
  # selectedRow <- eventReactive(input$tbl_rows_selected,{
  #   row.names(listdb())[c(input$tbl_rows_selected,1)]
  # })
  # 
  # output$info <- renderText({ 
  #   selectedRow()
  # })
  
}

shinyApp(ui, server)