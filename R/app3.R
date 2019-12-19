library(shiny)
library(shinydashboard)
library(DT)
library(brapi)
library(jsonlite)
library(httr)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      tabBox(
        title = "Module test",
        id = "tabset1",
        width = 12,
        tabPanel(
          "BrAPI calls",
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
              verbatimTextOutput('info'),
              actionButton("getPort", "Get port"),
              br(),
              br(),
              verbatimTextOutput("port"),
              h3("4. Select call"),
              selectInput("selectCall", "Select call", c("locations", "traits", "germplasm"))
              
            )
          ),
          fluidRow(
            column(
              12,
              h3("5. Show final results"),
              actionButton("showResults", "Show results"),
              DTOutput('tbl2')
            )
          )
        ),
        tabPanel(
          "URL calls",
          fluidRow(
            column(
              4,
              h3("1. Complete the URL"),
              #url <- "https://sweetpotatobase.org/brapi/v1/locations/"
              #http://www.cippotatobase.org/brapi/v1/locations?pageSize=1000
              selectInput("a", "", c("https://", "http://www.")),
              textInput("b", "", "sweetpotatobase"),
              textInput("c", "", ".org/brapi/"),
              selectInput("d", "", c("v1/", "v2/")),
              selectInput("e", "", c("locations", "traits", "germplasm"))
            ),
            column(
              8,
              h3("Path"),
              verbatimTextOutput("path"),
              h3("Options"),
              selectInput("pageSize", "Page size", c("10" = "?pageSize=10", "100" = "?pageSize=100", "1000" = "?pageSize=1000"))
            )
          ),
          fluidRow(
            column(
              12,
              h3("2. Show final results"),
              actionButton("showResults2", "Show results"),
              DTOutput('tbl3')
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
    cat('Row Selected: ')
    print(listdb()[row_count,1])
  })
  
  observeEvent(input$getPort, {
    output$port <- renderPrint({
      isolate({
        row_count <- input$tbl_rows_selected
        dbname <- listdb()[row_count,1]
        
        con <<- white_list[[dbname]]
        con
      })
    })
  })
  
  showdb <- function() {
    row_count <- input$tbl_rows_selected
    dbname <- listdb()[row_count,1]
    
    white_list <- brapi::ba_db()
    con <- white_list[[dbname]]
    
    if (input$selectCall == "locations") {
      data <- brapi::ba_locations(con)
      class(data) <- "data.frame"
      data
    } else if (input$selectCall == "traits") {
      data <- brapi::ba_traits(con)
      class(data) <- "data.frame"
      data
    } else if (input$selectCall == "germplasm") {
      data <- brapi::ba_germplasm(con)
      class(data) <- "data.frame"
      data
    }
  }
  
  observeEvent(input$showResults, {
    #print(showdb())
    output$tbl2 = renderDT(
      showdb(), options = list(lengthChange = FALSE, scrollX = TRUE), selection = 'single'
    )
  })
  
  showdb2 <- function() {
    url <- paste0(input$a, input$b, input$c, input$d, input$e, input$pageSize)
    
    res1 <-httr::GET(url = url, timeout(25), httr::add_headers(Authorization = paste("Bearer",NULL)))
    res2 <- httr::content(x = res1, as = "text", encoding = "UTF-8")

    lst <- jsonlite::fromJSON(txt = res2)
    lstdf <- lst$result
    bb <- as.data.frame(lstdf, stringsAsFactors = FALSE)
  }
  
  observeEvent(input$showResults2, {
    #print(showdb2())
    output$tbl3 = renderDT(
      showdb2(), options = list(lengthChange = FALSE, scrollX = TRUE), selection = 'single'
    )
  })
  
  output$path <- renderPrint({
    url <- paste0(input$a, input$b, input$c, input$d, input$e, input$pageSize)
    url
  })
  
}

shinyApp(ui, server)