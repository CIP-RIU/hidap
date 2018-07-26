

col_names <- c("fileID", "Crop", "Breeder","Study Name", "Book Name", "book_server_name" ,"Action", "Study Status", "Created", "Modified")
fb_registry <- reactiveValues()
fb_registry$table <- setNames(data.table(matrix(nrow = 0, ncol = 10)), col_names)

# output$tab_fb_registry_above <- renderUI({
#     h2("Study Registry")
# })

output$fb_registry_table <- renderDataTable({

  
  DT = fb_registry$table

  datatable(DT,
            escape=F,
            selection = list(mode = 'none'),
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              columnDefs = list(list(className = 'dt-center', targets =1:10),list(visible=FALSE, targets=c(1, 6) ))
            )
  )
  
})

output$tab_fb_registry_below <- renderUI({
  actionButton("btUpdateFBRFiles", "Update table")  
})

observeEvent(input$btUpdateFBRFiles, {
  
  if(session$userData$logged)
    fb_registry$table <- data.table(userFilesFromDb(session$userData$userId))
})



userFilesFromDb <- function(user_id){
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  
  strQry = paste0("SELECT 
                a.id as fileID,
      					a.crop,
                a.breeder_name as breeder,
                a.study,
                a.book_name,
                a.server_book_name as server_name,
                'action' as action,
      					a.status,
      					a.date_created,
      					a.date_updated
      				FROM files  a
      				WHERE a.owner_id = ",user_id, " order by a.date_created desc")
  
  files_fb = dbSendQuery(mydb,strQry)
  filesFromDb <- fetch(files_fb, n=-1)
  dbDisconnect(mydb)
  
  df_files = data.frame(filesFromDb)
  
  if(nrow(df_files) != 0) df_files[7] <- actionButtons(nrow(df_files))
  
  names(df_files) <- col_names
  
  return(df_files)
  
} 

actionButtons <- function(num_rows){
  buttons <- c()
  
  for( i in 1:num_rows){
    str <- paste0('<button type="button" style="width:100px;" id = "fb_registry_button_put_', i ,'"> Put</button> ')
    str <- paste0(str,'<button type="button" style="width:100px;" id = "fb_registry_button_update_', i ,'"> Update</button> ')
    buttons <- c(buttons, str)
  }
  return(buttons)
}
