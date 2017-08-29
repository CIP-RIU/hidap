get_dictionary_table <- function(crop){
  fp <- file.path(fname_dictionary, paste0("table_dictionary_", crop,".rda"))
  if(file.exists(fp)){
    load(fp)
    return(dictionary)
  }
  NULL
}
