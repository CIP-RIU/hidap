get_crop_modules <- function(crop){
  fp = file.path("data/table_modules.rda")
  load(fp)
  out = list()
  abbr = "PT"
  if(crop == 'sweetpotato') abbr <- "SP" #TODO improve
  table_modules <- table_modules[table_modules$crop == abbr, ]
  for(i in 1:nrow(table_modules)){
    out[[i]] <- table_modules[i, 2]
    names(out)[[i]] <- table_modules[i, 3]
  }
  out
}
