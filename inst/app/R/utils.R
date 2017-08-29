material_list_tree <- function(){
  level0 <- list.files(fname_material_list)
  n <- length(level0)
  out = as.list(character(n))
  if(n > 0){
    for(i in 1:n){
      fn <- list.files(file.path(fname_material_list, level0[i]))
      if(length(fn) == 0) {
        names(out)[i] = as.character(level0[i])
      } else {
        names(out)[i] = as.character(level0[i])
        out[[i]] = as.list(fn)
        for( j in 1:length(out[[i]])){
          names(out[[i]])[[j]] = fn[j]
          out[[i]][[j]] = ""
        }
      }
    }
  } else {
    out = list(NULL)
  }
  out
}
