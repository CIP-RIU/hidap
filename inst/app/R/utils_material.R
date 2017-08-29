
new_material_table <- function(){
  #numeration <- 1:2
  is_control   <- c(TRUE, FALSE) # must be two upper case letters
  scale_audpc <- c(6, NA)
  institutional_number <- c("CIP720064", "CIP377744.1")
  material_name <- c("Yungay", "Kori-INIA")
  material_code <- c("KB 507", "P-8")
  family_number <- c("", "")
  pedigree <- c("", "")
  female_number <- c("", "")
  female_code  <- c("", "")
  male_number  <- c("", "")
  male_code  <- c("", "")
  seed_source  <- c("", "")
  simultanious_trials  <- c("", "")
  previous_trials  <- c("", "")

  as.data.frame(cbind(institutional_number, material_name, material_code,
                      is_control, scale_audpc,
                      family_number, pedigree, female_number, female_code,
                      male_number, male_code, seed_source, simultanious_trials,
                      previous_trials),
                stringsAsFactors = FALSE)
}

get_material_table <- function(crop, year, mlist_name){

  fname_materials <- file.path(fname_materials, crop,
                               paste0(year,"_", mlist_name)
                               )
  #print(fname_materials)
  if (!file.exists(fname_materials)) {
    return(NULL)
  }
  #print(fname_materials)
  load(fname_materials)
  table_materials
}

post_material_table <- function(table_materials, crop, year, name){
  fname <- file.path(fname_materials, crop, paste0(year, "_", name))
  save(table_materials, file = fname)
}

list_material_lists <- function(){
  list.files(fname_material_list, recursive = TRUE )
}

import_list_from_prior <- function(crop, year, name, fname){
  dp <- file.path(getwd(), fname_material_list, crop)
  if(!dir.exists(dp)) dir.create(dp)
  dp <- file.path(dp, paste0(year, "_",name))
  # print("DP")
  # print(dp)

  if(stringr::str_detect(fname, ".xlsx")){
    #print(fname)
    table_materials <- readxl::read_excel(fname, "materials")

    save(table_materials, file = dp)
  } else {
    file.copy(fname, dp)
  }

}

# get_selected_tree_node <- function(tree) {
#   unlist(get_selected(tree))
# }


get_material_n <- function(fp){
  load(fp)
  #nrow(table_materials)
  table_materials$institutional_number
}

list_material_lists <- function(){
  paste0(fname_materials, .Platform$file.sep, list.files(fname_materials, rec=T))
}

get_material_total <- function(){
  out = lapply(list_material_lists(), get_material_n)
  #sum(unlist(out))
  length(unique(unlist(out)))
}
