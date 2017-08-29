table_dictionary_potato <- readxl::read_excel("potato_cip.xlsx", "potato")
saveRDS(table_dictionary_potato, "xdata/potato/dictionary/table_dictionary_potato.rda")
