
# Clear the enivornment
rm(list = ls())

source("scripts/script_name.R")

library(PackageName)

# Source all functions 
loadFunctions <- function() {
  write('- Sourcing Files ... ', stderr())
  directories = c("R/", "R/FolderName/")
  for (d in directories) {
    files = list.files(d, ".R")
    for (file in files) {
      filename = paste0(d, file)
      source(paste0(d, file))
      write(paste0('\t', filename), stderr())
    }
    write(sprintf('\t%-30s %s', d,  sprintf('[%2s files]', length(files))), stderr())
  }
}
loadFunctions()

write('<<<< Project Functions Loaded >>>>', stderr())

