if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)}

df = df %>% rename('New Variable Name 1' = VariableName1
                 , 'New Variable Name 2' = VariableName2)