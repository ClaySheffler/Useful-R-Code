if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)}




# Rename DF Variables
df_new = df %>% rename('New Variable Name 1' = VariableName1
                     , 'New Variable Name 2' = VariableName2)
  

# The filter function will return all the rows that satisfy a following condition.
df_new = df %>% filter(var1 < 60 & var2 == 'Primary')


# Mutate is used to add new variables to the data.
df_new = df %>% mutate(var_new = var1/var2)


# The summarise function is used to summarise multiple values into a single value. It is very powerful when used in conjunction with the other functions in the dplyr package, as demonstrated below. na.rm = TRUE will remove all NA values while calculating the mean, so that it doesn’t produce spurious results.
df_new = df %>% summarise(mean(var1, na.rm = TRUE))


# The group_by function is used to group data by one or more variables.
df_new = df %>% group_by(var2) %>% summarise(mean(var1, na.rm = TRUE))