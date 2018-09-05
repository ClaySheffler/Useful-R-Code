# LIFT AND GAIN TABLE
LiftKSandGain <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(min = min(predcol),
                                    max = max(predcol),
                                    median = median(predcol),
                                    avg = mean(predcol),
                                    total = n(),
                                    totalresp=sum(., na.rm = FALSE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           PercentOfEvents = totalresp/sum(totalresp)*100,
           PercentOfCumEvents = cumsum(totalresp)/sum(totalresp)*100,
           NonEvents = total - totalresp,
           PercentOfCumNonEvents = cumsum(total - totalresp)/sum(total - totalresp)*100,
           KS = PercentOfCumEvents - PercentOfCumNonEvents,
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  
  return(gaintable)
}

# To run the function...
dt = lift(df$target , df$prediction, groups = 10)

# To plot the curve...
graphics::plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")