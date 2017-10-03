lrmodel <- function(args){
  setwd('c:/citybike')
  save(args, file = "args")
  citibike.data <- data.frame(mapply( FUN = c,args))
  citibike.data$date <- NULL
  formula <- as.formula(paste0(colnames(citibike.data)[1]," ~ ."))
  lrmodel <- glm(data = citibike.data,formula = formula, family = "gaussian")
  citibike.lm.fit <- predict.lm(object = lrmodel, newdata = citibike.data)
  return (citibike.lm.fit)
}
lrmodel_r_squared <- function(args){
  setwd('c:/citybike')
  save(args, file = "args")
  citibike.data <- data.frame(mapply( FUN = c,args))
  citibike.data$date <- NULL
  formula <- as.formula(paste0(colnames(citibike.data)[1]," ~ ."))
  lrmodel <- lm(data = citibike.data,formula = formula)
  r.squared <- rep(summary(lrmodel)$r.squared, nrow(citibike.data))
  return (r.squared)
}
