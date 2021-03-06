
  
# boolean.vector is logical as to whether the ith of n data points should be included in the analysis

begin.object <- function(boolean.vector, y.data, x.set, nametag = NULL, rowtags = all.reads$`By mutation(s)`){
  list(y.data = y.data[,boolean.vector], x.set = x.set[boolean.vector,], nametag = nametag, rowtags = rowtags)
}






conduct.linear.pca <- function(list.object){
  
  linear.pca <- prcomp(t(list.object$y.data))
  
  c(list.object, linear.pca = list(linear.pca))
  
  
}






conduct.regression2 <- function(list.object, pcas = 1:3){
  
  # lm needs a formula
  regression.formula <- (formula.function(list.object))[[1]]
  
    
  num.list <- create.numlist(pcas)  
    
  lpca.regressions <- lapply(num.list, FUN = function(y){apply.regression(y.set = list.object$linear.pca$x[,y],
                                                                          x.set = list.object$x.set,
                                                                          formula = regression.formula
                                                                          )})
  return(c(list.object, lpca.regressions = list(lpca.regressions)))
  
}



apply.regression <- function(y.set, x.set, formula){
  design.matrix <- data.frame(y.set = y.set, x.set)
  lm(formula = formula, data = design.matrix)
  }



create.numlist <- function(integer.vector){
  names(integer.vector) <-  integer.vector
  as.list(integer.vector)
}



# conduct.regression depends on formula.function
formula.function <- function(list.object){
  cell.type.include <- length(unique(list.object$x.set$Cell.Type)) > 1
  treatment.type.include <- length(unique(list.object$x.set$trtmnt.type)) > 1
  
  possible.formulas[cell.type.include  + treatment.type.include + 1]
}



# formula.function depends on this constant
possible.formulas <- c(y.set ~ trtmnt.amnt, y.set ~ trtmnt.type + trtmnt.amnt, y.set ~ Cell.Type + trtmnt.type + trtmnt.amnt)






#### Graphics Functions

# pipeline takes a PCA regression from all.obj.reg2,
# with a specified number of principle components,
# generates the regression's fitted values on a given interval,
# and places the predictions and observations in ggplot2 long-format dataframe

# example:    graphic.pipeline(analysis = all.obj.reg2$all.dip, principle.components.used = 1:2)


graphic.pipeline <- function(analysis, principle.components.used){
  
  principle.components.used <- create.numlist(principle.components.used)
  
  
  observations <- analysis$y.data
  
  principle.comp.matrix <- analysis$linear.pca$rotation[,unlist(principle.components.used)]
  
  fitted.in.pca <- lapply(names(principle.components.used), FUN = function(a){analysis$lpca.regressions[[a]]})
  
  
  names(fitted.in.pca) <- principle.components.used
  
  
  
  
  #### Predicted Data
  
  fitted.in.pca.matrix <- sapply(fitted.in.pca, FUN = function(b){b$fitted.values})
  
  fitted.percents <- principle.comp.matrix %*% t(fitted.in.pca.matrix)
  
  # adding mutation columns!
  
 
  
  fitted.percents <- mutate(as.data.frame(fitted.percents), mutation = analysis$rowtags)
  
 
  
  
  
  # unstacking
  fitted.percents.unstack <- pivot_longer(data = fitted.percents, cols = -mutation, names_to = "sample", values_to = "percent")
  
  
  fitted.percents.unstack <- mutate(fitted.percents.unstack, obs.or.predict = "Prediction")
  
  
  
  
  ####  Observation Data
  
  observations <- mutate(observations, mutation = analysis$rowtags)
  
  observed.percents.unstack <- pivot_longer(data = observations, cols = -mutation, names_to = "sample", values_to = "percent")
  
  observed.percents.unstack <- mutate(observed.percents.unstack, obs.or.predict = "Observation")
  
  
  
  
  #### Row Binding
  
  
  
  final.percents <- rbind(fitted.percents.unstack, observed.percents.unstack)
  
  # adding experiment information
  rownames(experiment.information) <- experiment.information$entire
  final.percents <- cbind(final.percents, experiment.information[ final.percents$sample,])
  
  
  
  
  return(c(analysis, prediction.plotting = list(final.percents)))
  
  
}


plot.pca.regression <- function(analysis){
  
  relevant.experiments <- all.boolean[ , unlist(boolean.list[[analysis$nametag]]) ]
  
  
  all.percent.sum <- apply(relevant.experiments, 1, sum)
  
  mutation.subsample <- all.reads$`By mutation(s)`[order(all.percent.sum, decreasing = TRUE)][1:3]
  
  subsample <- filter(final.percents, (trtmnt.type == "Ola" | trtmnt.type == "mock") & mutation %in% mutation.subsample & !percent < 0.5)
  
  ggplot(data = subsample, aes(x = trtmnt.amnt, y = percent, color = mutation)) +
    geom_point(data = subset(subsample, obs.or.predict == "Observation"), aes(x = trtmnt.amnt, y = percent, color = mutation))  +
    geom_line(data = subset(subsample, obs.or.predict == "Prediction"), aes(x = trtmnt.amnt, y = percent, color = mutation))  +
    #theme(legend.position = "none")+
    labs(title = "Principle Component Analysis on Olaparib Diploid Experiments", x = "Olaparib Concentration", y = "Allele Percentage") 
  
}



require(crayon)
cat(crayon::green("core.functions.R file successfully sourced\n"))


