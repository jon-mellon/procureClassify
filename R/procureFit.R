procureFit <- function(procure, random = TRUE, 
                       text.var = "text",
                       classifier = c("svm", "maxent", 
                                      "glmnet")) {
  dtm <- formatProcureText(procure, text.var)
  if(random) {
    procure$fit <- 1
    procure$fit[sample(1:nrow(procure), 250)] <- 0
  }
  container <- create_container(dtm, procure$label, 
                                trainSize = which(procure$fit==1),
                                testSize = which(procure$fit==0), 
                                virgin=TRUE)
  if(classifier[1]=="svm") {
    SVM <- train_model(container, "SVM")
    predictions <- classify_model(container,SVM)
  }
  if(classifier[1]=="maxent") {
    MAXENT <- train_model(container, "MAXENT")
    predictions <- classify_model(container,MAXENT)
  }
  if(classifier[1]=="glmnet") {
    GLMNET <- train_model(container, "GLMNET")
    predictions <- classify_model(container,GLMNET)  
  }
  predictions$true.label <- procure$label[procure$fit==0]
  predictions$text <- procure[procure$fit==0, text.var]
  return(predictions)
}