# MetricX
#
# This is a package which works to calculate all the metrics for a n*n confusion matrix
# It returns various metrics like Sensitivity, Specificity, F1 Score, etc..
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Metricx = function(X){
  library(caret)
  Sensitivity = sensitivity(X)
  Specificity = specificity(X)
  Accuracy = sum(diag(X))/sum(X)
  False_Positive_Rate = ifelse(ncol(X)>2, "N/A", X[1,2]/sum(diag(X)))
  False_Negative_Rate = ifelse(ncol(X)>2, "N/A",X[2,1]/sum(diag(X)))
  Precision = precision(X)
  F1Score = (2*Precision*Sensitivity)/(Precision+Sensitivity)
  BCA = (Sensitivity+Specificity)/2
  Output = c(Sensitivity, Specificity, Accuracy, False_Positive_Rate, False_Negative_Rate, Precision, F1Score, BCA)
  names(Output)=c("Sensitivity a.k.a Recall a.k.a True Positive Rate","Specificity a.k.a True Negative Rate","Accuracy","False Positive Rate a.k.a. Type 1 Error rate","False Negative Rate a.k.a. Type 2 Error rate", "Precision", "F1 Score", "Balanced Classification Accuracy (BCA)")
  return(Output)
}
