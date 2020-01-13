getpredictorsfromcaptureoutput<-function(tree,predictors){
  texte<-capture.output(tree)
  texte<-texte[-(1:min(grep("1)",texte)-1))]
  sapply(predictors,function(x){any(grep(x,texte))})
}