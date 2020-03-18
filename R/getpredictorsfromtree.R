#' getpredictorsfromcaptureoutput
#' 
#' @param tree same format than output of partykit::ctree
#' @param predictors a vector of character strings, indicating  variable names.
#' @returns a named vector of booleans. The names correspond to `predictors`, and the boolean value
#' indicates if the variables was actually used in the fitted model or not.
#' @description for each terminal node of the tree, 
#' give the elements of "predictors" who appear in the node definition
#' @details A basic `grep` function is applied to each potential predictor. 
#' It is returned if it appears in the rules.
#' @seealso daniRules
#' @examples
#' getpredictorsfromcaptureoutput(party::ctree(Petal.Width~.,iris),names(iris))
getpredictorsfromcaptureoutput<-function(tree,predictors){
  texte<-capture.output(tree)
  texte<-texte[-(1:min(grep("1)",texte)-1))]
  sapply(predictors,function(x){any(grep(x,texte))})
}


#' getpredictorsfromtree
#' @param tree same format than output of partykit::ctree
#' @param predictors list of variables
#' @returns 
#' @description for each terminal node of the tree, give the elements of 
#' "predictors" who appear in the node definition
#' @seealso daniRules
#'@examples
#' tree<-partykit::ctree(Petal.Width~.,iris)
#' getpredictorsfromtree(tree,names(iris))
getpredictorsfromtree<-function(tree,predictors){
  texte<-daniRules(tree)
  sapply(texte,function(y){sapply(predictors,function(x){any(grep(x,y))})})}
