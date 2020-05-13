#' sample function
#' no modifications are made here
#' @examples
#' data(school,package="BigSyn")
#' 
#' L<-list(x = school[, 1:9], 
#' y = school$bscore, y.name = "bscore",
#'                    random = "schoolid", 
#'                    lgmodel = "slope",
#'                    rslope = "+ female + sclass",
#'                    id = school$schoolid)
#' attach(L)
#' fit.model<-do.call(fitmodel.ctree.new,L)
#' sample.ctree.new(x = school[, 1:9], fit.model)

sample.ctree.new <- function(xp,fit.model,smoothing="none",...){
  keep<-names(xp)[sapply(names(xp),function(x){
    any(grepl(pattern = x, x = fit.model$Rules$condition))})]
  xp <- preparepredictorsforctreefit(xp,keep=keep)
  # for (i in which(sapply(x, class) != sapply(xp, class))){ xp[,i] <- eval(parse(text = paste0("as.", class(x[, i])[1], "(xp[,i])", sep = "")))}
  newterminalnodes <- getnodesfromrules(xp,fit.model$Rules)
  ysyn <-   samplefrompool(fit.model$y,fit.model$terminalnodes,newterminalnodes)
  if (!is.factor(fit.model$y) & smoothing == "density"){ysyn <- synthpop:::syn.smooth(ysyn, fit.model$y)}
  ysyn}
