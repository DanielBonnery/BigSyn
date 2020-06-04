#' @author Yi Feng
#' @examples
#' library(BigSyn)
#' data(school,package="BigSyn")
#' L<-list(x = school[, 1:9], 
#' y = school$bscore, y.name = "bscore",
#'                    random = "schoolid", 
#'                    lgmodel = "slope",
#'                    fixed = "+ female + sclass",
#'                    rslope = "+ female + sclass",
#'                    id = school$schoolid)
#'                    rslope = "+ female + sclass")
#' attach(L)
#' fit.model<-do.call(fitmodel.ctree.new,L)
#' fit.model$FixedEffects
#' 
#' L<-list(x = school[, 1:9], 
#' y = school$bscore, y.name = "bscore",
#'                    random = "schoolid", 
#'                    lgmodel = "int",
#'                    fixed = "+ female + sclass",
#'                    rslope = NULL,
#'                    id = school$schoolid)
#' attach(L)
#' fit.model2<-do.call(fitmodel.ctree.new,L)
#' fit.model2$FixedEffects


fitmodel.ctree.new<-function(x,
                             y,
                             y.name = "bscore",
                             random = "schoolid", 
                             lgmodel = "slope",
                             fixed = "+ female + sclass",
                             rslope = "+ female + sclass",
                             treeplotsavepath=NULL,...){
  # prepare data
  #1 convert all posix to numeric
  id=x[random]
  y2<-y
  if(inherits(y2,"POSIXct")|inherits(y2,"POSIXt")){y2<-as.numeric(y2)}
  x2<-preparepredictorsforctreefit(x)
  data <- cbind(y2=y2, if(ncol(x2)==0){x}else{x2}, id)
  colnames(data)[1] <- y.name
  colnames(data)[ncol(data)] <- random
  
  # fit the clustered tree algorithm
  part.tree <- M.CART.new(formula = formula(paste(toString(y.name), "~", noquote(paste(colnames(x2),collapse="+")))), fixed = fixed, random = random, lgmodel = lgmodel, data = data, rslope = rslope)
  # extract the tree
  datact <- part.tree$Tree
  depthx<-try(treedepth(datact))
  # save the tree plot
  if(!is.null(treeplotsavepath)){try(BigSyn::treetopdf(datact,treeplotsavepath))} 
  
  # splitting rules
  splitnodeconditions <- daniRules(datact)
  splitnodeconditionsmerge<-paste0(splitnodeconditions,collapse="")
  Rules <- if(length(splitnodeconditions)>1){
    data.frame(terminalnode=strtoi(names(splitnodeconditions)),
               condition=splitnodeconditions,stringsAsFactors = FALSE)}else{
                 data.frame(terminalnode=1,condition="TRUE")}
  
  # terminal nodes for each observation
  terminalnodes<-getnodesfromrules(x,Rules)
  
  shortlist <- if(ncol(x2)==0){character(0)}else{names(x2)[
    sapply(names(x2),grepl,x=splitnodeconditionsmerge)]} 
  
  # output results
  list(Rules=Rules,
       y=y,
       terminalnodes=terminalnodes,
       shortlist=shortlist,
       EffectModel=part.tree$EffectModel,
       y.name = y.name,
       random = random, 
       lgmodel = lgmodel,
       rslope = rslope,
       FixedEffects=part.tree$FixedEffects,
       RandomEffects=part.tree$RandomEffects,
       
       depth=depthx,
       width=nrow(Rules),
       treeplotsavepath=treeplotsavepath)
}

