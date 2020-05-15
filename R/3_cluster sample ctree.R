#' sample functio no modifications are made here
#' @author Yi Feng
#' @examples
#' data(school,package="BigSyn")
#' 
#' L<-list(x = school[, 1:9], 
#'   y = school$bscore, 
#'   y.name = "bscore",
#'   random = "schoolid", 
#'   lgmodel = "slope",
#'   rslope = "+ female + sclass",
#'   id = school$schoolid)
#' attach(L)
#' fit.model<-do.call(fitmodel.ctree.new,L)
#' save(fit.model,file="fit.model.rda")
#' load("fit.model.rda")
#' sample.ctree.new(xp = school[1:9], fit.model)
#' sample.ctree.new(xp = school[1:10, 1:9], fit.model)

sample.ctree.new <- function(xp,fit.model,smoothing="none",...){
  id<-xp[fit.model$random]
  keep<-names(xp)[sapply(names(xp),function(x){
    any(grepl(pattern = x, x = fit.model$Rules$condition))})]
  xp <- preparepredictorsforctreefit(xp,keep=keep)
  # for (i in which(sapply(x, class) != sapply(xp, class))){ xp[,i] <- eval(parse(text = paste0("as.", class(x[, i])[1], "(xp[,i])", sep = "")))}
  newterminalnodes <- getnodesfromrules(xp,fit.model$Rules)
  # ysyn <-   samplefrompool(fit.model$y,fit.model$terminalnodes,newterminalnodes)
  
  
  nodeInd <- as.factor(newterminalnodes)
  newdata <- cbind(nodeInd=nodeInd,id,xp)
  colnames(newdata)[2] <- fit.model$random
  
  #--------- method 1: prediction using the lme4 model object  ----------#
  
  # random effects model
  reModel=fit.model$EffectModel
  predictFinal <- round(predict(reModel,newdata = newdata, type="response"))
  
  #--------- method 2: manual compute the predicted value using parameter estimates -------#
  fixed.par <- fit.model$FixedEffects
  random.par <- fit.model$RandomEffects
  
  newdata2 <- cbind(rep(1,nrow(newdata)),newdata$nodeInd, newdata[,names(fixed.par)[-grep("nodeInd|Intercept",names(fixed.par))]]) 
  colnames(newdata2) <- c("(Intercept)", "nodeInd", names(fixed.par)[-grep("nodeInd|Intercept",names(fixed.par))])
  newdata3 <- fastDummies::dummy_cols(newdata2,"nodeInd", remove_first_dummy = T)  # with all the predictors
  
  # parameters
  randomEffects <- cbind(rownames(random.par[[1]]),random.par[[1]])
  colnames(randomEffects) <- c(fit.model$random,names(random.par[[1]]))
  
  rd <- grep(paste(names(random.par[[1]]),collapse = "|"),names(fixed.par))
  node.d <- grep("node",names(fixed.par))
  fd <- names(fixed.par)[-c(rd,node.d)]
  
  randomEffects.long <- randomEffects[rep(1:nrow(randomEffects), times = table(id)),]
  fixedEffects.long <- as.data.frame(matrix(fixed.par,nrow=nrow(newdata),ncol=length(fixed.par),byrow=TRUE))
  colnames(fixedEffects.long) <- names(fixed.par)
  
  
  # initiate prediction
  predictFinal2 <- rep(NA, nrow(newdata))
  
  for (n in 1:nrow(newdata)){
    
    # random part   
    predict.random <- 0 
    for (i in 1:length(random.par[[1]])) {
      
      random.name <- names(random.par[[1]])[i]
      predict.random <- predict.random +
        newdata3[n,random.name]*(fixedEffects.long[n,random.name]+randomEffects.long[n,random.name])
    }
    
    # node part
    predict.node <- sum(newdata3[n, grep("nodeInd_",colnames(newdata3))]*fixedEffects.long[n,grep("nodeInd",colnames(fixedEffects.long))])
    
    # other fixed part
    predict.fixed <- 0 
    
    if (length(fd)>0){
      for (j in 1:length(fd)) {
        
        fixed.name <- names(fixed.par)[-c(rd,node.d)][j]
        predict.fixed <- predict.fixed +
          newdata3[n,fixed.name]*(fixedEffects.long[n,fixed.name])
      }
      
    }
    
    predictFinal2[n] <- predict.random + predict.node + predict.fixed
    
  }
  
  # probability
  predictFinal2.p <- exp(predictFinal2)/(1+exp(predictFinal2))
  # outcome
  sampleFinal3 <- as.numeric((predictFinal2.p > runif(nrow(newdata),0,1)))
  
  
  
  #  if (!is.factor(fit.model$y) & smoothing == "density"){ysyn <- synthpop:::syn.smooth(ysyn, fit.model$y)}
  return(predictFinal)
  return(sampleFinal3)  
}

