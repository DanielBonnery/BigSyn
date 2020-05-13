


#' adapted function to fit a tree model predicting a binary outcome at individual level taking into account the clustered data structure


#' @examples
#' data(school,package="BigSyn")
#' try_model_new <- 
#'   M.CART.new(formula = bscore ~ female + sclass + schtype + schurban + schdenom,
#'    data = school, 
#'    random = "schoolid", lgmodel = "slope", rslope = "+ female + sclass")
#' plot(try_model_new$Tree)
#' try_model_new$deviance
#' try_model_new$Treewhere
#' try_model_new$predMtree
#' pred <- matrix(NA, nrow(school), 1)
#' pred[try_model_new$predMtree >= 0.5] <- 1
#' pred[try_model_new$predMtree < 0.5] <- 0

#' confMat <- table(school$bscore, pred)
#' (accuracy <- sum(diag(confMat))/sum(confMat))



M.CART.new <- function(formula, 
                       data, 
                       random = "SID",
                       rslope = "+X1+X2", 
                       lgmodel = "int", 
                       ErrorTolerance = 0.00001, 
                       MaxIterations = 10000, 
                       verbose = FALSE, 
                       minbucket = 50,
                       mincriterion = 0.95){
  
  TotalObs <- dim(data)[1]                      
  originaldata <- data
  
  Predictors <- paste(attr(terms(formula), "term.labels"), collapse = "+")
  TargetName <- formula[[2]]
  if (length(TargetName) > 1) TargetName <- TargetName[3]
  if (verbose) print(paste("Target variable: ", TargetName))
  
  Target <- data[, toString(TargetName)]
  newdata <- data
  
  originaldata$random<-rep(0,TotalObs)
  originaldata$TID <- originaldata[, random]    
  
  for (i in 1:length(summary(originaldata$TID))) {
    originaldata$random[originaldata$TID == i] <-
      mean(originaldata[originaldata$TID == i,toString(TargetName)])-
      mean(originaldata[,toString(TargetName)])
  }    #cluster mean - grand mean (u)
  
  AdjustedTarget <-data[, toString(TargetName)]-originaldata$random 
  
  ContinueCondition <- TRUE
  iterations <- 0
  oldlik <- 0
  
  while (ContinueCondition) {
    iterations <- iterations + 1
    newdata[, "AdjustedTarget"] <- as.factor(AdjustedTarget)

# predict the adjusted target (individual-level variability free of cluster random effects)    
tree <- partykit::ctree(formula = formula(paste(c("AdjustedTarget", Predictors), collapse = "~")), data = as.data.frame(newdata), control = partykit::ctree_control(minbucket = minbucket, mincriterion = mincriterion))
if (verbose) print(tree)

where <- predict(tree, type = "node")    #which terminal node 
newdata[, "nodeInd"] <- where            #save as variable

# predict the outcome using multilevel logistic regression with terminal nodes as a predictor
if (min(where) == max(where)) {   
      glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName),
                                        paste("1 + (1|",random,")",
                                              sep = "")),collapse = "~")), 
                        data = newdata, family = binomial, nAGQ = 0, na.action = na.exclude)
    } else {
      if (lgmodel == "int") {   #random intercept model
        glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName),
                                           paste("as.factor(nodeInd)+(1|",random,")",
                                                 sep="")),collapse = "~")), 
                           data = newdata,family = binomial,nAGQ = 0, na.action = na.exclude)
      } else {  #random slope model
        glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName), 
                                           paste("as.factor(nodeInd)",paste("(1",rslope,"|",random,")",sep=""),sep="+")),collapse="~")), 
                           data=newdata, family=binomial,nAGQ =0, na.action = na.exclude)
      }
    }


    newlik<-logLik(glmerfit2)   #loglikelihood
    ContinueCondition <- (abs(newlik - oldlik) >
                            ErrorTolerance & iterations<MaxIterations)
    oldlik <- newlik 
    
# predicted outcome values
    AdjustedTarget2 <- (as.matrix(lme4::getME(glmerfit2,name = "X"))) %*% (as.matrix(lme4::getME(glmerfit2,name="beta")))
    AdjustedTarget<-AdjustedTarget2
    AdjustedTarget[is.na(AdjustedTarget[,1]),1]<-(data[,toString(TargetName)]-
                                                    originaldata$random)[is.na(AdjustedTarget[,1])]
  }  #end of iteration if converged
  
  if (lgmodel!="int") {
    Between<-cbind(lme4::VarCorr(glmerfit2)[[1]][1:2],
                   lme4::VarCorr(glmerfit2)[[1]][3:4])
  } else {
    Between<- lme4::VarCorr(glmerfit2)[[1]][1]
  }
  
  # final prediction
  preditFinal <- predict(glmerfit2,type="response")
  
  result <- list(data=data,
                 IterationsUsed=iterations,
                 Random=random, 
                 Rslope=rslope,
                 ErrorTolerance=ErrorTolerance, 
                 predMtree=preditFinal,
                 Tree=tree, 
                 Treewhere=where, 
                 EffectModel=glmerfit2, 
                 RandomEffects= lme4:: ranef(glmerfit2),
                 BetweenMatrix=as.matrix(Between)*sigma(glmerfit2)^2,
                 logLik=newlik,
                 AIC=AIC(glmerfit2), BIC=BIC(glmerfit2), deviance=
                   deviance(glmerfit2),
                 df=as.numeric(summary(glmerfit2)$AICtab[5]),
                 Formula=formula,Totalinter=iterations)
  class(result) <- "M.CART"
  return(result)
}
