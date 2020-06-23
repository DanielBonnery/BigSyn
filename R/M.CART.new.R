#' adapted function to fit a tree model predicting a binary outcome at individual level taking into account the clustered data structure
#' @author Yi Feng
#' @examples
#' data(school,package="BigSyn")
#' try_model_new <- 
#'   M.CART.new(formula = bscore ~ female + sclass + schtype + schurban + schdenom,
#'    data = school, 
#'    fixed = "+ female + sclass + schtype",
#'    random = "schoolid", lgmodel = "slope", rslope = "+ female + sclass")
#' plot(try_model_new$Tree)
#' try_model_new$deviance
#' try_model_new$Treewhere
#' try_model_new$predMtree
#' pred <- matrix(NA, nrow(school), 1)
#' pred[try_model_new$predMtree >= 0.5] <- 1
#' pred[try_model_new$predMtree < 0.5] <- 0
#' 
#' confMat <- table(school$bscore, pred)
#' (accuracy <- sum(diag(confMat))/sum(confMat))
#' 
#' 
#' try_model_new2 <- 
#'   M.CART.new(formula = bscore ~ female + sclass + schtype + schurban,
#'    data = school, 
#'    fixed = "+ female + sclass",
#'    random = "schoolid", lgmodel = "int", rslope = NULL)
#' plot(try_model_new2$Tree)
#' 
#' school$bscore<-factor(school$bscore)
#' try_model_new3 <- 
#'   M.CART.new(formula = bscore ~ female + sclass + schtype + schurban,
#'    data = school, 
#'    fixed = "+ female + sclass",
#'    random = "schoolid", lgmodel = "int", rslope = NULL)

M.CART.new <- function(formula, 
                       data, 
                       random = "SID",
                       fixed = "+X1+X2",
                       rslope = "+X1+X2", 
                       lgmodel = "int", 
                       ErrorTolerance = 0.00001, 
                       MaxIterations = 10000, 
                       verbose = FALSE, 
                       minbucket = 50,
                       mincriterion = 0.95){
  
  TotalObs <- dim(data)[1]                      
  originaldata <- data

  print("we are at 1")  
  print(paste0("           formula is:",formula))
  print(paste0("           random is :",random))
  print(paste0("           fixed is :",fixed))
  print(paste0("           rslope is :",rslope))
  print(paste0("           ErrorTolerance is :",ErrorTolerance))
  print(paste0("           MaxIterations is :",MaxIterations))
  print(paste0("           verbose is :",verbose))
  print(paste0("           minbucket is :",minbucket))
  print(paste0("           mincriterion is :",mincriterion))
  Predictors <- paste(attr(terms(formula), "term.labels"), collapse = "+")
  TargetName <- formula[[2]]
  if (length(TargetName) > 1) TargetName <- TargetName[3]
  if (verbose) print(paste("Target variable: ", TargetName))
  
  Target <- data[, toString(TargetName)]
  newdata <- data
  
  originaldata$random<-rep(0,TotalObs)
  originaldata$TID <- originaldata[, random]    
  
  for (i in unique(originaldata$TID)) {
    originaldata$random[originaldata$TID == i] <-
      mean(originaldata[originaldata$TID == i,toString(TargetName)])-
      mean(originaldata[,toString(TargetName)])
  }    #cluster mean - grand mean (u)
  print("we are at 2")  
  
  AdjustedTarget <-data[, toString(TargetName)]-originaldata$random 
  
  ContinueCondition <- TRUE
  iterations <- 0
  oldlik <- 0
  print("we are at 3")  
  
  
  while (ContinueCondition) {
    iterations <- iterations + 1
    print(paste0("we are at 4.0 :",iterations))  

    newdata[, "AdjustedTarget"] <- as.factor(AdjustedTarget)

# predict the adjusted target (individual-level variability free of cluster random effects)    
tree <- partykit::ctree(formula = formula(paste(c("AdjustedTarget", Predictors), collapse = "~")), 
                        data = as.data.frame(newdata), 
                        control = partykit::ctree_control(minbucket = minbucket, mincriterion = mincriterion))
if (verbose) print(tree)
print(paste0("we are at 4.1 :",iterations))  

where <- predict(tree, type = "node")    #which terminal node 
newdata[, "nodeInd"] <- where            #save as variable

# predict the outcome using multilevel logistic regression with terminal nodes as a predictor
if (min(where) == max(where)) {   
  glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName),
                                           paste("1", fixed, " + (1|",random,")",
                                                 sep = "")),collapse = "~")), 
                           data = newdata, family = binomial, nAGQ = 0, na.action = na.exclude)
} else {
  if (lgmodel == "int") {   #random intercept model
    glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName),
                                             paste("as.factor(nodeInd)", fixed, "+(1|",random,")",
                                                   sep="")),collapse = "~")), 
                             data = newdata,family = binomial,nAGQ = 0, na.action = na.exclude)
  } else {  #random slope model
    glmerfit2 <- lme4::glmer(formula(paste(c(toString(TargetName), 
                                             paste("as.factor(nodeInd)",fixed,paste("(1",rslope,"|",random,")",sep=""),sep="+")),collapse="~")), 
                             data=newdata, family=binomial,nAGQ =0, na.action = na.exclude)
  }
}

print(paste0("we are at 4.2 :",iterations))  

newlik<-logLik(glmerfit2)   #loglikelihood
ContinueCondition <- (abs(newlik - oldlik) >
                        ErrorTolerance & iterations<MaxIterations)
print(paste0("we are at 5 :",iterations," absdiff: ",abs(newlik - oldlik)))  
oldlik <- newlik 

# predicted outcome values using the tree nodes as predictors
    AdjustedTarget2 <- (as.matrix(lme4::getME(glmerfit2,name = "X"))) %*% (as.matrix(lme4::getME(glmerfit2,name="beta")))
    AdjustedTarget<-AdjustedTarget2
    AdjustedTarget[is.na(AdjustedTarget[,1]),1]<-(data[,toString(TargetName)]-
                                                    originaldata$random)[is.na(AdjustedTarget[,1])]
  }  #end of iteration if converged
  print(paste0("we are at 6"))  
  
  if (lgmodel!="int") {
    Between<-cbind(lme4::VarCorr(glmerfit2)[[1]][1:2],
                   lme4::VarCorr(glmerfit2)[[1]][3:4])
  } else {
    Between<- lme4::VarCorr(glmerfit2)[[1]][1]
  }
  
  # final prediction
  # preditFinal <- predict(glmerfit2,type="response")
  
  result <- list(data=data,
                 IterationsUsed=iterations,
                 Random=random, 
                 Rslope=rslope,
                 ErrorTolerance=ErrorTolerance, 
  #               predMtree=preditFinal,
                 Tree=tree, 
                 Treewhere=where, 
                 EffectModel=glmerfit2, 
  #               BetweenMatrix=as.matrix(Between)*sigma(glmerfit2)^2,
                 FixedEffects=lme4::fixef(glmerfit2),
                 RandomEffects= lme4::ranef(glmerfit2), 
  
                 logLik=newlik,
                 AIC=AIC(glmerfit2), BIC=BIC(glmerfit2), deviance=
                   deviance(glmerfit2),
                 df=as.numeric(summary(glmerfit2)$AICtab[5]),
                 Formula=formula,Totalinter=iterations)
  class(result) <- "M.CART"
  return(result)
}
