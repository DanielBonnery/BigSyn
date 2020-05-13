# NEW general model fitting function

fitthemodel.new <- function(Sparameters_i,
                            fitmodelsavepath,
                            TtableANAto0,
                            redocomputationsevenifexists=FALSE,
                            treeplotsavefolder=NULL){
  print("________________________________________________________________________",quote = F)
  print(paste0("Now fitting model for ",Sparameters_i$variable),quote = F)
  redocomputations=TRUE
  if(!is.null(fitmodelsavepath)&!redocomputationsevenifexists){
    wheretosavefit<-file.path(fitmodelsavepath,paste0(Sparameters_i$variable,".rda"))
    if(file.exists(wheretosavefit)){
      redocomputations=FALSE;
      print("-- Already done",quote = F)
      return(NULL)}
  }
  if(redocomputations){
    print("......................................................................",quote = F)
    print(paste0("-- Fit model for ", Sparameters_i$rank," - ",Sparameters_i$variable),quote = F)
    if(is.null(names(Sparameters_i$splits))){
      names(Sparameters_i$splits)<-seq_along(Sparameters_i$splits)}else{
        aaa<-names(Sparameters_i$splits)==""
        if(any(aaa)){
          names(Sparameters_i$splits)[aaa]<-
            setdiff(paste0("",seq_along(Sparameters_i$splits)),names(Sparameters_i$splits))[1:sum(aaa)]}}
    splitids<-names(Sparameters_i$splits)
    Sparameters_i$splits<-plyr::llply(seq_along(Sparameters_i$splits),
                                      function(iii){
                                        Split=Sparameters_i$splits[[iii]]
                                        Split$id=splitids[iii]
                                        start_time <- Sys.time()
                                        selT<-(rep(TRUE,nrow(TtableANAto0)))&with(TtableANAto0,eval(Split$condition))
                                        print(paste0("---- ",capture.output(Split$condition)),quote = F)
                                        print(paste0("------- Number of observations: ",sum(selT)),quote = F)
                                        Fit=NULL
                                        Split$method2=Split$method
                                        print(paste0("------- Method: ",Split$method2),quote = F)
                                        if(is.element(Split$method,c("ctree","rf"))){
                                          if(sum(selT)>2){
                                            if(!is.null(treeplotsavefolder)){Split$treeplotsavepath<-file.path(treeplotsavefolder,paste0(Sparameters_i$variable,'_',Split$id,'.pdf'))}
                                            x=TtableANAto0[selT,Split$predictors,drop=FALSE]
                                            y0=TtableANAto0[selT,Sparameters_i$variable]
                                            #if(is.factor(y0)){
                                            #  originallevels<-levels(y0)
                                            #  y0<-droplevels(y0)}
                                            #fit the model here
                                            Split$fit.model<-try(fitmodel.fn.new(
                                              Split$method,
                                              x,
                                              y0,
                                              #!need to code these into arguments                                            
                                              y.name = "bscore",
                                              random = "schoolid", 
                                              lgmodel = "slope",
                                              rslope = "+ female + sclass", nbuckets=30, tutu="not a good argument",
                                              treeplotsavepath=Split$treeplotsavepath,
                                              Split$synthparameters));
                                            if (class(Split$fit.model)=="try-error"){
                                              print("------ Error -  method changed to sample",quote = F)
                                              Split$method2="sample"}}else{
                                                print("------ warning -  cell<3 - method changed to sample",quote = F)
                                                Split$method2="sample"
                                              }
                                        }else{Split$fit.model=NULL}
                                        end_time <- Sys.time()
                                        Split$modelbuildingtime=end_time-start_time
                                        Split
                                      })}
  if(!is.null(fitmodelsavepath)&redocomputations){
    save(Sparameters_i,file=wheretosavefit);return(NULL)}else{return(Sparameters_i)}
}



