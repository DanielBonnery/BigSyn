#' Generic function: remove all the elements of a named list which names are not arguments of a specific function.
#' @details 
#' remove all the elements of a list which names are not arguments of a specific function.
#' @param fun a function
#' @param L a list
#' @return a list.  
#' @examples
#' onlygoodargs(lm,list(data=cars,formula=speed~dist,tutu="not an arg from lm"))
onlygoodargs<-function(fun,L){L[intersect(names(L),names(formals(fun)))]}

#' Select only arguments for a specific fitting function
#' @details 
#' Currently only works with method="ctree"
#' Only selects the arguments that match the function partykit::ctree_control
#' @param method a string. currently only method="ctree".
#' @param synparameters a named list.  
#' @return a sublist of synparameters, which names are possible arguments of partykit::ctree_control if method="ctree".
#' @examples
#' good.fit.parameters(method="ctree",list(teststat=30,tutu=3))
good.fit.parameters<-function(method,synparameters){
  if(method=="ctree"){synparameters[intersect(names(synparameters),
                                              names(formals(partykit::ctree_control)))]}}

#' Select only arguments for sampling.
#' @details 
#' In prevision of future developments. returns NULL for the moment
#' @param method a string. currently only method="ctree".
#' @param synparameters a named list.  
#' @return NULL
#' @examples
#' good.sample.parameters(method="ctree",list(teststat=30,tutu=3))
good.sample.parameters<-function(method,synparameters){NULL}
#' Select only arguments for a specific fitting function
#' This function is not used anymore
#' @details 
#' Currently only works with method="ctree"
#' Only selects the arguments that match the function synthpop::syn.ctree
#' @param method a string. currently only method="ctree".
#' @param synparameters a named list.  
#' @return a sublist of synparameters, which names are possible arguments of synthpop::syn.ctree if method="ctree".
#' @examples
#' good.syn.parameters(method="ctree",list(y=c(1:2),smoothing=TRUE,tutu="not in synthpop::syn.ctree arguments"))
good.syn.parameters<-function(method,synparameters){
  if(method=="ctree"){
    synparameters[intersect(names(synparameters),
                            names(formals(synthpop::syn.ctree)))]}}

#' Fit a model with a specific function
#' @param method a string. currently only method="ctree" or "rf" (random forest).
#' @param x a predictors, a dataframe.
#' @param y variable to predict, a vector
#' @param   treeplotsavepath a 
#' @param ... synthetic parameters to pass to the right fit model function. the fit model function name is the concatenation of "fit.model" and method
#' @return a sublist of synparameters, which names are possible arguments of synthpop::syn.ctree if method="ctree".
#' @examples
#' fitmodel.fn(method="ctree",x=iris[,-5],y=iris$Species,nbuckets=30,tutu="not a good argument")
fitmodel.fn<-function(method,x,y,treeplotsavepath=NULL,...){
  do.call((get(paste0("fitmodel.",method))),
          c(list(x=x,y=y,treeplotsavepath=treeplotsavepath),
            good.fit.parameters(method,list(...))))}

#' Sample a model with a specific function
#' @param method a string. currently only method="ctree".
#' @param x a predictors, a dataframe.
#' @param y variable to predict, a vector
#' @param ... synthetic parameters to pass to the right fit model function. the fit model function name is the concatenation of "fit.model" and method
#' @return a sublist of synparameters, which names are possible arguments of synthpop::syn.ctree if method="ctree".
#' @examples
#' sample.fn(method="ctree",
#'           xp=iris[,-5],
#'           fit.model=fitmodel.fn(method="ctree",x=iris[,-5],y=iris$Species,nbuckets=30),
#'           smoothing=FALSE)
sample.fn<-function(method,xp,fit.model,smoothing,...){
  do.call(get(paste0("sample.",method)),
          c(list(xp=xp,fit.model=fit.model,smoothing=smoothing),
            good.sample.parameters(method,list(...))))}

#' Prepare predictors for ctree fit
#' @param method a string. currently only method="ctree".
#' @param x a predictors, a dataframe.
#' @param y variable to predict, a vector
#' @param ... synthetic parameters to pass to the right fit model function. the fit model function name is the concatenation of "fit.model" and method
#' @return a sublist of synparameters, which names are possible arguments of synthpop::syn.ctree if method="ctree".
#' @examples
#' preparepredictorsforctreefit(x,
#'                              keep=NULL)
preparepredictorsforctreefit<-function(x,keep=NULL){
  datesx<-sapply(x,function(xx){inherits(xx,"POSIXct")|inherits(xx,"POSIXt")})
  if(any(datesx)){x[datesx]<-lapply(x[datesx],as.numeric)}
  factorswithtoomanylevels<-sapply(x,is.factor)
  if(any(factorswithtoomanylevels)){
    factorswithtoomanylevels[sapply(x[factorswithtoomanylevels],is.ordered)]<-FALSE
    factorswithtoomanylevels[sapply(x[factorswithtoomanylevels],nlevels)<30]<-FALSE
    x<-x[!factorswithtoomanylevels|is.element(names(x),keep)]}
  x[sapply(x,function(y){length(unique(y))})>1|is.element(names(x),keep)]}
#' Function to fit a ctree model.
#' @param x a dataframe of predictors 
#' @param y a vector :dependent variable
#' @param treeplotsavepath: a path to save the graph as a pdf. if NULL, no pdf is saved
#' @return a named list of 4 elements:
#' "Rules" a data.frame with two variables: terminalnode (a integer vector) and condition  a string that gives the rule for each terminal node.
#'  "y" the values of the predictor
#'  "terminalnodes" a vector: the terminal nodes for each element of $y$.
#'  "shortlist" a character string giving the names of the variables in x that were used for the classification   
#' @examples
#' fitmodel.ctree(x=iris[,-5],y=iris$Species)
fitmodel.ctree<-function(x,y,treeplotsavepath=NULL,...){
  #1 convert all posix to numeric
  y2<-y
  if(inherits(y2,"POSIXct")|inherits(y2,"POSIXt")){y2<-as.numeric(y2)}
  x2<-preparepredictorsforctreefit(x)
  datact <- partykit::ctree(y2 ~ ., 
                            data=cbind(y2=y2, if(ncol(x2)==0){x}else{x2}), 
                            control = do.call(partykit::ctree_control,
                                              onlygoodargs(partykit::ctree_control,list(...))))
  if(!is.null(treeplotsavepath)){try(BigSyn::treetopdf(datact,treeplotsavepath))}
  splitnodeconditions <- daniRules(datact)
  splitnodeconditionsmerge<-paste0(splitnodeconditions,collapse="")
  Rules=  if(length(splitnodeconditions)>1){
    data.frame(terminalnode=strtoi(names(splitnodeconditions)),
               condition=splitnodeconditions,stringsAsFactors = FALSE)}else{
                 data.frame(terminalnode=1,condition="TRUE")}
  terminalnodes<-getnodesfromrules(x,Rules)
  shortlist<-if(ncol(x2)==0){character(0)}else{names(x2)[sapply(names(x2),grepl,x=splitnodeconditionsmerge)]}  
  list(Rules=Rules,
       y=y,
       terminalnodes=terminalnodes,
       shortlist=shortlist)}

#' Function to get terminal node from a set of partitioning rules and new predictors
#' 
#' @param x a dataframe of predictors
#' @param Rules a data frame containing 2 character variables: "terminalnode" and "condition"
#' @return a vector of lenth the number of rows of x indicating the terminal nodes. 
#' @example
#' getnodesfromrules(x=iris[1:3,-5],Rules=fitmodel.ctree(x=iris[,-5],y=iris$Species)$Rules)
getnodesfromrules<-function(x,Rules){
  terminalnode=rep(NA,nrow(x))
  for(j in 1:nrow(Rules)){
    whh<-eval(parse(text=paste0("with(x,(rep(TRUE,nrow(x)))&(",Rules$condition[j],"))")))
    if(any(whh)){
    terminalnode[whh]<-Rules$terminalnode[j]}}
  terminalnode}

#' Function to sample from a set of partitioning rules
#' 
#' @param y a vector of values to pull from
#' @param terminalnodes a vector of terminal nodes
#' @param newterminalnodes: a path to save the graph
#' @return  a vector of the same size than terminalnodes, obtained by sampling betweenn the values of y such for the same terminal node.  
#' @example
#' y=iris$Species;x=iris[,-5];fit.mod<-fitmodel.ctree(x,y);terminalnodes<-getnodesfromrules(x,fit.mod$Rules);
#' newterminalnodes<-sample(unique(terminalnodes),10,replace=TRUE);
#' samplefrompool(y,terminalnodes,newterminalnodes)
#' y<-y[terminalnodes!=7]
#' terminalnodes<-terminalnodes[terminalnodes!=7]
#' samplefrompool(y,terminalnodes,newterminalnodes)
samplefrompool<-function(y,terminalnodes,newterminalnodes){
  newy=sample(y,size=length(newterminalnodes),replace=(length(y)<length(newterminalnodes)))
  newy[TRUE]<-NA
  replaced=rep(FALSE,length(newterminalnodes))
  for (i in intersect(unique(newterminalnodes),unique(terminalnodes))){
    whh<-(newterminalnodes == i&!is.na(newterminalnodes == i))
    if(any(whh&!is.na(whh))){
      newy[whh] <- sample(y[terminalnodes ==i], sum(whh), replace = TRUE)
      replaced[whh]<-TRUE}}
  for(i in setdiff(unique(newterminalnodes),unique(terminalnodes))){
    whh<-(newterminalnodes == i&!is.na(newterminalnodes == i))
    distance<-abs(i-unique(terminalnodes))
    siblingnode<-unique(terminalnodes)[which(distance==min(distance))]
    if(any(sapply(unique(terminalnodes),function(jj){is.element(jj,siblingnode)}))){
    if(any(whh&!is.na(whh))){
      newy[whh] <- sample(y[sapply(terminalnodes,function(jj){is.element(jj,siblingnode)})],
                                                   sum(whh), replace = TRUE)
      replaced[whh]<-TRUE}}}
  if(any(!replaced)){newy[!replaced]<-sample(y, sum(!replaced), replace = TRUE)}
  newy}

#' Function to sample from a ctree fitted model
#' 
#' @param y a vector of values to pull from
#' @param terminalnodes a vector of terminal nodes
#' @param newterminalnodes: a path to save the graph
#' @return  a vector of the same size than terminalnodes, obtained by sampling betweenn the values of y such for the same terminal node.  
#' @example
#' y=iris$Species;x<-xp<-iris[,-5];fit.model<-fitmodel.ctree(x,y);sample.ctree(x,fit.model)
sample.ctree<-function(xp,fit.model,smoothing="none",...){
  keep<-names(xp)[sapply(names(xp),function(x){
    any(grepl(pattern = x,x =fit.model$Rules$condition))})]
  xp <- preparepredictorsforctreefit(xp,keep=keep)
  # for (i in which(sapply(x, class) != sapply(xp, class))){ xp[,i] <- eval(parse(text = paste0("as.", class(x[, i])[1], "(xp[,i])", sep = "")))}
  newterminalnodes<-getnodesfromrules(xp,fit.model$Rules)
  ysyn <-   samplefrompool(fit.model$y,fit.model$terminalnodes,newterminalnodes)
  if (!is.factor(fit.model$y) & smoothing == "density"){ysyn <- synthpop:::syn.smooth(ysyn, fit.model$y)}
  ysyn}

#' Function to fit a ctree model.
#' @param x a dataframe of predictors 
#' @param y a vector :dependent variable
#' @param treeplotsavepath: a path to save the graph as a pdf. if NULL, no pdf is saved
#' @return a named list of 4 elements:
#' "Rules" a data.frame with two variables: terminalnode (a integer vector) and condition  a string that gives the rule for each terminal node.
#'  "y" the values of the predictor
#'  "terminalnodes" a vector: the terminal nodes for each element of $y$.
#'  "shortlist" a character string giving the names of the variables in x that were used for the classification   
#' @examples
#' fitmodel.ctree(x=iris[,-5],y=iris$Species)
fitmodel.rf<-function(x,y,treeplotsavepath=NULL,...){
  if (is.factor(y)) {
    obslevels <- levels(y)
    y <- droplevels(y)}
  do.call( randomForest::randomForest,onlygoodargs(randomForest::randomForest,c(list(x=y ~ ., data = cbind.data.frame(y, x)), list(...))))}

#' Function to fit the model.
#' 
#' @param Sparameters_i an element of the list output from Sparameters.default.f
#' @param fitmodelsavepath a folder path. Results will either be read from or stored in this folder. If the file exists, by default it is not replaced.
#' @param TtableANAto0 a table  containing the predictors without NAs as well as the outcome
#' @return a list.  
#' @examples
#' 
#' data(TtableA,package="BigSyn")
#' Sparameters<-Sparameters.default.f(ref.table=TtableA)
#' Sparameters_i<-Sparameters[[53]]; fitmodelsavepath=NULL; TtableANAto0<-NAto0(TtableA);redocomputationsevenifexists=FALSE
#' treeplotsavefolder=tempdir()
#' fitthemodel(Sparameters_i,fitmodelsavepath = NULL,TtableANAto0 = TtableANAto0,treeplotsavefolder=tempdir())
#' Sparameters_i<-Sparameters[["AA.present_La_La_Lrn1"]]; 
#' treeplotsavefolder=tempdir()
#' fitthemodel(Sparameters_i,NULL,TtableANAto0,treeplotsavefolder=tempdir())

fitthemodel<-function(Sparameters_i,fitmodelsavepath,TtableANAto0,redocomputationsevenifexists=FALSE,treeplotsavefolder=NULL){
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
            Split$fit.model<-try(fitmodel.fn(Split$method,x,y0,treeplotsavepath=Split$treeplotsavepath,Split$synthparameters));
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

#' General SDP function.
#' 
#'@example
#'
#'data(TtableA,package="BigSyn")
#'TtableA$AA.cont1_La_La<-rowSums(TtableA[grep("AA.cont1_La_La",names(TtableA))])
#'asis=NULL;notpredictor=asis;nrep=1;synparameters=NULL;
#'Sparameters=Sparameters.default.f(ref.table=TtableA,asis=asis,notpredictor=notpredictor,preferredmethod="ctree",
#'defaultsynparameters=c(as.list(synparameters),
#'eval(formals(Sparameters.default.f)$defaultsynparameters)[setdiff(names(formals(Sparameters.default.f)$defaultsynparameters),c("",names(synparameters)))]));
#'STtableA=plyr::rdply(nrep,TtableA[asis]);samplereportsavepath=NULL;stepbystepsavepath=NULL;doparallel=FALSE;recode=NULL;randomfitorder=TRUE;fitonly=FALSE;
#'fitmodelsavepath=tempdir()
#'treeplotsavefolder=tempdir()
#'sapply(list.files(tempdir(),full.names = TRUE  ),file.remove)
#'STtableA<-SDPSYN2(TtableA,asis=NULL,synparameters = defaultsynparameters,fitmodelsavepath = fitmodelsavepath,treeplotsavefolder=treeplotsavefolder)
#'todisplay<-grep("La_La_Lrn1",names(STtableA[[1]]),value=T);
#'STtableA[[1]][1:3,todisplay];TtableA[1:3,todisplay]

SDPSYN2<-function(TtableA,
                  asis=NULL,
                  notpredictor=asis,
                  nrep=1,
                  synparameters=NULL,
                  Sparameters=Sparameters.default.f(
                    ref.table=TtableA,
                    asis=asis,
                    notpredictor=notpredictor,
                    preferredmethod="ctree",
                    defaultsynparameters=c(as.list(synparameters),eval(formals(Sparameters.default.f)$defaultsynparameters)[setdiff(names(formals(Sparameters.default.f)$defaultsynparameters),c("",names(synparameters)))])),
                  STtableA=if(is.null(asis)){data.frame(.n=rep(nrep,each=nrow(TtableA)))}else{plyr::ddply(data.frame(.n=nrep),~.n,function(d){TtableA[asis]})},
                  fitmodelsavepath=NULL,
                  treeplotsavefolder=NULL,
                  samplereportsavepath=NULL,
                  stepbystepsavepath=NULL,
                  doparallel=TRUE,
                  recode=NULL,
                  saveeach=200,
                  randomfitorder=TRUE,
                  fitonly=FALSE){
  print(paste0(Sys.time()," I am Initialising tables and converting NA's"),quote = F)
  STtableANAto0<-NAto0(STtableA)
  TtableANAto0<-NAto0(TtableA)
  logiqueT<-names(TtableA)[sapply(TtableA,function(x){inherits(x,"logical")})]
  logiqueS<-names(STtableA)[sapply(STtableA,function(x){inherits(x,"logical")})]
  STtableANAto0[logiqueS]<-lapply(STtableANAto0[logiqueS],function(x){x*1})
  TtableANAto0[logiqueT]<-lapply(TtableANAto0[logiqueT],function(x){x*1})
  print("###########################################################################",quote = F)
  print(paste0(Sys.time()," I am Fitting  the models"),quote = F)
  stilltofit<-setdiff(names(Sparameters),if(is.null(fitmodelsavepath)){character(0)}else{gsub(".rda","",list.files(fitmodelsavepath))})
  
  print(paste0(Sys.time()," -- ",length(stilltofit)," models remaining, but my friends help me"), quote = F)
  if(randomfitorder&length(stilltofit)>0){stilltofit<-stilltofit[order(runif(length(stilltofit)))]}
  Sparameterswithfit<-plyr::llply(stilltofit,
                                  function(variable){if(!is.null(fitmodelsavepath)){
                                    if(!is.element(variable,gsub(".rda","",list.files(fitmodelsavepath)))){#We retest to allow parallel computing
                                      fitthemodel(Sparameters_i=Sparameters[[variable]],
                                                  fitmodelsavepath=fitmodelsavepath,
                                                  TtableANAto0=TtableANAto0,
                                                  treeplotsavefolder = treeplotsavefolder)}}},
                                    .progress = "text")
  #  if(!is.null(fitmodelsavepath)){save(Sparameters0,file.path(fitmodelsavepath,"Sparameters0.rda"))}
  
  print("###########################################################################",quote = F)
  print(paste0(Sys.time()," I am sampling from  the models"),quote = F)
  if(!is.null(stepbystepsavepath)){
    if(file.exists(stepbystepsavepath)){load(stepbystepsavepath)
      print("Done loading",quote = F)}
  }
  tosynthesize=setdiff(names(Sparameters),names(STtableA))
  alreadydone<-length(intersect(names(Sparameters),names(STtableA)))
  print(paste0("previously synthesised: ", alreadydone),quote = F)
  if(fitonly){tosynthesize<-NULL}
  for(variable in tosynthesize){
    print("________________________________________________________________________",quote = F)
    print(paste0("Now sampling ",variable),quote = F)
    print(paste0("-- ",which(tosynthesize==variable)+alreadydone,"/",(length(tosynthesize)+alreadydone)),quote = F)
    if(!is.null(fitmodelsavepath)){
      wheretosavefit<-file.path(fitmodelsavepath,paste0(variable,".rda"))
      x<-try(load(wheretosavefit));
      if(class(x)=="try-error"){
        print(paste0("-- could not load ",wheretosavefit,": have to refit"),quote = F)
        try(file.remove(wheretosavefit))
        fitthemodel(Sparameters[[variable]],fitmodelsavepath,TtableANAto0)
        load(wheretosavefit)}}else{Sparameters_i<-Sparameterswithfit[[variable]]}
    print(paste0("-- nrow:",nrow(STtableA)),quote = F)
    STtableA[[variable]]<-NA
    SSel<-rep(FALSE,nrow(STtableA))
    ReportonSample<-list(variable=variable,splits=list())
    for(Split in Sparameters_i$splits){
      start_time <- Sys.time()
      print("........................................................................",quote = F)
      print(paste0("---- ",capture.output(print(Split$condition))),quote = F)
      problem=NULL
      selT<-(rep(TRUE,nrow(TtableANAto0)))&with(TtableANAto0,eval(Split$condition))
      selS<-(rep(TRUE,nrow(STtableANAto0)))&with(STtableANAto0,eval(Split$condition))
      donors<-sum(selT);receptors<-sum(selS)
      print(paste0("---- donors: ",donors),quote = F)
      print(paste0("---- receptors: ",receptors),quote = F)
      print(paste0("---- method: ",Split$method),quote = F)
      if(is.null(Split$method2)){Split$method2<-Split$method}
      if(is.element(Split$method2,c("ctree"))){
        #syn.fn<-if(Split$method2=="ctree"){synthpop::syn.ctree}else{synthpop::syn.rf}
        predictors<-intersect(intersect(Split$predictors,names(STtableANAto0)),names(STtableANAto0))
        xp=STtableANAto0[selS,predictors,drop=FALSE]
        x=TtableANAto0[selT,predictors,drop=FALSE]
        #y0=TtableA[selT,variable]
        #if(ncol(x)==0){syn.fn=function(y,xp,...){list(Fit="sample",res=sample(y,size=nrow(xp),replace=TRUE))}}
        #if(logique&Split$method2!="ctree"){y0<-1*y0}
        #syn.cartbug<-is.factor(y0)
        #if(syn.cartbug){
        #  originallevels<-levels(y0)
        #  y0<-droplevels(y0)}
        xx<-try(y<-do.call(sample.ctree,c(list(xp=xp,fit.model=Split$fit.model),
                                          good.syn.parameters(Split$method,Split$synthparameters))))
        if (class(xx)=="try-error"){
          print("---- PROBLEM: All values set to missing",quote = F)
          y<-rep(NA,nrow(STtableA[selS,,drop=FALSE]));problem=xx}
        #if(logique&Split$method2!="ctree"){y<-(y==1)}
        #if(syn.cartbug){
        #  y<-factor(levels(y)[y],levels=originallevels)
        #}
      }
      y=if(Split$method2=="sample"){
        if(sum(selT)>0){
          sample(TtableANAto0[selT,variable],size = nrow(STtableA[selS,,drop=FALSE]),replace=TRUE)}else{rep(TtableANAto0[,variable],nrow(STtableA[selS,,drop=FALSE]))}
      }else{if(is.element(Split$method2,c("rf","ctree"))){y
      }else{if(Split$method2=="calculus"){with(STtableANAto0[selS,,drop=FALSE],eval(Split$calculus))}}}
      end_time <- Sys.time()
      # list(sel=(1:nrow(STtableA))[selS],
      #Split=c(Split,list(problem=problem,
      #                  numbersynthesised=nrow(STtableA[selS,]),
      #                  numberused=nrow(TtableA[selT,]),
      #                  synthetizationtime=end_time-start_time)),
      #       y=y)})
      STtableA[[variable]][selS]<-y
      SSel[selS]<-TRUE
      
      ReportonSample$splits<-c(ReportonSample$splits,list(list(donors=donors,receptors=receptors,problem=problem,samplingtime=end_time-start_time)))}
    
    fakesel<-!SSel#setdiff(1:nrow(STtableA),do.call(c, lapply(Syn, function(l) {l$sel})))
    if(any(fakesel)){
      print("......................................................................",quote = F)
      print("---- Fakes",quote = F)
      donors<-0;receptors<-sum(fakesel)
      print("---- donors: 0",quote = F)
      print(paste0("---- receptors:",receptors),quote = F)
      print("---- method: sample",quote = F)
      start_time <- Sys.time()
      STtableA[[variable]][fakesel]<-sample(TtableANAto0[, variable], size = sum(fakesel), replace = TRUE)
      end_time <- Sys.time()
      ReportonSample$splits<-c(ReportonSample$splits,list(list(donors=donors,receptors=receptors,problem="fake",samplingtime=end_time-start_time)))
    }
    
    if(is.factor(TtableA[[variable]])){
      STtableA[[variable]]<-
        factor(levels(TtableA[[variable]])[STtableA[[variable]]],levels=levels(TtableA[[variable]]))}
    STtableANAto0<-cbind(STtableANAto0,NAto0(STtableA[variable]))
    if(!is.null(samplereportsavepath)){save(ReportonSample,file=file.path(samplereportsavepath,paste0(variable,".rda")))}
    if(!is.null(stepbystepsavepath)&which(tosynthesize==variable)%%saveeach==length(tosynthesize)%%saveeach){save(STtableA,STtableANAto0,file=stepbystepsavepath)}
  }
  if(!fitonly){
    print("###########################################################################",quote = F)
    print("#  Now I am reconverting logical                                          #",quote = F)
    STtableA[unique(c(logiqueS,logiqueT))]<-lapply(STtableA[unique(c(logiqueS,logiqueT))], function(x){x==1})
    print("############################################",quote = F)
    print("#  I am changin id                         #",quote = F)
    print("############################################",quote = F)
    if(!is.null(recode)){recode<-intersect(recode,names(STtableA))
    STtableA[recode]<-lapply(STtableA[recode],function(x){as.integer(as.factor(x))})
    }
    return(split(STtableA, STtableA$".n"))}
  }
  