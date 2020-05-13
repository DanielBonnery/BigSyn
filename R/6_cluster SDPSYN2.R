# NEW SDPSYN function
# This is me trying but I don't think it is working properly

SDPSYN2.new<-function(TtableA,
                      asis=NULL,
                      notpredictor=asis,
                      nrep=1,
                      synparameters=NULL,
                      Sparameters=Sparameters.default.f(
                        ref.table=TtableA,
                        asis=asis,
                        notpredictor=notpredictor,
                        preferredmethod="ctree",
                        defaultsynparameters=c(as.list(synparameters),
                                               eval(formals(Sparameters.default.f)$defaultsynparameters)[setdiff(names(formals(Sparameters.default.f)$defaultsynparameters),c("",names(synparameters)))])),
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
  #  stilltofit<-setdiff(names(Sparameters),if(is.null(fitmodelsavepath)){character(0)}else{gsub(".rda","",list.files(fitmodelsavepath))})
  
  stilltofit <- setdiff(names(Sparameters),c("caseid","schoolid","score")) ###
  
  print(paste0(Sys.time()," -- ",length(stilltofit)," models remaining, but my friends help me"), quote = F)
  if(randomfitorder&length(stilltofit)>0){stilltofit<-stilltofit[order(runif(length(stilltofit)))]}
  
  Sparameterswithfit<-plyr::llply(stilltofit,
                                  function(variable){
                                    if(!is.null(fitmodelsavepath)){
                                      if(!is.element(variable,gsub(".rda","",list.files(fitmodelsavepath)))){
                                        #We retest to allow parallel computing
                                        # fit the model here.                                        
                                        fitthemodel.new(Sparameters_i=Sparameters[[variable]],
                                                        fitmodelsavepath=fitmodelsavepath,
                                                        TtableANAto0=TtableANAto0,
                                                        treeplotsavefolder = treeplotsavefolder)}}else{
                                                          #fit the model here.
                                                          fitthemodel.new(Sparameters_i=Sparameters[[variable]],
                                                                          fitmodelsavepath=fitmodelsavepath,
                                                                          TtableANAto0=TtableANAto0,
                                                                          treeplotsavefolder = treeplotsavefolder)
                                                        }
                                  },.progress = "text")
  names(Sparameterswithfit)<-stilltofit
  #  if(!is.null(fitmodelsavepath)){save(Sparameters0,file.path(fitmodelsavepath,"Sparameters0.rda"))}
  
  print("###########################################################################",quote = F)
  print(paste0(Sys.time()," I am sampling from  the models"),quote = F)
  if(!is.null(stepbystepsavepath)){
    if(file.exists(stepbystepsavepath)){load(stepbystepsavepath)
      print("Done loading",quote = F)}
  }
  #  tosynthesize=setdiff(names(Sparameters),names(STtableA))
  tosynthesize=setdiff(names(Sparameters),c("caseid","schoolid","score"))
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
        # fit the model
        fitthemodel.new(Sparameters[[variable]],fitmodelsavepath,TtableANAto0)
        load(wheretosavefit)}}else{
          load(wheretosavefit)  
          #Sparameters_i<-Sparameterswithfit[[variable]]
        }
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
        xx<-try(y<-do.call(sample.ctree,
                           c(list(xp=xp,fit.model=Split$fit.model), 
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
    #    return(split(STtableA, STtableA$".n"))
    return(STtableA)
  }
}


##### example 

TtableA=mydata
asis=NULL;notpredictor=asis;nrep=1;synparameters=NULL;
Sparameters=
  Sparameters.default.f(ref.table=mydata,
                        asis=asis,
                        notpredictor=notpredictor,
                        preferredmethod="ctree",
                        defaultsynparameters=
                          c(as.list(synparameters),
                            eval(formals(Sparameters.default.f)$defaultsynparameters)[
                              setdiff(names(formals(Sparameters.default.f)$defaultsynparameters),
                                      c("",names(synparameters)))]));

STtableA=plyr::rdply(nrep,ATtableA[c("caseid","schdenom")]);

samplereportsavepath=NULL;
stepbystepsavepath=NULL;
doparallel=FALSE;
recode=NULL;
randomfitorder=TRUE;
fitonly=FALSE;
fitmodelsavepath=tempdir()
treeplotsavefolder=tempdir()
#sapply(list.files(tempdir(),full.names = TRUE  ),file.remove)

SATtableA <- SDPSYN2.new(TtableA = TtableA, asis = NULL,
                         fitmodelsavepath = fitmodelsavepath,
                         treeplotsavefolder = treeplotsavefolder,
                         STtableA = TtableA)





View(SATtableA)
summary(SATtableA)
summary(mydata)

install.packages("ICCbin")
library(ICCbin)
iccbin(cid = schoolid, y = bscore, data = mydata)

m <- glmer(bscore ~ (1 | schoolid), data = mydata, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
summary(m)

m2 <- glmer(bscore ~ (1 | schoolid), data = SATtableA, family = binomial, control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 10)
summary(m2)

SATtableB <- SDPSYN2(TtableA = TtableA, asis = NULL,
                     #                         fitmodelsavepath = fitmodelsavepath,
                     treeplotsavefolder = treeplotsavefolder
)


