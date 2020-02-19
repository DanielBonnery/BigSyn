#' Reverse augmentT_f: Function that will convert cell and marginal ratios and overall total to cell values 
#'
#' @param .data data frame to "reduce"
#' @param verbose (default FALSE) if verbose, the formulae to compute the new variables is printed.
#' @param hack (default TRUE)
#' @param variables list of variable names roots
#' @details this functions looks for the Augmentation parameters in the package object Augmentparameters[[tablename]]$percent
#' For each variable listed in Augmentparameters[[tablename]]$percent, it looks for the corresponding variable in
#' .data and computes cell values from cell and marginal ratios and overall total
#' @example
#' library(BigSyn)
#' library(reshape2)
#' library(data.table)
#'  hack=TRUE
#'  verbose=TRUE
#' data(TtableA,package="BigSyn")
#' variablemax="AA.present"
#' variablesmax=variablemax
#' variablepct="AA.cont1"
#' variablespct=variablepct
#' ATtableA<-augmentT_f(TtableA,variablesmax=variablesmax,variablespct=variablespct)
#' .data=ATtableA
#' RATtableA<-reduceT_f(ATtableA,variables)
#' all(sapply(1:nrow(TtableA),function(i){
#' jj<-NAto0(TtableA)[i,]!=NAto0(RATtableA)[i,names(TtableA)]
#' identical(signif(NAto0(TtableA)[i,jj],3),signif(NAto0(RATtableA)[i,names(TtableA)[jj]],3))}))
#' randomcheck<-function(i=NULL){if(is.null(i)){i<-sample(1:nrow(TtableA),1)};
#' variablex="AA.cont1_La_La";
#' vx=c("AA.cont1_La_La_Lrn1", "AA.cont1_La_La_Lrn2", "AA.cont1_La_La_Lrn3", "AA.cont1_La_La_Lrn4");
#' BigSyn::get_presentind(variables = vx,refvariables = names(TtableA))->px
#' BigSyn::get_missingind(x=vx,variables = names(TtableA))->mx
#' list(i=i,total=ATtableA[i,"AA.cont1_"],LaRatio=ATtableA[i,"AA.cont1_La"],LaLaRatio=ATtableA[i,"AA.cont1_La_La"],
#' LaLaTotal=ATtableA[i,"AA.cont1_"]*ATtableA[i,"AA.cont1_La"]*ATtableA[i,"AA.cont1_La_La"],
#' rbind(rat=RATtableA[i,vx],at=ATtableA[i,vx],t=TtableA[i,vx]),
#' rbind(ratp=RATtableA[i,px],atp=ATtableA[i,px],tp=TtableA[i,px]),
#' rbind(ratp=RATtableA[i,mx],atp=ATtableA[i,mx],tp=TtableA[i,mx]))}
#' randomcheck(19)
#' randomcheck(109)
#' randomcheck(57)

reduceT_f<-function(.data,variables,verbose=FALSE,hack=TRUE){
  #
  .datareduced<-.data
  for(variable in variables){
    print(paste0(Sys.time()," ---- Now re-aggreagating ",variable),quote = F)
    variables<-names(.data)[get_var(names(.data))==variable]
    margincount<-get_cellXXmarginscount(variables)
    maxmargin<-max(margincount)
    atomicvariables<-variables[margincount==maxmargin]
    
    patterns<-do.call(rbind,
                      plyr::alply(1:maxmargin,1,function(i){
                        unique(cbind(i=i,
                                     pattern=get_cellXXgroup(atomicvariables,1:i,F),
                                     parent=get_cellXXgroup(atomicvariables,0:(i-1),F)))
                      }))
    patterns<-cbind(patterns,variable=paste(variable,patterns[,"pattern"],sep="_"))
    rownames(patterns)<-NULL
    patterns[patterns=="character(0)"]<-""
    
    patterns2<-do.call(cbind,plyr::alply(maxmargin:1,1,function(i){get_cellXXgroup(atomicvariables,1:i,F)}))
    patterns2<-cbind(patterns2,"")
    patterns2[patterns2=="character(0)"]<-""
    
    # print(paste0(Sys.time()," ---- Now imputing fractions of 0 for ",variable),quote = F)
    
    # for (aggregcell in setdiff(unique(patterns[,"parent"]),"")){
    #  varcell<-paste0(variable,"_",patterns[patterns[,"parent"]==aggregcell,2])
    #   exec.text<-paste0(".datareduced[.datareduced$",variable,"_",aggregcell,"==0|is.na(.datareduced$",variable,"_",aggregcell,"),varcell]<-NA")
    #  exec.text2<-paste0(".datareduced[.datareduced$",variable,"_",aggregcell,"==0|is.na(.datareduced$",variable,"_",aggregcell,"),c(",paste0("'",varcell,"'",collapse=","),")]<-NA")
    #  
    #  if(verbose){print(paste0(Sys.time()," ----- " ,exec.text2))}
    #  eval(parse(text=exec.text))}
    #.datareduced[unique(patterns[,2])]<-lapply(.datareduced[paste0(variable,"_",unique(patterns[,2]))],function(y){y[y==0]<-NA;y[is.na(y)]<-mean(y,na.rm=TRUE);y})}
    
    
    #Making data consistent with missing and presence indicators.
    
    for(variablex in patterns[,"variable"]){
      presentind=unlist(get_presentind(variablex,names(.data)))
      if(length(presentind)>0){
        .datareduced[[variablex]]<-.datareduced[[variablex]]*.datareduced[[presentind]]
      }
      missingind=unlist(get_missingind(variablex,names(.data)))
      if(length(missingind)>0){
        .datareduced[is.na(.datareduced[[missingind]])|.datareduced[[missingind]]==1,variablex]<-NA
      }
      
    } 
    
    
    print(paste0(Sys.time()," ---- Now computing  adjustments for percentages to add up to 1 for ",variable),quote = F)
    #sum all the percentages by margin. When result equals 0, replace by 1.
    correction<-do.call(cbind,c(plyr::alply(maxmargin:1,1,function(i){
      sumstocompute<-unique(patterns[patterns[,1]==i,3])
      zz<-lapply(sumstocompute,function(sumtocompute){
        x=rowSums(.datareduced[,paste0(variable,"_",patterns[patterns[,3]==sumtocompute,2]),drop=FALSE],na.rm=TRUE)
        x+(x==0)})
      names(zz)<-paste0(variable,"_",sumstocompute)
      as.data.frame(zz)
    }),list(deparse.level=0)))
    for (i in 1:maxmargin){names(correction)<-gsub(paste0(i,".",variable),variable,names(correction))}
    
    print(paste0(Sys.time()," ---- Now adjusting percentages to add up to 1 for ",variable),quote = F)
    for (i in 1:nrow(patterns)){
      commande<-paste0('.datareduced[,"',variable,"_",patterns[i,2],'"]<-.datareduced[,"',variable,"_",patterns[i,2],'"]/correction[,"',variable,"_",patterns[i,3],'"]')
      if(verbose){print(paste0(Sys.time()," ----- ",commande))}
      eval(parse(text=commande))}
    
    print(paste0(Sys.time()," ---- Now agregating ",variable),quote = F)
    for (j in nrow(patterns2):1){
      commande<-paste0(".datareduced[['",variable,"_",patterns2[j,1],"']]<-with(.data,",paste(c(sapply(1:(maxmargin+1),function(i){paste0(variable,"_",patterns2[j,i])}),get_presentind(paste0(variable,"_",patterns2[j,1]),names(.data))),collapse="*"),")")
      if(verbose){print(paste0(Sys.time()," ----- ",commande))}
      eval(parse(text=commande))}}
  
  #print(paste0(Sys.time()," ---- Now removing marginal totals for ",variable),quote = F)
  #.datareduced[!is.element(names(.datareduced),patterns[patterns[1,]>1])]
  .datareduced
}