#' resample  when synthetisation created incoherent high and low level aggregates
#'
#' @param .data data frame to "reduce"
#' @param verbose (default FALSE) if verbose, the formulae to compute the new variables is printed.
#' @param variables list of variable names roots
#' @details 
#' @example
#' library(BigSyn)
#' library(reshape2)
#' library(data.table)
#' data(TtableA,package="BigSyn")
#' variable="AA.cont1"
#' variables=variable
#' ATtableA<-augmentT_f(TtableA,variables)
#' set.seed(1)
#' SATtableA<-BigSyn::SDPSYN2(ATtableA,asis=c("id1a", "id1b"))[[1]]
#' problems<-SATtableA$AA.cont1_Lb_La>0&!is.na(SATtableA$AA.cont1_Lb_La)&((SATtableA$AA.cont1_Lb_La_Lrn1==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn1))&(SATtableA$AA.cont1_Lb_La_Lrn2==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn2))&(SATtableA$AA.cont1_Lb_La_Lrn3==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn3)))
#' varcell=c("AA.cont1_Lb_La_Lrn1", "AA.cont1_Lb_La_Lrn2", "AA.cont1_Lb_La_Lrn3")
#' varcellandpresenceind<-unlist(c(varcell,get_missingind(varcell,names(SATtableA)),get_presentind(varcell,names(SATtableA))))
#' replacements<-SATtableA$AA.cont1_Lb_La>0&!is.na(SATtableA$AA.cont1_Lb_La)&!((SATtableA$AA.cont1_Lb_La_Lrn1==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn1))&(SATtableA$AA.cont1_Lb_La_Lrn2==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn2))&(SATtableA$AA.cont1_Lb_La_Lrn3==0|is.na(SATtableA$AA.cont1_Lb_La_Lrn3)))
#' SATtableA[problems,c("AA.cont1_Lb_La",varcellandpresenceind)][1:3,]
#' SATtableA[replacements,c("AA.cont1_Lb_La",varcellandpresenceind)][1:3,]
#' CSATtableA<-resampleT_f(SATtableA,variables)
#' CSATtableA[problems,c("AA.cont1_Lb_La",varcellandpresenceind)][1:3,]
#' RCSATtableA<-reduceT_f(CSATtableA,variables)
#' RCSATtableA[problems,intersect(c("AA.cont1_Lb_La",varcellandpresenceind),names(RCSATtableA))][1:3,]
#' 

resampleT_f<-function(.data,variables,verbose=FALSE){
  .dataresampled<-.data
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
    
    
    
    for(variablex in patterns[,"variable"]){
      presentind=unlist(get_presentind(variablex,names(.data)))
      if(length(presentind)>0){
        .dataresampled[[variablex]]<-.dataresampled[[variablex]]*.dataresampled[[presentind]]
      }
      missingind=unlist(get_missingind(variablex,names(.data)))
      if(length(missingind)>0){
        .dataresampled[is.na(.dataresampled[[missingind]])|.dataresampled[[missingind]]==1,variablex]<-NA
      }
      
    } 
    
    
    if(verbose){print(paste0(Sys.time()," ---- Now chasing terms with all zeros for ",variable),quote = F)}
    
    for (aggregcell in setdiff(unique(patterns[,"parent"]),"")){
      varcell<-paste0(variable,"_",patterns[patterns[,"parent"]==aggregcell,2])
      problem.text<-paste0(".dataresampled$",variable,"_",aggregcell,">0&!is.na(.dataresampled$",variable,"_",aggregcell,")&(",
                           paste("(.dataresampled$",varcell,"==0|is.na(.dataresampled$",varcell,"))",sep="",collapse="&"),")")
      problems<-eval(parse(text=problem.text))
      if(any(problems)){
        varcellandpresenceind<-unlist(c(varcell,get_missingind(varcell,names(.dataresampled)),get_presentind(varcell,names(.dataresampled))))
        replacements<-eval(parse(text=paste0(".dataresampled$",variable,"_",aggregcell,">0&!is.na(.dataresampled$",variable,"_",aggregcell,")&!(",
                                             paste("(.dataresampled$",varcell,"==0|is.na(.dataresampled$",varcell,"))",sep="",collapse="&"),")")))
        
        if(any(replacements)){
          .dataresampled[problems,varcellandpresenceind]<-.dataresampled[sample((1:nrow(.dataresampled))[replacements],sum(problems),TRUE),varcellandpresenceind]
          if(verbose){print(paste0("identified ",sum(problems)," all 0s contradicting ",variable,"_",aggregcell,">0 : ",sum(replacements)," donors, bulk replacement of ",paste(varcellandpresenceind,collapse=", ")))}
        }}}}
  .dataresampled
  #End hack
}