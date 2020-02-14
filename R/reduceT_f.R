#' Function that will convert cell and marginal ratios and overall total to cell values
#'
#' @param .data data frame to "reduce"
#' @param verbose (default FALSE) if verbose, the formulae to compute the new variables is printed.
#' @param hack (default TRUE)
#' @param variables list of variable names roots
#' @details this functions looks for the Augmentation parameters in the package object Augmentparameters[[tablename]]$percent
#' For each variable listed in Augmentparameters[[tablename]]$percent, it looks for the corresponding variable in
#' .data and computes cell values from cell and marginal ratios and overall total
#' @example
#' library(reshape2)
#' library(data.table)
#'  hack=TRUE
#'  verbose=TRUE
#' .data=BigSyn::STtableA1
#' variable="AA.cont1"
#' variables=variable
#' .data<-STAtableA1<-augmentT_f(.data,variables)
#' reduceT_f(.data[1:10,],variables)
#'  

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
    
    print(paste0(Sys.time()," ---- Now imputing fractions of 0 for ",variable),quote = F)
    
    for (aggregcell in setdiff(unique(patterns[,"parent"]),"")){
      varcell<-paste0(variable,"_",patterns[patterns[,"parent"]==aggregcell,2])
      exec.text<-paste0(".datareduced[.datareduced$",variable,"_",aggregcell,"==0|is.na(.datareduced$",variable,"_",aggregcell,"),varcell]<-NA")
      if(verbose){print(paste0(Sys.time()," ----- " ,exec.text))}
      eval(parse(text=exec.text))}
    .datareduced[unique(patterns[,2])]<-lapply(.datareduced[paste0(variable,"_",unique(patterns[,2]))],function(y){y[y==0]<-NA;y[is.na(y)]<-mean(y,na.rm=TRUE);y})}
  
  
  if(hack){
    #HACK
    if(verbose){print(paste0(Sys.time()," ---- Now chasing terms with all zeros for ",variable),quote = F)}
    
    for (aggregcell in setdiff(unique(patterns[,"pattern"]),"")){
      varcell<-paste0(variable,"_",patterns[patterns[,"parent"]==aggregcell,2])
      problems<-eval(parse(text=paste0(".datareduced$",variable,"_",aggregcell,">0&
                                     !is.na(.datareduced$",variable,"_",aggregcell,")&
                                     (",
                                       paste("(.datareduced$",varcell,"==0|is.na(.datareduced$",varcell,"))",sep="",collapse="&
          "),")")))
      if(any(problems)){
        varcellandpresenceind<-unlist(c(varcell,get_missingind(varcell,names(.datareduced)),get_presentind(varcell,names(.datareduced))))
        replacements<-eval(parse(text=paste0(".datareduced$",variable,"_",aggregcell,">0&!is.na(.datareduced$",variable,"_",aggregcell,")&!(",
                                             paste("(.datareduced$",varcell,"==0|is.na(.datareduced$",varcell,"))",sep="",collapse="&"),")")))
        
        if(any(replacements)){
          .datareduced[problems,varcellandpresenceind]<-.datareduced[sample((1:nrow(.datareduced))[replacements],sum(problems),TRUE),varcellandpresenceind]
          if(verbose){print(paste0("identified ",sum(problems)," all 0s contradicting ",variable,"_",aggregcell,">0 : ",sum(replacements)," donors, bulk replacement of ",paste(varcellandpresenceind,collapse=", ")))}
        }}}}
  #End hack
  
  print(paste0(Sys.time()," ---- Now computing  adjustments for percentages to add up to 1 for ",variable),quote = F)
  #sum all the percentages by margin.
  correction<-do.call(cbind,c(plyr::alply(maxmargin:1,1,function(i){
    sumstocompute<-unique(patterns[patterns[,1]==i,3])
    zz<-lapply(sumstocompute,function(sumtocompute){
      rowSums(.datareduced[,paste0(variable,"_",patterns[patterns[,3]==sumtocompute,2]),drop=FALSE])})
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
    commande<-paste0(".datareduced[['",variable,"_",patterns2[j,1],"']]<-with(.data,",paste(sapply(2:(maxmargin+1),function(i){paste0(variable,"_",patterns2[j,i])}),collapse="*"),")")
    if(verbose){print(paste0(Sys.time()," ----- ",commande))}
    eval(parse(text=commande))}
  
  #print(paste0(Sys.time()," ---- Now removing marginal totals for ",variable),quote = F)
  #.datareduced[!is.element(names(.datareduced),patterns[patterns[1,]>1])]
  .datareduced
}
