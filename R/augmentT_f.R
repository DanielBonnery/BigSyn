#'  Convert cell totals to marginal ratios and create overall total.
#' 
#' @details Assume one runs the program
#'  
#' augmentT_f(.dataBigSyn::STtableA1,variables=c("AA.cont1","AA.cont1")).
#' The program looks for all the "cell variables" corresponding to "AA,cont1",
#' by using the function BigSyn::get_var
#' 
#' The results is this:
#' 
#' AA.cont1_La_La_Lrn1, AA.cont1_La_Ld_Lrn1, AA.cont1_Lb_Lb_Lrn1, 
#' AA.cont1_Lc_La_Lrn1, AA.cont1_Lc_Lb_Lrn1, AA.cont1_Lc_Ld_Lrn1,
#' AA.cont1_La_Lb_Lrn1, AA.cont1_La_Lc_Lrn1, AA.cont1_Lb_La_Lrn1,
#' AA.cont1_Lb_Lc_Lrn1, AA.cont1_Lb_Ld_Lrn1, AA.cont1_Lc_Lc_Lrn1, 
#' AA.cont1_La_La_Lrn2, AA.cont1_La_Ld_Lrn2, AA.cont1_Lb_Lb_Lrn2, 
#' AA.cont1_Lc_La_Lrn2, AA.cont1_Lc_Lb_Lrn2, AA.cont1_Lc_Ld_Lrn2, 
#' AA.cont1_La_Lb_Lrn2, AA.cont1_La_Lc_Lrn2, AA.cont1_Lb_La_Lrn2, 
#' AA.cont1_Lb_Lc_Lrn2, AA.cont1_Lb_Ld_Lrn2, AA.cont1_Lc_Lc_Lrn2, 
#' AA.cont1_La_La_Lrn3, AA.cont1_La_Ld_Lrn3, AA.cont1_Lb_Lb_Lrn3, 
#' AA.cont1_Lc_La_Lrn3, AA.cont1_Lc_Lb_Lrn3, AA.cont1_Lc_Ld_Lrn3,
#' AA.cont1_La_Lb_Lrn3, AA.cont1_La_Lc_Lrn3, AA.cont1_Lb_La_Lrn3, 
#' AA.cont1_Lb_Lc_Lrn3, AA.cont1_Lb_Ld_Lrn3, AA.cont1_Lc_Lc_Lrn3, 
#' AA.cont1_La_La_Lrn4, AA.cont1_La_Ld_Lrn4, AA.cont1_Lb_Lb_Lrn4,
#' AA.cont1_Lc_La_Lrn4, AA.cont1_Lc_Lb_Lrn4, AA.cont1_Lc_Ld_Lrn4
#' 
#' The programs computes the number of marginal variables 
#' with the function looks for  BigSyn::get_cellXXmarginscount.
#' Here it is 3
#' 
#' The program creates the following character matrix, named patterns:
#' 
#' "1" "La"         ""
#' 
#' "1" "Lb"         ""
#' 
#' "1" "Lc"         ""
#' 
#' "2" "La_La"      "La"          
#' 
#' "2" "La_Ld"      "La"          
#' 
#' "2" "Lb_Lb"      "Lb"          
#' 
#' "2" "Lc_La"      "Lc"          
#' 
#' "2" "Lc_Lb"      "Lc"          
#' 
#' "2" "Lc_Ld"      "Lc"          
#' 
#' "2" "La_Lb"      "La"          
#' 
#' "2" "La_Lc"      "La"          
#' 
#' "2" "Lb_La"      "Lb"          
#' 
#' "2" "Lb_Lc"      "Lb"          
#' 
#' "2" "Lb_Ld"      "Lb"          
#' 
#' "2" "Lc_Lc"      "Lc"          
#' 
#' "3" "La_La_Lrn1" "La_La"       
#' 
#' "3" "La_Ld_Lrn1" "La_Ld"       
#' 
#' "3" "Lb_Lb_Lrn1" "Lb_Lb"       
#' 
#' "3" "Lc_La_Lrn1" "Lc_La"       
#' 
#' "3" "Lc_Lb_Lrn1" "Lc_Lb"       
#' 
#' "3" "Lc_Ld_Lrn1" "Lc_Ld"       
#' 
#' "3" "La_Lb_Lrn1" "La_Lb"       
#' 
#' "3" "La_Lc_Lrn1" "La_Lc"       
#' 
#' "3" "Lb_La_Lrn1" "Lb_La"       
#' 
#' "3" "Lb_Lc_Lrn1" "Lb_Lc"       
#' 
#' "3" "Lb_Ld_Lrn1" "Lb_Ld"       
#' 
#' "3" "Lc_Lc_Lrn1" "Lc_Lc"       
#' 
#' "3" "La_La_Lrn2" "La_La"       
#' 
#' "3" "La_Ld_Lrn2" "La_Ld"       
#' 
#' "3" "Lb_Lb_Lrn2" "Lb_Lb"       
#' 
#' "3" "Lc_La_Lrn2" "Lc_La"       
#' 
#' "3" "Lc_Lb_Lrn2" "Lc_Lb"       
#' 
#' "3" "Lc_Ld_Lrn2" "Lc_Ld"       
#' 
#' "3" "La_Lb_Lrn2" "La_Lb"       
#' 
#' "3" "La_Lc_Lrn2" "La_Lc"       
#' 
#' "3" "Lb_La_Lrn2" "Lb_La"       
#' 
#' "3" "Lb_Lc_Lrn2" "Lb_Lc"       
#' 
#' "3" "Lb_Ld_Lrn2" "Lb_Ld"       
#' 
#' "3" "Lc_Lc_Lrn2" "Lc_Lc"       
#' 
#' "3" "La_La_Lrn3" "La_La"       
#' 
#' "3" "La_Ld_Lrn3" "La_Ld"       
#' 
#' "3" "Lb_Lb_Lrn3" "Lb_Lb"       
#' 
#' "3" "Lc_La_Lrn3" "Lc_La"       
#' 
#' "3" "Lc_Lb_Lrn3" "Lc_Lb"       
#' 
#' "3" "Lc_Ld_Lrn3" "Lc_Ld"       
#' 
#' "3" "La_Lb_Lrn3" "La_Lb"       
#' 
#' "3" "La_Lc_Lrn3" "La_Lc"       
#' 
#' "3" "Lb_La_Lrn3" "Lb_La"       
#' 
#' "3" "Lb_Lc_Lrn3" "Lb_Lc"       
#' 
#' "3" "Lb_Ld_Lrn3" "Lb_Ld"       
#' 
#' "3" "Lc_Lc_Lrn3" "Lc_Lc"       
#' 
#' "3" "La_La_Lrn4" "La_La"       
#' 
#' "3" "La_Ld_Lrn4" "La_Ld"       
#' 
#' "3" "Lb_Lb_Lrn4" "Lb_Lb"       
#' 
#' "3" "Lc_La_Lrn4" "Lc_La"       
#' 
#' "3" "Lc_Lb_Lrn4" "Lc_Lb"       
#' 
#' "3" "Lc_Ld_Lrn4" "Lc_Ld"
#' 
#'
#'
#'   Then for all i in 3:1 (starting with the maximum  depth)
#'   list the different aggregations to the upper level to perform.
#'   So for i=3, aggregating to the second level will be done by computing 
#'   the variables :
#'   AA.cont1_La_La, AA.cont1_La_Ld, AA.cont1_Lb_Lb, AA.cont1_Lc_La, 
#'   AA.cont1_Lc_Lb, AA.cont1_Lc_Ld, AA.cont1_La_Lb, AA.cont1_La_Lc, 
#'   AA.cont1_Lb_La, AA.cont1_Lb_Lc, AA.cont1_Lb_Ld, AA.cont1_Lc_Lc
#'  
#'  For example
#'  AA.cont1_La_La =rowSums(.data([,c("AA.cont1_La_La_Lrn1", "AA.cont1_La_La_Lrn2", "AA.cont1_La_La_Lrn3", "AA.cont1_La_La_Lrn4"),drop=FALSE])
#'  
#'  For i=2  aggregating to the upper level will be done by computing 
#'  the variables :
#'  AA.cont1_La, AA.cont1_Lb, AA.cont1_Lc
#'  AA.cont1_La =rowSums(.data([,c("AA.cont1_La_La", "AA.cont1_La_Ld", "AA.cont1_La_Lb", "AA.cont1_La_Lc"),drop=FALSE])
#'  
#'  For i=1 aggregating to theupper level will be done by computng the variable
#'   AA.cont1_=rowSums(.data([,c("AA.cont1_La", "AA.cont1_Lb", "AA.cont1_Lc"),drop=FALSE])
#'  
#'  The computation of the marginal totals is done, the second step is the computation of the 
#'  marginal ratios.
#'  
#'  It is done by looping on the rows of the patterns matrix
#'  
#'  Line j of pattern is a length 3 character vector.
#'  let call patterns[j,2] x and patterns[j,3] y
#'  The programs replaces the variable names paste0("AA.cont1",x) by 
#'  the ration of the variable paste0("AA.cont1",x) by the variable named paste0("AA.cont1",y).
#'  
#'  For example for the line  "3" "La_Ld_Lrn3" "La_Ld", the following replacement will be made:
#'  AA.cont1_La_Ld_Lrn3=AA.cont1_La_Ld_Lrn3/AA.cont1_La_Ld
#'  
#'  The same is applied to all the elements of the input parameter variables.
#'  
#' @param .data a dataframe
#' @param variables a vector of character strings
#' @return a dataframe.
#' @examples 
#' .data=BigSyn::STtableA1
#' variable="AA.cont1"
#' variables=variable
#' STAtableA1<-augmentT_f(.data,variables)
#' STAtableA1$AA.cont1_[6]
#' STtableA1[6,names(STtableA1)[get_var(names(STtableA1))=="AA.cont1"]]
#' sum(STtableA1[6,names(STtableA1)[get_var(names(STtableA1))=="AA.cont1"]],na.rm=TRUE)
#' STtableA1[6,"AA.cont1_Lc_La_Lrn1"]
#' STAtableA1[6,"AA.cont1_Lc_La_Lrn1"]
augmentT_f<-function(.data,variables,verbose=getOption("verbose")){
  for(variable in variables){
    if(verbose){print(paste0(Sys.time()," -- Now taking care of  ",variable),quote=F)}
    
    cellvariables<-names(.data)[get_var(names(.data))==variable]
    maxmargin<-max(get_cellXXmarginscount(cellvariables))
    patterns<-do.call(rbind,plyr::alply(1:maxmargin,1,function(i){
      unique(cbind(i,get_cellXXgroup(cellvariables,1:i,F),get_cellXXgroup(cellvariables,0:(i-1),F)))
    }))
    patterns[patterns=="character(0)"]<-""
   #Computation of marginal totals
    for(i in maxmargin:1){
      sumstocompute<-unique(patterns[patterns[,1]==i,3])
      .data2<-
        lapply(sumstocompute,function(sumtocompute){
          rowSums(.data[,paste0(variable,"_",patterns[patterns[,3]==sumtocompute,2]),drop=FALSE],na.rm=TRUE)})
      names(.data2)<-paste0(variable,"_",sumstocompute)
      
      .data<-cbind(.data,.data2)
    }
  }
  
  #Computation of ratios
  
  for (j in nrow(patterns):1){
    command<-paste0(".data[['",variable,"_",patterns[j,2],"']]<-",
                    "with(.data,",variable,"_",patterns[j,2],"/(",variable,"_",patterns[j,3],"+(",variable,"_",patterns[j,3],"==0)))")
    if(verbose){print(paste0(Sys.time()," --- ",command),quote=F)}
    eval(parse(text=command))
  }
  
  .data
}
