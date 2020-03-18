#'sort table within group without changing the group position
#' @details
#' Groups are defined by unique values of .data[groupvar]. Within each group, data is sorted according to sortvar.
#' @param .data a dataframe
#' @param groupvar a vector of character strings that are names of variables from .data
#' @param sortvar a vector  of character strings that are names of variables from .data
#' @param decreasing a boolean if TRUE, decreasing order is used.
#' @return a list.  
#' @examples
#' set.seed(1)
#' N=10
#'.data=data.frame(
#'  .group=sample(letters[1:2],N,replace=TRUE),
#'  y=runif(N),
#'  origorder=1:N)
#'  groupvar=".group"
#'  sortvar="y"
#'.data2=plyr::ddply(.data,".group",function(d){d$intraorder=order(d[[sortvar]]);d$neworder=d$origorder[order(order(d[[sortvar]]))];d})
#' .data4=.data2[.data2[[groupvar]]=="a",];.data4[.data4$intraorder,]
#' cbind(.data,"|",.data2)
#' cbind(.data,"|",.data2[order(.data2$origorder),])
#' cbind(.data[order(order(.data2$origorder)),],"|",.data2)
#' .data3=cbind(.data,"|",.data2[order(.data2$neworder),]);.data3[.data[[groupvar]]=="a",];.data3
#' .data3=cbind(.data,"|",.data[order(order(.data2$origorder)),][order(.data2$neworder),]);.data3[.data[[groupvar]]=="a",]
#' .data3=cbind(.data,"|",.data[order(order(.data2$origorder))[order(.data2$neworder)],]);.data3[.data[[groupvar]]=="a",]
#' cbind(.data,I="|",sorttablewithingroup(.data,groupvar,sortvar),I="|",sorttablewithingroup(.data,groupvar,sortvar,decreasing=TRUE),I="|",sorttablewithingroup(.data,NULL,groupvar,decreasing=TRUE))

sorttablewithingroup<-function(.data,groupvar,sortvar,decreasing=FALSE){
    if(!is.null(groupvar)){
    .data2=plyr::ddply(cbind(.data[c(groupvar,sortvar)],origorder=1:nrow(.data)),
                       groupvar,
                       function(d){
                       data.frame(origorder=d$origorder,
                                  neworder=d$origorder[order(do.call(order,c(d[c(sortvar,"origorder")],list(decreasing=decreasing))))])})[c("origorder","neworder")]
    return(.data[order(order(.data2$origorder))[order(.data2$neworder)],])}else{
      return(.data[do.call(order,cbind(.data[sortvar],1:nrow(.data))),])  
    }}

#' General Default ordering of variables for synthetisation based on name of the variable.
#' 
#' @param variables vector of character strings, indicating names of variables
#' @param orderwithinorigin a list, see example
#' @param id a vector of character strings
#' @param extrasort (default=NULL) list variables that should be used for an additional ordering
#' @return a list.  
#' @details After transposition, variable names follow this format: 
#' origin.variablename_margin1_margin2....lastmargin
#' Some rules have to be followed:
#'
#'  - Missing indicators have to be synthesised before the corresponding variables,
#' for example AA.factor1missingind_L1_L2_L1 needs to be synthesised before AA.factor1missingind_L1_L2_L1 
#' 
#' - Cell indicators must be synthesised before the corresponding variables.
#' For example  AA.present_L1_L2_L1 must be synthesised before AA.factor1_L1_L2_L1 and before AA.cont1_L1_L2_L1 
#' 
#' - Parent variables (aggregated) must be synthesised before their children:
#' For example AA.present_L1 must be synthesised before AA.present_L1_L2,
#' AA.cont2_L1_L2 must be synthesised before AA.cont2_L1_L2_L3
#' AA.present_L1 must be synthesised before AA.present_L1_L3
#' AA.cont2missingind_L1 must be synthesised before AA.cont2missingind_L1_L3
#' 
#' - if for examples variable AA.cont1 in each cell has to be synthesised before AA.cont2,
#' this can be specified with the orderwithinorigin argument
#' 
#' - for the use of the argument extrasort, refer to sorttablewithingroup
#' @examples
#' TK<-Tsampledata(TRUE)$TtableA
#' Sparameters.variables.reorder.default(names(TK$TtableA))
#' #Second example: no transposing variables
#' TtableA<-Tsampledata(TRUE)$TtableA
#' orderwithinorigin=c("AA.factor1","AA.factor2")
#' variables<-names(TtableA)
#' Sparameters.variables.reorder.default(variables,orderwithinorigin)
Sparameters.variables.reorder.default<-
  function(variables,
           orderwithinorigin=NULL,
           id=NULL,
           extrasort=NULL){
    variable.frame<-data.frame(
      variables=variables,
      root=get_var(variables),
      origin=get_origin(variables),
      depth=get_cellXXmarginscount(variables),
      cells=unlist(get_cell(variables)),
      missinginds=grepl("missingind",variables),
      correspondingvar=gsub("missingind","",variables),
      rns=get_cellrn(variables),
      isnotpresent=!grepl("present",variables),
      stringsAsFactors = F)
    
    if(!is.null(extrasort)){  
      if(is.list(extrasort)){nextrasort<-paste0("extrasort",1:length(extrasort))
            xx<-data.frame(do.call(cbind,extrasort))
            names(xx)<-nextrasort
            variable.frame<-cbind(variable.frame,xx)
      }else{
        nextrasort="extrasort"
        variable.frame$extrasort=extrasort}}
    for(i in 1:2){
      
      if(!is.null(extrasort)){
        variable.frame<-sorttablewithingroup(variable.frame,NULL,nextrasort)}
      variable.frame<-sorttablewithingroup(variable.frame,"root",c("depth","rns"))
      variable.frame<-sorttablewithingroup(variable.frame,"correspondingvar","missinginds",decreasing=TRUE)
      #variable.frame<-sorttablewithingroup(variable.frame,"cells","rns")
      variable.frame<-sorttablewithingroup(variable.frame,c("origin","rns"),"isnotpresent")
    }
    variable.frame$variables}

#' Get cell and row number
#' @details if x is "aa.x_a_1_f_1" returns "a_1_f_1"
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples 
#' get_cellrn("AA.char1_La_Ld_Lrn1")
#' data(TtableA);
#' unique(get_cellrn(names(TtableA)))
#' #Second example: no transposing variables
#' data(TtableB);data(XKB)
#' unique(get_cellrn(names(XKB)))
get_cellrn<-function(x){sapply(x,function(y){if(length(y)>0){if(grepl("_",y)){
  substr(y,stringi::stri_locate_first_fixed(y,"_")[,2]+1,nchar(y))}else{""}}else{""}},simplify=TRUE)}

#' Drop last margin position (Trims all strings of a vector of strings after the last "_")
#' 
#' @details if x is "AA.char1_La_Ld_Lrn1" returns "AA.char1_La_Ld", if x contains no "_", returns empty string
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples 
#' drop_last("AA.char1_La_Ld_Lrn1")
#' drop_last("iojoij")
#' drop_last("aa.iojoij")
drop_last<-function(x){sapply(x,function(y){if(length(y)>0){if(grepl("_",y)){
  substr(y,1,stringi::stri_locate_last_fixed(y,"_")[,2]-1)}else{""}}else{""}},simplify=TRUE)}

#' get cell without the row number
#' @details if x is "aa.xoijj_a_1_f_1_" returns "a_1_f"
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples
#' get_cell("aa.x_1_2_3_4")#default
#' get_cell("1_2_3",TRUE)
#' get_cell("1_2_3",FALSE,TRUE)
#' unique(Tsampledata(TRUE)$variables))
#' unique(get_cell(Tsampledata(FALSE)$variables))
get_cell<-function(x,iscellrn=FALSE,iscell=FALSE){if(iscell){x}else{if(iscellrn){drop_last(x)}else{get_cellrn(drop_last(x))}}}

#' get parent if any
#' @details if x is "aa.xoijj_a_1_f_1_" returns "a_1_f"
#' @param variables     a character strings 
#' @param variable_ref  a vector of character strings 
#' @return a vector of character strings
#' @examples
#' get_parent("aa.x_1_2_3_4","aa.x_1_2_3")#default
get_parent<-function(variables,variable_ref){
  sapply(variables,function(variable){
  paste0("",intersect(c(drop_last(variable),paste0(drop_last(variable),"_")),
                      setdiff(variable_ref,variable)))})}


#' Get variable name
#' @details if x is "aa.xoijj_a_1_f_1" returns "aa.xoijj"
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples 
#' get_var("aa.x_1_2_3_4")
#' data(TtableA)
#' unique(get_var(names(TtableA)))
#' #Second example: no transposing variables
#' TK<-Tsampledata(FALSE)
#' unique(get_var(TK$variables))
get_var<-function(x){sapply(x,function(y){
  substr(y,1,if(grepl("_",y)){stringi::stri_locate_first_fixed(y,"_")[,2]-1}else{nchar(y)})},simplify=TRUE)}

#' Get the number of margins for a cell
#' 
#' @details  if x is "aa.xoijj_a_1_f_1", cell=FALSE returns 4"; if x is "a_1_f_1", cell=TRUE returns 4"
#' @param x a vector of character strings 
#' @param iscell a boolean indicating if x is a variable name or a cell name. 
#' @return a vector of  integers.
#' @examples 
#' get_cellXXmarginscount("1_2_3_4",iscellXX=TRUE)
#' get_cellXXmarginscount("aa.x_1_2_3_4",iscellXX=FALSE)
#' data(TtableA)
#' unique(get_cellXXmarginscount(names(TtableA),iscellXX=FALSE))
#' #Second example: no transposing variables
#' TK<-Tsampledata(FALSE)
#' unique(get_cellXXmarginscount(TK$variables))
get_cellXXmarginscount<-function(x,iscellXX=FALSE){stringr::str_count(x,"_")+iscellXX}

#' split a cell
#' @details if x is "aa.xoijj_a_1_f_1" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @param iscell x a boolean indicating if x is a cell 
#' @return a vector of character strings
#' @examples 
#' get_cellXXsplit("aa.x_1_2_3_4",iscellXX=FALSE)
#' get_cellXXsplit("1_2_3_4",iscellXX=TRUE)
#' get_cellXXsplit("1_2_3_4",2:3,iscellXX=TRUE)
#' get_cellXXsplit("1_2_3_4",-(2:3),iscellXX=TRUE)
#' variables<-Tsampledata(TRUE)$variables
#' unique(get_cellXXsplit(variables,iscell=FALSE))
#' get_cellXXsplit(variables[50],iscell=FALSE)
#' get_cellXXsplit(variables[50],-(2:3),iscell=FALSE)
#' unique(get_cellXXsplit(variables,2,iscell=FALSE))
#' #Second example: no transposing variables
#' TK<-Tsampledata(FALSE)
#' unique(get_cellXXsplit(TK$variables,iscell=FALSE))
get_cellXXsplit<-function(x,marginpos=NULL,iscellXX=FALSE){
  sapply(x,function(y){strsplit(y,"_")[[1]][if(!iscellXX){-1}else{TRUE}][if(is.null(marginpos)){(1:get_cellXXmarginscount(y,iscellXX))}else{marginpos}]})}

#' Get cell group
#' @details #' if x is "a_1_f_2_aa.xoijj",marginpos=2 returns "1"; if x is "a_1_f_2_aa.xoijj",marginpos=-2 returns "a_f_2"; if x is "a_1_f_2_aa.xoijj",marginpos=c(1:2) returns "a_1"
#' @param x a vector of character strings 
#' @param marginpos a vector of integer 
#' @return a vector of character strings
#' @examples 
#' get_cellXXgroup(c("aa.x_1_2_3_4","bb.x_1_2_3_4"),2,iscellXX=FALSE)
#' get_cellXXgroup(c("1_2_3_4","1_2_3_4"),2:3,iscellXX=TRUE)
#' variables<-Tsampledata(TRUE)$variables
#' unique(get_cellXXgroup(variables,2,iscellXX=FALSE))
#' unique(get_cellXXgroup(variables,-2,iscellXX=FALSE))
#' get_cellXXgroup(variables[50],2,iscellXX=FALSE)
#' get_cellXXgroup(variables[50],-2,iscellXX=FALSE)
#' 
#' #Second example: no transposing variables
#' TK<-Tsampledata(FALSE)
#' unique(get_cellXXgroup(TK$variable,1,iscell=FALSE))
get_cellXXgroup<-function(x,marginpos,iscellXX=TRUE){sapply(x,function(y){
  paste(get_cellXXsplit(y,marginpos,iscellXX),collapse="_")})}



#' get cell predecessors at margin
#' 
#' @details if XXs is "aa.xoijj_a_1_f_1" and refXXs contains "aa.xoijj_a_1_e_1"  and marginpos=3 returns "aa.xoijj_a_1_e_1"
#' if XXs is "aa.xoijj_a_1_f_2" and refXXs contains "aa.xoijj_a_1_f_1"  and marginpos=NULL returns "aa.xoijj_a_1_f_1"
#' if XXs is "id1" and  iscellXX=FALSE whatever refXXs returns character(0)
#' if XXs is "" and iscellXX=FALSE whatever refXXs returns character(0)
#' if XXs is "b_1_f_1"  and iscellXX=TRUE and refXXs contains "a_1_f_1" returns "a_1_f_1"  
#' @param XXs a vector of character strings 
#' @param refXXs a vector of character strings containing the potential predecessors
#' @param marginpos a vector of integers
#' @return a vector of character strings
#' @examples 
#' get_XXpredecessoratmargin(XXs="aa.x_1_2_3_4", refXXs=c("bb.x_1_2_2_4","aa.x_1_2_2_4","aa.x_1_1_3_4"),2,iscellXX=FALSE)
#' get_XXpredecessoratmargin(XXs=c("1_2_2_4","1_2_2_4","1_1_3_4","1_1_3_3"),iscellXX=TRUE)
#' get_XXpredecessoratmargin(XXs="1_1_3_4",refXXs=c("1_2_2_4","1_2_2_4","1_1_3_4","1_1_3_3"),iscellXX=TRUE)
#' data(XKA)
#' cells<-unique(get_cellrn(XKA$variables))
#' get_XXpredecessoratmargin(cells,marginpos=1,iscellXX=TRUE)
#' get_XXpredecessoratmargin(cells[10],cells,1,iscellXX=TRUE)
#' 
get_XXpredecessorsatmargin<-function(XXs,
                                     marginpos,
                                     refXXs=XXs[order(get_cellXXgroup(XXs,marginpos))],
                                     iscellXX=FALSE,
                                     cellXXgroup    =get_cellXXgroup(refXXs,marginpos2,iscellXX),
                                     CompcellXXgroup=get_cellXXgroup(refXXs,-marginpos2,iscellXX)){
  plyr::alply(XXs,1,function(xx){
    refXXs[cellXXgroup    < get_cellXXgroup(xx,marginpos,iscellXX)&
           CompcellXXgroup==get_cellXXgroup(xx,-marginpos,iscellXX)]})}


#' get cell predecessors at margin
#' 
#' @details 
#' if XXs is "aa.xoijj_a_1_f_1" and refXXs contains "aa.xoijj_a_1_e_1"  and marginpos=3 returns "aa.xoijj_a_1_e_1"
#' if XXs is "aa.xoijj_a_1_f_2" and refXXs contains "aa.xoijj_a_1_f_1"  and marginpos=NULL returns "aa.xoijj_a_1_f_1"
#' if XXs is "id1" and  iscellXX=FALSE whatever refXXs returns character(0)
#' if XXs is "" and iscellXX=FALSE whatever refXXs returns character(0)
#' if XXs is "b_1_f_1"  and iscellXX=TRUE and refXXs contains "a_1_f_1" returns "a_1_f_1"  
#' @param XXs a vector of character strings 
#' @param refXXs a vector of character strings containing the potential predecessors
#' @param marginpos a vector of integers
#' @return a vector of character strings
#' @examples 
#' get_XXpredecessoratmargin(XXs="aa.x_1_2_3_4", refXXs=c("bb.x_1_2_2_4","aa.x_1_2_2_4","aa.x_1_1_3_4"),2,iscellXX=FALSE)
#' get_XXpredecessoratmargin(XXs=c("1_2_2_4","1_2_2_4","1_1_3_4","1_1_3_3"),iscellXX=TRUE)
#' get_XXpredecessoratmargin(XXs="1_1_3_4",refXXs=c("1_2_2_4","1_2_2_4","1_1_3_4","1_1_3_3"),iscellXX=TRUE)
#' data(XKA)
#' cells<-unique(get_cellrn(XKA$variables))
#' get_XXpredecessoratmargin(cells,marginpos=1,iscellXX=TRUE)
#' get_XXpredecessoratmargin(cells[10],cells,1,iscellXX=TRUE)
#' 

get_XXpredecessoratmargin<-function(XXs,refXXs=XXs,marginpos=NULL,iscellXX=FALSE){
  sapply(XXs,function(y){
    iscellXX<-if(is.null(iscellXX)){!is.variable(y)}else{iscellXX}
    marginpos2<-if(is.null(marginpos)){get_cellXXmarginscount(y,iscellXX)}else{marginpos}
    refXXs<-refXXs[order(get_cellXXgroup(refXXs,marginpos2))]
    pred<-refXXs[get_cellXXgroup(refXXs,marginpos2,iscellXX)<get_cellXXgroup(y,marginpos2,iscellXX)&
                   get_cellXXgroup(refXXs,-marginpos2,iscellXX)==get_cellXXgroup(y,-marginpos2,iscellXX)&
                   (if(iscellXX){TRUE}else{get_var(refXXs)==get_var(y)})]
    pred[length(pred)]},USE.NAMES=TRUE,simplify=TRUE)
}

is.variable<-function(x){sapply(x,function(y){grepl(".",y,fixed = TRUE)})} 


#' get variable predecessors at margin
#' 
#' @details if x is "a_1_f_1_aa.xoijj" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @param cells a vector of character strings containing the potential predecessors
#' @param marginpos a vector of integers
#' @return a vector of character strings
#' @examples 
#' get_XXpredecessoratmargin(cellXXs="aa.x_1_2_3_4", refcellXXs=c("bb.x_1_2_2_4","aa.x_1_2_2_4","aa.x_1_1_3_4"),2,iscellXX=FALSE)
#' get_XXpredecessoratmargin(cellXXs=c("1_2_2_4","1_2_2_4","1_1_3_4","1_1_3_3"),iscellXX=FALSE)
#' data(XKA)
#' cells<-unique(get_cellrn(XKA$variables))
#' get_XXpredecessoratmargin(cells,marginpos=1,iscellXX=TRUE)
#' get_XXpredecessoratmargin(cells[10],cells,1,iscellXX=TRUE)

#get_variablepredecessoratmargin<-function(variables,refvariables=variables,marginpos=NULL){
#  refcellrns<-unique(get_cellrn(variables))
#  sapply(variables,function(y){
#    marginpos2<-if(is.null(marginpos)){get_cellXXmarginscount(y)}else{marginpos}
#    intersect(refvariables,
#              gsub(get_cellrn(y),get_XXpredecessoratmargin(get_cellrn(y),refcellrns,marginpos2)))})}



#' Get natural predictors
#' 
#' @details if x is "a_1_f_1_aa.xoijj" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @param variables a vector of character strings 
#' @return a vector of character strings
#' @examples
#' TK<-TtableA
#' get_natural.predictors(x=sample(names(TtableA),5),variables=names(TtableA))

get_natural.predictors<-function(x,variables=x,predictors=NULL){
  sapply(x,function(y){
    #get same variable in previous cell
    #loop on margins.
    z<-do.call(c,plyr::alply(1:get_cellXXmarginscount(y),1,function(i){
      get_XXpredecessoratmargin(y,variables,i)}))
    z<-unlist(c(z,predictors[[z]]))
    #get missing indicators.
    z<-unique(unlist(c(z,get_missingind(z,variables))))
    #get present indicators.
    z<-unique(unlist(c(z,get_presentind(variables = z,refvariables = variables))))
    z<-intersect(z,variables[1:which(variables==y)])})}


#' get the present indicator for a cell
#' @details if x is "a_1_f_1_aa.xoijj" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples

#' get_presentind("AA.x_1_2_3_4","AA.present_1_2_3_4")
#' get_presentind("AA.present_1_2_3_4",c("AA.present_1_2_3_3","AA.present_1_2_3"))
#' get_presentind("AA.present_1_2_3_4",c("AA.present_1_2_3_3","AA.present_1_2_3_4"))
#' variables<-Tsampledata(TRUE)$variables
#' variable<-"AA.present_La_La_Lrn1"
#' get_presentind(variable,variables)
#' unlist(unique(get_presentind(variables)))
#' variables<-Tsampledata(FALSE)$variables
#' unlist(unique(get_presentind(variables,variables)))
get_presentind<-function(variables,refvariables=variables,rns=unlist(unique(get_cellrn(refvariables)))){
  refcellrns<-get_cellrn(refvariables)
  sapply(variables,function(y){
    z<-gsub(pattern = get_var(y),
            replacement =     paste0(substr(get_var(y),1,stringi::stri_locate_last_fixed(y,".")[,2]),
                                     "present"),y)
    if(z==y){z=get_parent(y,refvariables)
#      z<-gsub(pattern=get_cellrn(z),
#              replacement=get_XXpredecessoratmargin(get_cellrn(z),refXXs = refcellrns),z)
    }
    intersect(z,refvariables)})}

#' Get origin table
#' 
#' @details if x is "aa.xoijj_a_1_f_1_" returns c("aa")
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples
#' get_origin("tableA.cont1_1_Lrn1")
#' variables<-Tsampledata(TRUE)$variables
#' unlist(unique(get_origin(variables,variables)))
#' variables<-Tsampledata(FALSE)$variables
#' unlist(unique(get_origin(variables,variables)))

get_origin<-function(x){
  sapply(x,function(y){substr(y,1,stringi::stri_locate_last_fixed(y,".")[,2]-1)})}
#' Get missing indicator for a cell or variable
#' 
#' @details if x is "a_1_f_1_aa.xoijj" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @return a vector of character strings
#' @examples
#' variables<-Tsampledata(TRUE)$variables
#' unlist(unique(get_missingind(variables,variables)))
#' variables<-Tsampledata(FALSE)$variables
#' unlist(unique(get_missingind(variables,variables)))
get_missingind<-function(x,variables){sapply(x,function(y){
  intersect(paste0(get_var(y),"missingind",c("","_")[length(get_cellrn(y))>0],get_cellrn(y)),variables)})}

#' Define a default predictor matrix
#' @details Returns the lower diagonal matrix with ones.
#' @param variables a vector of character strings 
#' @return a matrix
#' @examples
#' variables<-Tsampledata(TRUE)$variables
#' predictor.matrix.default(TK$variables)
predictor.matrix.default<-
  function(variables){
    predictors.matrix<-outer(1:length(variables),1:length(variables),">")
    dimnames(predictors.matrix)<-list(variables,variables)
    predictors.matrix}

#' predictor.matrix.rate
#' 
#' @details if x is "aa.xoijj_a_1_f_1_" returns c("a","1","f","1")
#' @param x a vector of character strings 
#' @return a vector of character strings
predictor.matrix.rate<-function(variables,
                                nopredictor=character(0),
                                allpredictor=character(0),
                                marginposs=integer(0)){
  possible.predictors.matrix<-predictor.matrix.default(variables)
  predictor.matrix<-matrix(FALSE,length(variables),length(variables))
  dimnames(predictors.matrix)<-list(variables,variables)
  
  #predictors.matrix[nopredictor,]<-FALSE
  predictors.matrix[allpredictor,]<-possible.predictors.matrix[allpredictor,]
  predictors.matrix}

#' Default synthetisation parameters based on variable names
#' @details creates default synthetisation parameters
#' Some rules: parents variable are potential predictors of their children,
#' synthetisation is conditional to missingindicators,
#' synthetisation is conditional to presence in cell 
#' @param ref.table a dataframe
#' @param asis a vector of character strings, indicating which variables to keep as is.
#' @param notpredictor a vector of character strings, indicating which variables are not supposed to be used as predictors.
#' @param variables a vector of character strings, indicating the variables to synthesize. Order is important.
#' @param predictors.matrix a predictor matrix. Number of rows is the number of variables to synthesize, number of columns is all the variables from ref.table
#' @param moresplits an object of class moresplist (not defined yet) 
#' @param preferredmethod: "rf" for random forest or "ctree" for classification tree
#' @param defaultparameters  a list indicating default parameters for synthpop synthesisation functions, for example ntree=5, smoothing="none"
#' @return 
#' @examples
#' data(TtableA)
#' ATtableA<-augmentT_f(TtableA,variablespct="AA.cont1",variablesmax="AA.present")
#' ref.table<-ATtableA
#' Spa<-Sparameters.default.f(ref.table=ATtableA)
#' names(Spa)<-lapply(Spa,function(x){x$variable})
#' Spa$AA.present_La_Lb
#' Spa$AA.cont1_La_Lb

Sparameters.default.f<-
  function(ref.table,
           asis=NULL,
           notpredictor=NULL,
           variables=Sparameters.variables.reorder.default(names(ref.table)),
           predictors.matrix=predictor.matrix.default(variables)[!is.element(variables,asis),!is.element(variables,notpredictor)],
           splittingvar=NULL,
           moresplits=NULL,
           preferredmethod="ctree",
           splithreshold=100,
           defaultsynparameters=list(
             #ntree=5,
             #mtry=quote(min(60,ncol(xp))),
             smoothing="none",
             importance=TRUE,
             keep.forest=TRUE,
             minbucket=30)){
    
    #if(is.null(predictors.matrix)){predictors.matrix<-predictor.matrix.default(variables)}
    Sparameters<-plyr::alply(setdiff(variables,asis),1,function(variable){
      i=which(setdiff(variables,asis)==variable)
      #initialise entry
      splits=list()
      #class of the variable
      nextcondition="(TRUE)"
      presentind<-unlist(get_presentind(variable,variables))
      if (length(presentind)>0){
        splits<-c(splits,
                  list("notpresent"=list(
                    condition=eval(parse(text=paste0("quote(is.na(",presentind,") | ",presentind,"==0)"))),
                    predictors=NULL,
                    calculus=if(is.character(ref.table[[variable]])){quote(rep(NA,sum(selS)))
                    }else{
                      if(is.factor(ref.table[[variable]])){quote(rep(factor("missing",levels=levels(TtableA[[Sparameters_i$variable]])),sum(selS)))}else{quote(rep(0,sum(selS)))}},
                    method="calculus")))
        nextcondition<-paste(c(nextcondition,paste0("(!is.na(",presentind,")&(",presentind,"==1))")),collapse="&")
      }
      missingind<-unlist(get_missingind(variable,variables))
      if (length(missingind)>0){
        splits<-c(splits,
                  list("missing"=list(
                    condition=eval(parse(text=paste0("quote(",paste(c(nextcondition,paste0("(",missingind,"==1)")),collapse="&"),")"))),
                    predictors=NULL,
                    calculus=quote(rep(NA,sum(selS))),
                    method="calculus")))
        nextcondition=paste(c(nextcondition,paste0("(",missingind,"==0)")),collapse="&")
      }else{missingind<-0}
      parent<-get_parent(variable,variables)
      if(length(parent)!=0&parent!=""){
        splits<-c(splits,
                  list("parent"=list(
                    condition=eval(parse(text=paste0("quote(",paste(c(nextcondition,paste0("(is.na(",parent,")|(",parent,"==0))")),collapse="&"),")"))),
                    predictors=NULL,
                    calculus=quote(rep(0,sum(selS))),
                    method="calculus")))
        nextcondition=paste(c(nextcondition,paste0("(",parent,"!=0)")),collapse="&")
      }
      if(!is.null(moresplits[[variable]])){
        splits<-c(splits,if(!identical(presentind,1)|!identical(missingind,0)){moresplits[[variable]]}else{
          lapply(moresplits[[variable]],
                 function(Split){list(
                   condition=eval(parse(text=
                                          paste("quote(",
                                                paste(c(nextcondition,Split$condition),collapse=" & "),")"))),
                   calculus=Split$calculus,
                   synthparameters=if(!is.null(Split$synthparameters)){Split$synthparameters}else{defaultsynthparameters},
                   predictors=if(!is.null(Split$predictors)){Split$predictors}else{if(i==1){NULL}else{intersect(setdiff(colnames(predictors.matrix)[predictors.matrix[variable,]],c(asis,variable)),variables[1:i])}},
                   method=if(!is.null(Split$method)){Split$method}else{if(max(predictors.matrix[variable,])==0){"sample"}else{preferredmethod}})})})}
      if(is.null(moresplits[[variable]])){
        splitting=intersect(splittingvar,variables[1:i])
        if(length(splitting)==0){
          predictors=intersect(setdiff(colnames(predictors.matrix)[predictors.matrix[variable,]],c(asis,variable)),variables[1:i])
          splits<-c(splits,
                    list("nonmissingpresent"=list(
                      synthparameters=defaultsynparameters,
                      condition=
                        eval(parse(text=paste0("quote(",nextcondition,")"))),
                      predictors=predictors,
                      method=if(length(predictors)>0){preferredmethod}else{"sample"})))
        }else{
          remaining<-with(ref.table,eval(eval(parse(text=paste0("quote(",nextcondition,")")))))
          if(any(remaining)){
            #toto<-unique(ref.table[remaining,splitting])
            tata<-plyr::ddply(ref.table[remaining,],splitting,function(x){data.frame(count=nrow(x))})
            tata<-tata[order(tata$count,decreasing = TRUE),]
            tata2<-tata[tata$count>splithreshold,]
            nextcondition2<-nextcondition
            if(nrow(tata2)>1){
              for (j in 1:nrow(tata2)){
                coco<-sapply(1:(ncol(tata2)-1),function(ii){
                  coucou<-character(0)
                  if(is.character(tata2[j,ii])){coucou<-"'"}
                  if(is.factor(tata2[j,ii])){
                    if(!is.null(levels(tata2[j,ii]))){
                      if(is.character(levels(tata2[j,ii]))){coucou<-"'"}}}
                  paste0("(",names(tata2)[ii],"==",coucou,tata2[j,ii],coucou,")")})
                notcoco<-paste0("!(",coco,")")
                splits<-c(splits,
                          list(list(
                            synthparameters=defaultsynparameters,
                            condition=eval(parse(text=paste("quote(",paste(c(nextcondition,coco),collapse=" & "),")"))),
                            predictors=intersect(setdiff(colnames(predictors.matrix)[predictors.matrix[variable,]],c(asis,variable)),variables[1:i]),
                            method=if(max(predictors.matrix[variable,])==0){"sample"}else{preferredmethod})))
                nextcondition2<-paste(c(nextcondition2,notcoco),collapse=" & ")
              }}
            if(any(tata$count<=splithreshold)){
              splits<-c(splits,
                        list(list(
                          synthparameters=defaultsynparameters,
                          condition=eval(parse(text=paste("quote(",nextcondition2,")"))),
                          predictors=intersect(setdiff(colnames(predictors.matrix)[predictors.matrix[variable,]],c(asis,variable)),variables[1:i]),
                          method=if(max(predictors.matrix[variable,])==0){"sample"}else{preferredmethod})))
              
            }
          }
          #          splits<-c(splits,
          #                    plyr::dlply(toto,.variables = names(toto),
          #                                function(d){
          #                                  coco<-sapply(1:ncol(toto),function(i){
          #                                    coucou<-character(0)
          #                                    if(is.character(d[1,i])){coucou<-"'"}
          #                                    if(is.factor(d[1,i])){
          #                                      if(!is.null(levels(d[1,i]))){
          #                                        if(is.character(levels(d[1,i]))){coucou<-"'"}}}
          
          #                                    paste0("(",names(toto)[i],"==",coucou,d[1,i],coucou,")")})
          #                                  list(
          #                                    synthparameters=defaultsynparameters,
          #                                    condition=eval(parse(text=paste("quote(",paste(c(nextcondition,coco),collapse=" & "),")"))),
          #                                    predictors=intersect(setdiff(colnames(predictors.matrix)[predictors.matrix[variable,]],c(asis,variable)),variables[1:i]),
          #                                    method=if(max(predictors.matrix[variable,])==0){"sample"}else{preferredmethod})},.parallel=TRUE))
        }
      }
      if(is.null(names(splits))){names(splits)<-1:length(splits)}
      if(any(names(splits)=="")){names(splits)[names(splits)==""]<-setdiff(paste0(1:length(splits),""),names(splits))[1:sum(names(splits)=="")]}
      list(
        variable=variable,
        rank=i,
        splits=splits)
    }
    )
    names(Sparameters)<-setdiff(variables,asis)
    Sparameters
  }


#' Recoding of NAs to 0 or "NA"
#' 
#' @param tableA a dataframe
#' @return a dataframe  
#' @details for synthetisation to run, missing values are treated as a special factor level for factor variables,
#' or as 0 for continuous variables. To avoid issues, for continuous variables, a missing indicator is also created.
#' @examples
#' toto<-cars
#' toto$speed[sample(nrow(cars),3)]<-NA
#' NAto0(toto)
NAto0<-function(tableA){
  toreplace<-sapply(tableA,inherits,what = "numeric")
  logicalV<-sapply(tableA,inherits,what = "logical")
  factorV<-sapply(tableA,inherits,what = "factor")
  if(length(toreplace)==0){toreplace<-FALSE}
  if(length(logicalV)==0){logicalV<-FALSE}
  if(any(toreplace)){tableA[,toreplace][is.na(tableA[toreplace])]<-0}
  if(any(logicalV)){tableA[,logicalV][is.na(tableA[logicalV])]<-FALSE}
  #if(any(factorV)){tableA[,factorV][is.na(tableA[factorV])]<-0}
  tableA}

#' Converts all posixct variables of a dataframe into a numeric variable
#' 
#' @param tableA a dataframe
#' @return a list.  
#' @examples
#' toto<-cars
#' toto$now<-Sys.time()
#' posixcttonumeric(toto)
posixcttonumeric<-function(tableA){
  variables<-names(tableA)[sapply(tableA,function(x){inherits(x, "POSIXct")|inherits(x,"POSIXt")})]
  for(variable in variables){
  tableA[[variable]]<-as.numeric(tableA[[variable]])}
  tableA
}

