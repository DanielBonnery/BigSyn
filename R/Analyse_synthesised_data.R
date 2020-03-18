#' For each leave of each tree, counts the number of donors in the gold set  
#' vs the number of receptors in the synthetic dataset.
#' 
#' @param Rules a named list (names correspond to the gold.data and syn.data variable names) of lists of logical expressions (for each variable, the logical expression corresponds to a terminal leaf in a tree).
#' @param gold.data a data frame 
#' @param syn.data a data frame containing the same variables than gold.data
#' @examples
#' data(TtableA,package="BigSyn")
#' STtableA1=BigSyn::SDPSYN2(TtableA,
#'                           asis=c("id1a","id1b"),
#'                           fitmodelsavepath=tempdir())
#' load(file.path(tempdir(),"AA.cont2_La_Lc_Lrn1.rda"))
#' Rules<-Sparameters_i$splits[[3]]$fit.model$Rules
#' data(STtableA1,package="BigSyn")
#' data(package="BigSyn")
#' donors.receptors.check(Rules,TtableA,STtableA1[[1]])
#' 
#' ##                                 condition gold syn
#' ## 1 AA.cont1_La_Lc_Lrn1 <= 1.63041503938133  113 112
#' ## 2  AA.cont1_La_Lc_Lrn1 > 1.63041503938133    7   3
donors.receptors.check<-
  function(Rules,
          gold.data,
          syn.data){
  plyr::ddply(Rules,~condition,function(d){
    c(gold=with(gold.data, sum(eval(parse(text=d$condition)),na.rm=T)),
      syn =with(syn.data,  sum(eval(parse(text=d$condition)),na.rm=T)))})
  }
