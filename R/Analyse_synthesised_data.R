#' For each leave of each tree, counts the number of donors in the gold set  
#' vs the number of receptors in the synthetic dataset.
#' 
#' @param Rules a named list (names correspond to the gold.data and syn.data variable names) of lists of logical expressions (for each variable, the logical expression corresponds to a terminal leaf in a tree).
#' @param gold.data a data frame 
#' @param syn.data a data frame containing the same variables than gold.data
#' @examples
#' data(TtableA,package="BigSyn")
#' STtableA1=BigSyn::SDPSyn2(TtableA,asis=c("id1","id2"))
#' data(STtableA1,package="BigSyn")
#' data(package="BigSyn")
#' donors.receptors.check()


donors.receptors.check<-
  function(Rules,
          gold.data,
          syn.data){
  NULL}