#' `%notin%` 
#' @description negation de `%in%`
#'@examples
#' 1%notin%2:3
#' 1%notin%1:3

`%notin%` <- Negate(`%in%`)

#' daniRules
#' @param x same format than output of partykit::ctree
#' @param i (default: NULL)
#' @returns for each terminal node of the tree, give the definition. For example: node 11 corresponds to 'x>1 and y in ("a","b")' 
#' @description the rules for the tree. Add an extra rule. For example, if the left branch rule is X in (a,b,c) and the right branch rule is X in (e,f),
#' the right branch rule is modified by X not in (a,b,c). The extreme right branch rule is replaced by the negation of any of the other branches, for each node. 
#' @details Adapted from partykit:::.list.rules.party
#'@examples
#' x=partykit::ctree(Petal.Width~.,iris)
#' daniRules(x)   
daniRules<-function (x, i = NULL, ...){
  if (is.null(i)) 
    i <- partykit::nodeids(x, terminal = TRUE)
  if (length(i) > 1) {
    ret <- sapply(i, daniRules, x = x)
    names(ret) <- if (is.character(i)) 
      i
    else names(x)[i]
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) 
    i <- which(names(x) %in% i)
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i <- as.integer(i)
  dat <- partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) 
      dat <- x$data
  }
  else {
    fit <- NULL
    dat <- x$data
  }
  rule <- c()
  recFun <- function(node) {
    if (partykit::id_node(node) == i) {return(NULL)}
    kid <- sapply(partykit::kids_node(node), partykit::id_node)
    whichkid <- max(which(kid <= i))#length(kid[kid<=i])
    split <- partykit::split_node(node)
    ivar <- partykit::varid_split(split)
    svar <- names(dat)[ivar]
    index <- partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) {index <- ((1:nlevels(dat[, svar])) > partykit::breaks_split(split)) + 1}
      
      
      slevels <- levels(dat[, svar])[index == whichkid]
      complementslevels <- levels(dat[, svar])[index != whichkid]
      directsrule <- paste(svar, " %in% c(\"", paste(slevels, 
                                                     collapse = "\", \"", sep = ""), "\")", sep = "")
      complementsrule<- paste(svar, " %notin% c(\"", paste(complementslevels, 
                                                           collapse = "\", \"", sep = ""), "\")", sep = "")
      if(whichkid==2){srule<-complementsrule}else{srule<-directsrule}
    }  else {
      if (is.null(index)){ index <- 1:length(kid)}
      breaks <- cbind(c(-Inf, partykit::breaks_split(split)), 
                      c(partykit::breaks_split(split), 
                        Inf))
      sbreak <- breaks[index == whichkid, ]
      right <- partykit::right_split(split)
      srule <- c()
      if (is.finite(sbreak[1])) 
        srule <- c(srule, paste(svar, ifelse(right, ">", 
                                             ">="), sbreak[1]))
      if (is.finite(sbreak[2])) 
        srule <- c(srule, paste(svar, ifelse(right, "<=", 
                                             "<"), sbreak[2]))
      srule <- paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node <- recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}
