# NEW wrapper function for fitting a clustered tree

fitmodel.fn.new<-function(method,x,y,treeplotsavepath=NULL,...){
  do.call((get(paste0("fitmodel.",method,".new"))),
          c(list(x=x,
                 y=y,
                 y.name = y.name,
                 random = random, 
                 lgmodel = lgmodel,
                 rslope = rslope,
                 id,
                 treeplotsavepath=treeplotsavepath),
            good.fit.parameters(method,list(...))))}


fitmodel.fn.new(method="ctree",x = mydata[, 1:9], y = mydata$bscore, 
                y.name = "bscore",
                random = "schoolid", 
                lgmodel = "slope",
                id = mydata$schoolid,
                rslope = "+ female + sclass", nbuckets=30, tutu="not a good argument")