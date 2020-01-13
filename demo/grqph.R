if(FALSE){
  #Needed packages
install.packages("ggdendro")
install.packages("cowplot")
install.packages("bezier")
#
library(cowplot)
library("ggdendro")
library(ggplot2)
library(rpart)
library(gridExtra)
library(bezier)
#Create data
}
N<-10000
dd<-data.frame(
  sat=ceiling(sample(400:1600,N,replace=TRUE)),
  Gender =sample(as.factor(c("A","B")),N,replace=TRUE))

dd$wage<- ((dd$sat/200)*(dd$Gender=="A"))+2*(dd$Gender=="B")+rnorm(N) 


model<-wage~Gender+sat

# Tree

tree.rpart<-rpart(model,dd)

ddata <- dendro_data(tree.rpart)
ggplot.tree<-  ggplot() + 
  geom_segment(data = ddata$segments, 
               aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = ddata$labels, 
            aes(x = x, y = y, label = label), size = 3, vjust = 0) +
  geom_text(data = ddata$leaf_labels, 
            aes(x = x, y = y, label = label), size = 3, vjust = 1) +
  theme_dendro()


#Add graphs manually


ggde<-function(selection,title="",selection1=NULL,.dd=dd){
  if(!is.null(selection1)){.dd$x=0;.dd$x[selection1]=1}
  p<-ggplot(.dd[selection,], aes(x = wage))
  if(!is.null(selection1)){p<-p+
    geom_density(data = .dd[selection&(.dd$x==1),],color="gray",aes(x=wage))+
    geom_density(data = .dd[selection&(.dd$x==0),],color="gray",aes(x=wage))}
  p+    theme_bw()+
    geom_density()+ 
    theme(axis.text = element_blank(),
          axis.ticks=element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major =element_blank(),
          panel.grid.minor =element_blank())+
    scale_x_continuous(limits = range(dd$wage))+
    scale_y_continuous(limits = c(0,.4))+
      xlab(title)+ theme(legend.position="none")}

ggde(dd$Gender==dd$Gender,"All population",dd$Gender=="A")


gg1=ggde(dd$Gender==dd$Gender,"All population",dd$Gender=="A")
gg2=ggde(dd$Gender=="B","Gender=B")
gg3=ggde(dd$Gender=="A","Gender=A",dd$sat>1014)
gg4=ggde(dd$Gender=="A"&dd$sat>1014,"SAT>1014")
gg5=ggde(dd$Gender=="A"&dd$sat<=1014,"SAT<1014")


gg6=gg5
X=matrix(c( 8,23,
           1,12,
           16,12,
            12,1,
            22,1),5,2,byrow=TRUE)

Arrows=list(c(1,2),c(1,3),c(3,4),c(3,5))
pPPP = lapply(Arrows,function(i){list(x = c(X[i[1],1]+4,X[i[2],1]+4), 
         y = c(X[i[1],2], X[i[2],2]+8))})



toto<-ggplot(data.frame(a=1)) + xlim(1, max(X[,1]+8)) + ylim(1, max(X[1,]+8)) +
  annotation_custom(ggplotGrob(gg1), xmin = X[1,1], xmax = X[1,1]+8, ymin = X[1,2], ymax = X[1,2]+8) +
  annotation_custom(ggplotGrob(gg2), xmin = X[2,1], xmax = X[2,1]+8, ymin = X[2,2], ymax = X[2,2]+8) +
  annotation_custom(ggplotGrob(gg3), xmin = X[3,1], xmax = X[3,1]+8, ymin = X[3,2], ymax = X[3,2]+8) +
  annotation_custom(ggplotGrob(gg4), xmin = X[4,1], xmax = X[4,1]+8, ymin = X[4,2], ymax = X[4,2]+8) +
  annotation_custom(ggplotGrob(gg5), xmin = X[5,1], xmax = X[5,1]+8, ymin = X[5,2], ymax = X[5,2]+8) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = pPPP[[1]])),aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = pPPP[[2]])),aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = pPPP[[3]])),aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = pPPP[[4]])),aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  theme(rect = element_blank(),
        line = element_blank(),
        text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"))


png("toto.png")
print(toto)
dev.off()
pdf("toto.pdf")
print(toto)
dev.off()



