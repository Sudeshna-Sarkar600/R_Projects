data(mtcars)
head(mtcars)
newdata<-mtcars[,c(1:7,10,11)]
head(newdata)

#Scatter plot and Correlation
library("GGally")
ggpairs(newdata)

#Principle component analysis
pc<-prcomp(newdata,center=TRUE,scale=TRUE)
summary(pc)
attributes(pc)
print(pc)

var1<-round(pc$sdev[1]^2/sum(pc$sdev^2)*100,2)
var2<-round(pc$sdev[2]^2/sum(pc$sdev^2)*100,2)

#Scree plot
plot(pc)
screeplot(x=pc,type="line",main="Scree plot")
library("factoextra")
fviz_eig(pc)

#PCA plot
#Using Biplot
biplot(pc)
biplot(pc,cex=0.5)

#Using Autoplot
library("ggfortify")
autoplot(pc)
autoplot(pc,scale=0)

mtcars$vs<-factor(mtcars$vs)
mtcars$am<-factor(mtcars$am)
autoplot(pc,data=mtcars,colour="vs")
autoplot(pc,data=mtcars,colour="am")

autoplot(pc,data=mtcars,colour="am",
         loadings=TRUE,loadings.colour="green",
         loading.label=TRUE,
         loadings.label.size=3) 

fviz_pca_ind(pc,
             col.ind="cos2",
             gradient.cols=c("#00AFBB","#E7B801","#FC4E07"),
             repel=TRUE)

fviz_pca_var(pc,
             col.var="contrib",
             gradient.cols=c("#00AFBB","#E7B801","#FC4E07"),
             repel=TRUE)

fviz_pca_biplot(pc,
             repel=TRUE,
             col.var="#2E9FDF",
             col.ind="#696969")

mtcars$am<-factor(mtcars$am)
fviz_pca_ind(pc,
             col.ind=mtcars$am,
             palette=c("red", "blue"),
             addEllipses=TRUE,
             legend.title="am",
             xlab=paste("PC1(",62.84,"%)",sep=""),
             ylab=paste("PC2(",23.13,"%)",sep=""),
             repel=TRUE)

