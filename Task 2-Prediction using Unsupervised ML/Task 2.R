#installing packages 
install.packages('tidyverse')       
install.packages('cluster')         
install.packages("reshape")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(grid)
library(cluster)
 
           


#importing and exploring  the dataset
iris_d<-read.csv("C:\\GRIP\\Iris.csv")
iris_df<-data.frame(iris_d)
mydata <- iris_df
head(mydata)
View(mydata)
names(mydata)



#visualizing the dataset
#Let us visualize the data now with ggplot2

#Sepal-Length vs. Sepal-Width
ggplot(mydata)+geom_point(aes(x = SepalLengthCm, y = SepalWidthCm), stroke = 2)+facet_wrap(~ Species)+labs(x = 'Sepal Length', y = 'Sepal Width')+theme_linedraw()

#Petal-Length vs. Petal-Width
ggplot(mydata)+geom_point(aes(x = PetalLengthCm, y = PetalWidthCm), stroke = 2)+facet_wrap(~ Species)+ labs(x = 'Petal Length', y = 'Petal Width')+theme_gray()

#Sepal-Length vs. Petal-Length
ggplot(mydata)+geom_point(aes(x = SepalLengthCm, y = PetalLengthCm), stroke = 2)+facet_wrap(~ Species)+ labs(x = 'Sepal Length', y = 'Petal Length')+ theme_bw()

#Sepal-Width vs. Pedal-Width
ggplot(mydata)+geom_point(aes(x = SepalWidthCm, y = PetalWidthCm), stroke = 2)+ facet_wrap(~ Species)+ labs(x = 'Sepal Width', y = 'Pedal Width')+theme_bw()


#Box plots

#sepal.length
BpSl <- ggplot(mydata)+geom_boxplot(aes(x = Species, y = SepalLengthCm, fill = Species))+theme_bw()

#sepal.width
BpSw <- ggplot(mydata)+geom_boxplot(aes(x = Species, y = SepalWidthCm, fill = Species))+theme_bw()

#petal.length
BpPl <- ggplot(mydata)+geom_boxplot(aes(x = Species, y = PetalLengthCm, fill = Species))+theme_bw()

#petal.width
BpPw <- ggplot(mydata)+geom_boxplot(aes(x = Species, y = PetalWidthCm, fill = Species))+theme_bw()

grid.arrange(BpSl  + ggtitle(""),
             BpSw  + ggtitle(""),
             BpPl + ggtitle(""),
             BpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Box Plot", 
                            gp=gpar(fontsize=15))
)


#volin plots

#sepal length
VpSl <- ggplot(mydata, aes(Species, SepalLengthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Sepal Length", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

#sepal width
VpSw <- ggplot(mydata, aes(Species, SepalWidthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Sepal Width", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

#petal length
VpPl <-ggplot(mydata, aes(Species, PetalLengthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Petal Length", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  theme(legend.position="none")

#petal width
VpPw <-  ggplot(mydata, aes(Species, PetalWidthCm, fill=Species)) + 
  geom_violin(aes(color = Species), trim = T)+
  scale_y_continuous("Petal Width", breaks= seq(0,30, by=.5))+
  geom_boxplot(width=0.1)+
  labs(title = "Iris Box Plot", x = "Species")


#Ploting all visualizations
grid.arrange(VpSl  + ggtitle(""),
             VpSw  + ggtitle(""),
             VpPl + ggtitle(""),
             VpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Violin Plot", 
                            gp=gpar(fontsize=15))
)



#k-means Clustering


#Find the optimal number of clusters by Elbow Method
set.seed(123)
k.max <- 10

wcss <- vector()
for (i in 1:k.max) wcss[i] <- sum(kmeans(mydata[, -6], i)$withinss)
plot(1:k.max, wcss, type = 'b',  main = paste('The Elbow Method'),xlab = 'Number of Clusters',ylab = 'WCSS')
#the elbow point : k(centers) = 3



#Apply kmeans function to the feature columns
set.seed(123)
km <- kmeans( x = mydata[ ,3:4] , centers = 3)

yclus <- km$cluster
table(yclus)


#Visualize the kmeans clusters
clusplot(mydata[,-6],yclus,lines=0,shade = TRUE,color = TRUE, labels = 0, plotchar = FALSE, span = TRUE, main = paste('Clusters of Iris Flowers'))


#Comparing the clusters
mydata$cluster.kmean <- yclus
cm <- table(mydata$Species, mydata$cluster.kmean)
cm

#Scatter plots to view Species & kmeans custers
mydata$cluster.kmean <- as.factor(mydata$cluster.kmean)

# Sepal-Length vs. Sepal-Width (Species)
sepal <- ggplot(mydata)+geom_point(aes(x = SepalLengthCm, y = SepalWidthCm, color = Species) , size = 4)+ labs(x = 'Sepal Length', y = 'Sepal Width')+ggtitle("Species")+theme_bw()

# Sepal-Length vs. Sepal-Width (kmeans cluster)
sepal_cluster <-ggplot(mydata)+geom_point(aes(x = SepalLengthCm, y = SepalWidthCm,  color = cluster.kmean) , size = 4)+ labs(x = 'Sepal Length', y = 'Sepal Width')+ggtitle("kmeans Cluster")+theme_bw()

grid.arrange(sepal + ggtitle(""),
             sepal_cluster + ggtitle(""),
             nrow = 2,
             top = textGrob("comparision of sepals", 
                            gp=gpar(fontsize=15))
)

# Petal-Length vs. Petal-Width (Species)
petal <- ggplot(mydata)+geom_point(aes(x = PetalLengthCm, y = PetalWidthCm,color = Species) , size = 3)+ labs(x = 'Petal Length', y = 'Petal Width')+ggtitle("Species")+theme_bw()

# Petal-Length vs. Petal-Width (kmeans cluster)
petal_cluster <- ggplot(mydata)+ geom_point(aes(x = PetalLengthCm, y = PetalWidthCm, color = cluster.kmean) , size = 3)+   labs(x = 'Petal Length', y = 'Petal Width')+ ggtitle("kmeans Cluster")+theme_bw()
 
grid.arrange(petal + ggtitle(""),
             petal_cluster + ggtitle(""),
             nrow = 2,
             top = textGrob("comparision of petal", 
                            gp=gpar(fontsize=15))
)
