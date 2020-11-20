## To read the CSV File ##
D2C<-read.csv(file.choose(),header = TRUE)

##Dimensions of the data set
dim(D2C)

#To understand the Class of the data set
class(D2C)

#To view the number of rows
nrow(D2C)

#Number of columns in the data set
ncol(D2C)

#To view the column names in the data set
names(D2C)

#To view the structure of the data set
str(D2C)

#To view the Summary
summary(D2C)

#To view the first 6 rows of the column
head(D2C)

#To view the Last 6 rows of the column
tail(D2C)



#To remove the "?" in the given data set 
idx<-D2C=="?"
is.na(D2C)<-idx

#To view the excel form of the data set 
View(D2C)

#To check whether the variables are categorical or continuous
table(D2C$symboling)
table(D2C$normalized.losses)
table(D2C$make)
table(D2C$fuel.type)
table(D2C$aspiration)
table(D2C$num.of.doors)
table(D2C$body.style)
table(D2C$drive.wheels)
table(D2C$engine.location)
table(D2C$wheel.base)
table(D2C$length)
table(D2C$width)
table(D2C$height)
table(D2C$curb.weight)
table(D2C$engine.type)
table(D2C$num.of.cylinders)
table(D2C$engine.size)
table(D2C$fuel.system)
table(D2C$bore)
table(D2C$stroke)
table(D2C$compression.ratio)
table(D2C$horsepower)
table(D2C$peak.rpm)
table(D2C$city.mpg)
table(D2C$highway.mpg)
table(D2C$price)


#To find out the presence of the NA values
any(is.na(D2C))

#To find out the precise location of the NA values
colSums(is.na(D2C))

#To view the NA values using a plot
library(DataExplorer)
plot_missing(D2C)

#To convert the continuous variables NA values into median
D2C$horsepower[is.na(D2C$horsepower)] <-median(D2C$horsepower[!is.na(D2C$horsepower)])
D2C$peak.rpm[is.na(D2C$peak.rpm)]<-median(D2C$peak.rpm[!is.na(D2C$peak.rpm)])
D2C$price[is.na(D2C$price)] <-median(D2C$price[!is.na(D2C$price)])
D2C$stroke[is.na(D2C$stroke)] <-median(D2C$stroke[!is.na(D2C$stroke)])
D2C$bore[is.na(D2C$bore)] <-median(D2C$bore[!is.na(D2C$bore)])

D2C$normalized.losses<-as.numeric(D2C$normalized.losses)

D2C$normalized.losses[is.na(D2C$normalized.losses)]<-median(D2C$normalized.losses[!is.na(D2C$normalized.losses)])

#To convert the categorical variables NA values into mode
mode<-function(x){
  uniq<-unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}

mode<-mode(D2C$num.of.doors)
mode
D2C$num.of.doors[is.na(D2C$num.of.doors)]<-mode 

View(D2C)

# Correlation Plot:
plot_correlation(D2C)

#Regression analysis
#Removing the insignificant variables
View(D2C)
D2C<-D2C[-1]
D2C<-D2C[-1]
D2C<-D2C[-2]
D2C<-D2C[-3]
View(D2C)
D2C<-D2C[-3]
D2C<-D2C[-8]
View(D2C)
D2C<-D2C[-10]
D2C<-D2C[-11]
View(D2C)
D2C<-D2C[-11]
View(D2C)
D2C<-D2C[-12]
D2C<-D2C[-12]
View(D2C)
D2C<-D2C[-13]
View(D2C)
D2C<-D2C[-13]
View(D2C)

#Splitting of data
set.seed(74)
s<-sample(c(1:205),size = 164)
D2C.dev<-D2C[s,]    #training data set
D2C.holdout<-D2C[-s,]   #testing data set

#structure of the data set after splitting 
str(D2C)

#ggplot for better vizualization of data
ggplot(data=D2C)+geom_bar(mapping =aes(x=make,fill=make))


#ggplot with point
ggplot(data=D2C)+geom_point(mapping = aes(x=D2C$make,y=D2C$price))

# Data Visualization
#Overplotting using jitter
ggplot(data = D2C)+geom_point(mapping = aes(x=make,y=D2C$peak.rpm),position = "jitter")

#line type 
ggplot(data = D2C)+geom_smooth(mapping = aes(x=D2C$wheel.base,y=D2C$peak.rpm,linetype=D2C$aspiration))

#Scatter plot
ggplot(data=D2C)+geom_point(mapping = aes(x=price,y=engine.size,colour=make))

#Scatter plot with size:
ggplot(data=D2C)+geom_point(mapping = aes(x=D2C$length,y=price,size=D2C$stroke))

#Scatter plot with size:
ggplot(data=D2C)+geom_point(mapping = aes(x=length,y=price,colour=stroke))

#Facets splits the plot
ggplot(data=D2C)+geom_point(mapping = aes(x=price,y=length))+facet_wrap(~engine.type,nrow=2)

#Facets splits the plot
ggplot(data=D2C)+geom_point(mapping = aes(x=price,y=D2C$wheel.base))+facet_wrap(~engine.type,nrow=2)

#Facets splits the plot
ggplot(data=D2C)+geom_point(mapping = aes(x=price,y=engine.size))+facet_wrap(~engine.type,nrow=2)

#Facets splits the plot
ggplot(data=D2C)+geom_point(mapping = aes(x=price,y=length))+facet_wrap(~make,nrow=3)

#facet grid
ggplot(data=D2C)+geom_point(mapping = aes(x=peak.rpm,y=price))+facet_grid(aspiration~engine.type)

#coordination system and boxplot
ggplot(data=D2C,mapping = aes(x=make,y=price))+geom_boxplot()+coord_flip()

#coordination system and boxplot
ggplot(data=D2C,mapping = aes(x=make,y=length))+geom_boxplot()+coord_flip()

#coordination system and boxplot
ggplot(data=D2C,mapping = aes(x=make,y=D2C$engine.size))+geom_boxplot()+coord_flip()

#violin plot:
D2C$make=with(D2C,reorder(make,price,median))
ggplot(data = D2C,mapping = aes(x=engine.type,y=engine.size,fill=make))+geom_violin()

install.packages("ggridges")
library(ggridges)
ggplot(D2C,aes(x=price,y=D2C$engine.location,fill=make))+
  geom_density_ridges()+
  theme_ridges()+
  theme()

ggplot(D2C,aes(x=price,y=D2C$engine.location,fill=D2C$engine.location))+
  geom_density_ridges()+
  theme_ridges()+
  theme()

ggplot(D2C,aes(x=price,y=D2C$make,fill=D2C$stroke))+
  geom_density_ridges()+
  theme_ridges()+
  theme()

ggplot(D2C,aes(x=D2C$peak.rpm,y=D2C$engine.location,fill=D2C$engine.type))+
  geom_density_ridges()+
  theme_ridges()+
  theme()