library(ggplot2)
# IRIS Datasets

iris = iris
names(iris)

ggplot(iris,aes(Petal.Width,Petal.Length)) +
  geom_point()


ggplot(iris,aes(Petal.Width,Petal.Length)) +
  geom_point(color='darkgreen',size=4) +
  stat_smooth(method = 'lm')

plot(iris$Petal.Length,iris$Sepal.Width)

# step 1 -- creating base plot

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width))

  

# step 2 -- adding geom points to the plot

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width)) +
  geom_point()

# step 3 -- adding color aesthetics

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width,col=Species)) +
  geom_point()

# Step 4 -- adding size aesthetic

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width,col=Species,
                size=Petal.Width)) +
  geom_point()

# step 5 -- adding shape aesthetic

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width,
                col=Species,size=Petal.Width,
                shape=Species)) +
  geom_point()

# Step 6 -- adding transperancy aesthetic

ggplot(iris,aes(x=Petal.Length,y=Sepal.Width,
                col=Species,size=Petal.Width,
                shape=Species,
                alpha=Sepal.Length)) +
  geom_point()


# Bar and Boxplot

ggplot(iris,aes(Species)) +
  geom_bar()

ggplot(iris,aes(Species,Sepal.Width,fill=Species)) +
  geom_bar(stat = 'summary',fun='mean')


ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='blue')


ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='#fff000')


ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='#fff000',
           col='black')

ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='#fff000',
           col='black') +
  geom_point()


ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='#fff000',
           col='black') +
  geom_point(position = position_jitter())


myPlot <- ggplot(iris,aes(Species,Sepal.Width)) +
  geom_bar(stat = 'summary',fun='mean',fill='#a134eb',
           col='black') +
  geom_point(position = position_jitter(0.2),
             size = 3,shape='o')


myPlot + theme(panel.grid = element_blank())


myPlot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = 'white'))

myPlot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = 'white'),
               axis.line.y = element_line(colour = 'black',size=0.2),
               axis.line.x  = element_line(color = 'black',size=0.2))



myPlot + theme(panel.grid = element_blank(),
               panel.background = element_rect(fill = 'white'),
               panel.border = element_rect(colour = 'black',fill=NA,size=0.2))



myPlot
myPlot + theme_bw()
myPlot + theme_classic()
myPlot + theme_dark()
myPlot + theme_gray()
myPlot + theme_light()
myPlot + theme_linedraw() + theme(panel.background = element_rect(fill = 'green'))
myPlot + theme_minimal()



ggplot(iris,aes(Species,Sepal.Length)) + 
  geom_boxplot(fill ='#e234eb',col='black',notch = TRUE) +
  geom_point()

ggplot(iris,aes(Species,Sepal.Length)) + 
  geom_point() +
  geom_boxplot(fill ='#e234eb',col='black',notch = TRUE)


myPlot + theme_classic() +
  labs(x = '',y='Sepal Length (mm)') +
  ggtitle('Sepal length by iris species') +
  theme(plot.title = element_text(hjust = 0.5))


# saving out plot
ggsave('Graph1.pdf',width = 8,height = 5)


# FACTORIAL

data("ToothGrowth")
head(ToothGrowth)
summary(ToothGrowth)

ggplot(ToothGrowth,aes(supp,len,fill=as.factor(dose))) +
  geom_bar(stat = 'summary',fun='median',col='black',
           position = 'dodge') +
  geom_point(position = position_dodge(0.9))


ggplot(ToothGrowth,aes(as.factor(dose),len,group=supp,col=supp)) +
  geom_line(stat='summary',fun='mean') +
  geom_smooth(method = 'lm')
