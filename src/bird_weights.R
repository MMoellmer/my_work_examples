#Are the weights of robins different by study site? (lake, river, gardens)
  #linear model: Weight = Site(x=y)

library(ggpubr)
library(car)
library(pander)
library(mosaic)

#organizing data
head(bird_weights)
bird_weights$Site <- ordered(bird_weights$Site, levels = c("river", "gardens", "lake"))
levels(bird_weights$Site)

#summary of statistics
pander(favstats(Weight ~ Site, data = bird_weights))


#generating boxplot
ggboxplot(bird_weights, x = "Site", y = "Weight", 
          color = "Site", palette = c("blue", "darkgreen", "darkblue"))

#determine if parametric or non-parametric
Bords <- aov(Weight ~ Site, data = bird_weights)
summary(Bords)

plot(Bords, 2)
res.bord <- residuals(object = Bords)
shapiro.test(x = res.bord)
#shapiro test has p-value less than 0.05, therefore non-parametric

#determine homogeneity of variances
leveneTest(Weight ~ Site, data = bird_weights)
#p-value is less than 0.05, therefore do not assume homogeneity

#Non-parametric ANOVA test
kruskal.test(Weight ~ Site, data = bird_weights)

#Confirmed significance difference between groups, but which pairs of groups are different?
#multiple pairwise-comparison between groups
pairwise.wilcox.test(bird_weights$Weight, bird_weights$Site, p.adjust.method = "BH")
#Confirmed they are each different to each other. 

