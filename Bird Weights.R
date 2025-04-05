#Are the weights of robins different by study site? (lake, river, gardens)
  #linear model: Weight = Site(x=y)

library(ggpubr)
library(car)
library(pander)
library(mosaic)

#organizing data
head(ANOVA_Birds)
ANOVA_Birds$Site <- ordered(ANOVA_Birds$Site, levels = c("river", "gardens", "lake"))
levels(ANOVA_Birds$Site)

#summary of statistics
pander(favstats(Weight ~ Site, data = ANOVA_Birds))


#generating boxplot
ggboxplot(ANOVA_Birds, x = "Site", y = "Weight", 
          color = "Site", palette = c("blue", "darkgreen", "darkblue"))

#determine if parametric or non-parametric
Bords <- aov(Weight ~ Site, data = ANOVA_Birds)
summary(Bords)

plot(Bords, 2)
res.bord <- residuals(object = Bords)
shapiro.test(x = res.bord)
#shapiro test has p-value less than 0.05, therefore non-parametric

#determine homogeneity of variances
leveneTest(Weight ~ Site, data = ANOVA_Birds)
#p-value is less than 0.05, therefore do not assume homogeneity

#Non-parametric ANOVA test
kruskal.test(Weight ~ Site, data = ANOVA_Birds)

#Confirmed significance difference between groups, but which pairs of groups are different?
#multiple pairwise-comparison between groups
pairwise.wilcox.test(ANOVA_Birds$Weight, ANOVA_Birds$Site, p.adjust.method = "BH")
#Confirmed they are each different to each other. 

