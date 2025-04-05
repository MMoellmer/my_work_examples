library(mosaic)
library(pander)
library(gplots)
library(ggplot2)

#Do Sage Thrashers, Meadowlarks, and Brewer's Sparrows occur in equal abundance?

#Bar plot visualizing data
ggplot(data = Bird_count, aes(x = Species, y = Count)) + geom_bar(stat = "identity")

#Chi-squared goodness of fit test, compares things to determine if they occur in an expected proportion
Bc <- c(540, 622, 649)
res_1 <- chisq.test(Bc, p = c(1/3, 1/3, 1/3))
res_1
#P-value less than 0.05, therefore the birds are not equally common in abundance
#X-squared is best when closer to one.  Signifies how well the data fits the expected proportions. 
#in this case x^2 reaffirms the p-value in that the data are not occuring in the expected proportion



#Do different covered flowers of the same species grow in equal proportion if the proportion is 3:2:1 for red, yellow, and blue wildflowers?

#Bar plot data re-organization and visualization 
Plant_color$Species <- factor(Plant_color$Species, levels = c("Red milkvetch", "Yellow milkvetch", "Blue milkvetch"))

ggplot(data = Plant_color, aes(x = Species, y = Count)) + geom_bar(stat = "identity")

#Chi-square goodness of fit test
Fc <- c(113, 64, 33)
res_2 <- chisq.test(Fc, p = c(1/2, 1/3, 1/6))
res_2
#p-value greater than 0.05, and x^2 value close to 1, therefore the covered flowers occur in equal proportion to the expected proportion




