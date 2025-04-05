#All information found here can be found on sthda.com

library(ggplot2)
library(dplyr)
library(nycflights13)
library(knitr)
library(tidyr)
library(moderndive)
library(tidyverse)
library(fivethirtyeight)
library(pander)
library(mosaic)
library(ggpubr)
library(car)

#this is used to view data in various ways
View(flights)
glimpse(flights) #puts identification variables in the upper left of the table
kable(airlines)

#this shows a specific subset of the data using $ 
airlines$name


#A statistical graphic is a mapping of data variables to aesthetic attributes of geometric objects

#SCATTERPLOT

#for scatterplots name the data, the x and y axis, then call geom_point()
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + geom_point()

#overplotting is when there are many overlapping points in a single area on the figure. This can be addressed by adjusting the transparency of the points, or causing jitters to each point
#to adjust the transparency of points on a scatterplot set the alpha to a point less than 1 within the function
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + geom_point(alpha = 0.1)

#To adjust the visual placement slightly using jittering. Jittering is strictly a visualization tool
ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + geom_jitter(width = 30, height = 30)



#LINEGRAPH

#used to create linegraph from data
ggplot(data = early_january_weather, mapping = aes(x = time_hour, y = temp)) + geom_line()



#HISTOGRAM

#used to create histogram from data
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram()

#changing the border color and the fill color
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram(color = "white", fill = "steelblue")

#bin size is automatically set to 30, this code will adjust the bin size
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram(bins = 40, color = "white", fill = "steelblue")

#bin width is variable depending on the data, but can also be used to change visuals
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram(binwidth = 10, color = "white", fill = "steelblue")



#FACETED HISTOGRAMS

#Faceting is when you split one visualization into multiple side by side visualizations.  
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram(binwidth = 5, color = "white", fill = "steelblue") + facet_wrap(~ month)

#you can specify the number of rows and columns in each grid by using the nrow and ncol arguments
ggplot(data = weather, mapping = aes(x = temp)) + geom_histogram(binwidth = 5, color = "white", fill = "steelblue") + facet_wrap(~ month, nrow = 4)



#BOXPLOT

#this code creates a boxplot from data
ggplot(data = weather, mapping = aes(x = month, y = temp)) + geom_boxplot()

#boxplots only register categorical variables.  If data is continuous, then must convert to categorical using factor function
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) + geom_boxplot()
# dots found at the ends of teh whiskers are considered to be anomolies or outliers. 



#BARPLOT


#barplots are good for visualizing one variable and its counted amount - geom_col() can be used for horizontal bar plots
#this code creates a barplot from data
ggplot(data = flights, mapping = aes(x = carrier)) + geom_bar()

#barplots with two categorical variables are called stacked barplots - geom_col() can be used for horizontal bar plots
ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) + geom_bar()

#this wil create a stacked barplot with the variables side by side using the position dodge function.  This is useful when atributing multipl factors to a single x-variable
ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) + geom_bar(position = position_dodge(preserve = "single"))

#FACETED BARPLOTS 
ggplot(data = flights, mapping = aes(x = carrier)) + geom_bar() + facet_wrap(~ origin, ncol = 1)

#Quarter Quantile plots AKA Q-Q Plot - draws a correlation between a given sample and the normal distribution
ggqplot(mydata$weight, ylab = "weight", ggtheme = theme_minimal())


#DATA WRANGLING

#Pipe operator.  Useful when wanting a function of a function
alaska_flights <- flights %>%
  filter(carrier == "AS")

#Filter rows using equals, or, and, not
btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)

not_BTV_SEA <- flights %>% 
  filter(!(dest == "BTV" | dest == "SEA"))
View(not_BTV_SEA)

#the C function combines variables into a vector and %in% is used as the not operator
many_airports <- flights %>% 
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
View(many_airports)

#Summarize data - data that comes back as "NA" in R is either corrupted or missing. na.rm = TRUE argument removes NA variables from summary
summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_temp

#Group by rows - useful for grouping observations by the values of another variable
summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp

#summarize transforms the data to have only two columns
diamonds %>%
  group_by(cut)%>%
  summarize(depth = mean(depth))

#you can group multiple variables by using the then argument
by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarize(count = n())
by_origin_monthly_incorrect

#mutate changes or converts existing data
weather <- weather %>%
  mutate(temp_in_C = (temp - 32)/1.8)

#shows original data with the mutated data side by side
summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE), 
            mean_temp_in_C = mean(temp_in_C, na.rm = TRUE))
summary_monthly_temp

#organizing rows of data
#this calls the data to the console
freq_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n())
freq_dest

#ascending order
freq_dest %>% 
  arrange(num_flights)

#descending order
freq_dest %>% 
  arrange(desc(num_flights))

#selecting pertinent variables and data
#function select picks the variables that you want
flights %>%
  select(carrier,flight)

#This drops or deselects variables
flights_no_year <- flights %>% select(-year)
view(flights_no_year)

#This reorders columns how you please
flights_reorder <- flights %>% 
  select(year, month, day, hour, minute, time_hour, everything())
glimpse(flights_reorder)

#This re-names variables
flights_time_new <- flights %>% 
  select(dep_time, arr_time) %>% 
  rename(departure_time = dep_time, arrival_time = arr_time)
glimpse(flights_time_new)

#convert data into tidy data using pivot longer function
drinks_smaller_tidy <- drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = -country)
drinks_smaller_tidy




#When conducting statistical analysis follow these steps
#1. import dataset as excel file
#2. check data with view and favstats functions
#3. visualize data with a plot or graph
#4. Conduct normality test: parametric or non-parametric?
#5. conduct statistical test(1,2,3+ sample comparisons, correlative regressions, Chi-squared associations)
#6. interpret results of statistical tests


#Pander can show you various statistics such as average, mean, median, and more using the favstats function
pander(favstats(weight~area, data = data_set))

#statistical significance testing to see if the data distribution is normal
# a p-value of >0.05 means we can assume normality
shapiro.test(my_data$len)

#statistical significance testing for ANOVA to see if data distribution is normal
#Levene's test
levenTest(len ~ supp*dose, data = my_data)


#The student's t-test and the ANOVA test assume that the samples being compared have equal variancces
#the shapiro-wilkes test can compare variances for paired t-tests, and the levene's test can compare anova variances


#one sample T-test - compares one dataset to a known standard: if p-value <= 0.05, then reject null hypothesis
res <- t.test(my_data$weight, mu = 25)
res

#Two sample independant T-test
res <- t.test(data_1, data_2)
res

#Paired T-test
res <- t.test(data_1, data_2, paired = TRUE)
res

#Wilcoxon rank-sum test - is a t-test for non-normal distributed data
#paired wilcoxon
res <- wilcox.test(x, y, paired = TRUE)
res

#unpaired wilcoxon
res <- wilcox.test(data_x, data_y)
res

#one-sample wilcoxon (mu = known standard)
res <- wilcox.test(my_data$weight, mu = 25)
res

#One way ANOVA test - comparing 3+ groups and one of them is an independant variable and data is normal
res.aov <- aov(weight ~ group, data = my_data)
summary(res.aov)

#two way ANOVA test - comparing 3+ groups and two of them are independent variables and data is normal
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)

#Kruskal-Wallis test - same as one way ANOVA but for non-normal distribution
res <- kruskal.test(weight ~ group, data = my_data)
res

#chi-square independence test
chisq <- chisq.test(my_data)
chisq()
chisq$p.value
#Chi-square goodness of fit test
chisq.test(x,p)
    #compare observed to expected proportions
    res <- chisq.test(data, p = c(1/2, 1/3, 1/4))
    res
    
#Spearman's rank correlation test
res <- cor.test(my_data$category_1, mydata$catecory_a, method = "spearman")
res

#simple linear regression test - large f-statistics imply low p-values and high statistical significance
model <- lm(x_data, y_data, data = my_data)
model()
    #add the regression line to the scatterplot
    ggplot(my_data, aes(y_data, x_data)) + geom_point() + stat_smooth(method = lm)
summary(model)

#multiple linear regression test
model <- lm(x_data, y_data + z_variable, data = my_data)
summary(model)

















