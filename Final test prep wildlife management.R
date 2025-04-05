# 1. Squirrels: Do squirrels in Yellowstone National Park and Porter Park differ in weights (oz)? linear Equation:Weight Location Weight=Location Statistical Test: Two-sample t-test.
t_test_result <- t.test(Weight ~ location, data = Lab_Final_Practice)
library(ggplot2)
ggplot(Lab_Final_Practice, aes(x = location, y = Weight)) +
  geom_boxplot() +
  labs(title = "Squirrel Weights by Location",
       y = "Weight (oz)", x = "Location") +
  theme_minimal()

# Print results
print(t_test_result)
# Figure 1. The weights of squirrels in Porter Park and Yellowstone National Park. Squirrels in Porter Park way 13 oz more than those in YNP, on average (t = 4.5917, df =54.643, p-value = 2.624e-05).
#2. Elevation: Does elevation (m) affect species richness in Teton National Park? Elevation = Species Richness
# Spearman correlation
cor_result <- cor.test(Lab_Final_Practice$Elevation, Lab_Final_Practice$Species, method = "spearman")

# Scatterplot
ggplot(Lab_Final_Practice, aes(x = Species, y = Elevation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Elevation by Species Richness",
       x = "Species", y = "Elevation ") +
  theme_minimal()

# Print results
print(cor_result)
#Figure 2. Elevation by species richness in Teton National Park. There is not a significant correlation (S = 630.19, p-value = 0.7875, rho= 0.07325355).
#Fish: Do Yellowstone Cutthroat trout and Sculpin in Hope Creek, ID differ in lengths (mm)? Length = Species
pander(favstats(length~species, data=Lab_Final_Practice))
#Because the data is nonparametric, the median and quartiles are more important than the mean and sd
#3. Fish: Do Yellowstone Cutthroat trout and Sculpin differ in lengths (mm)?
mann_whitney_result <- wilcox.test(Length ~ Species, data = Lab_Final_Practice)

# Boxplot
ggplot(Lab_Final_Practice, aes(x = species, y = length)) +
  geom_boxplot() +
  labs(title = "Fish Lengths by Species",
       x = "species", y = "length (mm)") +
  theme_minimal()
#4 s deer density different between forest, grassland, and wetland habitats in Island Park, ID? Density=Habitat
#Figure 4. Deer densities in forest, grassland, and wetland habitats in Island Park, ID. There is a significant difference between at least two of the habitats (F(2,15) =301.6,p=5.51e-11). All three habitats are significantly different from each other (Grassland-Forest p=1e-07, Wetland-Forest p=1e-07, Wetland-Grassland p=0). Highlighted section: make sure you format your ANOVA test results this way in your figure caption! We went over this briefly in class, but it is easy to forget!


# Print results
print(mann_whitney_result)
# ANOVA
anova_result <- aov(Density ~ Habitat, data = Lab_Final_Practice)
summary(anova_result)

# Plot
ggplot(Lab_Final_Practice, aes(x = Habitat, y = Density)) +
  geom_boxplot() +
  labs(title = "Deer Density by Habitat",
       x = "Habitat", y = "Deer Density") +
  theme_minimal()
#5#Species: Does habitat size (acres) affect species richness in Moore, ID?
#Species richness = 4.3455 + 0.1778 x Habitat size+e
#N=10
#5 Species richness by habitat size in Moore, ID. There is a significantly strong, positive relationship between habitat size and species richness (t=12.30, p= 1.77e-06, Adjusted R-squared=0.99). For every increased acre in habitat size, species richness increases by 0.1778 (Species richness = 4.3455 + 0.1778 x Habitat size + e)
# Linear regression
lm_result <- lm(Species_richness ~ Habitat_size, data = Lab_Final_Practice)
summary(lm_result)

# Scatterplot
ggplot(Lab_Final_Practice, aes(x = Habitat_size, y = Species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Species Richness vs Habitat Size",
       x = "Habitat Size (acres)", y = "Species Richness") +
  theme_minimal()
#Figure 5. Species richness by habitat size in Moore, ID. There is a significantly strong, positive relationship between habitat size and species richness (t=12.30, p= 1.77e-06, Adjusted R-squared=0.99). For every increased acre in habitat size, species richness increases by 0.1778 (Species richness = 4.3455 + 0.1778 x Habitat size + e).
#6 Lake: Does Henrys Lake Surface temperatures (C) vary by season? Temp= Season
#Because the data is not parametric, the median and quartiles are more important.
#Figure 6. Lake surface temperature (C) of Henrys Lake in different seasons. At least one of the seasons is significantly different than another season (chi-squared = 81.212, df = 3, p-value < 2.2e-16). All seasons are significantly different from each other (Fall - Spring: p=1.2e-06, fall – summer: p=6.6e-11, fall-winter: p=3.9e-08, spring-summer: p=1.2e-06, spring-winter: p=9.3e-06, summer-winter: p=8.8e-07).
# Kruskal-Wallis test
kruskal_result <- kruskal.test(temp ~ time, data = Lab_Final_Practice)

# Boxplot
ggplot(Lab_Final_Practice, aes(x = time, y = temp)) +
  geom_boxplot() +
  labs(title = "Lake Temperature by Season",
       x = "Season", y = "Temperature (C)") +
  theme_minimal()

# Print results
print(kruskal_result)
# 7 Insects: Do these biological indicators (Ephemeroptera, Plecoptera, Trichoptera) occur in Moody Creek, ID in equal proportions? 
# Figure 7. Counts of EPT species in Moody Creek, ID. EPT species occur in equal proportions (X-squared = 0.18182, df = 2, p-value = 0.9131)
chisq.test(Lab_Final_Practice$count)
ggplot(Lab_Final_Practice, aes(x = species, y = count)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Proportions of Biological Indicators", x = "Species", y = "Count") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)  # Minor grid lines
  )


#8 Rabbits: In regards to fur color, is this population of rabbits in Hardy-Weinberg Equilibrium (or in other words, do the phenotypes occur in a ratio of 3:1)?
#Figure 8. Fur phenotypes for a rabbit population. The population is not in Hardy- Weinberg equilibrium (X-squared = 11.488, df = 1, p-value = 0.0007003) 
#Note: when reporting summary statistics, you may choose if it is appropriate to include
#in the figure caption or not. You may also choose if you would like to report all the
#statistics (Favstats table) or just the ones needed for the specific test. If you choose to
# report the favstats table, you must indicate which stats are actually useful by including
#them in your figure caption or stating it by the table as shown above somewhere else.
#As long as I can tell that you know the differences between them and you know which
# ones are appropriate for which tests.
# Observed counts
ggplot(Lab_Final_Practice, aes(x = Phenotype, y = Count)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Phenotype Proportions in Rabbits", x = "Phenotype", y = "Count") +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),       # Ensures all text is black
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Adds major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Adds minor gridlines
    panel.grid = element_line(color = "gray"),  # Explicitly ensures gridlines are included
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Adds a border around the plot panel
  ) +
  theme(axis.line = element_line(color = "black"))  # Ensures axis lines are visible


#fav stats1
favstats(~ variable, data = dataset)

t.test(x, mu = 0, alternative = "two.sided")
summary(my_data$weight)
library(ggpubr)
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())
library("ggpubr")
ggqqplot(my_data$weight, ylab = "Men's weight",
         ggtheme = theme_minimal())
# One-sample t-test
res <- t.test(my_data$weight, mu = 25)
# Printing the results
res 
t.test(my_data$weight, mu = 25,
       alternative = "less")
t.test(my_data$weight, mu = 25,
       alternative = "greater")
wilcox.test(x, mu = 0, alternative = "two.sided")
# If .txt tab file, use this
my_data <- read.delim(file.choose())
# Or, if .csv file, use this
my_data <- read.csv(file.choose())
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# One-sample wilcoxon test
res <- wilcox.test(my_data$weight, mu = 25)

# Printing the results
res 
wilcox.test(my_data$weight, mu = 25,
            alternative = "less")
shapiro.test(LF5$length)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")
# Shapiro-Wilk normality test for Men's weights
with(my_data, shapiro.test(weight[group == "Man"]))# p = 0.1
# Shapiro-Wilk normality test for Women's weights
with(my_data, shapiro.test(weight[group == "Woman"])) # p = 0.6
res.ftest <- var.test(weight ~ group, data = my_data)
res.ftest
# Compute t-test
res <- t.test(women_weight, men_weight, var.equal = TRUE)
res
# Compute t-test
res <- t.test(weight ~ group, data = my_data, var.equal = TRUE)
res
t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "less")
t.test(weight ~ group, data = my_data,
       var.equal = TRUE, alternative = "greater")
my_data$group <- ordered(my_data$group,
                        levels = c("ctrl", "trt1", "trt2"))
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")
# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")
# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 
# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)
library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")
library(car)
leveneTest(weight ~ group, data = my_data)
oneway.test(weight ~ group, data = my_data)
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)
# 2. Normality
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
kruskal.test(weight ~ group, data = my_data)

