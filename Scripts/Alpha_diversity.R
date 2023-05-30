##Plots and statistics for 18S alpha-diversity


library(ggplot2)
library(ggpubr)

##Load data
env.18S <- read.csv('Data/env_18S.csv', header = TRUE, row.names = 1)

##Set colours for pH graphs
mypal.pH <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b",
                               "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2"))

################### Sobs ##################

##plot graph
sobs.18S <-
  ggplot(env.18S, aes(pH, Sobs, color = pH)) + geom_point(size = 3) +
  xlab("pH") + ylab("Sobs") + ggtitle("18S") + theme_classic() + geom_smooth(
    size = 0.2,
    color = "black",
    method = 'lm',
    formula = y ~ x + I(x ^ 2)
  ) +
  scale_colour_gradientn(colours = mypal.pH(length(env.18S$pH)))

plot(sobs.18S)

##model statistics
summary(lm(env.18S, formula = Sobs ~ pH + I(pH^2)))

##ANOVA
anova(lm(env.18S, formula = Sobs ~ pH + I(pH^2)))


############### Shannon ###################

##plot graph
shan.18S <-
  ggplot(env.18S, aes(pH, Shan, color = pH)) + geom_point(size = 3) +
  xlab("pH") + ylab("Shannon") + ggtitle("18S") + theme_classic() + geom_smooth(
    size = 0.2,
    color = "black",
    method = 'lm',
    formula = y ~ x + I(x ^ 2)
  ) +
  scale_colour_gradientn(colours = mypal.pH(length(env.18S$pH)))

plot(shan.18S)

##model statistics
summary(lm(env.18S, formula = Shan ~ pH + I(pH^2)))

##ANOVA
anova(lm(env.18S, formula = Shan ~ pH + I(pH^2)))

##arrange graphs in one picture
ggarrange(sobs.18S,shan.18S, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")

