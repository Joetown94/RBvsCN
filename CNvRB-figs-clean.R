#### Libraries Required #### 
library(tidyverse)
library(ggplot2)
library(lmodel2)
library(SimplyAgree)
library(patchwork)

############################################################Indonesia Data#######################################
#### load Reefbudget data
indo <- read.csv("ExtractedData/Indonesia_transect.csv")
str(indo)
indo$CN_sumcover = indo$CN_coral_cover + indo$CN_CCA_cover
indo$RB_sumcover = indo$RB_coral_cover + indo$RB_CCA_cover

#### Gross G Model II regression #### 

#Model II regressions- x and y are not predictor variables

#V1
lm2.Nobe.v1.indo = lmodel2(CN_v1_without_bioerosion~RB_mean_without_bioerosion, data = indo, "relative","relative", nperm = 999)
lm2.Nobe.v1.indo
predict.v1.indo = data.frame(lm2.Nobe.v1.indo$x,
                             indo$RB_lower_without_bioerosion,
                             indo$RB_upper_without_bioerosion,
                             lm2.Nobe.v1.indo$y,
                             indo$CN_v1_lower_without_bioerosion,
                             indo$CN_v1_upper_without_bioerosion,
                             lm2.Nobe.v1.indo$x*lm2.Nobe.v1.indo$regression.results[2,3]+lm2.Nobe.v1.indo$regression.results[2,2],
                             lm2.Nobe.v1.indo$x*lm2.Nobe.v1.indo$confidence.intervals[2,4]+lm2.Nobe.v1.indo$confidence.intervals[2,2],
                             lm2.Nobe.v1.indo$x*lm2.Nobe.v1.indo$confidence.intervals[2,5]+lm2.Nobe.v1.indo$confidence.intervals[2,3])
names(predict.v1.indo) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#V2
lm2.Nobe.v2.indo = lmodel2(CN_v2_without_bioerosion~RB_mean_without_bioerosion, data = indo, "relative","relative", nperm = 999)
lm2.Nobe.v2.indo
predict.v2.indo = data.frame(lm2.Nobe.v2.indo$x,
                             indo$RB_lower_without_bioerosion,
                             indo$RB_upper_without_bioerosion,
                             lm2.Nobe.v2.indo$y,
                             indo$CN_v2_lower_without_bioerosion,
                             indo$CN_v2_upper_without_bioerosion,
                             lm2.Nobe.v2.indo$x*lm2.Nobe.v2.indo$regression.results[2,3]+lm2.Nobe.v2.indo$regression.results[2,2],
                             lm2.Nobe.v2.indo$x*lm2.Nobe.v2.indo$confidence.intervals[2,4]+lm2.Nobe.v2.indo$confidence.intervals[2,2],
                             lm2.Nobe.v2.indo$x*lm2.Nobe.v2.indo$confidence.intervals[2,5]+lm2.Nobe.v2.indo$confidence.intervals[2,3])
names(predict.v2.indo) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#RB-local vs regional rates
lm2.Nobe.RB = lmodel2(RB_Indo_mean_without_bioerosion~RB_mean_without_bioerosion, data = indo, "relative","relative", nperm = 999)
lm2.Nobe.RB
predict.RB.indo = data.frame(lm2.Nobe.RB$x,
                             indo$RB_lower_without_bioerosion,
                             indo$RB_upper_without_bioerosion,
                             lm2.Nobe.RB$y,
                             indo$RB_Indo_lower_without_bioerosion,
                             indo$RB_Indo_upper_without_bioerosion,
                             lm2.Nobe.RB$x*lm2.Nobe.RB$regression.results[2,3]+lm2.Nobe.RB$regression.results[2,2],
                             lm2.Nobe.RB$x*lm2.Nobe.RB$confidence.intervals[2,4]+lm2.Nobe.RB$confidence.intervals[2,2],
                             lm2.Nobe.RB$x*lm2.Nobe.RB$confidence.intervals[2,5]+lm2.Nobe.RB$confidence.intervals[2,3])
names(predict.RB.indo) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#V1vsV2 Direct
lm2.Nobe.v1v2.indo = lmodel2(CN_v2_without_bioerosion~CN_v1_without_bioerosion, data = indo, "relative","relative", nperm = 999)
lm2.Nobe.v1v2.indo
predict.v1v2.indo = data.frame(lm2.Nobe.v1v2.indo$x,
                             indo$CN_v1_upper_without_bioerosion,
                             indo$CN_v1_lower_without_bioerosion,
                             lm2.Nobe.v1v2.indo$y,
                             indo$CN_v2_lower_without_bioerosion,
                             indo$CN_v2_upper_without_bioerosion,
                             lm2.Nobe.v1v2.indo$x*lm2.Nobe.v1v2.indo$regression.results[2,3]+lm2.Nobe.v1v2.indo$regression.results[2,2],
                             lm2.Nobe.v1v2.indo$x*lm2.Nobe.v1v2.indo$confidence.intervals[2,4]+lm2.Nobe.v1v2.indo$confidence.intervals[2,2],
                             lm2.Nobe.v1v2.indo$x*lm2.Nobe.v1v2.indo$confidence.intervals[2,5]+lm2.Nobe.v1v2.indo$confidence.intervals[2,3])
names(predict.v1v2.indo) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#### calculate difference between method ####

indo.data = indo
indo.diff <- indo.data %>% 
  select(Site, Meter, RB_mean_without_bioerosion, CN_v2_without_bioerosion) %>% 
  group_by(Site, Meter) %>% 
  mutate(difference = CN_v2_without_bioerosion - RB_mean_without_bioerosion,
         mean_g = (RB_mean_without_bioerosion + CN_v2_without_bioerosion)/2) %>% 
  ungroup()

#v1 vs v2 differences
indo.diff.v1v2 <- indo.data %>% 
  select(Site, Meter, CN_v1_without_bioerosion, CN_v2_without_bioerosion) %>% 
  group_by(Site, Meter) %>% 
  mutate(difference = CN_v2_without_bioerosion - CN_v1_without_bioerosion, 
         mean_g = (CN_v2_without_bioerosion + CN_v1_without_bioerosion)/2) %>% 
  ungroup()


#Mean Difference
indo.mean.diff <- indo.diff %>% 
  summarise(mean_diff = mean(difference),
            sd_diff = sd(difference),
            LoA_low = mean_diff-(1.96*sd_diff),
            LoA_high = mean_diff+(1.96*sd_diff))

indo.mean.diff.v1v2 <- indo.diff.v1v2 %>% 
  summarise(mean_diff = mean(difference),
            sd_diff = sd(difference),
            LoA_low = mean_diff-(1.96*sd_diff),
            LoA_high = mean_diff+(1.96*sd_diff))


#### CCC - indo ####
(agree.indo <- agree_test(x = indo.data$RB_mean_without_bioerosion,
                          y = indo.data$CN_v2_without_bioerosion)
)

(agree.indo.v1v2 <- agree_test(x = indo.data$CN_v1_without_bioerosion,
                          y = indo.data$CN_v2_without_bioerosion)
)
#### plot data ##### 

# CN v1 vs ReefBudget
indo_v1 = ggplot(predict.v1.indo, aes(x = x, y = y)) +
            geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
            geom_point(cex = 2.5, colour = '#f46d43', shape = 16) +
            geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#f46d43', alpha = 0.3)+
            geom_abline(slope = lm2.Nobe.v1.indo$regression.results[2,3], intercept = lm2.Nobe.v1.indo$regression.results[2,2], colour = '#f46d43', linewidth = 1.3)+
            geom_abline(slope = lm2.Nobe.v1.indo$confidence.intervals[2,4], intercept = lm2.Nobe.v1.indo$confidence.intervals[2,2], colour = '#f46d43', linetype = 3, linewidth = 1.2)+
            geom_abline(slope = lm2.Nobe.v1.indo$confidence.intervals[2,5], intercept = lm2.Nobe.v1.indo$confidence.intervals[2,3], colour = '#f46d43', linetype = 3, linewidth = 1.2)+
            geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#f46d43', alpha = 0.6) +
            geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#f46d43', alpha = 0.6) +
            scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
            scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
            theme_classic() +
            coord_cartesian(ylim = c(0,27), xlim = c(0,27))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
            labs(title = "Gross G", x = bquote("ReefBudget (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v1 (kg "~CaCO[3]/m^2/yr~")"))+
            theme(plot.title = element_text(hjust = 0.5))
indo_v1

# CN v2 vs ReefBudget
indo_v2 = ggplot(predict.v2.indo, aes(x = x, y = y)) +
            geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
            geom_point(cex = 2.5, colour = '#a50026', shape = 15) +
            geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#a50026', alpha = 0.3)+
            geom_abline(slope = lm2.Nobe.v2.indo$regression.results[2,3], intercept = lm2.Nobe.v2.indo$regression.results[2,2], colour = '#a50026', linewidth = 1.3)+
            geom_abline(slope = lm2.Nobe.v2.indo$confidence.intervals[2,4], intercept = lm2.Nobe.v2.indo$confidence.intervals[2,2], colour = '#a50026', linetype = 3, linewidth = 1.2)+
            geom_abline(slope = lm2.Nobe.v2.indo$confidence.intervals[2,5], intercept = lm2.Nobe.v2.indo$confidence.intervals[2,3], colour = '#a50026', linetype = 3, linewidth = 1.2)+
            geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#a50026', alpha = 0.4) +
            geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#a50026', alpha = 0.4) +
            scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
            scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
            theme_classic() +
            coord_cartesian(ylim = c(0,27), xlim = c(0,27))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
            labs(title = "Gross G", x = bquote("ReefBudget (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
            theme(plot.title = element_text(hjust = 0.5))
indo_v2

#ReefBudget Regional Rates vs Local Rates
indo_RB.regional = ggplot(predict.RB.indo, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#a50026', shape = 18) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#a50026', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.RB$regression.results[2,3], intercept = lm2.Nobe.RB$regression.results[2,2], colour = '#a50026', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.RB$confidence.intervals[2,4], intercept = lm2.Nobe.RB$confidence.intervals[2,2], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.RB$confidence.intervals[2,5], intercept = lm2.Nobe.RB$confidence.intervals[2,3], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#a50026', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#a50026', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,30), xlim = c(0,30.5))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "ReefBudget (Gross G)", x = bquote("Indo-Pacific ReefBudget"), y = bquote("Indonesia ReefBudget"))+
  theme(plot.title = element_text(hjust = 0.5))
indo_RB.regional

#CoralNet v1 vs v2
indo_v1v2 = ggplot(predict.v1v2.indo, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#a50026', shape = 15) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#a50026', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.v1v2.indo$regression.results[2,3], intercept = lm2.Nobe.v1v2.indo$regression.results[2,2], colour = '#a50026', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.v1v2.indo$confidence.intervals[2,4], intercept = lm2.Nobe.v1v2.indo$confidence.intervals[2,2], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.v1v2.indo$confidence.intervals[2,5], intercept = lm2.Nobe.v1v2.indo$confidence.intervals[2,3], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#a50026', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#a50026', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,27), xlim = c(0,27))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Gross G", x = bquote("CoralNet v1 (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))
indo_v1v2


##Difference Plots

#CNv2 vs ReefBudget
Dif_indo <- ggplot(indo.diff) + 
  geom_abline(slope = 0,
              intercept= seq((agree.indo$loa$upper.ci[1]*-1), (agree.indo$loa$lower.ci[1]*-1), 0.1),
              colour = "#a50026", alpha = 0.1, linewidth = 1.8) +
  geom_hline(yintercept = 0, lty = 1, linewidth = 1.05)+
  geom_hline(yintercept=indo.mean.diff$mean_diff, colour = '#a50026', linewidth = 1.5) +
  geom_hline(yintercept=indo.mean.diff$LoA_low, lty = 3, colour = 'black') +
  geom_hline(yintercept=indo.mean.diff$LoA_high, lty = 3, colour = 'black') +
  geom_hline(yintercept=agree.indo$loa$lower.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_hline(yintercept=agree.indo$loa$upper.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_point(aes(mean_g, difference), cex = 2.5, colour = '#a50026', shape = 15) +
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 18, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(-10, 10, 2)) +
  coord_cartesian(ylim = c(-10,10), xlim = c(0,18))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(y= "CoralNet v2 - ReefBudget", 
       x = "Averaged Gross G",
       title = "Indo-Pacific rates")+
  theme(plot.title = element_text(hjust = 0.5))
Dif_indo

#CNv1 vs CNv2
Dif_indo.v1v2 <- ggplot(indo.diff.v1v2) + 
  geom_abline(slope = 0,
              intercept= seq((agree.indo.v1v2$loa$upper.ci[1]*-1), (agree.indo.v1v2$loa$lower.ci[1]*-1), 0.1),
              colour = "#a50026", alpha = 0.1, linewidth = 1.8) +
  geom_hline(yintercept = 0, lty = 1, linewidth = 1.05)+
  geom_hline(yintercept=indo.mean.diff.v1v2$mean_diff, colour = '#a50026', linewidth = 1.5) +
  geom_hline(yintercept=indo.mean.diff.v1v2$LoA_low, lty = 3, colour = 'black') +
  geom_hline(yintercept=indo.mean.diff.v1v2$LoA_high, lty = 3, colour = 'black') +
  geom_hline(yintercept=agree.indo.v1v2$loa$lower.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_hline(yintercept=agree.indo.v1v2$loa$upper.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_point(aes(mean_g, difference), cex = 2.5, colour = '#a50026', shape = 15) +
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 18, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(-10, 10, 2)) +
  coord_cartesian(ylim = c(-10,10), xlim = c(0,18))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(y= "CoralNet v2 - CoralNet V1", 
       x = "Averaged Gross G",
       title = "Bland-Altman Difference")+
  theme(plot.title = element_text(hjust = 0.5))
Dif_indo.v1v2





##### Differences in coral cover #####

## Differences in total calcifier cover
lm2.sumcover.v2.indo = lmodel2(CN_sumcover~RB_sumcover, data = indo, "relative","relative", nperm = 999)
lm2.sumcover.v2.indo
predict.sumcover.indo = data.frame(lm2.sumcover.v2.indo$x,
                                   lm2.sumcover.v2.indo$y,
                                   lm2.sumcover.v2.indo$x*lm2.sumcover.v2.indo$regression.results[2,3]+lm2.sumcover.v2.indo$regression.results[2,2],
                                   lm2.sumcover.v2.indo$x*lm2.sumcover.v2.indo$confidence.intervals[2,4]+lm2.sumcover.v2.indo$confidence.intervals[2,2],
                                   lm2.sumcover.v2.indo$x*lm2.sumcover.v2.indo$confidence.intervals[2,5]+lm2.sumcover.v2.indo$confidence.intervals[2,3])
names(predict.sumcover.indo) = c("x","y","fit","lwr","upr")

## Differences in Coral cover
lm2.cover.v2.indo = lmodel2(CN_coral_cover~RB_coral_cover, data = indo, "relative","relative", nperm = 999)
lm2.cover.v2.indo
predict.cover.indo = data.frame(lm2.cover.v2.indo$x,
                             lm2.cover.v2.indo$y,
                             lm2.cover.v2.indo$x*lm2.cover.v2.indo$regression.results[2,3]+lm2.cover.v2.indo$regression.results[2,2],
                             lm2.cover.v2.indo$x*lm2.cover.v2.indo$confidence.intervals[2,4]+lm2.cover.v2.indo$confidence.intervals[2,2],
                             lm2.cover.v2.indo$x*lm2.cover.v2.indo$confidence.intervals[2,5]+lm2.cover.v2.indo$confidence.intervals[2,3])
names(predict.cover.indo) = c("x","y","fit","lwr","upr")

## Differences in CCA cover
lm2.cover.CCA.indo = lmodel2(CN_CCA_cover~RB_CCA_cover, data = indo, "relative","relative", nperm = 999)
lm2.cover.CCA.indo
predict.cover.CCA.indo = data.frame(lm2.cover.CCA.indo$x,
                                lm2.cover.CCA.indo$y,
                                lm2.cover.CCA.indo$x*lm2.cover.CCA.indo$regression.results[2,3]+lm2.cover.CCA.indo$regression.results[2,2],
                                lm2.cover.CCA.indo$x*lm2.cover.CCA.indo$confidence.intervals[2,4]+lm2.cover.CCA.indo$confidence.intervals[2,2],
                                lm2.cover.CCA.indo$x*lm2.cover.CCA.indo$confidence.intervals[2,5]+lm2.cover.CCA.indo$confidence.intervals[2,3])
names(predict.cover.CCA.indo) = c("x","y","fit","lwr","upr")




### CN and RB calcifying cover plots

#Total Calcifier Cover- RB vs CN
indo_Sumcov =  ggplot(predict.sumcover.indo, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.sumcover.v2.indo$regression.results[2,3], intercept = lm2.sumcover.v2.indo$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.sumcover.v2.indo$confidence.intervals[2,4], intercept = lm2.sumcover.v2.indo$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.sumcover.v2.indo$confidence.intervals[2,5], intercept = lm2.sumcover.v2.indo$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,85), xlim = c(0,85))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Total Calcifier Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))
indo_Sumcov

#Coral Cover- RB vs CN
indo_cov =  ggplot(predict.cover.indo, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.cover.v2.indo$regression.results[2,3], intercept = lm2.cover.v2.indo$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.cover.v2.indo$confidence.intervals[2,4], intercept = lm2.cover.v2.indo$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.cover.v2.indo$confidence.intervals[2,5], intercept = lm2.cover.v2.indo$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,82), xlim = c(0,82))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Coral Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))
indo_cov

#CCA Cover- RB vs CN
indo_cov.CCA =  ggplot(predict.cover.CCA.indo, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.cover.CCA.indo$regression.results[2,3], intercept = lm2.cover.CCA.indo$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.cover.CCA.indo$confidence.intervals[2,4], intercept = lm2.cover.CCA.indo$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.cover.CCA.indo$confidence.intervals[2,5], intercept = lm2.cover.CCA.indo$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 10, 2)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 10, 2)) +
  theme_classic() +
  coord_cartesian(ylim = c(-0.05,10), xlim = c(-0.05,10))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "CCA Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))
indo_cov.CCA

############################################################Caribbean Data#######################################
#Import Cleaned Datasets
CN.all.tidy = read.csv("ExtractedData/tidy_totalg_CI_cn.csv")
RB.tidy = read.csv("ExtractedData/tidy_totalg_CI_rb.csv")

BE.split = split(CN.all.tidy, CN.all.tidy$rates)
CN.Nobe.tidy = data.frame(BE.split$Cari_v1_wo_bio)
CN.Nobe.tidy.V2.GBncrmp = data.frame(BE.split$Carib_v2_wo_bio.GBncrmp)

#link datasets through link column
CN.Nobe.tidy$Link = paste(CN.Nobe.tidy$Site, CN.Nobe.tidy$Transect)
CN.Nobe.tidy.V2.GBncrmp$Link = paste(CN.Nobe.tidy.V2.GBncrmp$Site, CN.Nobe.tidy.V2.GBncrmp$Transect)
RB.tidy1 = RB.tidy
RB.tidy1$Link = paste(RB.tidy1$site, RB.tidy1$transect)

#change dataset names for merge
names(CN.Nobe.tidy) = c("methodNobe","ratetypeNobe","SiteCNNobe","TransectCNNobe","TotalgCNNobe","lower.ciCNNobe","upper.ciCNNobe","Link")

names(CN.Nobe.tidy.V2.GBncrmp) = c("methodNobeV2ncrmpGB","ratetypeNobeV2ncrmpGB","SiteCNNobeV2ncrmpGB","TransectCNNobeV2ncrmpGB","TotalgCNNobeV2ncrmpGB","lower.ciCNNobeV2ncrmpGB","upper.ciCNNobeV2ncrmpGB","Link")

names(RB.tidy1) = c("Totalg.RB", "lower.ci.totalg.RB","upper.ci.totalg.RB","MicroBE.RB","MacroBE.RB","Rugosity.RB","Transect.length.RB","Coralg.RB","lower.ci.coral.RB","upper.ci.coral.RB","CCAg.RB","lower.ci.cca.RB","upper.ci.cca.RB","Netg.RB","depth.RB","site.RB","transect.RB","Link")

#merge
all_data <- list(RB.tidy1, 
                 CN.Nobe.tidy.V2.GBncrmp, 
                 CN.Nobe.tidy)
all_data2 = all_data %>% reduce(full_join, by='Link')

library(plyr)
Site.Totalg = ddply(all_data2, .(site.RB), summarise,
                    meanCN_G = mean(TotalgCNNobeV2ncrmpGB),
                    CN.lwr_G = (mean(TotalgCNNobeV2ncrmpGB))-(2*(sd(TotalgCNNobeV2ncrmpGB))),
                    CN.upr_G = (mean(TotalgCNNobeV2ncrmpGB))+(2*(sd(TotalgCNNobeV2ncrmpGB))),
                    meanRB_G = mean(Totalg.RB),
                    RB.lwr_G = (mean(Totalg.RB))-(2*(sd(Totalg.RB))),
                    RB.upr_G = (mean(Totalg.RB))+(2*(sd(Totalg.RB))))
#### Gross G Model II regression and Plots #### 

#Model II regressions- x and y are not predictor variables

#V1
lm2.Nobe.v1.car = lmodel2(TotalgCNNobe~Totalg.RB, data = all_data2, "relative","relative", nperm = 999)
predict.v1.car = data.frame(lm2.Nobe.v1.car$x,
                             all_data2$lower.ci.totalg.RB,
                             all_data2$upper.ci.totalg.RB,
                             lm2.Nobe.v1.car$y,
                             all_data2$lower.ciCNNobe,
                             all_data2$upper.ciCNNobe,
                             lm2.Nobe.v1.car$x*lm2.Nobe.v1.car$regression.results[2,3]+lm2.Nobe.v1.car$regression.results[2,2],
                             lm2.Nobe.v1.car$x*lm2.Nobe.v1.car$confidence.intervals[2,4]+lm2.Nobe.v1.car$confidence.intervals[2,2],
                             lm2.Nobe.v1.car$x*lm2.Nobe.v1.car$confidence.intervals[2,5]+lm2.Nobe.v1.car$confidence.intervals[2,3])

names(predict.v1.car) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#v2
lm2.Nobe.v2.car = lmodel2(TotalgCNNobeV2ncrmpGB~Totalg.RB, data = all_data2, "relative","relative", nperm = 999)
predict.v2.car = data.frame(lm2.Nobe.v2.car$x,
                            all_data2$lower.ci.totalg.RB,
                            all_data2$upper.ci.totalg.RB,
                            lm2.Nobe.v2.car$y,
                            all_data2$lower.ciCNNobeV2ncrmpGB,
                            all_data2$upper.ciCNNobeV2ncrmpGB,
                            lm2.Nobe.v2.car$x*lm2.Nobe.v2.car$regression.results[2,3]+lm2.Nobe.v2.car$regression.results[2,2],
                            lm2.Nobe.v2.car$x*lm2.Nobe.v2.car$confidence.intervals[2,4]+lm2.Nobe.v2.car$confidence.intervals[2,2],
                            lm2.Nobe.v2.car$x*lm2.Nobe.v2.car$confidence.intervals[2,5]+lm2.Nobe.v2.car$confidence.intervals[2,3])

names(predict.v2.car) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#v1 vs v2
lm2.Nobe.v1v2.car = lmodel2(TotalgCNNobeV2ncrmpGB~TotalgCNNobe, data = all_data2, "relative","relative", nperm = 999)
lm2.Nobe.v1v2.car
predict.v1v2.car = data.frame(lm2.Nobe.v1v2.car$x,
                               all_data2$upper.ciCNNobeV2ncrmpGB,
                               all_data2$lower.ciCNNobeV2ncrmpGB,
                               lm2.Nobe.v1v2.car$y,
                               all_data2$upper.ciCNNobe,
                               all_data2$lower.ciCNNobe,
                               lm2.Nobe.v1v2.car$x*lm2.Nobe.v1v2.car$regression.results[2,3]+lm2.Nobe.v1v2.car$regression.results[2,2],
                               lm2.Nobe.v1v2.car$x*lm2.Nobe.v1v2.car$confidence.intervals[2,4]+lm2.Nobe.v1v2.car$confidence.intervals[2,2],
                               lm2.Nobe.v1v2.car$x*lm2.Nobe.v1v2.car$confidence.intervals[2,5]+lm2.Nobe.v1v2.car$confidence.intervals[2,3])
names(predict.v1v2.car) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#####Graph
#CNv1 vs RB
car_v1 = ggplot(predict.v1.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#f46d43', shape = 16) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#f46d43', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.v1.car$regression.results[2,3], intercept = lm2.Nobe.v1.car$regression.results[2,2], colour = '#f46d43', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.v1.car$confidence.intervals[2,4], intercept = lm2.Nobe.v1.car$confidence.intervals[2,2], colour = '#f46d43', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.v1.car$confidence.intervals[2,5], intercept = lm2.Nobe.v1.car$confidence.intervals[2,3], colour = '#f46d43', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#f46d43', alpha = 0.6) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#f46d43', alpha = 0.6) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 3.0, 0.5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 3.2, 0.5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,2.7), xlim = c(0,2.7))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(title = "Gross G", x = bquote("ReefBudget (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v1 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))
car_v1

#CNv2 vs RB
car_v2 = ggplot(predict.v2.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#a50026', shape = 15) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#a50026', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.v2.car$regression.results[2,3], intercept = lm2.Nobe.v2.car$regression.results[2,2], colour = '#a50026', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.v2.car$confidence.intervals[2,4], intercept = lm2.Nobe.v2.car$confidence.intervals[2,2], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.v2.car$confidence.intervals[2,5], intercept = lm2.Nobe.v2.car$confidence.intervals[2,3], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#a50026', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#a50026', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 3.0, 0.5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 3.2, 0.5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,2.7), xlim = c(0,2.7))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(title = "Gross G",x = bquote("ReefBudget (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))
car_v2

#CNv1 vs CNv2
car_v1v2 = ggplot(predict.v1v2.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#a50026', shape = 15) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#a50026', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.v1v2.car$regression.results[2,3], intercept = lm2.Nobe.v1v2.car$regression.results[2,2], colour = '#a50026', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.v1v2.car$confidence.intervals[2,4], intercept = lm2.Nobe.v1v2.car$confidence.intervals[2,2], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.v1v2.car$confidence.intervals[2,5], intercept = lm2.Nobe.v1v2.car$confidence.intervals[2,3], colour = '#a50026', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#a50026', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#a50026', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 3.0, 0.5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 3.2, 0.5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,2.7), xlim = c(0,2.7))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(title = "Gross G",x = bquote("CoralNet v1 (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))
car_v1v2

#Plot Site-level Gross G
SiteTotalg = ggplot(Site.Totalg, aes(x = meanRB_G, y = meanCN_G))+
  theme_classic()+
  geom_abline(intercept = 0, slope = 1, colour = 'black')+
  geom_errorbarh(aes(xmin = RB.lwr_G, xmax = RB.upr_G), height = 0, colour = '#a50026')+
  geom_errorbar(aes(ymin = CN.lwr_G, ymax = CN.upr_G), width = 0, colour = '#a50026')+
  geom_point(cex = 4, shape = 15, colour = '#a50026')+
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 1.7, 0.5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 1.7, 0.5)) +
  coord_cartesian(ylim = c(-0.4,1.7), xlim = c(-0.4,1.7))+
  theme(text = element_text(size = 14), plot.title = element_text(size = 14), axis.text = element_text(size = 14), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(title = "Site-Level Gross G",x = bquote("ReefBudget (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))
SiteTotalg
#### Benthic Cover Analysis and Plots #### 
#CoralNet Cover
CN.allcover.HC = read.csv("ExtractedData/tidy_cn_benthic_cover.csv", header = T)
CN.allcover.HC$method = "CoralNet"

#ReefBudget Cover
RB.allcover.HC = read.csv("ExtractedData/tidy-rb-allcover_per.csv", header = T)
RB.allcover.HC$X = NULL
RB.allcover.HC$method = "ReefBudget"
names(RB.allcover.HC)[6] = "RB_cover" 

#Other Taxa Cover, combine CCA labels and other CN labels to match RB labels

CN.allcover.HC$Group = revalue(CN.allcover.HC$categories, c(
  "Articulated.coralline.algae" = "Secondary Carbonate Producers",
  "Bare.Rock" = "Rock",
  "Biofilmed.Rock" = "Rock",
  "CCA..crustose.coralline.algae." = "Secondary Carbonate Producers",
  "Cliona" = "Others",
  "Coral.Rock" = "Dead Coral and Pavement",
  "Dead.coral" = "Dead Coral and Pavement",
  "Green.Rock" = "Limestone Pavement",
  "Halimeda" = "Sediment Producers",
  "HC" = "Hard Coral",
  "Lightly.Biofilmed.Rock" = "Limestone Pavement",
  "Macroalgae" = "Articulate Macroalgae",
  "Macroalgae..Laminate..red..peysonnelia" = "Secondary Carbonate Producers",
  "Other" = "Others",
  "Reel" = "Others",
  "Rock" = "Others",
  "Rock.Crustose.Coralline.Algae" = "Secondary Carbonate Producers",
  "Rock_Pavement" = "Limestone Pavement",
  "Rubble" = "Rubble",
  "Sand" = "Sand & Seagrass",
  "Soft.Coral" = "Soft Coral",
  "Sponge" = "Others",
  "TAPE" = "Others",
  "Thick.Sediment.Over.Rock" = "Rock",
  "Thin.Sediment.over.Rock" = "Rock",
  "Thin.Turf.Biofilm.on.Rock" = "Turf",
  "Turf.algae" = "Turf"
))

CN.allcover.RB = ddply(CN.allcover.HC, .(Region,Site,Transect,method,Group), summarise,
                       CN_cover = sum(mean.cover),
                       CN_se = sum(se.cover))
names(CN.allcover.RB) = c("region","site","transect","method.CN","Group","CN_cover","CN_se")

Allcov.merge = merge(CN.allcover.RB, RB.allcover.HC, by = c("site","transect","Group"))

Allcov.split = split(Allcov.merge, Allcov.merge$Group)

Site.Cov = ddply(Allcov.split$`Hard Coral`, .(site), summarise,
                 meanCN = mean(CN_cover),
                 CN.conf.low = (mean(CN_cover))-(2*sd(CN_cover)),
                 CN.conf.upr = (mean(CN_cover))+(2*sd(CN_cover)),
                 meanRB = mean(RB_cover),
                 RB.conf.low = (mean(RB_cover))-(2*sd(RB_cover)),
                 RB.conf.upr = (mean(RB_cover))+(2*sd(RB_cover)))

detach(package:plyr,unload=TRUE) #detach plyr to prevent issues with Tidyverse


#Combine CCA cover and Coral cover for total Calcifier Cover
Allcov.split$`Hard Coral`$SecondaryCN = Allcov.split$`Secondary Carbonate Producers`$CN_cover
Allcov.split$`Hard Coral`$SecondaryRB = Allcov.split$`Secondary Carbonate Producers`$RB_cover
Allcov.split$`Hard Coral`$SumCovCN = Allcov.split$`Hard Coral`$CN_cover + Allcov.split$`Hard Coral`$SecondaryCN
Allcov.split$`Hard Coral`$SumCovRB = Allcov.split$`Hard Coral`$RB_cover + Allcov.split$`Hard Coral`$SecondaryRB

###Linear model Type II 
#All Calcifying Cover
lm2.Sumcover.v2.car = lmodel2(SumCovCN~SumCovRB, data = Allcov.split$`Hard Coral`, "relative","relative", nperm = 999)
lm2.Sumcover.v2.car

predict.Sumcover.car = data.frame(lm2.Sumcover.v2.car$x,
                                  lm2.Sumcover.v2.car$y,
                                  lm2.Sumcover.v2.car$x*lm2.Sumcover.v2.car$regression.results[2,3]+lm2.Sumcover.v2.car$regression.results[2,2],
                                  lm2.Sumcover.v2.car$x*lm2.Sumcover.v2.car$confidence.intervals[2,4]+lm2.Sumcover.v2.car$confidence.intervals[2,2],
                                  lm2.Sumcover.v2.car$x*lm2.Sumcover.v2.car$confidence.intervals[2,5]+lm2.Sumcover.v2.car$confidence.intervals[2,3])
names(predict.Sumcover.car) = c("x","y","fit","lwr","upr")

#Coral Cover Only
lm2.cover.v2.car = lmodel2(CN_cover~RB_cover, data = Allcov.split$`Hard Coral`, "relative","relative", nperm = 999)
lm2.cover.v2.car

predict.cover.car = data.frame(lm2.cover.v2.car$x,
                                lm2.cover.v2.car$y,
                                lm2.cover.v2.car$x*lm2.cover.v2.car$regression.results[2,3]+lm2.cover.v2.car$regression.results[2,2],
                                lm2.cover.v2.car$x*lm2.cover.v2.car$confidence.intervals[2,4]+lm2.cover.v2.car$confidence.intervals[2,2],
                                lm2.cover.v2.car$x*lm2.cover.v2.car$confidence.intervals[2,5]+lm2.cover.v2.car$confidence.intervals[2,3])
names(predict.cover.car) = c("x","y","fit","lwr","upr")

#CCA Only
lm2.cover.CCA.car = lmodel2(CN_cover~RB_cover, data = Allcov.split$`Secondary Carbonate Producers`, "relative","relative", nperm = 999)
lm2.cover.CCA.car

predict.cover.CCA.car = data.frame(lm2.cover.CCA.car$x,
                               lm2.cover.CCA.car$y,
                               lm2.cover.CCA.car$x*lm2.cover.CCA.car$regression.results[2,3]+lm2.cover.CCA.car$regression.results[2,2],
                               lm2.cover.CCA.car$x*lm2.cover.CCA.car$confidence.intervals[2,4]+lm2.cover.CCA.car$confidence.intervals[2,2],
                               lm2.cover.CCA.car$x*lm2.cover.CCA.car$confidence.intervals[2,5]+lm2.cover.CCA.car$confidence.intervals[2,3])
names(predict.cover.CCA.car) = c("x","y","fit","lwr","upr")

# Plot CN and RB Calcifying cover
Sumcar_cov =  ggplot(predict.Sumcover.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17)+
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.Sumcover.v2.car$regression.results[2,3], intercept = lm2.Sumcover.v2.car$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.Sumcover.v2.car$confidence.intervals[2,4], intercept = lm2.Sumcover.v2.car$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Sumcover.v2.car$confidence.intervals[2,5], intercept = lm2.Sumcover.v2.car$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,85), xlim = c(0,85))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Total Calcifier Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))

Sumcar_cov

#Plot CN and RB coral cover
car_cov =  ggplot(predict.cover.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.cover.v2.car$regression.results[2,3], intercept = lm2.cover.v2.car$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.cover.v2.car$confidence.intervals[2,4], intercept = lm2.cover.v2.car$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.cover.v2.car$confidence.intervals[2,5], intercept = lm2.cover.v2.car$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 8, 2.5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 8, 2.5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,7.8), xlim = c(0,7.8))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Coral Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))

car_cov

#Plot CN and RB CCA Cover
car_cov.CCA =  ggplot(predict.cover.CCA.car, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#313695', shape = 17)+
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#313695', alpha = 0.3)+
  geom_abline(slope = lm2.cover.CCA.car$regression.results[2,3], intercept = lm2.cover.CCA.car$regression.results[2,2], colour = '#313695', linewidth = 1.3)+
  geom_abline(slope = lm2.cover.CCA.car$confidence.intervals[2,4], intercept = lm2.cover.CCA.car$confidence.intervals[2,2], colour = '#313695', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.cover.CCA.car$confidence.intervals[2,5], intercept = lm2.cover.CCA.car$confidence.intervals[2,3], colour = '#313695', linetype = 3, linewidth = 1.2)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 85, 15)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,85), xlim = c(0,85))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "CCA Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))

car_cov.CCA

#Plot site-level coral cover
SiteCOV = ggplot(Site.Cov, aes(x = meanRB, y = meanCN))+
  theme_classic()+
  geom_abline(intercept = 0, slope = 1, colour = 'black')+
  geom_errorbarh(aes(xmin = RB.conf.low, xmax = RB.conf.upr), height = 0, colour = '#313695')+
  geom_errorbar(aes(ymin = CN.conf.low, ymax = CN.conf.upr), width = 0, colour = '#313695')+
  geom_point(cex = 4, colour = '#313695', shape = 17)+
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 10, 2.5))+
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 10, 2.5))+
  coord_cartesian(ylim = c(-1.5,10.5), xlim = c(-1.5,10.5))+
  theme(text = element_text(size = 14),plot.title = element_text(size = 14),axis.text = element_text(size = 14),axis.text.x = element_text(color="black"),axis.ticks = element_line(color = "black"))+
  labs(title = "Site-Level Coral Cover", x = "ReefBudget (%)", y = "CoralNet (%)")+
  theme(plot.title = element_text(hjust = 0.5))

SiteCOV

#### Difference Plots#######-----------------------------------------
#load data - this is Gross G 
prico.cn.raw <- CN.all.tidy
prico.rb.raw <- RB.tidy

unique(prico.cn.raw$rates)

#merge cn and rb dfs 
prico.cn.v1 <- prico.cn.raw %>% 
  select(totalg, Site, Transect, rates) %>% 
  filter(rates == "Cari_v1_wo_bio") %>%
  rename(site = Site,
         transect = Transect)

prico.cn <- prico.cn.raw %>% 
  select(totalg, Site, Transect, rates) %>% 
  filter(rates == "Carib_v2_wo_bio.GBncrmp") %>%
  rename(site = Site,
         transect = Transect)


str(prico.cn)

#gross g - coral g + cca g 
prico.rb <- prico.rb.raw %>% 
  select(Totalg, site, transect) %>% 
  mutate(rates = "ReefBudget") %>% 
  rename(totalg = Totalg)

str(prico.rb)

prico <- rbind(prico.rb, prico.cn)
prico$rates[prico$rates == "Carib_v2_wo_bio.GBncrmp"] <- "CoralNet"
str(prico)
unique(prico$rates)
unique(prico$transect)

prico.v1v2 = rbind(prico.cn.v1,prico.cn)

## calculate and plot bias between method
prico.diff <- prico %>% 
  pivot_wider(names_from = "rates", values_from = "totalg") %>% 
  group_by(site, transect) %>% 
  mutate(difference = CoralNet - ReefBudget, 
         mean_g = (ReefBudget + CoralNet)/2)  %>% 
  ungroup()

prico.mean.diff <- prico.diff %>% 
  summarise(mean_diff = mean(difference),
            sd_diff = sd(difference),
            LoA_low = mean_diff-(1.96*sd_diff),
            LoA_high = mean_diff+(1.96*sd_diff))

## calculate and plot bias between CoralNet versions
prico.diff.v1v2 <- prico.v1v2 %>% 
  pivot_wider(names_from = "rates", values_from = "totalg") %>% 
  group_by(site, transect) %>% 
  mutate(difference = Carib_v2_wo_bio.GBncrmp - Cari_v1_wo_bio, 
         mean_g = (Carib_v2_wo_bio.GBncrmp + Cari_v1_wo_bio)/2)  %>% 
  ungroup()

prico.mean.diff.v1v2 <- prico.diff.v1v2 %>% 
  summarise(mean_diff = mean(difference),
            sd_diff = sd(difference),
            LoA_low = mean_diff-(1.96*sd_diff),
            LoA_high = mean_diff+(1.96*sd_diff))


#### CCC - puerto rico ####
#Between Methods
(agree.prico <- agree_test(x = prico.diff$ReefBudget,
                           y = prico.diff$CoralNet)
)

#Between CoralNet versions
(agree.prico.v1v2 <- agree_test(x = prico.diff.v1v2$Cari_v1_wo_bio,
                                y = prico.diff.v1v2$Carib_v2_wo_bio.GBncrmp)
)

#Plot difference between methods in Bland Altman plot
Dif_car <- ggplot(prico.diff) + 
  geom_abline(slope = 0,
              intercept= seq((agree.prico$loa$upper.ci[1]*-1), (agree.prico$loa$lower.ci[1]*-1), 0.01),
              colour = "#a50026", alpha = 0.1, linewidth = 1.8) +
  geom_hline(yintercept = 0, lty = 1, linewidth = 1.05)+
  geom_hline(yintercept=prico.mean.diff$mean_diff, colour = '#a50026', linewidth = 1.5) +
  geom_hline(yintercept=prico.mean.diff$LoA_low, lty = 3, colour = 'black') +
  geom_hline(yintercept=prico.mean.diff$LoA_high, lty = 3, colour = 'black') +
  geom_hline(yintercept=agree.prico$loa$lower.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_hline(yintercept=agree.prico$loa$upper.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_point(aes(mean_g, difference), cex = 2.5, colour = '#a50026', shape = 15) +
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 1.8, .5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(-1.0, 1.0, .2)) +
  coord_cartesian(ylim = c(-1.0,1.0), xlim = c(0,1.7))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(y= "CoralNet v2 - ReefBudget", 
       x = "Averaged Gross G",
       title = "Western Atlantic rates")+
  theme(plot.title = element_text(hjust = 0.5))

Dif_car

#Plot difference between CoralNet versions in Bland Altman plot
Dif_car.v1v2 <- ggplot(prico.diff.v1v2) + 
  geom_abline(slope = 0,
              intercept= seq((agree.prico.v1v2$loa$upper.ci[1]*-1), (agree.prico.v1v2$loa$lower.ci[1]*-1), 0.01),
              colour = "#a50026", alpha = 0.15, linewidth = 1.8) +
  geom_hline(yintercept = 0, lty = 1, linewidth = 1.05)+
  geom_hline(yintercept=prico.mean.diff.v1v2$mean_diff, colour = '#a50026', linewidth = 1.5) +
  geom_hline(yintercept=prico.mean.diff.v1v2$LoA_low, lty = 3, colour = 'black') +
  geom_hline(yintercept=prico.mean.diff.v1v2$LoA_high, lty = 3, colour = 'black') +
  geom_hline(yintercept=agree.prico.v1v2$loa$lower.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_hline(yintercept=agree.prico.v1v2$loa$upper.ci[1]*-1, lty = 2, colour = '#a50026') +
  geom_point(aes(mean_g, difference), cex = 2.5, colour = '#a50026', shape = 15) +
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 1.8, .5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(-1.0, 1.0, .2)) +
  coord_cartesian(ylim = c(-1.0,1.0), xlim = c(0,1.7))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(y= "CoralNet v2 - CoralNet v1", 
       x = "Averaged Gross G",
       title = "Bland-Altman Difference")+
  theme(plot.title = element_text(hjust = 0.5))

Dif_car.v1v2


############################################################Chagos Data#######################################
#load data 
data <-  read.csv("ExtractedData/tidy_rb_cn_totalcoralg_CI.csv")

#create a df for coral carbonate for each method/rates
carb.method <- data %>% 
  select(-c(lower.ci, upper.ci, method)) %>% 
  group_by(atoll, site, transect) %>% 
  pivot_wider(names_from = rates, values_from = coralg) %>% 
  ungroup()

#create a df for upper ci only 
upper.ci <- data %>% 
  select(-c(lower.ci, coralg, method)) %>% 
  group_by(atoll, site, transect) %>% 
  pivot_wider(names_from = rates, values_from = upper.ci) %>% 
  ungroup()

#create a df for lower ci only  
lower.ci <-  data %>% 
  select(-c(upper.ci, coralg, method)) %>% 
  group_by(atoll, site, transect) %>% 
  pivot_wider(names_from = rates, values_from = lower.ci) %>% 
  ungroup()

#### Coral G Model II regression and Plots #### 
#Model II regression
#CoralNet v2 (Chagos) vs RB CCRI
lm2.Nobe.v2.chag = lmodel2(chagos_v2_wo_bio~chagos_v2_wo_bio_rb, data = carb.method, "relative","relative", nperm = 999)
lm2.Nobe.v2.chag
predict.v2.chag = data.frame(lm2.Nobe.v2.chag$x,
                            lower.ci$chagos_v2_wo_bio_rb,
                            upper.ci$chagos_v2_wo_bio_rb,
                            lm2.Nobe.v2.chag$y,
                            lower.ci$chagos_v2_wo_bio,
                            upper.ci$chagos_v2_wo_bio,
                            lm2.Nobe.v2.chag$x*lm2.Nobe.v2.chag$regression.results[2,3]+lm2.Nobe.v2.chag$regression.results[2,2],
                            lm2.Nobe.v2.chag$x*lm2.Nobe.v2.chag$confidence.intervals[2,4]+lm2.Nobe.v2.chag$confidence.intervals[2,2],
                            lm2.Nobe.v2.chag$x*lm2.Nobe.v2.chag$confidence.intervals[2,5]+lm2.Nobe.v2.chag$confidence.intervals[2,3])

names(predict.v2.chag) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#CN Chagos vs CN Indo-Pacific (v2)
lm2.Nobe.CN = lmodel2(chagos_v2_wo_bio~indo_v2_wo_bio, data = carb.method, "relative","relative", nperm = 999)
lm2.Nobe.CN
predict.CN.chag = data.frame(lm2.Nobe.CN$x,
                             lower.ci$indo_v2_wo_bio,
                             upper.ci$indo_v2_wo_bio,
                             lm2.Nobe.CN$y,
                             lower.ci$chagos_v2_wo_bio,
                             upper.ci$chagos_v2_wo_bio,
                             lm2.Nobe.CN$x*lm2.Nobe.CN$regression.results[2,3]+lm2.Nobe.CN$regression.results[2,2],
                             lm2.Nobe.CN$x*lm2.Nobe.CN$confidence.intervals[2,4]+lm2.Nobe.CN$confidence.intervals[2,2],
                             lm2.Nobe.CN$x*lm2.Nobe.CN$confidence.intervals[2,5]+lm2.Nobe.CN$confidence.intervals[2,3])

names(predict.CN.chag) = c("x","x.lwr","x.upr", "y","y.lwr","y.upr", "fit","lwr","upr")

#### Plot
#Plot CNv2 vs RB CCRI Coral G
chag_v2 = ggplot(predict.v2.chag, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#e8994d', shape = 15) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#e8994d', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.v2.chag$regression.results[2,3], intercept = lm2.Nobe.v2.chag$regression.results[2,2], colour = '#e8994d', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.v2.chag$confidence.intervals[2,4], intercept = lm2.Nobe.v2.chag$confidence.intervals[2,2], colour = '#e8994d', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.v2.chag$confidence.intervals[2,5], intercept = lm2.Nobe.v2.chag$confidence.intervals[2,3], colour = '#e8994d', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#e8994d', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#e8994d', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,27), xlim = c(0,27))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "Coral G", x = bquote("ReefBudget-CCRI (kg "~CaCO[3]/m^2/yr~")"), y = bquote("CoralNet v2 (kg "~CaCO[3]/m^2/yr~")"))+
  theme(plot.title = element_text(hjust = 0.5))

chag_v2

#Plot CNv2 (Chagos) vs CNv2 (Indo-Pacific)
chag_CN = ggplot(predict.CN.chag, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = 1) +
  geom_point(cex = 2.5, colour = '#e8994d', shape = 15) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), fill = '#e8994d', alpha = 0.3)+
  geom_abline(slope = lm2.Nobe.CN$regression.results[2,3], intercept = lm2.Nobe.CN$regression.results[2,2], colour = '#e8994d', linewidth = 1.3)+
  geom_abline(slope = lm2.Nobe.CN$confidence.intervals[2,4], intercept = lm2.Nobe.CN$confidence.intervals[2,2], colour = '#e8994d', linetype = 3, linewidth = 1.2)+
  geom_abline(slope = lm2.Nobe.CN$confidence.intervals[2,5], intercept = lm2.Nobe.CN$confidence.intervals[2,3], colour = '#e8994d', linetype = 3, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = x.lwr, xmax = x.upr), height = 0,colour = '#e8994d', alpha = 0.4) +
  geom_errorbar(aes(ymin = y.lwr, ymax = y.upr), width = 0,colour = '#e8994d', alpha = 0.4) +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 30, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(0, 32, 5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0,30), xlim = c(0,30.5))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black"))+
  labs(title = "CoralNet (Coral G)", x = bquote("Indo-Pacific CoralNet"), y = bquote("Chagos CoralNet"))+
  theme(plot.title = element_text(hjust = 0.5))

chag_CN

#### Difference plot - chagos ####
#load data 
chagos.data <-  data
#create a col for total carb for each method/rates
chagos.method <- chagos.data %>% 
  mutate(site = paste(atoll, site, sep = "_")) %>% 
  filter(rates %in% c( "chagos_v2_wo_bio",  "chagos_v2_wo_bio_rb")) %>% 
  select(site, transect, method, coralg, lower.ci, upper.ci) %>% 
  pivot_wider(names_from = method, values_from = c(coralg, lower.ci, upper.ci)) %>% 
  ungroup()

#### calculate and plot margin of error between chagos v2 rates
(chagos.diff <- chagos.method %>% 
   group_by(site, transect) %>% 
   mutate(difference = coralg_CoralNet - coralg_ReefBudget, 
          mean_g = (coralg_ReefBudget + coralg_CoralNet)/2) %>% 
   ungroup()
)

#mean bias
chagos.mean.diff <- chagos.diff %>% 
  summarise(mean_diff = mean(difference),
            sd_diff = sd(difference),
            LoA_low = mean_diff-(1.96*sd_diff),
            LoA_high = mean_diff+(1.96*sd_diff))

(agree.chagos <- agree_test(x = chagos.method$coralg_ReefBudget,
                            y = chagos.method$coralg_CoralNet)
)

#Plot differences between CoralNet v2 (Chagos) and ReefBudget CCRI
Dif_chag = ggplot(chagos.diff) +
  geom_abline(slope = 0,
              intercept= seq((agree.chagos$loa$upper.ci[1]*-1), (agree.chagos$loa$lower.ci[1]*-1), 0.1),
              colour = "#e8994d", alpha = 0.15, linewidth = 1.8) +
  geom_hline(yintercept = 0, lty = 1, linewidth = 1.05)+
  geom_hline(yintercept=chagos.mean.diff$mean_diff, colour = '#e8994d', linewidth = 1.5) +
  geom_hline(yintercept=chagos.mean.diff$LoA_low, lty = 3, colour = 'black') +
  geom_hline(yintercept=chagos.mean.diff$LoA_high, lty = 3, colour = 'black') +
  geom_hline(yintercept=agree.chagos$loa$lower.ci[1]*-1, lty = 2, colour = '#e8994d') +
  geom_hline(yintercept=agree.chagos$loa$upper.ci[1]*-1, lty = 2, colour = '#e8994d') +
  geom_point(aes(mean_g, difference), cex = 2.5, colour = '#e8994d', shape = 15) +
  theme_classic() +
  scale_x_continuous(expand= c(0,0), breaks = seq(0, 18, 5)) +
  scale_y_continuous(expand= c(0,0), breaks = seq(-10, 10, 2)) +
  coord_cartesian(ylim = c(-10,10), xlim = c(0,17))+
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  labs(y= "CoralNet v2 - ReefBudget CCRI", 
       x = "Averaged Coral G",
       title = "Bland-Altman Difference in Coral G")+
  theme(plot.title = element_text(hjust = 0.5))
Dif_chag


############################################################Combined Figures######################################


#Combine models
lm2.Nobe.v1.indo$regression.results$Data = "Indo-Pacific_v1"
lm2.Nobe.v2.indo$regression.results$Data = "Indo-Pacific_v2"
lm2.Nobe.v1.car$regression.results$Data = "Western Atlantic_v1"
lm2.Nobe.v2.car$regression.results$Data = "Western Atlantic_v2"
lm2.sumcover.v2.indo$regression.results$Data = "Indo-Pacific_cover"
lm2.Sumcover.v2.car$regression.results$Data = "Western Atlantic_cover"
lm2.Nobe.v2.chag$regression.results$Data = "Chagos_v2"
lm2.Nobe.v1v2.car$regression.results$Data = "Western Atlantic_v1v2"
lm2.Nobe.v1v2.indo$regression.results$Data = "Indo-Pacific_v1v2"

All.reg = rbind(lm2.Nobe.v1.indo$regression.results[2,],
                lm2.Nobe.v2.indo$regression.results[2,],
                lm2.Nobe.v1.car$regression.results[2,],
                lm2.Nobe.v2.car$regression.results[2,],
                lm2.sumcover.v2.indo$regression.results[2,],
                lm2.Sumcover.v2.car$regression.results[2,],
                lm2.Nobe.v2.chag$regression.results[2,],
                lm2.Nobe.v1v2.car$regression.results[2,],
                lm2.Nobe.v1v2.indo$regression.results[2,])

lm2.Nobe.v1.indo$confidence.intervals$Data = "Indo-Pacific_v1"
lm2.Nobe.v2.indo$confidence.intervals$Data = "Indo-Pacific_v2"
lm2.Nobe.v1.car$confidence.intervals$Data = "Western Atlantic_v1"
lm2.Nobe.v2.car$confidence.intervals$Data = "Western Atlantic_v2"
lm2.sumcover.v2.indo$confidence.intervals$Data = "Indo-Pacific_cover"
lm2.Sumcover.v2.car$confidence.intervals$Data = "Western Atlantic_cover"
lm2.Nobe.v2.chag$confidence.intervals$Data = "Chagos_v2"
lm2.Nobe.v1v2.car$confidence.intervals$Data = "Western Atlantic_v1v2"
lm2.Nobe.v1v2.indo$confidence.intervals$Data = "Indo-Pacific_v1v2"

All.conf = rbind(lm2.Nobe.v1.indo$confidence.intervals[2,],
                lm2.Nobe.v2.indo$confidence.intervals[2,],
                lm2.Nobe.v1.car$confidence.intervals[2,],
                lm2.Nobe.v2.car$confidence.intervals[2,],
                lm2.sumcover.v2.indo$confidence.intervals[2,],
                lm2.Sumcover.v2.car$confidence.intervals[2,],
                lm2.Nobe.v2.chag$confidence.intervals[2,],
                lm2.Nobe.v1v2.car$confidence.intervals[2,],
                lm2.Nobe.v1v2.indo$confidence.intervals[2,])

All.reg.conf2 = merge(All.reg,All.conf, by = c("Method","Data"))

All.reg.conf = All.reg.conf2 %>% separate(Data, c("Region","Version"), "_")
All.reg.conf$Data = All.reg.conf2$Data
names(All.reg.conf) = c("Method","Region","Version","Intercept","Slope","Angle","p-value","lwr.intercept","upr.intercept","lwr.slope","upr.slope","Data")
All.reg.conf$Region = factor(All.reg.conf$Region, levels = c("Western Atlantic", "Indo-Pacific", "Chagos"))
All.reg.conf$Version = factor(All.reg.conf$Version, levels = c("v1","v2","cover","v1v2"))
All.reg.region = split(All.reg.conf, All.reg.conf$Region)

#### lineplots ####
##slope- all regions
Slope.lineplot = ggplot(All.reg.conf[All.reg.conf$Region != 'Chagos' & All.reg.conf$Version != 'v1v2',], aes(x = Region, y = Slope, colour = Version))+
  geom_hline(yintercept = 1, colour = 'black', linetype = 2)+
  geom_linerange(aes(x = Region, ymin = lwr.slope, ymax = upr.slope, colour = Version), linewidth = 3, position = position_dodge(0.5), alpha = 0.6)+
  geom_point(aes(x = Region, y = Slope, colour = Version, shape = Version), position = position_dodge(0.5), size = 6, fill = "white")+
  scale_colour_manual(values = c("#f46d43","#a50026","#313695"), labels = c('CoralNet v1','CoralNet v2','Calcifier Cover'))+
  scale_shape_manual(values = c(16,15,17))+
  guides(shape = 'none')+
  theme_classic() +
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  theme(legend.position = c(0.3,0.9), legend.title = element_blank(), legend.text = element_text(size = 10))+
  labs(title = "Model II Major Axis Slope", x = "Regional Rates", y = "Slope")+
  theme(plot.title = element_text(hjust = 0.5))
Slope.lineplot

#Chagos-slope
Slope.lineplot.chag = ggplot(All.reg.region$Chagos, aes(x = Region, y = Slope))+
  geom_hline(yintercept = 1, colour = 'black', linetype = 2)+
  geom_linerange(aes(x = Region, ymin = lwr.slope, ymax = upr.slope),colour = '#e8994d', size = 3, position = position_dodge(0.5), alpha = 0.6)+
  geom_point(aes(x = Region, y = Slope, shape = Version), colour = '#e8994d', position = position_dodge(0.5), size = 6, fill = "white")+
  theme_classic() +
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none", legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(0.50,1.5)+
  labs(title = "Model II Major Axis Slope", x = "Chagos", y = "Slope")+
  theme(plot.title = element_text(hjust = 0.5))
Slope.lineplot.chag

#V1v2 Western Atlantic
Slope.lineplot.car.v1v2 = ggplot(All.reg.region$`Western Atlantic`[All.reg.region$`Western Atlantic`$Version == 'v1v2',], aes(x = Region, y = Slope))+
  geom_hline(yintercept = 1, colour = 'black', linetype = 2)+
  geom_linerange(aes(x = Region, ymin = lwr.slope, ymax = upr.slope),colour = "#a50026", size = 2, position = position_dodge(0.5), alpha = 0.6)+
  geom_point(aes(x = Region, y = Slope), colour = "#a50026", shape = 15, position = position_dodge(0.5), size = 3.5, fill = "white")+
  theme_classic() +
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none", legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(0.4,1.6)+
  labs(title = "Major Axis Type II Slope", x = "Western Atlantic", y = "Slope")+
  theme(plot.title = element_text(hjust = 0.5))
Slope.lineplot.car.v1v2

#V1v2 Indo-Pacific
Slope.lineplot.indo.v1v2 = ggplot(All.reg.region$`Indo-Pacific`[All.reg.region$`Indo-Pacific`$Version == 'v1v2',], aes(x = Region, y = Slope))+
  geom_hline(yintercept = 1, colour = 'black', linetype = 2)+
  geom_linerange(aes(x = Region, ymin = lwr.slope, ymax = upr.slope),colour = "#a50026", size = 2, position = position_dodge(0.5), alpha = 0.6)+
  geom_point(aes(x = Region, y = Slope),colour = "#a50026", shape = 15, position = position_dodge(0.5), size = 3.5, fill = "white")+
  theme_classic() +
  theme(text = element_text(size = 11), plot.title = element_text(size = 11), axis.text = element_text(size = 11), axis.text.x = element_text(color="black"), axis.ticks = element_line(color = "black")) +
  theme(legend.position = "none", legend.title = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylim(0.4,1.6)+
  labs(title = "Major Axis Type II Slope", x = "Indo-Pacific", y = "Slope")+
  theme(plot.title = element_text(hjust = 0.5))
Slope.lineplot.indo.v1v2


#### Plot Parking Lot ####
#Final Plot Names
indo_v1 #1:1 indo CNv1 vs RB Gross G
indo_v2 #1:1 indo CNv2 vs RB Gross G
indo_v1v2 #1:1 indo CNv1 vs CNv2 Gross G
indo_RB.regional #1:1 indo RB Gross G using Regional Rates vs Local Rates
indo_Sumcov #1:1 indo calcifier cover (Coral + CCA sum)
indo_cov #1:1 indo CN vs RB coral cover
indo_cov.CCA #1:1 indo CN vs RB CCA cover

car_v1 #1:1 caribbean CNv1 vs RB Gross G
car_v2 #1:1 caribbean CNv2 vs RB Gross G
car_v1v2 #1:1 caribbean CNv2 vs CNv1 Gross G
Sumcar_cov #1:1 caribbean calcifier cover (Coral + CCA sum)
car_cov #1:1 caribbean CN vs RB coral cover
car_cov.CCA #1:1 caribbean CN vs RB CCA cover

Dif_car #Difference plot of CNv2 from RB across Average CN/RB Gross G for the caribbean
Dif_car.v1v2 #Difference plot of CNv2 from Cnv1 across Average CNv1/CNv2 Gross G for the caribbean
Dif_indo #Difference plot of CNv2 from RB across Average CN/RB Gross G for Indonesia
Dif_indo.v1v2 #Difference plot of CNv2 from CNv1 across average CNv1/CNv2 Gross G for Indonesia

chag_v2 #1:1 chagos CNv2 vs RB Coral G
Dif_chag #Difference plot of CNv2 from RB across Average CN/RB Coral G for Chagos
chag_CN #1:1 chagos CNv2 vs CNv2 Indo-Indo-Pacific

Slope.lineplot # Model II Major Axis (MA) Slopes of Caribbean (v1,v2,cover), Indonesia(v1,v2,cover)
Slope.lineplot.chag #Model II Major Axis (MA) Slope of Chagos (v2 only)
Slope.lineplot.car.v1v2 #Model II Major Axis (MA) Slope of Western Atlantic (v1 vs v2)
Slope.lineplot.indo.v1v2 #Model II Major Axis (MA) Slope of Indo-Indo-Pacific (v1 vs v2)

#Figure 2
Fig2.bottom = indo_v1 + indo_v2 + indo_Sumcov + plot_annotation(title = "Indo-Pacific rates",
                                                             theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("d","e","f"))) & theme(plot.tag = element_text(size = 18))
Fig2.bottom
Fig2.top = car_v1 + car_v2 + Sumcar_cov + plot_annotation(title = "Western Atlantic rates",
                                                             theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("a","b","c"))) & theme(plot.tag = element_text(size = 18))
Fig2.top
#Put them together in another software

#Figure 3
Slope.lineplot + Dif_car + Dif_indo +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))

#Figure 4
chag_v2 + Slope.lineplot.chag + Dif_chag +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))

#Figure 5
indo_RB.regional + chag_CN +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))

#Figure S1
FigS1.top = car_v1v2 + Slope.lineplot.car.v1v2 +  Dif_car.v1v2 + plot_annotation(title = "Western Atlantic rates", theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("a","b","c"))) & theme(plot.tag = element_text(size = 18))
FigS1.top

FigS1.bottom = indo_v1v2 + Slope.lineplot.indo.v1v2 + Dif_indo.v1v2 + plot_annotation(title = "Indo-Pacific rates", theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("d","e","f"))) & theme(plot.tag = element_text(size = 18))
FigS1.bottom
#Put them together in another software

#Figure S2
FigS2.top = Sumcar_cov + car_cov + car_cov.CCA + plot_annotation(title = "Western Atlantic rates", theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("a","b","c"))) & theme(plot.tag = element_text(size = 18))
FigS2.top

FigS2.bottom = indo_Sumcov + indo_cov + indo_cov.CCA + plot_annotation(title = "Indo-Pacific rates", theme = theme(plot.title = element_text(size = 18)), tag_levels = list(c("d","e","f"))) & theme(plot.tag = element_text(size = 18))
FigS2.bottom
#Put them together in another software

#Figure S3
FigS3_site = SiteCOV + SiteTotalg +
  plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 18))
FigS3_site
