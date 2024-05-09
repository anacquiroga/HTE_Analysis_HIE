###################################################################################################################
#
#* Picture this: Making health insurance choices easier for those who need it
#* 
#* Ana Cecilia Quiroga Gutierrez
#* University of Lucerne
#*
#* ################################################################################################################


#* Note: heterogeneous treatment effect analysis is based on work by Helmut Farbmacher et al.: 
#* Farbmacher, H., K?gel, H., & Spindler, M. (2021). Heterogeneous effects of poverty on attention. Labour Economics, 71, 102028. https://doi.org/10.1016/j.labeco.2021.102028


setwd("C:\\Users\\qra1\\OneDrive\\personal\\JBEE_revision")

library(haven)
library(ivreg)
library(grf)
library(caret)
library(ggplot2)
library(fastDummies)
library(dplyr)
library(ggpubr)
library(DiagrammeR)
library(jtools)
library(tidyverse)
library(lmtest)
library(sandwich)
library(vtable)
library(openxlsx)
library(estimatr)


# Open data 
data_ <- read_dta("C:/Users/qra1/Dropbox/PhD Ana/experiment_2021/R_hte/submission/data2811.dta")

# Generate binary variable for those who had 2 optional types of information 
data_$two_infotypes <- ifelse(data_$treat==4, 1, 0)

# saving variables female and health status as numeric for descriptive statistics 
data_$female <- as.numeric(data_$female)
data_$healthstat <- as.numeric(data_$healthstat)
data_$sawpinfo3 <- as.numeric(ifelse(data_$saw_pinfo3==1,1,0))
data_$newbalance100 <- as.numeric(data_$newbalance / 100)


#generating a variable for cumulative switches for each participant through the experiment
data_ <- data_ %>%
  arrange(id, round) %>% 
  group_by(id) %>% 
  mutate(cumulative_switch = cumsum(switched))
data_$cumulative_switch
#table showing cumulative switches per round per participant
cumulative_switch_table <- data_ %>%
  select(id, round, cumulative_switch) %>%
  spread(round, cumulative_switch)
head(cumulative_switch_table)
cumulative_switch_table

# same cumulative variable but for lag_switched:
data_ <- data_ %>%
  arrange(id, round) %>% 
  group_by(id) %>% 
  mutate(cumulative_lagswitch = cumsum(lag_switched))
data_$cumulative_lagswitch

#original analysis included balance or 'newbalance' in random forest analysis, but since balance is mostly affected by plan choices and switching HI
# this was omitted and instead, cumulative switches and current plan were included, also number of correct and answered trivia questions 
ols_newbalance <- lm(newbalance ~ cumulative_lagswitch + correct_quiz + total_qs_answered + factor(plan), data = data_)
summary(ols_newbalance, vcov = vcovCL(model, cluster = "id"))
tidy(ols_newbalance, conf.int = TRUE)

ols_newbalance_clusterse <- vcovCL(ols_newbalance, cluster = ~ id, type = "HC1")
summary(ols_newbalance_clusterse)

# OLS model with clustered standard errrors (robust) (stata replication)
ols_newbalance_clusterse_corrected <- coeftest(ols_newbalance,
                                              vcov = vcovCL,
                                              type = "HC1",
                                              df = 432,  # There are 433 participants, so 433-1 = 432
                                              cluster = ~id)
tidy(ols_newbalance_clusterse_corrected, conf.int = TRUE)


#Heterogeneous effects of information provision for HI decision support.
# Outcomes of interest:
# 1. Heterogeneous probability of visualizing personalized information on round 8
# 2. Heterogeneous treatment effects of # of personalized information options on likelihood of visualizing personalized information
# 3. Heterogeneous treatment effects of graphical and numerical personalized information on distance to plan with lowest expected costs on rounds 4-7 and 9-12
# !. MAYBE: ADD HETEROGENEOUS TREATMENT EFFECTS OF INFO ON SWITCHING PLAN AFTER MAJOR HEALTH EVENTS?

#Analysis steps for each outcome:
# 1. Use generalized random forest to find important variables
# 2. predict conditional probabilities and treatment effect
# 3. Analysis of differences in treatment effect (heterogeneity)
# 4. Verify using statistics, ols, interactions, etc
# 5. Include description of important variables for example hilm domains and their distribution

# Distribution of final balance for all treatments 
ggplot(subset(data_, round ==12 ), aes(x = finalbalance, fill = factor(treat))) +
  geom_density(alpha = 0.5) +
  theme_classic() 


##################################################################################
# Descriptive statistics for outcome and main regressors
################################################################################


#%%%%%%%%%
# outcome variable 1: visualizing personalized information - Causal Forest
#   main regresors: having 1 or 2 information options (treatment)

# Round 3
datar3 <- subset(data_, round == 3 & treat > 1)
datar3_descriptivevars <- c('saw_pinfo3','two_infotypes')
datar3_descriptive <- subset(datar3[datar3_descriptivevars])
st(datar3_descriptive)
st(datar3_descriptive, out='csv', file='descriptive_outcome2_r3.csv')


#%%%%%%%%%
# outcome variable 2: distance to best plan based on expected costs
#   main regresors: treatment (having optional personalized information), visualizing personalized information

# Graph info round 4-7: distance to best plan based on expected costs
datat12r47 <- subset(data_, round > 3 & round < 8 & treat < 2 | round > 3 & round < 8 & treat == 2)
datat12r47_descriptivevars <- c('distance_best_plan', 'treat', 'saw_pinfo3')
datat12r47_descriptive <- subset(datat12r47[datat12r47_descriptivevars])
datat12r47_descriptive$treat <- ifelse(datat12r47_descriptive$treat==2,1,0)
st(datat12r47_descriptive)
st(datat12r47_descriptive, out='csv', file='descriptive_outcome3_graph47.csv')


# Numerical info round 4-7: distance to best plan based on expected costs
datat13r47 <- subset(data_, round > 3 & round < 8 & treat < 2 | round > 3 & round < 8 & treat == 3)
datat13r47_descriptivevars <- c('distance_best_plan', 'treat', 'saw_pinfo3')
datat13r47_descriptive <- subset(datat13r47[datat13r47_descriptivevars])
datat13r47_descriptive$treat <- ifelse(datat13r47_descriptive$treat==3,1,0)
st(datat13r47_descriptive)
st(datat13r47_descriptive, out='csv', file='descriptive_outcome3_numerical47.csv')



##################################################################################
# Descriptive statistics for covariates included in the analysis
################################################################################

allvarsused <- c( 'female',
                  'age',
                  'uni',
                  's_doc',
                  'total_ok',
                  'hilm_beh_avg',
                  'hilm_conf_avg',
                  'm_income',
                  'healthstat',
                  'ra_gains',
                  'rs_loss',
                  'reflection',
                  'pw1060',
                  'pw6090',
                  'pw1090',
                  's_canton',
                  's_study_area',
                  's_general_risk',
                  's_swiss',
                  'sure',
                  'round_qs',
                  'correct_quiz',
                  'total_qs_answered',
                  'cumulative_lagswitch',
                  'saw_pinfo3',
                  's_switchedhi',
                  's_ded',
                  's_supplementary',
                  'plan',
                  'c_current',
                  'c_previous',
                  'minor_event',
                  'round',
                  'cum_c',
                  'two_infotypes',
                  'newbalance'

  )


descriptivedata <- subset(data_[allvarsused], round == 12)
st(descriptivedata)
st(descriptivedata, out='csv', file='descriptivestats.csv')

###############################################################################
#%%%% ANALYSIS



#==============================================================================
###################################################################################################
# CAUSAL FOREST
# Heterogeneous treatment effect for visualizing personalized information when having 2 vs 1 
# Personalized information options (treatment 4 vs treatments 1 and 2)
###################################################################################################
#==============================================================================

######################################################################
#Round 3 - causal forest, effect of having 2 vs 1 types of personal info on accessing peronal info
######################################################################

#Data used for this analysis: datar3
#datar3 <- subset(data_, round == 3 & treat > 1)

table(datar3$treat, datar3$saw_pinfo)
summary(datar3$two_infotypes)

# Set of variables to be included in causal Forest
vars <- c('female',
          'uni',
          's_doc',
          'cumulative_lagswitch',
          'total_ok',
          'hilm_beh_avg',
          'hilm_conf_avg',
          'm_income',
          'age',
          'healthstat',
          'ra_gains',
          'rs_loss',
          'reflection',
          'pw1060',
          'pw6090',
          'pw1090',
          'c_current',
          'c_previous',
          'cum_c',
          'plan',
          'minor_event',
          's_canton',
          's_study_area',
          'sure',
          's_general_risk',
          's_swiss',
          's_ded',
          's_supplementary',
          's_switchedhi',
          'total_qs_answered',
          'correct_quiz',
          'newbalance',
          'round_qs'
          
)

vars.full <- vars

names_cf <- c('Female',
              'Uni',
              'Doctor visits',
              'Cumulative HI switches',
              'Objective HI knowledge',
              'HILM-CH: Behavior',
              'HILM-CH: Confidence',
              'Income',
              'Age',
              'Health status',
              'Risk averse for gains',
              'Risk seeking for losses',
              'Reflection effect',
              'Probability overweighting 10-60%',
              'Probability overweighting 60-90%',
              'Probability overweighting 10-90%',
              'Major event in current round',
              'Major event in previous round',
              'Cumulative major events',
              'Plan',
              'Minor event',
              'Canton',
              'Study area',
              'DCS scale',
              'General risk question',
              'Swiss',
              'Deductible',
              'Supplementary insurance',
              'Last time switching HI',
              'Total trivia questions answered',
              'Correct trivia questions',
              'Balance',
              'Round trivia questions'
)





#covariates
X <- as.matrix(subset(datar3, select= vars))
colnames(X)<-vars

#outcome:
Y <- as.vector(datar3$saw_pinfo)

#treatment:
W <- as.vector(datar3$two_infotypes)

causal_forest3 <- causal_forest(X,
                               Y,
                               W,
                               num.trees=3000,
                               seed=123
)


# Building causal forest
vi.causal_forest3 <- variable_importance(causal_forest3)
rownames(vi.causal_forest3)<-vars
print(vi.causal_forest3)

plot(plot_causal3 <- get_tree(causal_forest3, 1))
plot_causal3

vi.saw_pinfo3 <- variable_importance(causal_forest3)
vi.saw_pinfo3 <- data.frame(vi.saw_pinfo3)
vi.saw_pinfo3$var.names <- names_cf
vi.saw_pinfo3 <- vi.saw_pinfo3[order(-vi.saw_pinfo3$vi.saw_pinfo3),]  # Order the data frame such that the covariate with the highest variable importance is in the last row
vi.saw_pinfo3$var.names <- factor(vi.saw_pinfo3$var.names, levels=vi.saw_pinfo3$var.names)
vi.saw_pinfo3$vi.saw_pinfo3.100 <- vi.saw_pinfo3$vi.saw_pinfo3*100  # Multiply the variable importance measure by 100 for readability
print(vi.saw_pinfo3)

# Generating variable importance plot for causal forest - visualizing personalized info on r3 having 2 vs 1 types of info
vi.plot.causal_pinfo3 <- ggplot(data=vi.saw_pinfo3, aes(x=var.names, y=vi.saw_pinfo3.100)) + geom_col(width=0.6, fill="darkblue", colour="darkblue") +
  theme_bw() +
  scale_y_continuous(expand=c(0.007,0), breaks=c(seq(0,20,5)), limits=c(0, 21)) +
  #theme(text=element_text(family="Times New Roman"),
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=8, margin = margin(t = 7, r = 0, b = 7, l = 0)),
        axis.title=element_text(size=15),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.background = element_rect(colour = "black", linewidth = 0.7),
        axis.ticks.x=element_line(linewidth = 0.7)) +
  labs(y="Round 3") + coord_flip()
vi.plot.causal_pinfo3
#------------------ included in draft --------------------------- 
# Grid of variable importance for causal forest of visualizing peronalized information when having 2 or 1 types of info on rounds 3 and 8
library(gridExtra)
grid.arrange(vi.plot.causal_pinfo3, ncol = 1, 
             top = "Variable Importance for Treatment Effect of 2 Personalized \nInfomation Sources on Acces to Information")
#----------------------------------------------------------------


# calculating tau.hat
c.pred <- predict(causal_forest3)$predictions
c.pred
summary(datar3$saw_pinfo)
summary(c.pred)
datar3$sawpinfohat_cf <- c.pred

# ATE estimation
ate_estimation <- average_treatment_effect(causal_forest3)
# Extract ATE estimate and standard error
ate_estimate <- ate_estimation["estimate"]
ate_se <- ate_estimation["std.err"]

# Print the results
print(paste("ATE Estimate:", ate_estimate))
print(paste("Standard Error:", ate_se))

# Calculate the 95% confidence interval
ci_lower <- ate_estimate - 1.96 * ate_se
ci_upper <- ate_estimate + 1.96 * ate_se
print(paste("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]"))

# Calculate the p-value
ate_p_value <- 2 * pnorm(-abs(ate_estimate / ate_se))
print(paste("P-value:", ate_p_value))



# Graph showing conditional treatment effect of 2 optional personalized info sources by accumulated balance and minor event taking place
cfr3_1 <- ggplot(data = datar3, aes(x = total_qs_answered, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  xlab("Total trivia questions") + 
  ylab("Predicted individual treatment effect") + 
  labs(color = "Minor event") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")
cfr3_1

# Graph showing conditional treatment effect of 2 optional personalized info sources by hilm-ch behavior domain and minor event taking place
cfr3_2 <- ggplot(data = datar3, aes(x = hilm_beh_avg, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  xlab("HILM-CH: Behavior") + 
  ylab("Predicted individual treatment effect") +
  labs(color = "Minor event") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")
cfr3_2

# Graph showing conditional treatment effect of 2 optional personalized info sources by accumulated balance and minor event taking place
cfr3_3 <- ggplot(data = datar3, aes(x = newbalance, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  xlab("Balance") + 
  ylab("Predictd individual treatment effect") +
  labs(color = "Minor event")+
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) 
cfr3_3


#------------- included in draft -------------------------------------
# grid with graphs of conditional TE on R3 of 2 optioanl personalized info graph by: trivia questions, balance, hilm-confidcence
# Generating image grid containing cfr3_1, cfr3_2, and cfr3_3 
library(grid)
# Extract the legend from one of the plots
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- g_legend(cfr3_1)
# Remove the legends from all plots
cfr3_1 <- cfr3_1 + theme(legend.position = "none")
cfr3_2 <- cfr3_2 + theme(legend.position = "none")
cfr3_3 <- cfr3_3 + theme(legend.position = "none")

# Arrange the plots and legend in a grid
fig6 <- grid.arrange(arrangeGrob(cfr3_1, cfr3_2, cfr3_3, ncol = 3), 
                     arrangeGrob(nullGrob(),nullGrob(),nullGrob(),
                                 legend,
                                 ncol=1),
                     nrow=2,
                     heights=c(0.9,0.1),
                     top = "Predicted individual treatment effect of 2 types of personalized information on visualization")
fig6
ggsave(filename = "fig6.png", plot = fig6, width = 174/25.4, height = 100/25.4, units = "in", dpi = 300)
#-----------------------------------------------------------------------



ggplot(data = datar3, aes(x = hilm_conf_avg, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
geom_point() + 
geom_smooth(method = "lm", fullrange=TRUE) + 
xlab("HILM-CH: Confidence") + 
ylab("Conditional treatment effect") +
labs(color = "Minor event") +
theme_bw() +
theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
      legend.position="bottom")


a2_fig1 <- ggplot(data = datar3, aes(x = s_doc, y = sawpinfohat_cf)) + 
           geom_point() + 
           geom_smooth(method = "lm", fullrange=TRUE) + 
           xlab("Doctor visits") + 
           ylab("Conditional treatment effect") +
           theme_bw() +
           theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
                 legend.position="bottom")
a2_fig1
ggsave(filename = "a2_fig1.png", plot = a2_fig1, width = 174/25.4, height = 117/25.4, units = "in", dpi = 300)


ggplot(data = datar3, aes(x = correct_quiz, y = sawpinfohat_cf)) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  xlab("Correct trivia answers in round 3") + 
  ylab("Conditional treatment effect") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")

# Graph showing conditional treatment effect of 2 optional personalized info sources by # of trivia questions answered and minor event taking place
ggplot(data = datar3, aes(x = total_qs_answered, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
geom_point() + 
geom_smooth(method = "lm", fullrange=TRUE) + 
xlab("Total trivia questions answered through experiment") + 
ylab("Conditional treatment effect") + 
labs(color = "Minor event") +
theme_bw() +
theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
      legend.position="bottom")


ggplot(data = datar3, aes(x = s_doc, y = sawpinfohat_cf, color = factor(minor_event, labels=c("No", "A", "B")))) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) + 
  xlab("Doctor visits") + 
  ylab("Conditional treatment effect") +
  labs(color = "Minor event") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")


table(datar3$minor_event)
minorevent.aov <- aov(sawpinfohat_cf ~ minor_event, data = datar3)
summary(minorevent.aov)

library("ggpubr")
ggboxplot(datar3, x = "minor_event", y = "sawpinfohat_cf", 
          color = "minor_event", palette = c("blue", "darkgreen", "orange"),
          order = c("0", "1", "2"),
          ylab = "Conditional average treatment effect on accessing personalized info", xlab = "Events" )+
  scale_x_discrete(labels = c("No minor event", "Minor event A", "Minor event B"))


c.pred <- predict(causal_forest3)$predictions

#Table 4 - OLS Regression Analysis: Effect of 2 Information Types on Likelihood of Visualizing Personalized Information 
#Using the Most Important Variables According to Causal Forest Analysis as Predictors
olscf3 <- lm(sawpinfohat_cf ~ total_qs_answered + newbalance100 + hilm_beh_avg + factor(minor_event), data = datar3)
summary(olscf3, vcov = vcovCL(model, cluster = "id"))
tidy(olscf3, conf.int = TRUE)
olscf3_clusterse <- vcovCL(olscf3, cluster = ~ id, type = "HC1")
summary(olscf3_clusterse)
# OLS model with clustered standard errrors (robust) (stata replication)
olscf3_clusterse_corrected <- coeftest(olscf3,
                                             vcov = vcovCL,
                                             type = "HC1",
                                             df = 259,  # There are subsample of 260 participants
                                             cluster = ~id)
tidy(olscf3_clusterse_corrected, conf.int = TRUE)


# generate targeting operator characteristic graph (toc)
rate_cfr3 <- rank_average_treatment_effect(causal_forest3,
                                           predict(causal_forest3)$predictions)
plot(rate_cfr3)

write.xlsx(datar3, file="datar3_cf_sapinfohat.xlsx", asTable=TRUE)

#Appendix 2 - Causal forest analysis supplementary material

# Table 7. Subgroup Analysis of the Effect of Having two Optional Types of 
# personalized information on the Likelihood of Accessing it in Round 3 (OLS Estimates)

# ATE
ols_ap2.1 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3)
summary(ols_ap2.1, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.1, conf.int = TRUE)
ols_ap2.1_clusterse <- vcovCL(ols_ap2.1, cluster = ~ id, type = "HC1")
summary(ols_ap2.1_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.1_clusterse_corrected <- coeftest(ols_ap2.1,
                                        vcov = vcovCL , 
                                        type = "HC1",
                                        df = 259, #the substample is 260 participants
                                        cluster = ~id)
tidy(ols_ap2.1_clusterse_corrected, conf.int = TRUE)

# total trivia questions <34, and >=34

datar3$total_qs_34 <- as.numeric(ifelse(datar3$total_qs_answered<34,0,1))
datar3$total_qs_34
datar3totalqs34_0 <- subset(datar3, total_qs_34==0)
datar3totalqs34_1 <- subset(datar3, total_qs_34==1)
#less than 34 trivia questions
ols_ap2.2 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3totalqs34_0)
summary(ols_ap2.2, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.2, conf.int = TRUE)
ols_ap2.2_clusterse <- vcovCL(ols_ap2.2, cluster = ~ id, type = "HC1")
summary(ols_ap2.2_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.2_clusterse_corrected <- coeftest(ols_ap2.2,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 127, #the subsample is 128 participants
                                          cluster = ~id)
tidy(ols_ap2.2_clusterse_corrected, conf.int = TRUE)

#more than 34 trivia questions
ols_ap2.3 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3totalqs34_1)
summary(ols_ap2.3, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.2, conf.int = TRUE)
ols_ap2.3_clusterse <- vcovCL(ols_ap2.3, cluster = ~ id, type = "HC1")
summary(ols_ap2.3_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.3_clusterse_corrected <- coeftest(ols_ap2.3,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 131, #the subsample is 132 participants
                                          cluster = ~id)
tidy(ols_ap2.3_clusterse_corrected, conf.int = TRUE)

# HILM < 2.93 (median)
datar3$hilm293 <- as.numeric(ifelse(datar3$hilm_beh_avg >= 2.93,1,0))
datar3hilm293_0 <- subset(datar3, hilm293==0)
datar3hilm293_1 <- subset(datar3, hilm293==1)

#less than 2.93 on HILM behavior scale
ols_ap2.4 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3hilm293_0)
summary(ols_ap2.4, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.4, conf.int = TRUE)
ols_ap2.4_clusterse <- vcovCL(ols_ap2.4, cluster = ~ id, type = "HC1")
summary(ols_ap2.4_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.4_clusterse_corrected <- coeftest(ols_ap2.4,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 134, #the subsample is 135 participants
                                          cluster = ~id)
tidy(ols_ap2.4_clusterse_corrected, conf.int = TRUE)

#more than 2.93 on HILM behavior scale
ols_ap2.5 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3hilm293_1)
summary(ols_ap2.5, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.5, conf.int = TRUE)
ols_ap2.5_clusterse <- vcovCL(ols_ap2.5, cluster = ~ id, type = "HC1")
summary(ols_ap2.5_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.5_clusterse_corrected <- coeftest(ols_ap2.5,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 124, #the subsample is 125 participants
                                          cluster = ~id)
tidy(ols_ap2.5_clusterse_corrected, conf.int = TRUE)

# Balance <1195 (median)
datar3$newbalance1195 <- as.numeric(ifelse(datar3$newbalance<1195,0,1))
datar3balance1195_0 <- subset(datar3, newbalance1195==0)
datar3balance1195_1 <- subset(datar3, newbalance1195==1)
# less than 1195
ols_ap2.6 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3balance1195_0)
summary(ols_ap2.6, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.6, conf.int = TRUE)
ols_ap2.6_clusterse <- vcovCL(ols_ap2.6, cluster = ~ id, type = "HC1")
summary(ols_ap2.6_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.6_clusterse_corrected <- coeftest(ols_ap2.6,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 129, #the substample is 130 participants
                                          cluster = ~id)
tidy(ols_ap2.6_clusterse_corrected, conf.int = TRUE)
# more than 1195
ols_ap2.7 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3balance1195_1)
summary(ols_ap2.7, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.7, conf.int = TRUE)
ols_ap2.7_clusterse <- vcovCL(ols_ap2.7, cluster = ~ id, type = "HC1")
summary(ols_ap2.7_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.7_clusterse_corrected <- coeftest(ols_ap2.7,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 129, #the substample is 130 participants
                                          cluster = ~id)
tidy(ols_ap2.7_clusterse_corrected, conf.int = TRUE)


# Minor event
datar3$eventb <- as.numeric(ifelse(datar3$minor_event==2,1,0))
datar3eventb_0 <- subset(datar3, eventb==0)
datar3eventb_1 <- subset(datar3, eventb==1)

#no event B
ols_ap2.8 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3eventb_0)
summary(ols_ap2.8, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.8, conf.int = TRUE)
ols_ap2.8_clusterse <- vcovCL(ols_ap2.8, cluster = ~ id, type = "HC1")
summary(ols_ap2.8_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.8_clusterse_corrected <- coeftest(ols_ap2.8,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 190, #the substample is 191 participants
                                          cluster = ~id)
tidy(ols_ap2.8_clusterse_corrected, conf.int = TRUE)

#eventB
ols_ap2.9 <- lm(saw_pinfo ~ factor(two_infotypes), data = datar3eventb_1)
summary(ols_ap2.9, vcov = vcovCL(model, cluster = "id"))
tidy(ols_ap2.9, conf.int = TRUE)
ols_ap2.9_clusterse <- vcovCL(ols_ap2.9, cluster = ~ id, type = "HC1")
summary(ols_ap2.9_clusterse)
#OLS model with clustered standard errors (robust) (stata replication)
ols_ap2.9_clusterse_corrected <- coeftest(ols_ap2.9,
                                          vcov = vcovCL , 
                                          type = "HC1",
                                          df = 68, #the substample is 69 participants
                                          cluster = ~id)
tidy(ols_ap2.9_clusterse_corrected, conf.int = TRUE)



#=====================================================================================
#########################################################################################
# INSTRUMENTAL FOREST
# heterogeneous effects of personalized information on distance to best plan given expected costs - IV forest T2 (graphical per info) vs T1 (general info) rounds 4-7
########################################################################################
#=================================================================================

# Using control treatment and treatment 1 as control since both groups did not receive personalized information 
##########################

####################
# graphical info rounds 4-7
###################

# data used: datat12r47 
datat12r47 <- subset(data_, round > 3 & round < 8 & treat < 2 | round > 3 & round < 8 & treat == 2)

# Set of variables to be included in causal Forest
vars <- c('female',
          'uni',
          's_doc',
          'total_ok',
          'hilm_beh_avg',
          'hilm_conf_avg',
          'm_income',
          'age',
          'healthstat',
          'ra_gains',
          'rs_loss',
          'reflection',
          'pw1060',
          'pw6090',
          'pw1090',
          'c_current',
          'c_previous',
          'minor_event',
          's_canton',
          's_study_area',
          's_general_risk',
          's_swiss',
          'round',
          'cum_c',
          'sure',
          's_switchedhi',
          's_ded',
          's_supplementary',
          'total_qs_answered',
          'correct_quiz',
          'cumulative_lagswitch',
          'newbalance',
          'round_qs'
          
)


vars.full <- vars


names2 <- c('Female',
          'Uni',
          'Doctor visits in last 12 months',
          'Objective HI knowledge',
          'HILM-CH: Behavior',
          'HILM-CH: Confidence',
          'Income category median',
          'Age',
          'Health status',
          'Risk averse for gains',
          'Risk seeking for losses',
          'Reflection effect',
          'Probability overweighting 10-60%',
          'Probability overweighting 60-90%',
          'Probability overweighitng 10-90%',
          'Major event in current round',
          'Major event in previous round',
          'Minor event',
          'Canton',
          'Study area',
          'General risk',
          'Swiss',
          'Round',
          'Cumulative major events',
          'SURE scale',
          'Last time switching HI',
          'Deductible',
          'Supplementary insurance',
          'Total trivia questions answered',
          'Correct trivia questions',
          'Experiment cumulative HI switches',
          'Balance',
          'Round trivia questions'
          
)

#covariates
X <- as.matrix(subset(datat12r47, select= vars))
colnames(X)<-vars

#outcome:
Y <- as.vector(datat12r47$distance_best_plan)

#instrument (treatment assignment)
Z <- as.vector(ifelse(datat12r47$treat<2,0,1))

# treatment (seeing information)
W <- as.vector(datat12r47$saw_pinfo3)
instrumental_forestg47 <- instrumental_forest(X,
                                           Y,
                                           W,
                                           Z,
                                           cluster = datat12r47$participant,
                                           num.trees=3000,
                                           seed=123)


vi.instrumental_forestg47 <- variable_importance(instrumental_forestg47)
rownames(vi.instrumental_forestg47)<-vars
print(vi.instrumental_forestg47)

plot(plot_iv <- get_tree(instrumental_forestg47, 1))
plot_iv

vi.distance_best_plang47 <- variable_importance(instrumental_forestg47)
vi.distance_best_plang47 <- data.frame(vi.distance_best_plang47)
vi.distance_best_plang47$var.names <- names2
vi.distance_best_plang47 <- vi.distance_best_plang47[order(-vi.distance_best_plang47$vi.distance_best_plang47),]  # Order the data frame such that the covariate with the highest variable importance is in the last row
vi.distance_best_plang47$var.names <- factor(vi.distance_best_plang47$var.names, levels=vi.distance_best_plang47$var.names)
vi.distance_best_plang47$vi.distance_best_plang47.100 <- vi.distance_best_plang47$vi.distance_best_plang47*100  # Multiply the variable importance measure by 100 for readability
print(vi.distance_best_plang47)

vi.plot.distance_best_plang47 <- ggplot(data=vi.distance_best_plang47, aes(x=var.names, y=vi.distance_best_plang47.100)) + geom_col(width=0.6, fill="darkblue", colour="darkblue") +
  theme_bw() +
  scale_y_continuous(expand=c(0.007,0), breaks=c(seq(0,30,5)), limits=c(0, 31)) +
  #theme(text=element_text(family="Times New Roman"),
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=8, margin = margin(t = 7, r = 0, b = 7, l = 0)),
        axis.title=element_text(size=10),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.background = element_rect(colour = "black", size=0.7),
        axis.ticks.x=element_line(size=0.7)) +
  labs(y="Graphical info: Rounds 4-7") + coord_flip()
vi.plot.distance_best_plang47


# calculating tau.hat
ivtau.hat_g47 <- predict(instrumental_forestg47)$predictions
ivtau.hat_g47
datat12r47$ivtauhat_g47 <- ivtau.hat_g47

summary(ivtau.hat_g47)

# Calculate the ATE for the instrumental forest
ate_result_iv1 <- average_treatment_effect(instrumental_forestg47)

# Extract the ATE estimate and standard error
ate_estimate_iv1 <- ate_result_iv1[1]
ate_se_iv1 <- ate_result_iv1[2]

# Print the results
print(paste("ATE Estimate for Instrumental Forest:", ate_estimate_iv1))
print(paste("Standard Error for Instrumental Forest:", ate_se_iv1))

# Calculate the 95% confidence interval
ci_lower_iv1 <- ate_estimate_iv1 - 1.96 * ate_se_iv1
ci_upper_iv1 <- ate_estimate_iv1 + 1.96 * ate_se_iv1
print(paste("95% Confidence Interval for Instrumental Forest: [", ci_lower_iv1, ",", ci_upper_iv1, "]"))

# Calculate the p-value
ate_p_value_iv1 <- 2 * pnorm(-abs(ate_estimate_iv1 / ate_se_iv1))
print(paste("P-value for Instrumental Forest:", ate_p_value_iv1))




ggplot(data = datat12r47, aes(x = s_general_risk, y = ivtau.hat_g47)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("General Risk Question") + 
  ylab("Predicted individual treatment effect")

# Fit a linear regression model
linear_model <- lm(ivtau.hat_g47 ~ newbalance, data = datat12r47)

# Fit a quadratic regression model
quadratic_model <- lm(ivtau.hat_g47 ~ newbalance + I(newbalance^2), data = datat12r47)

# Perform an F-test to compare the models
anova(linear_model, quadratic_model)


#------------------ Panel A - 1 --------------------------- 
datat12r47$riskloving <- ifelse(datat12r47$s_general_risk>7,1,0)
datat12r47$riskavoiding <- ifelse(datat12r47$s_general_risk<4,1,0)

ivg47_1 <-  ggplot(data = datat12r47, aes(x = newbalance, y = ivtau.hat_g47, color=factor(riskavoiding, labels=c("Yes", "No")))) + 
            geom_point() + 
            geom_smooth(method = "lm", fullrange=TRUE) + 
            xlab("Balance") + 
            ylab("Predicted individual treatment effect") +
            labs(color="General risk question < 4") +
            theme_bw() +
            theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
                  legend.position="bottom")
ivg47_1
#-----------------------------------------------------------------------

# Fit a linear regression model
linear_model <- lm(ivtau.hat_g47 ~ hilm_beh_avg, data = datat12r47)

# Fit a quadratic regression model
quadratic_model <- lm(ivtau.hat_g47 ~ hilm_beh_avg + I(hilm_beh_avg^2), data = datat12r47)

# Perform an F-test to compare the models
anova(linear_model, quadratic_model)

#------------------ Panel A - 2 --------------------------- 
ivg47_2 <-ggplot(data = datat12r47, aes(x = hilm_beh_avg, y = ivtau.hat_g47, color=factor(riskavoiding, labels=c("Yes", "No")))) + 
          geom_point() + 
          geom_smooth(method = "lm", fullrange=TRUE) +
          xlab("HILM-CH: Behavior") + 
          ylab("Predicted individual treatment effect") +
          labs(color="General risk question < 4") +
          theme_bw() +
          theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
                legend.position="bottom")
ivg47_2
#-----------------------------------------------------------------------

#------------- Panel A -------------------------------------
# grid with graphs of conditional TE on R8 of 2 optioanl personalized info graph by: trivia questions, balance, hilm-confidcence
# Generating image grid containing ivg47_1, ivg47_2
library(gridExtra)
# Extract the legend from one of the plots
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- g_legend(ivg47_1)
# Remove the legends from all plots
ivg47_1g <- ivg47_1 + theme(legend.position = "none")
ivg47_2g <- ivg47_2 + theme(legend.position = "none")

# Arrange the plots and legend in a grid
library(grid)
fig9 <- grid.arrange(arrangeGrob(ivg47_1g, ivg47_2g, ncol = 2), 
             arrangeGrob(nullGrob(),nullGrob(),
                         legend,
                         ncol=1),
             nrow=2,
             heights=c(0.9,0.1),
             top = "a) Personalized Graphical Information")
fig9
ggsave(filename = "fig9.png", plot = fig9, width = 174/25.4, height = 117/25.4, units = "in", dpi = 300)
#-------------------------------------------------------------------------------



library(labelled)
labelled::val_labels(datat12r47$s_switchedhi)


ggplot(data = datat12r47, aes(x = newbalance, y = ivtau.hat_g47, color=factor(s_switchedhi, labels=c("Never", "Last year", "2 years ago", "> 2 years ago", "Don't know")))) +
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) +
  xlab("Balance") + 
  ylab("Conditional treatment effect") +
  labs(color="Last time switching HI") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")


ggplot(data = datat12r47, aes(x = newbalance, y = ivtau.hat_g47, color=factor(s_switchedhi))) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) +
  xlab("Balance") + 
  ylab("Conditional treatment effect") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")


ols_iv_g47 <- lm(ivtau.hat_g47 ~ newbalance100 + hilm_beh_avg + hilm_conf_avg +  s_general_risk + factor(round) , data = datat12r47)
# OLS model with clustered standard errrors (robust) (stata replication)
ols_iv_g47_clusterse_corrected <- coeftest(ols_iv_g47,
                                       vcov = vcovCL,
                                       type = "HC1",
                                       df = 1031,  # 1032 observations
                                       cluster = ~id)
tidy(ols_iv_g47_clusterse_corrected, conf.int = TRUE)

# Compute a prioritization based on estimated treatment effects.
rateifg47 <-  predict(instrumental_forestg47)$predictions
rate_ifg47 <- rank_average_treatment_effect(instrumental_forestg47, rateifg47)
plot(rate_ifg47)


write.xlsx(datat12r47, "datat12r47_tauhat.xlsx", asTable=TRUE)



############################################
# numerical info rounds 4-7
###########################################

# Using control treatment and treatment 1 as control since both groups did not receive personalized information 

#used data: datat13r47
datat13r47 <- subset(data_, round > 3 & round < 8 & treat < 2 | round > 3 & round < 8 & treat == 3)

# Set of variables to be included in causal Forest (same as previous)
vars <- c('female',
          'uni',
          's_doc',
          'total_ok',
          'hilm_beh_avg',
          'hilm_conf_avg',
          'm_income','age',
          'healthstat',
          'ra_gains',
          'rs_loss',
          'reflection',
          'pw1060',
          'pw6090',
          'pw1090',
          'c_current',
          'c_previous',
          'minor_event',
          's_canton',
          's_study_area',
          's_general_risk',
          's_swiss',
          'round',
          'cum_c',
          'sure',
          's_switchedhi',
          's_ded',
          's_supplementary',
          'total_qs_answered',
          'correct_quiz',
          'cumulative_lagswitch',
          'newbalance',
          'round_qs'
)
vars.full <- vars

#covariates
X <- as.matrix(subset(datat13r47, select= vars))
colnames(X)<-vars

#outcome:
Y <- as.vector(datat13r47$distance_best_plan)

#instrument (treatment assignment)
Z <- as.vector(ifelse(datat13r47$treat<2,0,1))

# treatment (seeing information)
W <- as.vector(datat13r47$saw_pinfo3)
instrumental_forestn47 <- instrumental_forest(X,
                                           Y,
                                           W,
                                           Z,
                                           cluster = datat13r47$participant,
                                           num.trees=3000,
                                           seed=123)


vi.instrumental_forestn47 <- variable_importance(instrumental_forestn47)
rownames(vi.instrumental_forestn47)<-vars
print(vi.instrumental_forestn47)

plot(plot_iv <- get_tree(instrumental_forestn47, 1))
plot_iv

vi.distance_best_plann47 <- variable_importance(instrumental_forestn47)
vi.distance_best_plann47 <- data.frame(vi.distance_best_plann47)
vi.distance_best_plann47$var.names <- names2
vi.distance_best_plann47 <- vi.distance_best_plann47[order(-vi.distance_best_plann47$vi.distance_best_plann47),]  # Order the data frame such that the covariate with the highest variable importance is in the last row
vi.distance_best_plann47$var.names <- factor(vi.distance_best_plann47$var.names, levels=vi.distance_best_plann47$var.names)
vi.distance_best_plann47$vi.distance_best_plann47.100 <- vi.distance_best_plann47$vi.distance_best_plann47*100  # Multiply the variable importance measure by 100 for readability
print(vi.distance_best_plann47)

vi.plot.distance_best_plann47 <- ggplot(data=vi.distance_best_plann47, aes(x=var.names, y=vi.distance_best_plann47.100)) + geom_col(width=0.6, fill="darkblue", colour="darkblue") +
  theme_bw() +
  scale_y_continuous(expand=c(0.007,0), breaks=c(seq(0,30,5)), limits=c(0, 31)) +
  #theme(text=element_text(family="Times New Roman"),
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=8, margin = margin(t = 7, r = 0, b = 7, l = 0)),
        axis.title=element_text(size=10),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.background = element_rect(colour = "black", size=0.7),
        axis.ticks.x=element_line(size=0.7)) +
  labs(y="Numerical info: Rounds 4-7") + coord_flip()
vi.plot.distance_best_plann47

#------------------ included in draft --------------------------- 
# Grid of variable importance for causal forest of visualizing peronalized information when having 2 or 1 types of info on rounds 3 and 8
vi_if47 <- grid.arrange(vi.plot.distance_best_plang47, vi.plot.distance_best_plann47, ncol = 2, 
             top = "Variable Importance for Treatment Effect of Personalized Information on Rounds 4-7")
ggsave(filename = "vi_if47.png", plot = vi_if47, width = 174/25.4, height = 117/25.4, units = "in", dpi = 300)
#----------------------------------------------------------------

# ATE: calculating tau.hat
ivtau.hat <- predict(instrumental_forestn47)$predictions
ivtau.hat
datat13r47$ivtauhat <- ivtau.hat

summary(ivtau.hat)

# Calculate the ATE for the instrumental forest
ate_result_iv2 <- average_treatment_effect(instrumental_forestn47)

# Extract the ATE estimate and standard error
ate_estimate_iv2 <- ate_result_iv2[1]
ate_se_iv2 <- ate_result_iv2[2]

# Print the results
print(paste("ATE Estimate for Instrumental Forest:", ate_estimate_iv2))
print(paste("Standard Error for Instrumental Forest:", ate_se_iv2))

# Calculate the 95% confidence interval
ci_lower_iv2 <- ate_estimate_iv2 - 1.96 * ate_se_iv2
ci_upper_iv2 <- ate_estimate_iv2 + 1.96 * ate_se_iv2
print(paste("95% Confidence Interval for Instrumental Forest: [", ci_lower_iv2, ",", ci_upper_iv2, "]"))

# Calculate the p-value
ate_p_value_iv2 <- 2 * pnorm(-abs(ate_estimate_iv2 / ate_se_iv2))
print(paste("P-value for Instrumental Forest:", ate_p_value_iv2))


## Testing if difference between graphical and numberical pinfo is significant (according to IV forest analysis)
# Calculate the test statistic
test_statistic <- (ate_estimate_iv1 - ate_estimate_iv2) / sqrt(ate_se_iv1^2 + ate_se_iv2^2)

# Degrees of freedom
df <- min(length(ate_se_iv1), length(ate_se_iv2)) - 1

# Calculate the p-value (two-tailed test)
p_value <- 2 * pnorm(abs(test_statistic), df)

# Significance level (alpha)
alpha <- 0.05

# Decision Rule
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is a statistically significant difference between graphical and numerical information.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no statistically significant difference between graphical and numerical information.\n")
}

# Print the test results
cat("Test Statistic:", test_statistic, "\n")
cat("P-value:", p_value, "\n")
cat("Significance Level (alpha):", alpha, "\n")


datat13r47$high_hilmbeh <- ifelse(datat13r47$hilm_beh_avg>=3,1,0)
ggplot(data = datat13r47, aes(x = total_qs_answered, y = ivtau.hat, color=factor(high_hilmbeh, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("Total trivia questions answered") + 
  ylab("Conditional treatment effect") +
  labs(color="HILM-CH: Behavior domains > 3") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")

ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=hilm_conf_avg)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("HILM-CH: Behavior domains average") + 
  ylab("Conditional treatment effect")+
  labs(color = "HILM-CH: Confidnce")


ggplot(data = datat13r47, aes(x = hilm_conf_avg, y = ivtau.hat, color=factor(pw1060, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("HILM-CH: Behavior domains average") + 
  ylab("Conditional treatment effect")+
  labs(color = "Probability overweighting between 10-60%")

ggplot(data = datat13r47, aes(x = newbalance, y = ivtau.hat, color=factor(pw1060, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("Balance") + 
  ylab("Conditional treatment effect")+
  labs(color = "Probability overweighting between 10-60%")


# Fit a linear regression model
linear_model <- lm(ivtau.hat ~ hilm_beh_avg, data = datat13r47)

# Fit a quadratic regression model
quadratic_model <- lm(ivtau.hat ~ hilm_beh_avg + I(hilm_beh_avg^2), data = datat13r47)

# Perform an F-test to compare the models
anova(linear_model, quadratic_model)



# Fit a linear regression model
linear_model <- lm(ivtau.hat ~ newbalance, data = datat13r47)

# Fit a quadratic regression model
quadratic_model <- lm(ivtau.hat ~ newbalance + I(newbalance^2), data = datat13r47)

# Perform an F-test to compare the models
anova(linear_model, quadratic_model)

#------------------ Panel B image 1 --------------------------- 
ifn47_1 <-  ggplot(data = datat13r47, aes(x = newbalance, y = ivtau.hat, color=hilm_conf_avg)) +
            geom_point() + 
            geom_smooth(method = "lm", formula = y ~ poly(x, 2), fullrange=TRUE) +
            xlab("Balance") + 
            ylab("Predicted individual treatment effect")+
            labs(color = "HILM-CH: Confidence") +
            theme_bw() +
            theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
                  legend.position="bottom")
ifn47_1
#------------------------------------------------------------------------------

#------------------ Panel B image 2 --------------------------- 
datat13r47$high_hilmconf <- ifelse(datat13r47$hilm_conf_avg>=3,1,0)
datat13r47$high_hilmbeh <- ifelse(datat13r47$hilm_beh_avg>=3,1,0)
ifn47_2 <-  ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=hilm_conf_avg)) + 
  geom_point() + 
  geom_smooth(method = "lm", fullrange=TRUE) +
  xlab("HILM-CH: Behavior") + 
  ylab("Predicted individual treatment effect")+
  labs(color = "HILM-CH: Confidence") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
        legend.position="bottom")
ifn47_2
#------------------------------------------------------------------------------



#------------- Panel B (individual)-------------------------------------
# grid with graphs of conditional TE on R8 of 2 optioanl personalized info graph by: trivia questions, balance, hilm-confidcence
# Generating image grid containing ivg47_1, ivg47_2
library(grid)
# Extract the legend from one of the plots
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- g_legend(ifn47_1)
# Remove the legends from all plots
ifn47_1g <- ifn47_1 + theme(legend.position = "none")
ifn47_2g <- ifn47_2 + theme(legend.position = "none")

# Arrange the plots and legend in a grid
fig10 <- grid.arrange(arrangeGrob(ifn47_1g, ifn47_2g, ncol = 2), 
             arrangeGrob(nullGrob(),nullGrob(),
                         legend,
                         ncol=1),
             nrow=2,
             heights=c(0.9,0.1),
             top = "b) Personalized Numerical Information")
ggsave(filename = "fig10.png", plot = fig10, width = 174/25.4, height = 117/25.4, units = "in", dpi = 300)
#-------------------------------------------------------------------------------

#-------------------------Panel A and B 
ivplots <- grid.arrange(arrangeGrob(fig9,  fig10, nrow = 2,
                                    padding= unit(2,"lines"),
                                    top= "Predicted Individual Treatment Effect of Personalized Information on Distance to Plan with Lowest Expected Cost in Rounds 4-7"))
ivplots









datat13r47$totalqs50 <- ifelse(datat13r47$total_qs_answered > 49, 1,0)
ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=factor(totalqs50, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("HILM-CH: Behavior domains average") + 
  ylab("Conditional treatment effect")+
  labs(color = "50 or more Trivia questions answered")

datat13r47$high_hilmconf <- ifelse(datat13r47$hilm_conf_avg > 3, 1,0)
ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=factor(high_hilmconf, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("HILM-CH: Behavior domains average") + 
  ylab("Conditional treatment effect")+
  labs(color = "HILM-CH: confidence > 3")


datat13r47$riskloving <- ifelse(datat13r47$s_general_risk>7,1,0)
  ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=factor(riskloving, labels=c("No", "Yes")))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("HILM-CH: Behavior domains average") + 
  ylab("Conditional treatment effect")+
  labs(color = "General risk quesiton > 7")


ggplot(data = datat13r47, aes(x = hilm_beh_avg, y = ivtau.hat, color=factor(pw1060, labels=c("No", "Yes")))) + 
geom_point() + 
geom_smooth(method = "lm", fullrange=TRUE) + 
xlab("HILM-CH: Confidence domains average") + 
ylab("Conditional treatment effect")+
labs(color = "Probability overweighting between 10-60%") +
scale_color_manual(values = c("#7CAE00", "#C77CFF")) +
theme_bw() +
theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
      legend.position="bottom")





#------------------ included in appendix --------------------------- 

a3_fig2 <- ggplot(data = datat13r47, aes(x = total_qs_answered, y = ivtau.hat, color=total_ok)) + 
           geom_point() + 
           geom_smooth(method = "lm", formula = y ~ poly(x, 2), fullrange=TRUE) + 
           xlab("Total trivia questions answered") + 
           ylab("Conditional treatment effect") +
           labs(color="Objective HI knowledge") +
           theme_bw() +
           theme(panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_line(colour = "grey95"),
                 legend.position="bottom")
a3_fig2
ggsave(filename = "a3_fig2.png", plot = a3_fig2, width = 174/25.4, height = 117/25.4, units = "in", dpi = 300)
#-----------------------------------------------------------------------
  
ggplot(data = datat13r47, aes(x = healthstat, y = ivtau.hat)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("health status") + 
  ylab("Conditional treatment effect")



ols_iv_n47 <- lm(ivtau.hat ~ hilm_beh_avg + newbalance100 + hilm_conf_avg + total_qs_answered  + total_ok + factor(round), data = datat13r47)
# OLS model with clustered standard errrors (robust) (stata replication)
ols_iv_n47_clusterse_corrected <- coeftest(ols_iv_n47,
                                           vcov = vcovCL,
                                           type = "HC1",
                                           df = 1035,  # 1036 observations
                                           cluster = ~id)
tidy(ols_iv_n47_clusterse_corrected, conf.int = TRUE)

# Compute a prioritization based on estimated treatment effects.
# -1: the treatment should reduce the distance to plan with lowest expected cost
rateifn47 <-  predict(instrumental_forestn47)$predictions
rate_ifn47 <- rank_average_treatment_effect(instrumental_forestn47, rateifn47)
plot(rate_ifn47)


write.xlsx(datat13r47, "datat13r47tau.xlsx", asTable=TRUE)



# Appendix 3 - Instrumental Forest Supplementary Material

data_47 <- subset(data_, round %in% 4:7)
data_47$t <- as.factor(ifelse(data_47$treat>1,data_47$treat,0))

#ATE for all personalized info
ivreg_1 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_47)
summary(ivreg_1)
# Calculate clustered standard errors
coeftest(ivreg_1, vcov = vcovCL, cluster = ~participant, data = data_47)

# ATE for 2 types of info 
ivreg_2 <- ivreg(distance_best_plan ~ factor(r3_numinfo) + factor(r3_graphinfo) + factor(round) | t + factor(round), data = data_47)
# Calculate clustered standard errors
ivreg_2_summary <- coeftest(ivreg_2, vcov = vcovCL(ivreg_2, type = "HC1", cluster = ~participant, data = data_47))
print(ivreg_2_summary)

#testing if difference of ATE for graph and num pinfo is statistically significant:
library(car)
# Fit the model
ivreg_2 <- ivreg(distance_best_plan ~ factor(r3_numinfo) + factor(r3_graphinfo) + factor(round) | t + factor(round), data = data_47)
# Test the hypothesis
hypothesis_test <- linearHypothesis(ivreg_2, "factor(r3_numinfo)1 = factor(r3_graphinfo)1")
# Print the results
print(hypothesis_test)


## Table 8. HILM-CH Behavior Domain Subgroup Instrumental Variable Estimates of 
## the Effect of Personalized Information on Distance to the Plan with the Lowest 
## Expected Cost During rounds 4-7


# Filter data based on hilm_beh_avg
data_below_289 <- subset(data_47, hilm_beh_avg < 2.89)
data_above_289 <- subset(data_47, hilm_beh_avg >= 2.89)

# IV regression for hilm_beh_avg < 2.89
# saw_pinfo3 as endogenous variable
ivreg_A3 <- ivreg(distance_best_plan ~ factor(saw_pinfo3) + factor(round) | t + factor(round), data = data_below_289)
summary_A3 <- coeftest(ivreg_A3, vcov = vcovCL(ivreg_A3, type = "HC1", cluster = ~participant, data = data_below_289))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_A4 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_below_289)
summary_A4 <- coeftest(ivreg_A4, vcov = vcovCL(ivreg_A4, type = "HC1", cluster = ~participant, data = data_below_289))
print(summary_A4)
# Test the hypothesis that graph and num are the same
hypothesis_testA4 <- linearHypothesis(ivreg_A4, "r3_numinfo = r3_graphinfo")
# Print the results
print(hypothesis_testA4)


# IV regression for hilm_beh_avg >= 2.89
# saw_pinfo3 as endogenous variable
ivreg_A5 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_above_289)
summary_A5 <- coeftest(ivreg_A5, vcov = vcovCL(ivreg_A5, type = "HC1", cluster = ~participant, data = data_above_289))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_A6 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_above_289)
summary_A6 <- coeftest(ivreg_A6, vcov = vcovCL(ivreg_A6, type = "HC1", cluster = ~participant, data = data_above_289))

# Print summaries
print(summary_A3)
print(summary_A4)
print(summary_A5)
print(summary_A6)

## Table 9. Balance Subgroup Instrumental Variable Estimates of the Effect of 
## Personalized Information on Distance to the Plan with the Lowest Expected During Rounds 4-7

# Filter data based on balance
data_below_1860 <- subset(data_47, balance < 1860)
data_above_1860 <- subset(data_47, balance >= 1860)

# IV regression for balance < 1860
# saw_pinfo3 as endogenous variable
ivreg_B3 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_below_1860)
summary_B3 <- coeftest(ivreg_B3, vcov = vcovCL(ivreg_B3, type = "HC1", cluster = ~participant, data = data_below_1860))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_B4 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_below_1860)
summary_B4 <- coeftest(ivreg_B4, vcov = vcovCL(ivreg_B4, type = "HC1", cluster = ~participant, data = data_below_1860))
# Test the hypothesis that graph and num are the same
hypothesis_testB4 <- linearHypothesis(ivreg_B4, "r3_numinfo = r3_graphinfo")
# Print the results
print(hypothesis_testB4)


# IV regression for balance >= 1860
# saw_pinfo3 as endogenous variable
ivreg_B5 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_above_1860)
summary_B5 <- coeftest(ivreg_B5, vcov = vcovCL(ivreg_B5, type = "HC1", cluster = ~participant, data = data_above_1860))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_B6 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_above_1860)
summary_B6 <- coeftest(ivreg_B6, vcov = vcovCL(ivreg_B6, type = "HC1", cluster = ~participant, data = data_above_1860))

# Print summaries
print(summary_B3)
print(summary_B4)
print(summary_B5)
print(summary_B6)

## Table 10. Risk Preference Subgroup Instrumental Variable Estimates of the 
## Effect of Personalized Information on Distance to the Plan with the Lowest 
## Expected Cost During Rounds 4-7

# Filter data based on s_general_risk
data_risk_below_4 <- subset(data_47, s_general_risk < 4)
data_risk_above_4 <- subset(data_47, s_general_risk >= 4)

# IV regression for s_general_risk < 4
# saw_pinfo3 as endogenous variable
ivreg_C3 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_risk_below_4)
summary_C3 <- coeftest(ivreg_C3, vcov = vcovCL(ivreg_C3, type = "HC1", cluster = ~participant, data = data_risk_below_4))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_C4 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_risk_below_4)
summary_C4 <- coeftest(ivreg_C4, vcov = vcovCL(ivreg_C4, type = "HC1", cluster = ~participant, data = data_risk_below_4))

# IV regression for s_general_risk >= 4
# saw_pinfo3 as endogenous variable
ivreg_C5 <- ivreg(distance_best_plan ~ saw_pinfo3 + factor(round) | t + factor(round), data = data_risk_above_4)
summary_C5 <- coeftest(ivreg_C5, vcov = vcovCL(ivreg_C5, type = "HC1", cluster = ~participant, data = data_risk_above_4))

# r3_numinfo and r3_graphinfo as endogenous variables
ivreg_C6 <- ivreg(distance_best_plan ~ r3_numinfo + r3_graphinfo + factor(round) | t + factor(round), data = data_risk_above_4)
summary_C6 <- coeftest(ivreg_C6, vcov = vcovCL(ivreg_C6, type = "HC1", cluster = ~participant, data = data_risk_above_4))
print(summary_C6)



# Print summaries
print(summary_C3)
print(summary_C4)
print(summary_C5)
print(summary_C6)
