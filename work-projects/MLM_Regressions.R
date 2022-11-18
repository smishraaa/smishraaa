library(haven)
library(sjPlot)
library(lme4)
library(ggeffects)
library(ggplot2)
library(arm)
library(vctrs)
library(tidyverse)


data <- read_dta("data_Thiha.dta")

country <- read.csv("country.csv")

wcar <- merge(data,country,by="country")


#####ANAR primary
wcar_primary = wcar %>%
  mutate(wealth = as.factor(windex5),
         urban = ifelse(HH6 == 1, 1, 0),
         boy = ifelse(HL4==1, 1, 0),
         CB3squared=CB3*CB3) %>% 
  filter(age_school==1) 

######ANAR LS
wcar_ls = wcar %>%
  mutate(wealth = as.factor(windex5),
         urban = ifelse(HH6 == 1, 1, 0),
         boy = ifelse(HL4==1, 1, 0),
         CB3squared=CB3*CB3) %>% 
  filter(age_school==2) 

######Attendance 6 to 14
wcar_att = wcar %>%
  mutate(wealth = as.factor(windex5),
         urban = ifelse(HH6 == 1, 1, 0),
         boy = ifelse(HL4==1, 1, 0),
         CB3squared=CB3*CB3) %>% 
  filter(schage >=6 & schage <=14) 

#####foundational learning
wcar_fls = wcar %>%
  mutate(wealth = as.factor(windex5),
         urban = ifelse(HH6 == 1, 1, 0),
         boy = ifelse(HL4==1, 1, 0),
         CB3squared=CB3*CB3) %>% 
  filter(CB3 >=7 & CB3 <=14)

###############################WITHOUT MACRO-LEVEL VARS

#######Primary attendance: Varying intercept only
model_ANAR_Primary_VI = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                         wealth + hearing + seeing + walking + selfcare + 
                           communication + learning + remembering +
                           concentrating + accepting +
                           controlling + makefriends +
                           anxiety + depression + (1|country), 
                       data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_Primary_VI, type= "pred")


model_ANAR_ls_VI = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                               wealth + hearing + seeing + walking + selfcare + 
                               communication + learning + remembering +
                               concentrating + accepting +
                               controlling + makefriends +
                               anxiety + depression + (1|country), 
                             data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_ls_VI, type= "est")


model_ANAR_att_VI = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                          wealth + hearing + seeing + walking + selfcare + 
                          communication + learning + remembering +
                          concentrating + accepting +
                          controlling + makefriends +
                          anxiety + depression + (1|country), 
                        data = wcar_att, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_att_VI, type= "est")

#######Reading Skill
model_readskill_VI = lmer(readskill ~  CB3 + CB3squared + boy + urban  + 
                               wealth + hearing + seeing + walking + selfcare + 
                               communication + learning + remembering +
                               concentrating + accepting +
                               controlling + makefriends +
                               anxiety + depression + (1|country), 
                             data = wcar_fls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_readskill, type= "est")


model_numbskil_VI = lmer(numbskill ~  CB3 + CB3squared + boy + urban  + 
                         wealth + hearing + seeing + walking + selfcare + 
                         communication + learning + remembering +
                         concentrating + accepting +
                         controlling + makefriends +
                         anxiety + depression + (1|country), 
                       data = wcar_fls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_numbskill, type= "est")
#########################################
#########################################
#########################################WITH MACRO LEVEL Vars
#######Primary attendance: Varying intercept only
model_ANAR_Primary_VI_Macro = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                               wealth + pop_density + l.GDP_per_capita. +
                               Share_eduexp_govexp + hearing + seeing + walking + selfcare + 
                               communication + learning + remembering +
                               concentrating + accepting +
                               controlling + makefriends +
                               anxiety + depression + (1|country), 
                             data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_Primary_VI_Macro, type= "est")

model_ANAR_ls_VI_Macro = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                          wealth + hearing + seeing + walking + selfcare + 
                          communication + learning + remembering +
                          concentrating + accepting +
                          controlling + makefriends +
                          anxiety + depression + pop_density + l.GDP_per_capita. +
                          Share_eduexp_govexp + (1|country), 
                        data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_ls_VI_Macro, type= "est")


model_ANAR_att_VI_Macro = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                           wealth + hearing + seeing + walking + selfcare + 
                           communication + learning + remembering +
                           concentrating + accepting +
                           controlling + makefriends +
                           anxiety + depression + pop_density + l.GDP_per_capita. +
                           Share_eduexp_govexp +  (1|country), 
                         data = wcar_att, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_att_VI_Macro, type= "est")

#######Reading Skill
model_readskill_VI_Macro = lmer(readskill ~  CB3 + CB3squared + boy + urban  + 
                         wealth + hearing + seeing + walking + selfcare + 
                         communication + learning + remembering +
                         concentrating + accepting +
                         controlling + makefriends +
                         anxiety + depression +  pop_density + l.GDP_per_capita. +
                         Share_eduexp_govexp + (1|country), 
                       data = wcar_fls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_readskill_VI_Macro, type= "est")


model_numbskill_VI_Macro = lmer(numbskill ~  CB3 + CB3squared + boy + urban  + 
                         wealth + hearing + seeing + walking + selfcare + 
                         communication + learning + remembering +
                         concentrating + accepting +
                         controlling + makefriends +
                         anxiety + depression + pop_density + l.GDP_per_capita. +
                         Share_eduexp_govexp +  (1|country), 
                       data = wcar_fls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_numbskill_VI_Macro, type= "est")

###########################Ask for help here: ANAR Primary each Functional difficulty type
#1 hearing: Thiha
model_ANAR_Primary_VI_VC_hearing_Thiha = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                                wealth  + hearing   + (1 + hearing|country), 
                                              data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))



plot_model(model_ANAR_Primary_VI_VC_hearing_Thiha, type = "re") #Plot model doesnt work

#1 hearing: My model
model_ANAR_Primary_VI_VC_hearing = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                               wealth  + hearing + seeing + walking + selfcare + 
                               communication + learning + remembering +
                               concentrating + accepting +
                               controlling + makefriends +
                               anxiety + depression  + (1 + hearing|country), 
                             data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_Primary_VI_VC_hearing, type = "re") #Plot model doesnt work

# Also you can get the exact values of coefficients and standard deviation using the following formula

as.data.frame(ranef(model_ANAR_Primary_VI_VC_hearing)) #condval refers to conditional coefficient and condsd refers to conditional standard error


#Storing SE (This does not work either)
model_ANAR_Primary_VI_VC_hearing_se = arm::se.coef(model_ANAR_Primary_VI_VC_hearing) 


#2 seeing
model_ANAR_Primary_VI_VC_seeing = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + seeing|country), 
                                        data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_Primary_VI_VC_seeing, type = "re", grid = FALSE)

#3 walking
model_ANAR_Primary_VI_VC_walking = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + walking|country), 
                                        data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_Primary_VI_VC_walking, type = "re", grid = FALSE)

#4 selfcare
model_ANAR_Primary_VI_VC_selfcare = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + selfcare|country), 
                                        data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_Primary_VI_VC_selfcare, type = "re", grid = FALSE)

#5 communication
model_ANAR_Primary_VI_VC_communication = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                           wealth + hearing + seeing + walking + selfcare + 
                                           communication + learning + remembering +
                                           concentrating + accepting +
                                           controlling + makefriends +
                                           anxiety + depression  + (1 + communication|country), 
                                         data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_Primary_VI_VC_communication, type = "re", grid = FALSE)

#6 Learning
model_ANAR_Primary_VI_VC_learning = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                           wealth + hearing + seeing + walking + selfcare + 
                                           communication + learning + remembering +
                                           concentrating + accepting +
                                           controlling + makefriends +
                                           anxiety + depression  + (1 + learning|country), 
                                         data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))
#7 remembering
model_ANAR_Primary_VI_VC_remembering = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                           wealth + hearing + seeing + walking + selfcare + 
                                           communication + learning + remembering +
                                           concentrating + accepting +
                                           controlling + makefriends +
                                           anxiety + depression  + (1 + remembering|country), 
                                         data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#8 concentrating
model_ANAR_Primary_VI_VC_concentrating = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + concentrating|country), 
                                            data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#9 accepting
model_ANAR_Primary_VI_VC_accepting = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                                wealth + hearing + seeing + walking + selfcare + 
                                                communication + learning + remembering +
                                                concentrating + accepting +
                                                controlling + makefriends +
                                                anxiety + depression  + (1 + accepting|country), 
                                              data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#10 controlling
model_ANAR_Primary_VI_VC_controlling = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                            wealth + hearing + seeing + walking + selfcare + 
                                            communication + learning + remembering +
                                            concentrating + accepting +
                                            controlling + makefriends +
                                            anxiety + depression  + (1 + controlling|country), 
                                          data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#11 makefriends
model_ANAR_Primary_VI_VC_makefriends = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + makefriends|country), 
                                            data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#12 anxiety
model_ANAR_Primary_VI_VC_anxiety = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + anxiety|country), 
                                            data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))

#13 depression
model_ANAR_Primary_VI_VC_anxiety = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + depression|country), 
                                        data = wcar_primary, control = lmerControl(optimizer ="Nelder_Mead"))


##############################ANAR lower secondary
#1 hearing: Thiha
model_ANAR_ls_VI_VC_hearing_Thiha = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                                wealth  + hearing   + (1 + hearing|country), 
                                              data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))



plot_model(model_ANAR_ls_VI_VC_hearing_Thiha, type = "re") #Plot model doesnt work

#1 hearing: My model
model_ANAR_ls_VI_VC_hearing = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth  + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + hearing|country), 
                                        data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

plot_model(model_ANAR_ls_VI_VC_hearing, type = "re") #Plot model doesnt work

#Storing coef
#Storing SE (This does not work either)
model_ANAR_ls_VI_VC_hearing_se = arm::se.coef(model_ANAR_ls_VI_VC_hearing) 


#2 seeing
model_ANAR_ls_VI_VC_seeing = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                         wealth + hearing + seeing + walking + selfcare + 
                                         communication + learning + remembering +
                                         concentrating + accepting +
                                         controlling + makefriends +
                                         anxiety + depression  + (1 + seeing|country), 
                                       data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_ls_VI_VC_seeing, type = "re", grid = FALSE)

#3 walking
model_ANAR_ls_VI_VC_walking = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + walking|country), 
                                        data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_ls_VI_VC_walking, type = "re", grid = FALSE)

#4 selfcare
model_ANAR_ls_VI_VC_selfcare = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                           wealth + hearing + seeing + walking + selfcare + 
                                           communication + learning + remembering +
                                           concentrating + accepting +
                                           controlling + makefriends +
                                           anxiety + depression  + (1 + selfcare|country), 
                                         data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_ls_VI_VC_selfcare, type = "re", grid = FALSE)

#5 communication
model_ANAR_ls_VI_VC_communication = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                                wealth + hearing + seeing + walking + selfcare + 
                                                communication + learning + remembering +
                                                concentrating + accepting +
                                                controlling + makefriends +
                                                anxiety + depression  + (1 + communication|country), 
                                              data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
plot_model(model_ANAR_ls_VI_VC_communication, type = "re", grid = FALSE)

#6 Learning
model_ANAR_ls_VI_VC_learning = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                           wealth + hearing + seeing + walking + selfcare + 
                                           communication + learning + remembering +
                                           concentrating + accepting +
                                           controlling + makefriends +
                                           anxiety + depression  + (1 + learning|country), 
                                         data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
#7 remembering
model_ANAR_ls_VI_VC_remembering = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + remembering|country), 
                                            data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#8 concentrating
model_ANAR_ls_VI_VC_concentrating = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                                wealth + hearing + seeing + walking + selfcare + 
                                                communication + learning + remembering +
                                                concentrating + accepting +
                                                controlling + makefriends +
                                                anxiety + depression  + (1 + concentrating|country), 
                                              data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#9 accepting
model_ANAR_ls_VI_VC_accepting = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                            wealth + hearing + seeing + walking + selfcare + 
                                            communication + learning + remembering +
                                            concentrating + accepting +
                                            controlling + makefriends +
                                            anxiety + depression  + (1 + accepting|country), 
                                          data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#10 controlling
model_ANAR_ls_VI_VC_controlling = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + controlling|country), 
                                            data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#11 makefriends
model_ANAR_ls_VI_VC_makefriends = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                              wealth + hearing + seeing + walking + selfcare + 
                                              communication + learning + remembering +
                                              concentrating + accepting +
                                              controlling + makefriends +
                                              anxiety + depression  + (1 + makefriends|country), 
                                            data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#12 anxiety
model_ANAR_ls_VI_VC_anxiety = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + anxiety|country), 
                                        data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))

#13 depression
model_ANAR_ls_VI_VC_anxiety = lmer(attendance ~  CB3 + CB3squared + boy + urban  + 
                                          wealth + hearing + seeing + walking + selfcare + 
                                          communication + learning + remembering +
                                          concentrating + accepting +
                                          controlling + makefriends +
                                          anxiety + depression  + (1 + depression|country), 
                                        data = wcar_ls, control = lmerControl(optimizer ="Nelder_Mead"))
