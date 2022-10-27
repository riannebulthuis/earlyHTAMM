#### Code description ####
# Monthly cycle time
# State-time dependent

# load libraries
library(heemod)
library(flexsurv)
library(diagram)
library(ggplot2)
library(readr)
library(tictoc)
library(writexl)

#### write data to path ####
xlsx_path = "//ad.utwente.nl/TNW//M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20221020 - Paediatrics 120 cycles opt/df_results.xlsx"
RDS_path_mod = "//ad.utwente.nl/TNW//M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20221020 - Paediatrics 120 cycles opt/res_mod.rds"
RDS_path_DSA = "//ad.utwente.nl/TNW//M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20221020 - Paediatrics 120 cycles opt/res_DSA.rds"

tic("total")
#### Load function ####
source('//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Functions/surv_article.R')

tic("load data")
#### Load data ####
## Shah ####
## Survival data primary ##
Shah_Primary <- "//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Survival_data/Shah/Shah_Primary.csv"
Primary_surv <- surv_article(7399,Shah_Primary)
## Survival data second revision ##
Shah_First_revision <- "//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Survival_data/Shah/First_revision_2.csv"
First_revision_surv <- surv_article(1479,Shah_First_revision)
Shah_Second_revision <- "//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Survival_data/Shah/Second_revision_2.csv"
Second_revision_surv <- surv_article(557,Shah_Second_revision)
revision_comb <- mix(First_revision_surv, Second_revision_surv,weights = c(.62,.38))

## Jeremiah ####
## survival data poor ##
Jeremiah_poor_ia <- "//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Survival_data/Jeremiah/Poor_survival_ia_r_2.csv"
Poor_ia_surv <- surv_article(10,Jeremiah_poor_ia)                       
## Gmeiner ####
## KM for time from ia to death ##
Gmeiner_survival <- "//ad.utwente.nl/tnw/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Survival_data/Gmeiner/Survival_1992.csv"
Survival_death_children <- surv_article(67,Gmeiner_survival)
toc()
# 
tic("Define model parameters")
#### Model parameters (par_mod) ####
par_mod <- define_parameters(
  time_transf = 12, #12 #from year to week (52), from year to months (12)
  cycl_len = 30, #30 #survival curves are in days, transform to weekly probabilities (7), tranform to montly probabilities (30)
  
  age_base = 0, #children 
  age_cycle = model_time/time_transf + age_base, 

## Shunt survival ####
  shunt_surv_ia = compute_surv(
    Primary_surv,
    time = model_time,
    cycle_length = cycl_len,
    type = "prob"),
  shunt_surv_r = compute_surv(
    revision_comb,
    time = state_time, #model_time
    cycle_length = cycl_len,
    type = "prob"),
  shunt_surv_poor_ia = compute_surv(
    Poor_ia_surv,
    time = model_time,
    cycle_length = cycl_len,
    type = "prob"),
  shunt_surv_poor_ia_r = compute_surv(
    Poor_ia_surv,
    time = state_time, #model_time
    cycle_length = cycl_len,
    type = "prob"),
  p_death_pediatr = compute_surv(
    Survival_death_children,
    time = model_time,
    cycle_length = round(cycl_len/7),
    type = "prob"),

## death due to age ####
  sex_indiv = "MLE", # MLE => male in the WHO database
  p_death_mle = get_who_mr(
    age = age_cycle,
    sex = sex_indiv,
    country = "NLD",
    local = TRUE),
  sex_indiv = "FMLE", # MLE => male in the WHO database
  p_death_fmle = get_who_mr(
    age = age_cycle,
    sex = sex_indiv,
    country = "NLD",
    local = TRUE),
  p_death_all = .5*p_death_mle+.5*p_death_fmle,

# Transition probabilities in pediatrics
  p_death = combine_probs(
    p_death_all,
    p_death_pediatr),

# transition probability death surgery
  p_death_surg = 0.008,

# Transition probabilities freehand technique
  # placement weights
  w_opt = 0.565,
  w_interm = 0.116,
  w_incorr = 0.319,
  
  # attempts weights
  p_one = 0.2,
  p_multi = 1 - p_one,
  
  # probability optimal
  p_opt = 0.565,
  p_opt_one = p_opt*p_one, ####
  p_opt_multi = p_opt*p_multi,
  p_interm_one = (w_interm-(w_interm/(w_interm+w_incorr))*(p_opt-w_opt))*p_one,
  p_interm_multi = (w_interm-(w_interm/(w_interm+w_incorr))*(p_opt-w_opt))*p_multi,
  p_incorr_one = (w_incorr-(w_incorr/(w_interm+w_incorr))*(p_opt-w_opt))*p_one,
  p_incorr_multi = (w_incorr-(w_incorr/(w_interm+w_incorr))*(p_opt-w_opt))*p_multi,
  
  # probability interm
  # p_interm = 0.116,
  # p_opt_one = (w_opt-(w_opt/(w_opt+w_incorr))*(p_interm-w_interm))*p_one,
  # p_opt_multi = (w_opt-(w_opt/(w_opt+w_incorr))*(p_interm-w_interm))*p_multi,
  # p_interm_one = p_interm*p_one,
  # p_interm_multi = p_interm*p_multi,
  # p_incorr_one = (w_incorr-(w_incorr/(w_opt+w_incorr))*(p_interm-w_interm))*p_one,
  # p_incorr_multi = (w_incorr-(w_incorr/(w_opt+w_incorr))*(p_interm-w_interm))*p_multi,
  
  # probability incorr
  # p_incorr = 0.319,
  # p_opt_one = (w_opt-(w_opt/(w_opt+w_interm))*(p_incorr-w_incorr))*p_one,
  # p_opt_multi = (w_opt-(w_opt/(w_opt+w_interm))*(p_incorr-w_incorr))*p_multi,
  # p_interm_one = (w_interm-(w_interm/(w_opt+w_interm))*(p_incorr-w_incorr))*p_one,
  # p_interm_multi = (w_interm-(w_interm/(w_opt+w_interm))*(p_incorr-w_incorr))*p_multi,
  # p_incorr_one = p_incorr*p_one,
  # p_incorr_multi = p_incorr*p_multi,
  
  p_ae_one = 0.1,
  p_ae_multi = 0.1,
  p_one_ae_opt = p_ae_one,
  p_one_h_opt = 1 - p_one_ae_opt - p_death_surg, 
  p_multi_ae_opt = p_ae_multi,
  p_multi_h_opt = 1 - p_multi_ae_opt - p_death_surg,
  p_one_ae_interm = p_ae_one, 
  p_one_h_interm = 1 - p_one_ae_interm - p_death_surg, 
  p_multi_ae_interm = p_ae_multi, 
  p_multi_h_interm = 1- p_multi_ae_interm - p_death_surg,
  p_one_ae_incorr = p_ae_one, 
  p_one_h_incorr = 1- p_one_ae_incorr - p_death_surg, 
  p_multi_ae_incorr = p_ae_multi, 
  p_multi_h_incorr = 1 - p_multi_ae_incorr - p_death_surg,
  p_h_opt_compl = shunt_surv_ia, 
  p_h_opt_incorr = 0, 
  p_h_opt_r = 1 - p_h_opt_compl - p_h_opt_incorr - p_death,
  
  p_ae_compl = 0.34,
  p_ae_opt_compl = p_ae_compl,
  p_ae_opt_r = 0,
  p_ae_opt_h_opt = 1 - p_ae_opt_r - p_ae_opt_compl - p_death,
  p_h_interm_compl = shunt_surv_ia,
  p_h_interm_incorr = 0, 
  p_h_interm_r = 1 - p_h_interm_compl - p_h_interm_incorr - p_death, 
  p_ae_interm_compl = p_ae_compl, 
  p_ae_interm_r = 0,
  p_ae_interm_h_interm = 1 - p_ae_interm_r - p_ae_interm_compl - p_death, 
  p_h_incorr_revision = shunt_surv_poor_ia, 
  p_h_incorr_r = 1 - p_h_incorr_revision - p_death, 
  p_ae_incorr_revision = p_ae_compl,
  p_ae_incorr_h_incorr = 1 - p_ae_incorr_revision - p_death,
  p_r_h_opt_compl = shunt_surv_r,
  p_r_h_opt_incorr = 0, 
  p_r_h_opt_r = 1 - p_r_h_opt_compl - p_r_h_opt_incorr - p_death, 
  p_r_ae_opt_compl = p_ae_compl, 
  p_r_ae_opt_r = 0,
  p_r_ae_opt_h_opt = 1 - p_r_ae_opt_r - p_r_ae_opt_compl - p_death,
  p_r_h_interm_compl = shunt_surv_r, 
  p_r_h_interm_incorr = 0, 
  p_r_h_interm_r = 1 - p_r_h_interm_compl - p_r_h_interm_incorr - p_death, 
  p_r_ae_interm_compl = p_ae_compl,
  p_r_ae_interm_r = 0,
  p_r_ae_interm_h_interm = 1 - p_r_ae_interm_r - p_r_ae_interm_compl - p_death,
  p_r_h_incorr_revision = shunt_surv_poor_ia_r, 
  p_r_h_incorr_r = 1 - p_r_h_incorr_revision - p_death, 
  p_r_ae_incorr_revision = p_ae_compl,
  p_r_ae_incorr_h_incorr = 1 - p_r_ae_incorr_revision - p_death, 

  # Transition probabilities USPAN
  p_opt_one_USPAN = 1,#0.144 
  p_opt_multi_USPAN = 0, #0.576
  p_interm_one_USPAN = 0,#1-p_opt_one_USPAN/2,# 0.056,
  p_interm_multi_USPAN = 0,#0.224,
  p_incorr_one_USPAN = 0,#1-p_opt_one_USPAN/2, #0,
  p_incorr_multi_USPAN = 0,#0,
  
  
  # costs
  costs_ia = 2799.44,
  costs_surg = 8098.86,
  costs_surg_r = 4884.48,
  costs_h = 16.46333,#197.56/time_transf, #months(12), weeks(7)
  costs_ae = 5386.85,
  costs_revision = 3622.51,
  costs_death = 0,
  costs_USPAN = 0, # expected costs 750 (500,1000)
  costs_treat = dispatch_strategy(
    FH = costs_surg,
   USPAN = costs_surg+costs_USPAN),
  costs_treat_r = dispatch_strategy(
    FH = costs_surg_r,
    USPAN = costs_surg_r+costs_USPAN),
  
  # utilities
  # pediatrics
  qol_ia = 0.63,
  qol_surg = 0.63,
  qol_h = 0.77,
  qol_ae = 0.66,
  qol_hydro_incorr = 0.63,
  qol_ae_incorr = 0.52,
  # qol_compl = 0.59,
  qol_revision = 0.59,
  qol_death = 0,
  # adults
  # qol_ia = 0.57,
  # qol_surg = 0.57,
  # qol_h = 0.71,
  # qol_ae = 0.60,
  # qol_hydro_incorr = 0.57,
  # qol_ae_incorr = 0.46,
  # qol_compl = 0.53,
  # qol_revision = 0.53,
  # qol_death = 0,
  
  # discount rate
  dr_costs = rescale_discount_rate(.04,time_transf,1),
  dr_qaly = rescale_discount_rate(.015,time_transf,1)
)
toc()


tic("Define transition matrix")
#### Transitions ####
## Freehand strategy ##
mat_FH <- define_transition(
  state_names = c("initial_admission","opt_one","opt_multi","interm_one","interm_multi","incorr_one","incorr_multi","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","revision","r_opt_one","r_opt_multi","r_interm_one","r_interm_multi","r_incorr_one","r_incorr_multi","r_h_opt","r_ae_opt","r_h_interm","r_ae_interm","r_h_incorr","r_ae_incorr","dead"),

  0, p_opt_one, p_opt_multi, p_interm_one, p_interm_multi, p_incorr_one, p_incorr_multi,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                               0,
  0,         0,           0,            0,              0,            0,              0,    p_one_h_opt,   p_one_ae_opt,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,  p_multi_h_opt, p_multi_ae_opt,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,       p_one_h_interm,   p_one_ae_interm,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,     p_multi_h_interm, p_multi_ae_interm,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,      p_one_h_incorr,      p_one_ae_incorr,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,    p_multi_h_incorr,    p_multi_ae_incorr,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,      p_h_opt_r,              0,                    0,                 0,      p_h_opt_incorr,                    0,                           p_h_opt_compl,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0, p_ae_opt_h_opt,     p_ae_opt_r,                    0,                 0,                   0,                    0,                          p_ae_opt_compl,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,         p_h_interm_r,                 0,   p_h_interm_incorr,                    0,                        p_h_interm_compl,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0, p_ae_interm_h_interm,     p_ae_interm_r,                   0,                    0,                       p_ae_interm_compl,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,        p_h_incorr_r,                    0,                     p_h_incorr_revision,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,p_ae_incorr_h_incorr,                    0,                    p_ae_incorr_revision,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0, p_opt_one, p_opt_multi, p_interm_one, p_interm_multi, p_incorr_one, p_incorr_multi,                0,              0,                      0,                 0,                      0,                 0,                               0,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,      p_one_h_opt,   p_one_ae_opt,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,    p_multi_h_opt, p_multi_ae_opt,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,         p_one_h_interm,   p_one_ae_interm,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,       p_multi_h_interm, p_multi_ae_interm,                      0,                 0,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,         p_one_h_incorr,   p_one_ae_incorr,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,       p_multi_h_incorr, p_multi_ae_incorr,                    p_death_surg,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                         p_r_h_opt_compl,         0,           0,            0,              0,            0,              0,      p_r_h_opt_r,              0,                      0,                 0,       p_r_h_opt_incorr,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                        p_r_ae_opt_compl,         0,           0,            0,              0,            0,              0, p_r_ae_opt_h_opt,   p_r_ae_opt_r,                      0,                 0,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                      p_r_h_interm_compl,         0,           0,            0,              0,            0,              0,                0,              0,         p_r_h_interm_r,                 0,    p_r_h_interm_incorr,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                     p_r_ae_interm_compl,         0,           0,            0,              0,            0,              0,                0,              0, p_r_ae_interm_h_interm,   p_r_ae_interm_r,                      0,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                   p_r_h_incorr_revision,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,         p_r_h_incorr_r,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                  p_r_ae_incorr_revision,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0, p_r_ae_incorr_h_incorr,                 0,                         p_death,
  0,         0,           0,            0,              0,            0,              0,              0,              0,                    0,                 0,                   0,                    0,                                       0,         0,           0,            0,              0,            0,              0,                0,              0,                      0,                 0,                      0,                 0,                                1
)



## USPAN strategy ##
mat_USPAN <- define_transition(
  state_names = c("initial_admission","opt_one","opt_multi","interm_one","interm_multi","incorr_one","incorr_multi","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","revision","r_opt_one","r_opt_multi","r_interm_one","r_interm_multi","r_incorr_one","r_incorr_multi","r_h_opt","r_ae_opt","r_h_interm","r_ae_interm","r_h_incorr","r_ae_incorr","dead"),
  
  0, p_opt_one_USPAN, p_opt_multi_USPAN, p_interm_one_USPAN, p_interm_multi_USPAN, p_incorr_one_USPAN, p_incorr_multi_USPAN,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                               0,          
  0,               0,                 0,                  0,                    0,                  0,                    0,    p_one_h_opt,   p_one_ae_opt,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,  p_multi_h_opt, p_multi_ae_opt,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,       p_one_h_interm,   p_one_ae_interm,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,     p_multi_h_interm, p_multi_ae_interm,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,      p_one_h_incorr,      p_one_ae_incorr,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,    p_multi_h_incorr,    p_multi_ae_incorr,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,      p_h_opt_r,              0,                    0,                 0,      p_h_opt_incorr,                    0,                           p_h_opt_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0, p_ae_opt_h_opt,     p_ae_opt_r,                    0,                 0,                   0,                    0,                          p_ae_opt_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,         p_h_interm_r,                 0,   p_h_interm_incorr,                    0,                        p_h_interm_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death, 
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0, p_ae_interm_h_interm,     p_ae_interm_r,                   0,                    0,                       p_ae_interm_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,        p_h_incorr_r,                    0,                     p_h_incorr_revision,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,p_ae_incorr_h_incorr,                    0,                    p_ae_incorr_revision,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0, p_opt_one_USPAN, p_opt_multi_USPAN, p_interm_one_USPAN, p_interm_multi_USPAN, p_incorr_one_USPAN, p_incorr_multi_USPAN,                0,              0,                      0,                 0,                      0,                 0,                               0,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,      p_one_h_opt,   p_one_ae_opt,                      0,                 0,                      0,                 0,                    p_death_surg, 
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,    p_multi_h_opt, p_multi_ae_opt,                      0,                 0,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,         p_one_h_interm,   p_one_ae_interm,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,       p_multi_h_interm, p_multi_ae_interm,                      0,                 0,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,         p_one_h_incorr,   p_one_ae_incorr,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,       p_multi_h_incorr, p_multi_ae_incorr,                    p_death_surg,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                         p_r_h_opt_compl,               0,                 0,                  0,                    0,                  0,                    0,      p_r_h_opt_r,              0,                      0,                 0,       p_r_h_opt_incorr,                 0,                         p_death,     
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                        p_r_ae_opt_compl,               0,                 0,                  0,                    0,                  0,                    0, p_r_ae_opt_h_opt,   p_r_ae_opt_r,                      0,                 0,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                      p_r_h_interm_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,         p_r_h_interm_r,                 0,    p_r_h_interm_incorr,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                     p_r_ae_interm_compl,               0,                 0,                  0,                    0,                  0,                    0,                0,              0, p_r_ae_interm_h_interm,   p_r_ae_interm_r,                      0,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                   p_r_h_incorr_revision,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,         p_r_h_incorr_r,                 0,                         p_death, 
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                  p_r_ae_incorr_revision,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0, p_r_ae_incorr_h_incorr,                 0,                         p_death,
  0,               0,                 0,                  0,                    0,                  0,                    0,              0,              0,                    0,                 0,                   0,                    0,                                       0,               0,                 0,                  0,                    0,                  0,                    0,                0,              0,                      0,                 0,                      0,                 0,                                1
)

toc()

tic("Define states")
### State values ####
state_initial_admission <- define_state(
  
  cost_total = discount(costs_ia, r=dr_costs),
  qaly = discount(qol_ia, r=dr_qaly)
)

state_opt_one <- define_state(

  cost_total = discount(costs_treat, r=dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_opt_multi <- define_state(
  
  cost_total = discount(costs_treat, r=dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_interm_one <- define_state(
  
  cost_total = discount(costs_treat, r = dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_interm_multi <- define_state(
  
  cost_total = discount(costs_treat, r = dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_incorr_one <- define_state(
  
  cost_total = discount(costs_treat, r = dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_incorr_multi <- define_state(
  
  cost_total = discount(costs_treat, r = dr_costs),
  qaly = discount(qol_surg, r = dr_qaly)
)

state_h_opt <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_h, r=dr_qaly)
)

state_ae_opt <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae, r=dr_qaly)
)

state_h_interm <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_h, r=dr_qaly)
)

state_ae_interm <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae, r=dr_qaly)
)

state_h_incorr <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_hydro_incorr, r=dr_qaly)
)

state_ae_incorr <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae_incorr, r=dr_qaly)
)

state_revision <- define_state(
  
  cost_total = discount(costs_revision, r=dr_costs),
  qaly = discount(qol_revision, r=dr_qaly)
)

state_r_opt_one <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_opt_multi <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_interm_one <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_interm_multi <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_incorr_one <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_incorr_multi <- define_state(
  
  cost_total = discount(costs_treat_r, r=dr_costs),
  qaly = discount(qol_surg, r=dr_qaly)
)

state_r_h_opt <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_h, r=dr_qaly)
)

state_r_ae_opt <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae, r=dr_qaly)
)

state_r_h_interm <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_h, r=dr_qaly)
)

state_r_ae_interm <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae, r=dr_qaly)
)

state_r_h_incorr <- define_state(
  
  cost_total = discount(costs_h, r=dr_costs),
  qaly = discount(qol_hydro_incorr, r=dr_qaly)
)

state_r_ae_incorr <- define_state(
  
  cost_total = discount(costs_ae, r=dr_costs),
  qaly = discount(qol_ae_incorr, r=dr_qaly)
)

state_dead <- define_state(
  
  cost_total = costs_death,
  qaly = qol_death
)

# state_initial_admission_USPAN <- define_state(
#   
#   cost_total = discount(costs_surg + costs_USPAN, r = dr_costs),
#   qaly = discount(qol_surg, r = dr_qaly)
# )

# state_revision_USPAN <- define_state(
#   
#   cost_total = discount(costs_revision + costs_USPAN, r=dr_costs),
#   qaly = discount(qol_revision, r=dr_qaly)
# )
toc()


tic("Define strategies")
#### Strategies ####
strat_FH <- define_strategy(
  transition = mat_FH,
  
  initial_admission = state_initial_admission,
  opt_one = state_opt_one,
  opt_multi = state_opt_multi,
  interm_one = state_interm_one,
  interm_multi = state_interm_multi,
  incorr_one = state_incorr_one,
  incorr_multi = state_incorr_multi,
  h_opt = state_h_opt,
  ae_opt = state_ae_opt,
  h_interm = state_h_interm,
  ae_interm = state_ae_interm,
  h_incorr = state_h_incorr,
  ae_incorr = state_ae_incorr,
  revision = state_revision,
  r_opt_one = state_r_opt_one,
  r_opt_multi = state_r_opt_multi,
  r_interm_one = state_r_interm_one,
  r_interm_multi = state_r_interm_multi,
  r_incorr_one = state_r_incorr_one,
  r_incorr_multi = state_r_incorr_multi,
  r_h_opt = state_r_h_opt,
  r_ae_opt = state_r_ae_opt,
  r_h_interm = state_r_h_interm,
  r_ae_interm = state_ae_interm,
  r_h_incorr = state_r_h_incorr,
  r_ae_incorr = state_r_ae_incorr,
  dead = state_dead
)

strat_USPAN <- define_strategy(
  transition = mat_USPAN,
  
  initial_admission = state_initial_admission, 
  opt_one = state_opt_one,
  opt_multi = state_opt_multi,
  interm_one = state_interm_one,
  interm_multi = state_interm_multi,
  incorr_one = state_incorr_one,
  incorr_multi = state_incorr_multi,
  h_opt = state_h_opt,
  ae_opt = state_ae_opt,
  h_interm = state_h_interm,
  ae_interm = state_ae_interm,
  h_incorr = state_h_incorr,
  ae_incorr = state_ae_incorr,
  revision = state_revision, 
  r_opt_one = state_r_opt_one,
  r_opt_multi = state_r_opt_multi,
  r_interm_one = state_r_interm_one,
  r_interm_multi = state_r_interm_multi,
  r_incorr_one = state_r_incorr_one,
  r_incorr_multi = state_r_incorr_multi,
  r_h_opt = state_r_h_opt,
  r_ae_opt = state_r_ae_opt,
  r_h_interm = state_r_h_interm,
  r_ae_interm = state_ae_interm,
  r_h_incorr = state_r_h_incorr,
  r_ae_incorr = state_r_ae_incorr,
  dead = state_dead
)
toc()



tic("running model")
res_mod <- run_model(
  parameters = par_mod,

  FH = strat_FH,
  USPAN = strat_USPAN,

  init = c(1000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),

  cycles = 120,

  cost = cost_total,
  effect = qaly,
  # state_time_limit = 
  method = "life-table"
)
toc()
toc()

saveRDS(res_mod, RDS_path_mod)
save.image("//ad.utwente.nl/TNW//M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20221020 - Paediatrics 120 cycles opt/results.rdata")

##### save and export results ####
df_results <- data.frame("time_transf" = par_mod$time_transf$expr,# time_transf,
                         "cycl_len" = par_mod$cycl_len$expr,
                         "h_opt" = res_mod$eval_strategy_list$FH$counts$h_opt,
                         "ae_opt"= res_mod$eval_strategy_list$FH$counts$ae_opt,
                         "h_interm"=res_mod$eval_strategy_list$FH$counts$h_interm,
                         "ae_interm" = res_mod$eval_strategy_list$FH$counts$ae_interm,
                         "h_incorr" = res_mod$eval_strategy_list$FH$counts$h_incorr,
                         "ae_incorr" = res_mod$eval_strategy_list$FH$counts$ae_incorr,
                         "revision" = res_mod$eval_strategy_list$FH$counts$revision,
                         "r_h_opt" = res_mod$eval_strategy_list$FH$counts$r_h_opt,
                         "r_ae_opt" = res_mod$eval_strategy_list$FH$counts$r_ae_opt,
                         "r_h_interm" = res_mod$eval_strategy_list$FH$counts$r_h_interm,
                         "r_ae_interm" = res_mod$eval_strategy_list$FH$counts$r_ae_interm,
                         "r_h_incorr" = res_mod$eval_strategy_list$FH$counts$r_h_incorr,
                         "r_ae_incorr" = res_mod$eval_strategy_list$FH$counts$r_ae_incorr,
                         "dead" = res_mod$eval_strategy_list$FH$counts$dead,
                         "total_costs" = res_mod$eval_strategy_list$FH$values$cost_total,
                         "total_qaly" = res_mod$eval_strategy_list$FH$values$qaly,
                         "h_opt_USPAN" = res_mod$eval_strategy_list$USPAN$counts$h_opt,
                         "ae_opt_USPAN"= res_mod$eval_strategy_list$USPAN$counts$ae_opt,
                         "h_interm_USPAN"=res_mod$eval_strategy_list$USPAN$counts$h_interm,
                         "ae_interm_USPAN" = res_mod$eval_strategy_list$USPAN$counts$ae_interm,
                         "h_incorr_USPAN" = res_mod$eval_strategy_list$USPAN$counts$h_incorr,
                         "ae_incorr_USPAN" = res_mod$eval_strategy_list$USPAN$counts$ae_incorr,
                         "revision_USPAN" = res_mod$eval_strategy_list$USPAN$counts$revision,
                         "r_h_opt_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_h_opt,
                         "r_ae_opt_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_ae_opt,
                         "r_h_interm_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_h_interm,
                         "r_ae_interm_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_ae_interm,
                         "r_h_incorr_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_h_incorr,
                         "r_ae_incorr_USPAN" = res_mod$eval_strategy_list$USPAN$counts$r_ae_incorr,
                         "dead_USPAN" = res_mod$eval_strategy_list$USPAN$counts$dead,
                         "total_costs_USPAN" = res_mod$eval_strategy_list$USPAN$values$cost_total,
                         "total_qaly_USPAN" = res_mod$eval_strategy_list$USPAN$values$qaly)

write_xlsx(df_results,xlsx_path)

#### Calculate HR ####
dQALY <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly)-sum(res_mod$eval_strategy_list$FH$values$qaly))/12)/1000
dcosts <- (sum(res_mod$eval_strategy_list$USPAN$values$cost_total)-sum(res_mod$eval_strategy_list$FH$values$cost_total))/1000
HR20 <- (dQALY*20000)-dcosts
HR50 <- (dQALY*50000)-dcosts
HR80 <- (dQALY*80000)-dcosts

#### Visualisation of survival curves ####
# Shunt survival initial shunt in pediatrics
# plot(Primary_surv, B = 1000,xlim=c(0,1250), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Shunt survival after initial shunt placement in pediatrics", xlab="Time (days)", ylab="Survival probability")
# legend("topright",title = "placement",c("Fit corr/interm","KM corr/interm"),fill=c("#304FFE","#B0BEC5"))
# 
# ## Shunt survival revision shunt in pediatrics
# plot(First_revision_surv,B=1479, xlim=c(0,1800),col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1)
# par(new=TRUE)
# plot(Second_revision_surv,col.obs = "#FFB74D",col = "#EF6C00",lwd.obs= .1,xlim=c(0,1800), main = "Shunt survival curves after first and second revision in pediatrics",xlab="Time (days)", ylab="Survival probability")
# legend("topright",title = "Revision",c("Fit first","KM first","Fit second", "KM second"),fill=c("#304FFE","#B0BEC5","#EF6C00","#FFB74D"))
# 
# ## Shunt survival after poor placement in pediatrics and adults
# plot(Poor_ia_surv, B = 1000,xlim=c(0,1800), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Shunt survival curves after poor shunt placement", xlab="Time (days)", ylab="Survival probability")
# legend("topright",title = "Placement",c("Fit poor","KM poor"),fill=c("#304FFE","#B0BEC5"))
# 
# ## Survival after initial shunt placement in pediatrics
# plot(Survival_death_children, B = 1000,xlim=c(0,1250), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Survival after initial shunt placement in pediatrics", xlab="Time (weeks)", ylab="Survival probability")
# legend("topright",title = "Survival",c("Fit","KM"),fill=c("#304FFE","#B0BEC5"))

#### Visualization of base case ####
plot(res_mod,type="counts", panel = "by_state", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"))
# plot(res_mod,type="counts", panel = "by_state", states = c("revision","r_h_opt","r_ae_opt","r_h_interm","r_ae_interm","r_h_incorr","ae_incorr","dead"))
#plot(res_mod,type="values", panel = "by_value",  values = "cost_total", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"))
# plot(res_mod,type="values", panel = "by_value",  values = "qaly", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"))

dsa_min = 0.8
dsa_max = 1.2
dsa_min_q = 0.95
dsa_max_q = 1.05

# #### DSA ####
# ## parameters ##
# def_dsa <- define_dsa(
#   p_death_surg, par_mod$p_death_surg$expr*dsa_min, par_mod$p_death_surg$expr*dsa_max,
#   p_one, par_mod$p_one$expr*dsa_min, par_mod$p_one$expr*dsa_max,
#   p_opt, par_mod$p_opt$expr*dsa_min, par_mod$p_opt$expr*dsa_max,
#   p_ae_one, par_mod$p_ae_one$expr*0.5, par_mod$p_ae_one$expr*1.5,
#   p_ae_multi, par_mod$p_ae_multi$expr*0.5, par_mod$p_ae_multi$expr*1.5,
#   p_ae_compl, par_mod$p_ae_compl$expr*dsa_min,par_mod$p_ae_compl$expr*dsa_max,
#   costs_ia, 1976.37, 3622.51,
#   costs_surg, 5629.65, 10568.07,
#   costs_surg_r, 3238.34, 5268.65,
#   costs_revision, 1976.37, 5268.65,
#   costs_ae, par_mod$costs_ae$expr*dsa_min, par_mod$costs_ae$expr*dsa_max,
#   costs_h, par_mod$costs_h$expr*dsa_min, par_mod$costs_h$expr*dsa_max,
#   qol_ia, par_mod$qol_ia$expr*dsa_min_q, 0.66,
#   qol_surg, par_mod$qol_surg$expr*dsa_min_q, 0.66,
#   qol_h, par_mod$qol_h$expr*dsa_min_q, par_mod$qol_h$expr*dsa_max_q,
#   qol_ae, par_mod$qol_ae$expr*dsa_min_q, par_mod$qol_ae$expr*dsa_max_q,
#   qol_hydro_incorr, par_mod$qol_hydro_incorr$expr*dsa_min_q, 0.66,
#   qol_ae_incorr, par_mod$qol_ae_incorr$expr*dsa_min_q, par_mod$qol_ae_incorr$expr*dsa_max_q,
#   qol_revision, par_mod$qol_revision$expr*dsa_min_q, par_mod$qol_revision$expr*dsa_max_q
#   )
# 
# 
# # ## run DSA ##
# tic("running DSA")
# # use_cluster(4, cluster = NULL, close = TRUE)
# res_dsa <- run_dsa(res_mod, dsa = def_dsa)
# toc()
# 
# #### save and export DSA results ####
# saveRDS(res_dsa, RDS_path_DSA)
# save.image("//ad.utwente.nl/TNW//M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20221020 - Paediatrics 120 cycles opt/results2.rdata")
# #### Visualisation of survival curves ####
# # Shunt survival initial shunt in pediatrics
# # plot(Primary_surv, B = 1000,xlim=c(0,1250), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Shunt survival after initial shunt placement in pediatrics", xlab="Time (days)", ylab="Survival probability")
# # legend("topright",title = "placement",c("Fit corr/interm","KM corr/interm"),fill=c("#304FFE","#B0BEC5"))
# # 
# # ## Shunt survival revision shunt in pediatrics
# # plot(First_revision_surv,B=1479, xlim=c(0,1800),col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1)
# # par(new=TRUE)
# # plot(Second_revision_surv,col.obs = "#FFB74D",col = "#EF6C00",lwd.obs= .1,xlim=c(0,1800), main = "Shunt survival curves after first and second revision in pediatrics",xlab="Time (days)", ylab="Survival probability")
# # legend("topright",title = "Revision",c("Fit first","KM first","Fit second", "KM second"),fill=c("#304FFE","#B0BEC5","#EF6C00","#FFB74D"))
# # 
# # ## Shunt survival after poor placement in pediatrics and adults
# # plot(Poor_ia_surv, B = 1000,xlim=c(0,1800), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Shunt survival curves after poor shunt placement", xlab="Time (days)", ylab="Survival probability")
# # legend("topright",title = "Placement",c("Fit poor","KM poor"),fill=c("#304FFE","#B0BEC5"))
# # 
# # ## Survival after initial shunt placement in pediatrics
# # plot(Survival_death_children, B = 1000,xlim=c(0,1250), col.obs = "#B0BEC5", col = "#304FFE",lwd.obs= .1, main = "Survival after initial shunt placement in pediatrics", xlab="Time (weeks)", ylab="Survival probability")
# # legend("topright",title = "Survival",c("Fit","KM"),fill=c("#304FFE","#B0BEC5"))
# 
# 
# 
# # title_dsa_plot_costs <- c(difference = "Incremental Costs (???)")
# 
# # plot(res_dsa,type = "difference", result = "cost", remove_ns = FALSE) +
#     # geom_vline(xintercept =  -1286, linetype = "dashed") +
#     # annotate("text", x=-1130, y=6.4, color = "dodgerblue3", label = ">> Larger effect than the mean") +
#     # annotate("text", x=-1450, y=6.4, color = "red2", label = "Smaller effect than the mean <<")
