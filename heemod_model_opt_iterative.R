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

n = 20
m = 20

p_opt_itt_USPAN = seq(from = 0.56, to = 0.99999, length = n)
costs_itt_USPAN = seq(from = 0, to = 1200, length = m)

output_HR20 =matrix(NA, n,m)
rownames(output_HR20) = p_opt_itt_USPAN[1:n]
colnames(output_HR20) = costs_itt_USPAN[1:m]
output_HR50 =matrix(NA, n,m)
rownames(output_HR50) = p_opt_itt_USPAN[1:n]
colnames(output_HR50) = costs_itt_USPAN[1:m]
output_HR80 =matrix(NA, n,m)
rownames(output_HR80) = p_opt_itt_USPAN[1:n]
colnames(output_HR80) = costs_itt_USPAN[1:m]


for(i in 1:n){
  for(j in 1:m){

#### write data to path ####
rdata_path <-file.path("//ad.utwente.nl/TNW//M3I/Students/Master/Rianne Bulthuis/R_stat/Plots/20210411 - Pediatrics 60 cycles opt costs iterative",paste("p_opt_", round(p_opt_itt_USPAN[i],digits = 2),"_costs_",round(costs_itt_USPAN[j]),".rdata",sep = ""))

tic("total")
#### Load function ####
source('//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Functions/surv_article.R')

tic("load data")
#### Load data ####
## Shah ####
## Survival data primary ##
Shah_Primary <- "//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Survival_data/Shah/Shah_Primary.csv"
Primary_surv <- surv_article(7399,Shah_Primary)
## Survival data second revision ##
Shah_First_revision <- "//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Survival_data/Shah/First_revision_2.csv"
First_revision_surv <- surv_article(1479,Shah_First_revision)
Shah_Second_revision <- "//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Survival_data/Shah/Second_revision_2.csv"
Second_revision_surv <- surv_article(557,Shah_Second_revision)
revision_comb <- mix(First_revision_surv, Second_revision_surv,weights = c(.62,.38))

## Jeremiah ####
## survival data poor ##
Jeremiah_poor_ia <- "//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Survival_data/Jeremiah/Poor_survival_ia_r_2.csv"
Poor_ia_surv <- surv_article(10,Jeremiah_poor_ia)                       
## Gmeiner ####
## KM for time from ia to death ##
Gmeiner_survival <- "//ad.utwente.nl/tnw/M3I/Students/Master/Rianne Bulthuis/R_stat/Survival_data/Gmeiner/Survival_1992.csv"
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
    time = state_time,
    cycle_length = cycl_len,
    type = "prob"),
  shunt_surv_poor_ia = compute_surv(
    Poor_ia_surv,
    time = model_time,
    cycle_length = cycl_len,
    type = "prob"),
  shunt_surv_poor_ia_r = compute_surv(
    Poor_ia_surv,
    time = state_time, 
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
  # probability optimal USPAN
  p_opt_USPAN = p_opt_itt_USPAN[i],
  p_opt_one_USPAN = p_opt_USPAN*p_one, 
  p_opt_multi_USPAN = p_opt_USPAN*p_multi,
  p_interm_one_USPAN = (w_interm-(w_interm/(w_interm+w_incorr))*(p_opt_USPAN-w_opt))*p_one,
  p_interm_multi_USPAN = (w_interm-(w_interm/(w_interm+w_incorr))*(p_opt_USPAN-w_opt))*p_multi,
  p_incorr_one_USPAN = (w_incorr-(w_incorr/(w_interm+w_incorr))*(p_opt_USPAN-w_opt))*p_one,
  p_incorr_multi_USPAN = (w_incorr-(w_incorr/(w_interm+w_incorr))*(p_opt_USPAN-w_opt))*p_multi,

  # costs
  costs_ia = 2799.44,
  costs_surg = 8098.86,
  costs_surg_r = 4884.48,
  costs_h = 197.56/time_transf, #months(12), weeks(7)
  costs_ae = 5386.85,
  costs_revision = 3622.51,
  costs_death = 0,
  costs_USPAN = costs_itt_USPAN[j], # expected costs 750 (500,1000)
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

  cycles = 60,

  cost = cost_total,
  effect = qaly,
  # state_time_limit = 
  method = "life-table"
)
toc()
toc()

#### Calculate HR ####
dQALY <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly)-sum(res_mod$eval_strategy_list$FH$values$qaly))/12)/1000
dcosts <- (sum(res_mod$eval_strategy_list$USPAN$values$cost_total)-sum(res_mod$eval_strategy_list$FH$values$cost_total))/1000
HR20 <- (dQALY*20000)-dcosts
HR50 <- (dQALY*50000)-dcosts
HR80 <- (dQALY*80000)-dcosts

output_HR20[i,j] = HR20
output_HR50[i,j] = HR50
output_HR80[i,j] = HR80

save.image(rdata_path)

  }
}

