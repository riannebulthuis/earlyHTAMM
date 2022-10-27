library(heemod)
library(ggplot2)

# res_mod <- readRDS("//AD.utwente.nl/TNW/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20210407 - Pediatrics 60 cycles opt/res_mod.rds")
res_mod <- readRDS("//AD.utwente.nl/TNW/M3I/Students/Master/2021 - Rianne Bulthuis/R_stat/Plots/20220816 - Pediatrics 60 cycles opt/res_mod.rds")

plot(res_mod,type="counts", panel = "by_state", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead")) #+ facet_matrix(cols = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"), scales = "free_y")
plot(res_mod,type="counts", panel = "by_state", states = c("revision","r_h_opt","r_ae_opt","r_h_interm","r_ae_interm","r_h_incorr","ae_incorr","dead"))
plot(res_mod,type="values", panel = "by_value",  values = "cost_total", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"))
plot(res_mod,type="values", panel = "by_value",  values = "qaly", states = c("initial_admission","h_opt","ae_opt","h_interm","ae_interm","h_incorr","ae_incorr","dead"))

# calculate HR 5 years
dQALY <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly)-sum(res_mod$eval_strategy_list$FH$values$qal))/12)/1000
dCosts <- (sum(res_mod$eval_strategy_list$USPAN$values$cost_total)-sum(res_mod$eval_strategy_list$FH$values$cost_total))/1000
HR20 <- (dQALY*20000)-dCosts
HR50 <- (dQALY*50000)-dCosts
HR80 <- (dQALY*80000)-dCosts

HR <- data.frame(dQALY,dCosts, row.names = "USPAN") 

QALY = seq(0,0.5, by=0.3)
WTP_20 = 20000*QALY
WTP_50 = 50000*QALY
WTP_80 = 80000*QALY

WTP20 = data.frame(QALY,WTP_20)
WTP50 = data.frame(QALY,WTP_50)
WTP80 = data.frame(QALY,WTP_80)

ggplot() +
  geom_point(data = HR,aes(dQALY,dCosts),color ="#1E88E5", size = 5) + 
  geom_line(data = WTP20,aes(QALY,WTP_20), size = 2, color = "red") +
  geom_line(data = WTP50,aes(QALY,WTP_50), size = 2, color = "red") +
  geom_line(data = WTP80,aes(QALY,WTP_80), size = 2, color = "red") +
  labs(x = expression(paste(Delta, "QALY")),y = expression(paste(Delta, "Costs", sep = " ",("\u20AC")))) +
  theme_bw()+
  theme(axis.text = element_text(size = 20),axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  annotate("text", x=0.15, y=-3700, label= "USPAN",size = 8) +
  annotate("text", x=0.25, y= 3500, label= "WTP = \u20AC 20,000",size = 5) +
  annotate("text", x=0.25, y= 10000, label= "WTP = \u20AC 50,000",size = 5) +
  annotate("text", x=0.25, y= 16500, label= "WTP = \u20AC 80,000",size = 5) +
  ggtitle("Time Horizon 5 years, Pediatrics (HR20 = \u20AC 5889, HR50 = \u20AC 9420, HR80 = \u20AC 12951)")


# calculate costs per patient
costs_USPAN <- 0:2000

dQALY3 <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly[0:36])-sum(res_mod$eval_strategy_list$FH$values$qaly[0:36]))/12)/1000
dQALY5 <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly[0:60])-sum(res_mod$eval_strategy_list$FH$values$qaly[0:60]))/12)/1000
# dQALY10 <- ((sum(res_mod$eval_strategy_list$USPAN$values$qaly[0:120])-sum(res_mod$eval_strategy_list$FH$values$qaly[0:120]))/12)/1000

costs_USPAN_3 <- (sum(res_mod$eval_strategy_list$USPAN$counts$revision)+1000)*costs_USPAN+sum(res_mod$eval_strategy_list$USPAN$values$cost_total[0:36])
dCosts3 <- (costs_USPAN_3-sum(res_mod$eval_strategy_list$FH$values$cost_total[0:36]))/1000
costs_USPAN_5 <- (sum(res_mod$eval_strategy_list$USPAN$counts$revision)+1000)*costs_USPAN+sum(res_mod$eval_strategy_list$USPAN$values$cost_total[0:60])
dCosts5 <- (costs_USPAN_5-sum(res_mod$eval_strategy_list$FH$values$cost_total[0:60]))/1000
# costs_USPAN_10 <- (sum(res_mod$eval_strategy_list$USPAN$counts$revision)+1000)*costs_USPAN+sum(res_mod$eval_strategy_list$USPAN$values$cost_total[0:120])
# dCosts10 <- (costs_USPAN_10-sum(res_mod$eval_strategy_list$FH$values$cost_total[0:120]))/1000

HR20_USPAN <- (dQALY5*20000)-dCosts5
HR50_USPAN <- (dQALY5*50000)-dCosts5
HR80_USPAN <- (dQALY5*80000)-dCosts5


HR20USPAN = data.frame(costs_USPAN = costs_USPAN,HR_USPAN = HR20_USPAN)
HR50USPAN = data.frame(costs_USPAN = costs_USPAN,HR_USPAN = HR50_USPAN)
HR80USPAN = data.frame(costs_USPAN = costs_USPAN,HR_USPAN = HR80_USPAN)


HRUSPAN <- rbind.data.frame(HR20_USPAN,HR50_USPAN,HR80_USPAN)

ggplot() +
  geom_line(data = HR20USPAN,aes(costs_USPAN,HR20_USPAN), size = 2, color = "red") +
  geom_line(data = HR50USPAN,aes(costs_USPAN,HR50_USPAN), size = 2, color = "blue") +
  geom_line(data = HR80USPAN,aes(costs_USPAN,HR80_USPAN), size = 2, color = "green") +
  labs(x = "Production costs USPAN (\u20AC)", y = "Headroom (\u20AC)") +
  theme_bw()+
  theme(axis.text = element_text(size = 20),axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  annotate("text", x=1500, y=4500, label= "WTP = \u20AC 20,000",size = 5) +
  annotate("text", x=1500, y= 8000, label= "WTP = \u20AC 50,000",size = 5) +
  annotate("text", x=1500, y= 12000, label= "WTP = \u20AC 80,000",size = 5)

#### plot
theme_set(theme_light() + theme(panel.grid.minor = element_line(linetype = "dotted"))) #+  theme(legend.title=element_text(size=20),legend.text=element_text(size=14)))
df_results$markov_cycle = seq(1,240,1)

# initial admission
ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = h_opt), color = "#D16103") + 
  geom_point(aes(y = h_opt), color = "#D16103") +
  geom_line(aes(y = h_opt_USPAN), color = "#00AFBB") +
  geom_point(aes(y = h_opt_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")
  
ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = ae_opt), color = "#D16103") + 
  geom_point(aes(y = ae_opt), color = "#D16103") +
  geom_line(aes(y = ae_opt_USPAN), color = "#00AFBB") +
  geom_point(aes(y = ae_opt_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = h_interm), color = "#D16103") + 
  geom_point(aes(y = h_interm), color = "#D16103") +
  geom_line(aes(y = h_interm_USPAN), color = "#00AFBB") +
  geom_point(aes(y = h_interm_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = ae_interm), color = "#D16103") + 
  geom_point(aes(y = ae_interm), color = "#D16103") +
  geom_line(aes(y = ae_interm_USPAN), color = "#00AFBB") +
  geom_point(aes(y = ae_interm_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = h_incorr), color = "#D16103") + 
  geom_point(aes(y = h_incorr), color = "#D16103") +
  geom_line(aes(y = h_incorr_USPAN), color = "#00AFBB") +
  geom_point(aes(y = h_incorr_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = ae_incorr), color = "#D16103") + 
  geom_point(aes(y = ae_incorr), color = "#D16103") +
  geom_line(aes(y = ae_incorr_USPAN), color = "#00AFBB") +
  geom_point(aes(y = ae_incorr_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

# revision
ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_h_opt), color = "#D16103") + 
  geom_point(aes(y = r_h_opt), color = "#D16103") +
  geom_line(aes(y = r_h_opt_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_h_opt_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_ae_opt), color = "#D16103") + 
  geom_point(aes(y = r_ae_opt), color = "#D16103") +
  geom_line(aes(y = r_ae_opt_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_ae_opt_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_h_interm), color = "#D16103") + 
  geom_point(aes(y = r_h_interm), color = "#D16103") +
  geom_line(aes(y = r_h_interm_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_h_interm_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_ae_interm), color = "#D16103") + 
  geom_point(aes(y = r_ae_interm), color = "#D16103") +
  geom_line(aes(y = r_ae_interm_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_ae_interm_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_h_incorr), color = "#D16103") + 
  geom_point(aes(y = r_h_incorr), color = "#D16103") +
  geom_line(aes(y = r_h_incorr_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_h_incorr_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = r_ae_incorr), color = "#D16103") + 
  geom_point(aes(y = r_ae_incorr), color = "#D16103") +
  geom_line(aes(y = r_ae_incorr_USPAN), color = "#00AFBB") +
  geom_point(aes(y = r_ae_incorr_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

#

df_results$total_costs = df_results$total_costs/1000
df_results$total_costs_USPAN = df_results$total_costs_USPAN/1000

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = total_costs), color = "#D16103") + 
  geom_point(aes(y = total_costs), color = "#D16103") +
  geom_line(aes(y = total_costs_USPAN), color = "#00AFBB") +
  geom_point(aes(y = total_costs_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")# +
  # ylim(0, 500)

df_results$total_qaly = df_results$total_qaly/12
df_results$total_qaly_USPAN = df_results$total_qaly_USPAN/12

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = total_qaly), color = "#D16103") + 
  geom_point(aes(y = total_qaly), color = "#D16103") +
  geom_line(aes(y = total_qaly_USPAN), color = "#00AFBB") +
  geom_point(aes(y = total_qaly_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

# revision
ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = revision), color = "#D16103") + 
  geom_point(aes(y = revision), color = "#D16103") +
  geom_line(aes(y = revision_USPAN), color = "#00AFBB") +
  geom_point(aes(y = revision_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")

ggplot(data = df_results, aes(x = markov_cycle)) +
  geom_line(aes(y = dead), color = "#D16103") + 
  geom_point(aes(y = dead), color = "#D16103") +
  geom_line(aes(y = dead_USPAN), color = "#00AFBB") +
  geom_point(aes(y = dead_USPAN), color = "#00AFBB") +
  labs(x = "Months", y = "")
