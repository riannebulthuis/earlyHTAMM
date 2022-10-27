library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)

# load results DSA

cycletime = 12

# create df from DSA results
costs_DSA = res_dsa[["dsa"]][["cost_total"]]/1000
qaly_DSA = res_dsa[["dsa"]][["qaly"]]/1000/cycletime
X_costs_DSA = res_dsa[["dsa"]][[".par_names"]]
input_DSA =res_dsa[["dsa"]][[".par_value"]]



df_DSA = data.frame(X_costs_DSA,input_DSA,costs_DSA,qaly_DSA)

WTP = 50000

Parameter = df_DSA[seq(1,nrow(df_DSA),4),1]
FH.LB_costs = df_DSA[seq(1,nrow(df_DSA),4),3]
FH.UB_costs = df_DSA[seq(2,nrow(df_DSA),4),3]
USPAN.LB_costs = df_DSA[seq(3,nrow(df_DSA),4),3]
USPAN.UB_costs = df_DSA[seq(4,nrow(df_DSA),4),3]
diff.LB_costs = USPAN.LB_costs - FH.LB_costs
diff.UB_costs = USPAN.UB_costs - FH.UB_costs
diff.LU_costs = abs(diff.UB_costs - diff.LB_costs)
FH.LB_qaly = df_DSA[seq(1,nrow(df_DSA),4),4]
FH.UB_qaly = df_DSA[seq(2,nrow(df_DSA),4),4]
USPAN.LB_qaly = df_DSA[seq(3,nrow(df_DSA),4),4]
USPAN.UB_qaly = df_DSA[seq(4,nrow(df_DSA),4),4]
diff.LB_qaly = USPAN.LB_qaly - FH.LB_qaly
diff.UB_qaly = USPAN.UB_qaly - FH.UB_qaly
diff.LU_qaly = abs(diff.UB_qaly - diff.LB_qaly)
HR.LB = diff.LB_qaly*WTP - diff.LB_costs
HR.UB = diff.UB_qaly*WTP - diff.UB_costs
HR.diff_LU = abs(HR.UB - HR.LB)

df_DSA_A = data.frame(Parameter,FH.LB_costs,FH.UB_costs,USPAN.LB_costs,USPAN.UB_costs,diff.LB_costs,diff.UB_costs,diff.LU_costs,FH.LB_qaly,FH.UB_qaly,USPAN.LB_qaly,USPAN.UB_qaly,diff.LB_qaly,diff.UB_qaly,diff.LU_qaly,HR.LB,HR.UB,HR.diff_LU)

df_DSA_A$label.parameters = c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14","p15","p16","p17","p18","p19","p20")


base.value.costs <- dcosts#/cycletime
base.value.qaly <- dQALY
base.value.HR <- HR50

order.parameters.costs <- df_DSA_A %>% arrange(diff.LU_costs) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

order.parameters.qaly <- df_DSA_A %>% arrange(diff.LU_qaly) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

order.parameters.HR <- df_DSA_A %>% arrange(HR.diff_LU) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()


# width of columns in plot (value between 0 and 1)
width <- 0.95

# get data frame in shape for ggplot and geom_rect
df.costs <- df_DSA_A %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value.costs', diff.LB_costs:diff.UB_costs) %>%
  # just reordering columns
  select(Parameter, type, output.value.costs, diff.LU_costs) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters.costs),
         ymin=pmin(output.value.costs, base.value.costs),
         ymax=pmax(output.value.costs, base.value.costs),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# get data frame in shape for ggplot and geom_rect
df.qaly <- df_DSA_A %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value.qaly', diff.LB_qaly:diff.UB_qaly) %>%
  # just reordering columns
  select(Parameter, type, output.value.qaly, diff.LU_qaly) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters.qaly),
         ymin=pmin(output.value.qaly, base.value.qaly),
         ymax=pmax(output.value.qaly, base.value.qaly),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# get data frame in shape for ggplot and geom_rect
df.HR <- df_DSA_A %>% 
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  gather(key='type', value='output.value.HR', HR.LB:HR.UB) %>% 
  # just reordering columns
  select(Parameter, type, output.value.HR, HR.diff_LU) %>%
  # create the columns for geom_rect
  mutate(Parameter=factor(Parameter, levels=order.parameters.HR),
         ymin=pmin(output.value.HR, base.value.HR),
         ymax=pmax(output.value.HR, base.value.HR),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# df.HR <- rbind(df.HR[1:10,],df.HR[21:30,])

##### plot figure
# Costs
ggplot() + 
  geom_rect(data = df.costs, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value.costs) +
  scale_x_continuous(breaks = c(1:length(order.parameters.costs)), 
                     labels = order.parameters.costs) +
  labs(y = "dCosts (\u20ac)", x = "parameter") +
  coord_flip()

# QALY
ggplot() + 
  geom_rect(data = df.qaly, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value.qaly) +
  scale_x_continuous(breaks = c(1:length(order.parameters.qaly)), 
                     labels = order.parameters.qaly) +
  labs(y = "dQALY", x = "parameter") +
  coord_flip()

# HR
ggplot() + 
  geom_rect(data = df.HR, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base.value.HR) +
  scale_x_continuous(breaks = c(1:length(order.parameters.HR)), 
                     labels = order.parameters.HR) +
  labs(y = "HR", x = "parameter") +
  coord_flip()

