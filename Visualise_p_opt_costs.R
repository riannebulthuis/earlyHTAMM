library(ggplot2)
library(reshape2)



output_20 <- melt(output_HR20)
output_50 <- melt(output_HR50)
output_80 <- melt(output_HR80)

ggplot(output_80, aes(Var1,Var2)) + 
  geom_raster(aes(fill = value)) + 
  scale_fill_gradient2(low = "red3",high ="green3", name = "HR") +
  labs(x = "Probability of optimal placement with USPAN",y = "Surgery costs with use USPAN (\u20AC)") +
  ggtitle("Time horizon 5 years, Pediatrics, WTP = \u20AC 80,000")
