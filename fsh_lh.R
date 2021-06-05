library(tidyverse)
library(cowplot)

df <- data.frame(
  conc_l = c(4, 4, 10, 40, 6, 5, 75, 30),
  conc_u = c(25, 30, 90, 250, 23, 30, 150, 200),
  time = factor(x = rep(1:4, 2), levels = 1:4, labels = c("male", "female follicular", "female midcycle", "female postmenopause")),
  hormone = factor(x = rep(1:2, each = 4), levels = 1:2, labels = c("Follicle-Stimulating Hormone", "Luteinizing Hormone"))
)

df %>% glimpse

p <- df %>% 
  ggplot(aes(x = time, color = hormone), position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = conc_l, ymax = conc_u, width = 0.25), size = 0.8  , position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_l, label=conc_l), vjust = 1.4, position = position_dodge(width=0.9)) +
  geom_text(aes(y = conc_u, label=conc_u), vjust = -0.5, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  scale_color_manual(values = c("#e9dc3d", "#3ab141")) +
  ylab("Serum Concentration (mIU/mL)") +
  xlab("") +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 8)) 

setwd("/Users/USC/Desktop")
png("Reproductive_Hormones.png", res = 300, width = 7, height = 5, units = "in")
print(p)
dev.off()
