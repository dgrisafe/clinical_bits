library(tidyverse)
source("fun_save_png.R")
source("format_sex.R")
source("fun_plot.R")

color_csf_serum <- list(CSF = "#55aaaf", Serum = "#ab4a4c")


# CSF Chloride
df_chloride <- data.frame(
  conc_l = c(95, 118),
  conc_u = c(105, 132),
  fluid = factor(1:2, 1:2, labels = c("Serum", "CSF"))
)
p_chloride <- df_chloride %>% 
  plot_errorbar(xvar = fluid, color = fluid) +
  ylim(c(90, 132)) +
  scale_color_manual(values = color_csf_serum) +
  labs(x = "Fluid", y = "Concentration (mEq/L)") +
  ggtitle("Chloride (Cl-)")
save_png("CSFChloride.png", p_chloride)


# CSF Protein
df_protein <- data.frame(
  conc_l = c(6.0, 0),
  conc_u = c(7.8, 0.040),
  fluid = factor(1:2, 1:2, labels = c("Serum", "CSF"))
)
p_protein <- df_protein %>% 
  plot_errorbar(xvar = fluid, color = fluid) +
  ylim(c(0, 8)) +
  scale_color_manual(values = color_csf_serum) +
  labs(x = "Fluid", y = "Concentration (g/dL)", caption = "CSF Gamma Globulin 3% to 12% of Total Protein") +
  ggtitle("Total Protein")
save_png("CSFProtein.png", p_protein)


# CSF Glucose
df_glucose <- data.frame(
  conc_l = c(70, 70, 40),
  conc_u = c(110, 140, 70),
  fluid = factor(1:3, 1:3, labels = c("Serum Fasting Glucose", "Serum Random, Non-Fasting Glucose", "CSF")),
  fluid_col = factor(c(1,1,2), 1:2, labels = c("Serum", "CSF"))
)
p_glucose <- df_glucose %>% 
  plot_errorbar(xvar = fluid, color = fluid_col) +
  ylim(c(40, 140)) +
  scale_color_manual(values = color_csf_serum) +
  labs(x = "Fluid", y = "Concentration (mg/dL)") +
  ggtitle("Glucose")
save_png("CSFGlucose.png", p_glucose)
