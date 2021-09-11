library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/format_sex.R")
source("./programs/fun_plot.R")


# Cholesterol
df_cholesterol <- data.frame(
  conc_l = c(0, 240, 40, 0),
  conc_u = c(200, Inf, 60, 160),
  cholesterol = factor(1:4, 1:4, labels = c("Total, Normal", "Total, High", "HDL", "LDL"))
)
p_cholesterol <- df_cholesterol %>% 
  plot_errorbar(xvar = cholesterol) +
  ylim(c(0, 300)) +
  labs(x = "", y = "Serum Concentration (mg/dL)") +
  ggtitle("Serum Cholesterol") +
  guides(color=guide_legend(nrow=2, byrow=FALSE))
save_png("normal_labs/SerumCholesterol.png", p_cholesterol)


# Triglycerides
df_triglycerides <- data.frame(
  conc_l = c(0, 151, 200),
  conc_u = c(150, 199, Inf),
  triglycerides = factor(1:3, 1:3, labels = c("Normal", "Borderline", "Elevated"))
)
p_triglycerides <- df_triglycerides %>% 
  plot_errorbar(xvar = triglycerides) +
  ylim(c(0, 300)) +
  labs(x = "", y = "Serum Concentration (mg/dL)") +
  ggtitle("Serum Triglycerides") +
  guides(color=guide_legend(nrow=2, byrow=FALSE))
save_png("normal_labs/SerumTriglycerides.png", p_triglycerides)


# Creatinine Kinase
df_creatinine_kinase <- data.frame(
  conc_l = c(10, 25),
  conc_u = c(70, 90),
  sex = form_sex
)
p_creatinine_kinase <- df_creatinine_kinase %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Serum Creatinine Kinase (U/L)") +
  ylim(c(0,100)) +
  xlab("") +
  ggtitle("Serum Creatinine Kinase") 
save_png("normal_labs/SerumCreatinineKinase.png", p_creatinine_kinase)


# Electrolytes
df_electrolytes <- data.frame(
  conc_l = c(136, 3.5, 95, 22, 1.5),
  conc_u = c(146, 5.0, 105, 28, 2.0),
  electrolyte = factor(1:5, 1:5, labels = c("Sodium (Na+)", "Potassium (K+)", "Chloride (Cl–)", "Bicarbonate (HCO3–)", "Magnesium (Mg2+)"))
)
p_electrolytes <- df_electrolytes %>% 
  plot_errorbar(xvar = electrolyte, color = electrolyte) +
  scale_color_manual(values = c("#f8f800", "#008f00", "#803000", "#d40704", "#00005c")) +
  ylim(c(0, 150)) +
  labs(x = "", y = "Serum Concentration (mEq/L)") +
  ggtitle("Serum Electrolytes") +
  guides(color=guide_legend(nrow=2, byrow=FALSE))
save_png("normal_labs/SerumElectrolytes.png", p_electrolytes)


# Serum Cortisol
df_cortisol <- data.frame(
  conc_l = c(5, 3, 0),
  conc_u = c(23, 15, 11.5),
  time = c(0800, 1600, 2000)
)
p_cortisol <- df_cortisol %>% 
  plot_errorbar(xvar = time) +
  ylim(c(0, 25)) +
  scale_x_continuous(limits = c(0000, 2400), breaks = c(0000, 0800, 1600, 2000, 2400)) +
  labs(x = "24-Hour Day", y = "Serum Cortisol (µg/dL)", caption = "Cortisol at 2000 ≤ 50% of 0800 Level") +
  ggtitle("Serum Cortisol")
save_png("normal_labs/SerumCortisol.png", p_cortisol)


# Serum Bilirubin
df_bilirubin <- data.frame(
  conc_l = c(0.1, 0),
  conc_u = c(1.0, 0.3),
  bilirubin = factor(x = 1:2, levels = 1:2, labels = c("Total", "Direct (Conjugated)"))
)
p_bilirubin <- df_bilirubin %>% 
  plot_errorbar(xvar = bilirubin) +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (mg/dL)") +
  ylim(c(0,1)) +
  xlab("Bilirubin") +
  ggtitle("Serum Bilirubin") 
save_png("normal_labs/SerumBilirubin.png", p_bilirubin)


# Serum Iron
df_iron <- data.frame(
  conc_l = c(50, 65),
  conc_u = c(170, 175),
  sex = form_sex
)
p_iron <- df_iron %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Serum Iron (µg/dL)") +
  ylim(c(0,180)) +
  xlab("") +
  ggtitle("Serum Iron") 
save_png("normal_labs/SerumIron.png", p_iron)


# serum ferritin
df_ferritin <- data.frame(
  conc_l = c(10, 20),
  conc_u = c(120, 250),
  sex = form_sex
)
p_ferritin <- df_ferritin %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (ng/mL)") +
  ylim(c(0,250)) +
  xlab("") +
  ggtitle("Serum Ferritin")
save_png("normal_labs/SerumFerritin.png", p_ferritin)


# reproductive hormones
df_hor_repro <- data.frame(
  conc_l = c(4, 4, 10, 40, 6, 5, 75, 30),
  conc_u = c(25, 30, 90, 250, 23, 30, 150, 200),
  time = factor(x = rep(1:4, 2), levels = 1:4, labels = c("Male", "Female Follicular", "Female Midcycle", "Female Postmenopause")),
  hormone = factor(x = rep(1:2, each = 4), levels = 1:2, labels = c("Follicle-Stimulating Hormone", "Luteinizing Hormone"))
)
p_hor_repro <- df_hor_repro %>% 
  plot_errorbar(xvar = time, color = hormone) +
  scale_color_manual(values = c("#e9dc3d", "#3ab141")) +
  ylab("Serum Concentration (mIU/mL)") +
  xlab("") +
  ggtitle("Serum Gonadotropic Hormones")
save_png("normal_labs/SerumHormoneFSHLH.png", p_hor_repro)


# prolactin hormone
df_prolactin <- data.frame(
  conc_l = c(0, 0),
  conc_u = c(25, 17),
  sex = form_sex
)
p_prolactin <- df_prolactin %>% 
  plot_errorbar(xvar = sex, color = sex) +
  scale_color_manual(values = color_sex) +
  ylab("Serum Concentration (ng/mL)") +
  ylim(c(0,25)) +
  xlab("") +
  ggtitle("Serum Prolactin")
save_png("normal_labs/SerumHormoneProlactin.png", p_prolactin)


# Serum Proteins
df_protein <- data.frame(
  conc_l = c(6, 3.5, 2.3),
  conc_u = c(7.8, 5.5, 3.5),
  protein = factor(x = 1:3, levels = 1:3, labels = c("Total Protein", "Albumin", "Globulin"))
)
p_protein <- df_protein %>% 
  plot_errorbar(xvar = protein, color = protein) +
  ylab("Serum Protein (g/dL)") +
  scale_color_manual(values = c("#e32c0a", "#1ec091", "#8a2ecd")) +
  ylim(c(0, 8)) +
  xlab("") +
  ggtitle("Serum Proteins") 
save_png("normal_labs/SerumProtein.png", p_protein)


# Serum Thyroid Hormones
df_thyroid <- data.frame(
  conc_l = c(5, 0.9, 0.1),
  conc_u = c(12, 1.7, 0.2),
  hormone = factor(x = 1:3, levels = 1:3, labels = c("Thyroxine (T4)", "Free Thyroxine (T4)", "Triiodothyronine (T3)"))
)
p_thyroid <- df_thyroid %>% 
  plot_errorbar(xvar = hormone) +
  ylab("Serum Concentration (µg/dL)") +
  ylim(c(0, 12)) +
  xlab("") +
  ggtitle("Serum Thyroid Hormone") 
save_png("normal_labs/SerumHormoneThyroid.png", p_thyroid)


# Serum Immunoglobulins
df_immunoglobulin <- data.frame(
  conc_l = c(76, 0, 650, 50),
  conc_u = c(390, 0.0912, 15000, 300),
  igb = factor(x = 1:4, levels = 1:4, labels = c("IgA", "IgE", "IgG", "IgM"))
)
p_immunoglobulin <- df_immunoglobulin %>% 
  plot_errorbar(xvar = igb, color = igb) +
  ylim(c(0, 15000)) +
  labs(x = "Immunoglobulin", y = "Serum Concentration (mg/dL)", caption = "IgE 380 IU/mL = 0.0912 mg/dL") +
  ggtitle("Serum Immunoglobulins") 
save_png("normal_labs/SerumImmunoglobulin.png", p_immunoglobulin)


# Serum ABG
df_abg <- data.frame(
  conc_l = c(33, 75),
  conc_u = c(45, 105),
  gas = factor(x = 1:2, levels = 1:2, labels = c("Carbon Dioxide (CO2)", "Oxygen (O2)"))
)
p_abg <- df_abg %>% 
  plot_errorbar(xvar = gas) +
  ylim(c(0, 110)) +
  labs(x = "", y = "Pressure (mmHg)", caption = "7.35 < pH < 7.45") +
  ggtitle("Serum Arterial Blood Gas (ABG)") 
save_png("normal_labs/SerumArterialBloodGas.png", p_abg)
