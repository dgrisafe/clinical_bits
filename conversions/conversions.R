library(tidyverse)
source("./programs/fun_save_png.R")
source("./programs/fun_plot.R")

plot_conversion <- function(df, xvar, yvar, color = NULL, label_suffix = NULL){
  dplyr_xvar <- enquo(xvar)
  dplyr_yvar <- enquo(yvar)
  ggplot(data = df, aes(x = !!dplyr_xvar, y = !!dplyr_yvar)) +
    geom_col(size = 0.8, position = position_dodge(width=0.9)) +
    scale_y_continuous(trans = 'log10') +
    geom_text(aes(y = !!dplyr_yvar, label = paste0(!!dplyr_yvar, label_suffix)), vjust = -0.5, position = position_dodge(width=0.9)) +
    theme_cowplot() +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(size = 10))
}


# Length
df_length_us_si <- data.frame(
  length_m = c(0.02540, 0.3048, 0.9144, 1609),
  label_us = factor(1:4, 1:4, labels = c("Inch (in)", "Foot (ft)", "Yard (yd)", "Mile (mi)"))
)
p_length_us_si <- df_length_us_si %>% 
  plot_conversion(xvar = label_us, yvar = length_m, label_suffix = " m") +
  labs(x = "Imperial", y = "SI (log meter)") +
  ggtitle("Length US to SI")
save_png("conversions/ConversionLengthUSSI.png", p_length_us_si)


# Mass
df_mass_us_si <- data.frame(
  mass_g = c(28.35, 453.6, 907200),
  label_us = factor(1:3, 1:3, labels = c("Ounce (oz)", "Pound (lb)", "Short Ton (ton)"))
)
p_mass_us_si <- df_mass_us_si %>% 
  plot_conversion(xvar = label_us, yvar = mass_g, label_suffix = " g") +
  labs(x = "Imperial", y = "SI (log grams)") +
  ggtitle("Mass US to SI")
save_png("conversions/ConversionMassUSSI.png", p_mass_us_si)


# Temperature
df_temperature <- data.frame(farenheit = c(0, 32, 98.6, 100:104, 212, seq(250, 500, 25))) %>% 
  dplyr::mutate(celsius =  5/9*(farenheit - 32))
breaks <- df_temperature %>% filter(!(farenheit %in% c(100:103)))
p_temperature <- ggplot(data = df_temperature, aes(x = farenheit, y = celsius)) +
  geom_point(size = 0.8, position = position_dodge(width=0.9)) +
  theme_cowplot() +
  theme(
    legend.position = "top", 
    legend.title = element_blank(), 
    axis.text.x = element_text(size = 5, angle = -90, vjust = 0.5),
    axis.text.y = element_text(size = 5)
  ) +
  scale_x_continuous(breaks = breaks$farenheit) +
  scale_y_continuous(breaks = round(breaks$celsius, digits = 1)) +
  labs(x = "Farenheit", y = "Celsius") + 
  ggtitle("Temperature Farenheit to Celsius") 
save_png("conversions/ConversionTemperatureUSSI.png", p_temperature)

