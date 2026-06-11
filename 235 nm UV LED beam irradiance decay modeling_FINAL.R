# UV LED beam irradiance decay modeling in R
# Data: Irradiance (µW/cm^2) measured at multiple displacement distances (mm)
# Goal: Fit and compare two candidate models:
#       (1) exponential decay
#       (2) inverse-square 
#       assess goodness-of-fit (AIC, residuals), evaluate physical plausibility,
#       and report parameter uncertainty (95% CIs).

library(ggplot2)

# Data

df <- data.frame(
  Distance_mm = c(29, 31, 33, 35, 37, 41, 45, 49, 53, 58, 63, 73, 83),
  Irradiance = c(73, 66.4, 60.2, 54.5, 50.3,
                 42, 35.3, 29.9, 25.6,
                 21.4, 18, 13, 9.5)
)

str(df)
head(df)

# Fit models

# Exponential model:
# E(x) = a exp(-kx) + c
fit_exp <- nls(
  Irradiance ~ a * exp(-k * Distance_mm) + c,
  data = df,
  start = list(a = 60, k = 0.02, c = 8)
)

# Inverse-square model:
# E(x) = a / (x + b)^2 + c
fit_inv <- nls(
  Irradiance ~ a / (Distance_mm + b)^2 + c,
  data = df,
  start = list(a = 100000, b = 0, c = 5)
)

# Plot 

xg <- seq(min(df$Distance_mm), 150, length.out = 400)

pred_df <- rbind(
  data.frame(
    Distance_mm = xg,
    Irradiance = predict(fit_exp, newdata = data.frame(Distance_mm = xg)),
    Series = "Exponential"
  ),
  data.frame(
    Distance_mm = xg,
    Irradiance = predict(fit_inv, newdata = data.frame(Distance_mm = xg)),
    Series = "Inverse-square"
  )
)

p4 <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.6,
    colour = "black"
  ) +
  
  geom_point(
    data = df,
    aes(x = Distance_mm, y = Irradiance, colour = "Measured data"),
    size = 3,
    shape = 16
  ) +
  
  geom_line(
    data = pred_df,
    aes(x = Distance_mm, y = Irradiance, colour = Series, linetype = Series),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "Measured data" = "black",
      "Exponential" = "red",
      "Inverse-square" = "blue"
    ),
    breaks = c("Measured data", "Exponential", "Inverse-square")
  ) +
  
  scale_linetype_manual(
    values = c(
      "Exponential" = "dashed",
      "Inverse-square" = "dotdash"
    ),
    breaks = c("Exponential", "Inverse-square")
  ) +
  
  scale_x_continuous(
    limits = c(min(df$Distance_mm), 150),
    breaks = seq(30, 150, 20)
  ) +
  
  scale_y_continuous(
    limits = c(-5, 80),
    breaks = seq(0, 80, 10)
  ) +
  
  labs(
    x = "Displacement (mm)",
    y = expression(bold("Measured Irradiance ("*mu*"W/cm"^2*")"))
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key = element_blank(),
    legend.position = c(0.74, 0.82)
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = c(0, 2, 4),
        shape = c(16, NA, NA),
        linewidth = c(0, 1, 1)
      )
    ),
    linetype = "none"
  )

print(p4)

# Statistical outputs

# Model summaries
summary(fit_exp)
summary(fit_inv)

# 95% confidence intervals for model parameters
confint(fit_exp)
confint(fit_inv)

# Residuals
resid(fit_exp)
resid(fit_inv)

# AIC model comparison
AIC(fit_exp, fit_inv)

#Fitted equations printed clearly
coef_exp <- coef(fit_exp)
coef_inv <- coef(fit_inv)

cat("\nExponential model:\n")
cat(
  "E(x) = ",
  round(coef_exp["a"], 4),
  " * exp(-",
  round(coef_exp["k"], 5),
  " * x) + ",
  round(coef_exp["c"], 4),
  "\n",
  sep = ""
)

cat("\nInverse-square model:\n")
cat(
  "E(x) = ",
  round(coef_inv["a"], 4),
  " / (x + ",
  round(coef_inv["b"], 4),
  ")^2 + ",
  round(coef_inv["c"], 4),
  "\n",
  sep = ""
)