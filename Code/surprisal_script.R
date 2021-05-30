require(concurve)
require(ggplot2)
require(gridExtra)

## Table 1: Some P-values and Their Corresponding S-values

pvalue <- c(
  0.99, 0.90, 0.50, 0.25, 0.125, 0.10,
  0.05, 0.025, 0.01, 0.005, 0.0001,
  0.0000003, 0.000000001
)

## Calculate S-values

svalue <- c(
  round((-log2(0.99)), 2),
  round((-log2(0.90)), 2),
  round((-log2(0.50)), 2),
  round((-log2(0.25)), 2),
  round((-log2(0.125)), 2),
  round((-log2(0.10)), 2),
  round((-log2(0.05)), 2), ## statistical significance
  round((-log2(0.025)), 2),
  round((-log2(0.01)), 2),
  round((-log2(0.005)), 2),
  round((-log2(0.0001)), 2),
  round((-log2(0.0000003)), 2), ## five sigma
  round((-log2(0.000000001)), 2) ## six sigma
)

table1 <- data.frame(pvalue, svalue)

print(table1)

library(gridExtra)
grid.table(table1) ## for image of table


########################################################################################################

## Taken from the Brown et al data DOI: 10.1001/jama.2017.3415

## Calculate the standard errors from the point estimate and the confidence (compatibility) limits

se <- log(2.59 / 0.997) / 3.92

logUL <- log(2.59)
logLL <- log(0.997)

logpoint <- log(1.61)

logpoint + (1.96 * se)
logpoint - (1.96 * se)

########################################################################################################


## Table 2: P-values, S-values, and Relative Likelihood Ratios
## (LR) for Targeted Test Hypotheses About Hazard Ratios (HR) for Brown et al.

## Compute P-values, S-values, and likelihood ratios

testhypothesis <- c(
  "Halving of hazard", "No effect (null)", "Point estimate",
  "Doubling of hazard", "Tripling of hazard", "Quintupling of hazard"
)
hazardratios <- c(0.5, 1, 1.61, 2, 3, 5)
pvals <- c(1.6e-06, 0.05, 1.00, 0.37, 0.01, 3.2e-06)
svals <- -log2(pvals)
lr <- c(
  exp((((log(0.5 / 1.61)) / se)^2) / (2)),
  exp((((log(1 / 1.61)) / se)^2) / (2)),
  exp((((log(1.61 / 1.61)) / se)^2) / (2)),
  exp((((log(2 / 1.61)) / se)^2) / (2)),
  exp((((log(3 / 1.61)) / se)^2) / (2)),
  exp((((log(5 / 1.61)) / se)^2) / (2))
)
lr <- formatC(lr, format = "e", digits = 2) ## calculating large numbers from small, likely to be imprecise

table2 <- data.frame(testhypothesis, hazardratios, pvals, svals, lr)

print(table2)
grid.table(table2) ## for image of table

########################################################################################################

## Plot the point estimate and 95% compatibility interval using ggplot2

label <- ("Brown et al.")
point <- (1.61)
lower <- (0.997)
upper <- (2.59)

df <- data.frame(label, point, lower, upper)

## Install and load ggplot2 R package if not already done so

library(ggplot2)

ggplot(data = df, mapping = aes(x = label, y = point, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "black", size = 1.1) +
  geom_hline(yintercept = 1, lty = 1, color = "dark red") +
  coord_flip() +
  scale_y_log10(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Association Between Serotonergic Antidepressant Exposure \nDuring Pregnancy and Child Autism Spectrum Disorder",
    subtitle = "95% Compatibility Interval",
    caption = "Figure 2",
    x = "Study",
    y = "Hazard Ratio"
  )

########################################################################################################

## Compute and plot the consonance and surprisal functions using `concurve` R package.

## Install and load concurve R package if not already done so

library(concurve)

## Enter point estimates and compatibility limits and produce all possible intervals + P-values + S-values

df <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio", steps = 10000) ## may take some time

## Stored in data frame "df"

## Figure 3: P-value (Compatibility) Function


## Plot the P-value (Compatibility) function of the Brown et al data with ggplot2 graphics

ggcurve(
  data = df[[1]], type = "c",  measure = "ratio", nullvalue = TRUE,
  title = "P-Value (Compatibility) as a Function of the Hazard Ratio",
  subtitle = "Association Between Serotonergic Antidepressant Exposure \nDuring Pregnancy and Child Autism Spectrum Disorder",
  xaxis = "Hazard Ratio", yaxis1 = "Compatibility (Confidence) Level %"
)

## Figure 4: S-value (Suprisal) Function

## Plot the S-value (Surprisal) function of the Brown et al data with ggplot2 graphics

ggcurve(
  data = df[[1]], type = "s",  measure = "ratio", nullvalue = TRUE,
  title = "S-Value (Surprisal) as a Function of the Hazard Ratio",
  subtitle = "Association Between Serotonergic Antidepressant Exposure \nDuring Pregnancy and Child Autism Spectrum Disorder",
  xaxis = "Hazard Ratio", yaxis1 = "S-value (bits of information)"
)

########################################################################################################

## Calculate and Plot Likelihood (Support) Intervals

hrvalues <- seq(from = 0.65, to = 3.98, by = 0.01)

se <- log(2.59 / 0.997) / 3.92

zscore <- sapply(hrvalues,
            function(i)(log(i / 1.61) / se))


# Relative Likelihood MLR

support <- exp((-zscore^2)/2)

likfunction <- data.frame(hrvalues, zscore, support)

ggplot(data = likfunction, mapping = aes(x = hrvalues, y = support)) +
  geom_line() +
  geom_ribbon(aes(x = hrvalues, ymin = min(support), ymax = support), fill = "#239a98", alpha = 0.30) +
  labs(
    x = "Hazard Ratio (HR)",
    y = "Relative Likelihood 1/MLR"
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  ) +
  scale_x_log10(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size = 15)) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8))


#######################################################################################################

# upward-concave parabola Z^2/2 = -ln(MLR) which is the likelihood analog of the S-value function

support <- (zscore^2)/2

likfunction <- data.frame(hrvalues, zscore, support)

ggplot(data = likfunction, mapping = aes(x = hrvalues, y = support)) +
  geom_line() +
  geom_ribbon(aes(x = hrvalues, ymin = support, ymax = max(support)), fill = "#239a98", alpha = 0.30) +
  labs(
    x = "Hazard Ratio (HR)",
    y = "ln(MLR) "
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  ) +
  scale_x_log10(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size = 15)) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8))


######################################################################################################

# Deviance Statistics (-2ln(MLR))

support <- (zscore^2)

likfunction <- data.frame(hrvalues, zscore, support)

ggplot(data = likfunction, mapping = aes(x = hrvalues, y = support)) +
  geom_line() +
  geom_ribbon(aes(x = hrvalues, ymin = support, ymax = max(support)), fill = "#239a98", alpha = 0.30) +
  labs(
    x = "Hazard Ratio (HR)",
    y = " Deviance Statistic 2ln(MLR) "
  ) +
  theme_light() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  ) +
  scale_x_log10(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(text = element_text(size = 15)) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8))

