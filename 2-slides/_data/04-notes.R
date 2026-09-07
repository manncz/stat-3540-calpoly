
# Example 4.1 -- Randomization -------------------------------------------------

library(edibble)
library(tidyverse)

# randomization
des <- design(name = "Example Full Facotrial CRD") |> 
  set_units(exp_unit = 48) |>                  
  set_trts(factorA = c("A1", "A2"),
           factorB = c("B1", "B2", "B3", "B4")
           ) |> 
  allot_trts(factorA + factorB ~ exp_unit) |>                
  assign_trts("random")  

factorial_table <- serve_table(des)
factorial_table$response <- NA
head(factorial_table)

# ensure r replicates per factor combination (treatment)
factorial_table |> 
  count(factorA, factorB)

# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(emmeans)
library(multcompView)
library(multcomp)

# set as sum to zero
options(contrasts = c("contr.sum", "contr.poly"))

# Example 4.2: Bakery ----------------------------------------------------------

bakery_data <- read_csv("02-notes/data/04_bakery_data.csv")
head(bakery_data)


# visual for model effects######################################################

# Compute means at each level
overall_mean <- bakery_data |> 
  summarise(mean_sales = mean(sales))

width_means <- bakery_data |>
  group_by(width) |>
  summarise(mean_sales = mean(sales))

height_means <- bakery_data |>
  group_by(height) |>
  summarise(mean_sales = mean(sales))

placement_means <- bakery_data |>
  group_by(height, width, placement) |>
  summarise(mean_sales = mean(sales))

# Make sure placement is a factor in the right order
bakery_data <- bakery_data |>
  mutate(placement = factor(placement))

placement_means <- placement_means |>
  mutate(x = as.numeric(placement))

# Map each width/height to the range of x positions for its levels
width_x_ranges <- placement_means |>
  group_by(width) |>
  summarise(xmin = min(x),
            xmax = max(x))

height_x_ranges <- placement_means |>
  group_by(height) |>
  summarise(xmin = min(x),
            xmax = max(x))

# Merge ranges with the mean data
width_means <- left_join(width_means, width_x_ranges, by = "width")
height_means <- left_join(height_means, height_x_ranges, by = "height")

# ---- PLOT ----
ggplot(bakery_data, aes(x = placement, y = sales)) +
  geom_point(size = 3, aes(shape = width)) +
  
  # 1️⃣ Overall mean (dotted black line)
  geom_segment(
    data = overall_mean,
    aes(x = 0.5, xend = length(levels(bakery_data$placement)) + 0.5,
        y = mean_sales, yend = mean_sales),
    color = "black", linetype = "solid", linewidth = 1
  ) +
  
  # 2️⃣ Width means (blue)
  # geom_segment(
  #   data = width_means,
  #   aes(x = xmin - 0.4, xend = xmax + 0.4,
  #       y = mean_sales, yend = mean_sales,
  #       linetype = width),
  #   color = "steelblue", linewidth = 1.2
  # ) +
  
  # 3️⃣ Height means (orange dashed)
  geom_segment(
    data = height_means,
    aes(x = xmin - 0.4, xend = xmax + 0.4,
        y = mean_sales, yend = mean_sales),
    color = "darkorange", linetype = "dashed", linewidth = 1.2
  ) +
  
  # 4️⃣ Placement (width × height) means (solid black)
  # geom_segment(
  #   data = placement_means,
  #   aes(x = x - 0.4, xend = x + 0.4,
  #       y = mean_sales, yend = mean_sales),
  #   color = "black", linewidth = 1.2,
  #   linetype = "dotted"
  # ) +
  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Placement (height × width)",
    y = "Sales",
    title = "Model Effects Decomposition",
    subtitle = "Main Effects of Height (Bottom, Middle, Top)"
  )








# ---- PLOT ----
bakery_data |> 
  ggplot(aes(x = placement, y = sales)) +
  geom_point(size = 3, aes(shape = width)) +
  
  # 1️⃣ Overall mean (dotted black line)
  geom_hline(
    data = overall_mean,
    aes(yintercept = mean_sales),
    color = "black", linetype = "solid", linewidth = 1
  ) +
  
  # 2️⃣ Width means (blue)
  geom_hline(
    data = width_means,
    aes(yintercept = mean_sales),
    color = "steelblue", linewidth = 1.2,
    linetype = "dashed"
  ) +
  # facet_wrap(~width, scales = "free_x") +
  
  # 3️⃣ Height means (orange dashed)
  geom_segment(
    data = height_means,
    aes(x = xmin - 0.4, xend = xmax + 0.4,
        y = mean_sales, yend = mean_sales),
    color = "darkorange", linetype = "dashed", linewidth = 1.2
  ) +
  
  # 4️⃣ Placement (width × height) means (solid black)
  geom_segment(
    data = placement_means,
    aes(x = x - 0.4, xend = x + 0.4,
        y = mean_sales, yend = mean_sales),
    color = "black", linewidth = 1.2,
    linetype = "dotted"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = "Sales",
    title = "Model Effects Decomposition"
  )



# ---- PLOT ----
bakery_data |> 
  ggplot(aes(x = placement, y = sales)) +
  geom_point(size = 3, aes(shape = width)) +
  
  # 1️⃣ Overall mean (dotted black line)
  geom_hline(
    data = overall_mean,
    aes(yintercept = mean_sales),
    color = "black", linetype = "solid", linewidth = 1
  ) +
  
  # 2️⃣ Width means (blue)
  # geom_hline(
  #   data = width_means,
  #   aes(yintercept = mean_sales),
  #   color = "steelblue", linewidth = 1.2,
  #   linetype = "dashed"
  # ) +
  # facet_wrap(~width, scales = "free_x") +
  
  # 3️⃣ Height means (orange dashed)
  # geom_segment(
  #   data = height_means,
  #   aes(x = xmin - 0.4, xend = xmax + 0.4,
  #       y = mean_sales, yend = mean_sales),
  #   color = "darkorange", linetype = "dashed", linewidth = 1.2
  # ) +
  
  # 4️⃣ Placement (width × height) means (solid black)
  geom_segment(
    data = placement_means,
    aes(x = x - 0.4, xend = x + 0.4,
        y = mean_sales, yend = mean_sales),
    color = "black", linewidth = 1.2,
    linetype = "dotted"
  ) +
  
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Placement (height × width)",
    y = "Sales",
    title = "Model Effects Decomposition",
    subtitle = "Interaction Effects (Height x Width)"
  )

#################################################################

# fit model
bakery_mod <- lm(sales ~ height + width + height:width,
                 data = bakery_data)

anova(bakery_mod) # look at anova/effects tests
summary(bakery_mod)  # obtain estimated parameters
plot(bakery_mod) # evaluate model fit

# after investigating ANOVA, no interaction between height and width
# dig into the main effect of height

emmip(bakery_mod, ~ height, CIs = TRUE, adjust = "tukey") # plot lsmeans
height_lsmeans <- emmeans(bakery_mod, specs = ~ height) # lsmeans table

# tukey pairwise comparisons of main effect for height
cld(height_lsmeans, Letters = LETTERS, decreasing = T, adjust = "tukey")
pairs(height_lsmeans, adjust = "tukey", infer = c(T,T))

# Example 4.3: Asphalt ---------------------------------------------------------

asphalt_data <- read_csv("02-notes/data/04_asphalt_data.csv")
head(asphalt_data)

# fit model
asphalt_mod <- lm(strength ~ aggregate + compaction + aggregate:compaction,
                  data = asphalt_data) # same as strength ~ aggregate*compaction

anova(asphalt_mod) # look at anova/effects tests
summary(asphalt_mod) # obtain estimated parameters
plot(asphalt_mod) # evaluate model fit

# after investigating ANOVA, significant interaction between compaction and aggregate
# dig into the interaction effect

# plot interaction (slice in both directions; same estimates, just different views)
emmip(asphalt_mod, aggregate ~ compaction, CIs = TRUE, adjust = "tukey")
emmip(asphalt_mod, compaction ~ aggregate, CIs = TRUE, adjust = "tukey")

# extract lsmeans for factor combinations
aggregatexcompaction_lsmeans <- emmeans(asphalt_mod, specs = ~ aggregate*compaction)

# slice tests
joint_tests(aggregatexcompaction_lsmeans, by = "aggregate")
joint_tests(aggregatexcompaction_lsmeans, by = "compaction")

# pairwise comparisons within aggregate (slice contrasts)
# could slice other way with ~ aggregate | compaction
emmeans(asphalt_mod, specs = ~ compaction | aggregate) |> 
  pairs(adjust = "tukey", infer = c(T,T))

# pairwise comparisons across all treatment combinations
emmeans(asphalt_mod, specs = ~ compaction*aggregate) |> 
  pairs(adjust = "tukey", infer = c(T,T))

# Example 4.4: Stress ----------------------------------------------------------

stress_data <- read_csv("02-notes/data/04_stress_data.csv")
head(stress_data)

# fit model
# same as Tolerance ~ Warmup + Preworkout + Music + Warmup:Preworkout + Warmup:Music + Preworkout:Music + Warmup:Preworkout:Music
stress_mod <- lm(Tolerance ~ Warmup*Preworkout*Music, data = stress_data)
anova(stress_mod) # look at anova/effects tests
summary(stress_mod) # obtain estimated parameters
plot(stress_mod) # evaluate model fit

# after investigating ANOVA, no significant 3-way interaction.

# significant interaction effect between preworkout and music
# dig into the interaction effect
# music*preworkout
emmip(stress_mod, Music ~ Preworkout, CIs = TRUE, adjust = "tukey") # plot

emmeans(stress_mod, specs = ~ Preworkout*Music) |> # letter display
  cld(Letters = LETTERS, decreasing = T, adjust = "tukey")

emmeans(stress_mod, specs = ~ Preworkout*Music) |> # all pairwise comparisons, note you could slice Preworkout | Muisc
  pairs(adjust = "tukey", infer = c(T,T))

# signficant main effect of warmup
# dig into the main effect
# warmup
emmip(stress_mod, ~ Warmup, CIs = TRUE) # plot

emmeans(stress_mod, specs = ~ Warmup) |> # letter display
  cld(Letters = LETTERS, decreasing = T, adjust = "tukey")

emmeans(stress_mod, specs = ~ Warmup) |>  # pairwise comparisons
  pairs(adjust = "tukey", infer = c(T,T))

# had there been a significant 3-way interaction...
emmip(stress_mod, Preworkout ~ Music | Warmup, CIs = TRUE, adjust = "tukey")


