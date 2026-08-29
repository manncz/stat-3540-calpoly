## DR ROBINSON CODE ---- NEED TO REVIEW


# Load the tidyverse package for data manipulation and visualization
library(tidyverse)

### shoe Data Loading and Preparation ----------------------------------------------
# Read in the shoe coagulation rate data from a CSV file
shoe_data <- read_csv("02-notes/data/02-shoes.csv") |> 
  mutate(across(Runner:Shoe, ~ as.factor(.x))) # Convert shoe column to a factor

shoe_data # Print the data to the console to confirm it's loaded correctly

### Graphs for Visual Exploration ----------------------------------------------------
# Graph without considering Shoe (jitter plot of coagulation rates)
shoe_data |> 
  ggplot(aes(x = "", # No x-axis grouping
             y = `Lap Time (seconds)`)
  ) +
  geom_jitter(height = 0, width = 0.1, size = 3) + # Add jitter for visualization
  scale_y_continuous(breaks = seq(50,70,2)) + # Customize y-axis scale
  labs(y = "Lap Time (Seconds)", # Add axis labels
       x = "") +
  theme(aspect.ratio = 3) # Adjust the aspect ratio for visualization

# Graph of coagulation rates grouped by Shoe
shoe_data |> 
  ggplot(aes(x = Shoe,
             y = `Lap Time (seconds)`,
             shape = Shoe)
  ) +
  geom_jitter(height = 0, width = 0.1, size = 3, show.legend = F) + # Add jitter to visualize data points
  scale_y_continuous(breaks = seq(50,70,2)) + # Customize y-axis
  labs(y = "Lap Time (Seconds)", 
       x = "Shoe") + # Add axis labels
  theme(aspect.ratio = 2.5) # Adjust aspect ratio

# Graph with Shoe and treatment means
shoe_data |> 
  ggplot(aes(x = Shoe, # Group by Shoe
             y = `Lap Time (seconds)`)
  ) +
  # geom_hline(data = shoe_data |> summarize(mean = mean(`Lap Time (seconds)`)), # Add overall mean line
  #            aes(yintercept = mean),
  #            linewidth = 0.8,
  #            linetype = "dashed"
  # ) +
  geom_hline(aes(yintercept = 61),
             linewidth = 0.8,
             linetype = "dashed"
  ) +
  geom_crossbar(data = shoe_data |> group_by(Shoe) |> summarize(mean = mean(`Lap Time (seconds)`)), # Add treatment means
                aes(x = Shoe, 
                    y = mean, 
                    ymin = mean, 
                    ymax = mean),
                width = 0.8,  # Bar width for each Shoe
                color = "steelblue",
                linewidth = 0.5
  ) +
  geom_jitter(height = 0, width = 0.1, size = 3) + # Add jittered points
  scale_y_continuous(breaks = seq(50,70,2)) + # Customize y-axis
  labs(y = "Lap Time (Seconds)", 
       x = "Shoe") + # Add axis labels
  theme(aspect.ratio = 1.5) # Adjust aspect ratio

### Inference (Model Fitting and Comparisons) ----------------------------------------

# Set contrasts to "sum to zero"
options(contrasts = c("contr.sum", "contr.poly"))

# Fit a linear model to test for Shoe effects on coagulation rate
shoe_mod <- lm(`Lap Time (seconds)` ~ Shoe, data = shoe_data)

# Perform an ANOVA to test for overall Shoe effects
anova(shoe_mod)

# Summarize the linear model
summary(shoe_mod)

# Diagnostic plots for the model
par(mfrow = c(2,2)) # Arrange plots in a 2x2 grid
plot(shoe_mod)
par(mfrow = c(1,1)) # Reset plotting layout

# Estimated means for each Shoe group using the emmeans package
library(emmeans)
shoe_lsmeans <- emmeans(shoe_mod, specs = ~ Shoe)
shoe_lsmeans # Display the estimated means
emmip(shoe_mod, ~ Shoe, CIs = T) # Plot the estimated means

# Custom contrasts for comparing Shoes
# Lightweight vs Stability
levels(shoe_data$Shoe)
contrast(shoe_lsmeans,
         method = list("Lightweight vs Stability" = c(0, 1, -1)),
         infer = c(TRUE, TRUE)
)

# Average of Control & Stability vs Lightweight
contrast(shoe_lsmeans,
         method = list("Control & Stability vs Lightweight" = c(0.5, -1, 0.5)),
         infer = c(TRUE, TRUE)
)

contrast(shoe_lsmeans,
         method = list("Control & Stability vs Lightweight" = c(-0.5, 1, -0.5)),
         infer = c(TRUE, TRUE)
)

# Pairwise comparisons with and without adjustment
shoe_lsmeans <- emmeans(shoe_mod, specs = ~ Shoe)
shoe_pairs <- pairs(shoe_lsmeans, 
                      adjust = "tukey",
                      infer = c(TRUE, TRUE),
                      reverse = FALSE
)
shoe_pairs # Display pairwise comparisons

# Group comparisons with multcompView (optional, may require R and package updates)
library(multcompView) 
library(multcomp)
cld(shoe_lsmeans, 
    Letters = LETTERS, # Letter groupings for significant differences
    decreasing = TRUE,
    adjust = "tukey") # No adjustment (alternatives: Tukey, Bonferroni, etc.)

# Dunnett's test for comparisons against the control Shoe
contrast(shoe_lsmeans, 
         method = "trt.vs.ctrl", # Treatment vs. control contrast
         adjust = "dunnet", # Dunnett adjustment
         infer = c(TRUE, TRUE),
         ref = "Control"
)


# Reset contrasts to default settings ("set to zero") --------------------------
options(contrasts = c("contr.treatment", "contr.poly"))
