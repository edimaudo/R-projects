# Install necessary library if you don't have it
# install.packages("tidyverse")
# install.packages("ggplot2")

library(tidyverse)
library(ggplot2)

# Inputs
pricemachine <- 1000000
nlife_options <- c(5, 10, 20)
pdefault1_options <- c(0.1, 0.2, 0.3)
Decaydefault <- 0.9
pndefault <- 0.4
rrecovery <- 0.4
interest_rates <- seq(0.30, 0.40, 0.05)
num_iterations <- 1000

# Function to simulate loan cash flows for a single iteration
simulate_loan <- function(nlife, pdefault1, Decaydefault, pndefault, rrecovery, rinterest) {
  cash_flows <- numeric(nlife + 2) # Potential for 2 extra years due to bankruptcy
  
  # Year 0: Loan disbursement
  cash_flows[1] <- -pricemachine
  
  defaulted <- FALSE
  for (year in 1:nlife) {
    # Calculate default probability for the current year
    if (year == 1) {
      prob_default <- pdefault1
    } else if (year == nlife) {
      prob_default <- pndefault
    } else {
      prob_default <- pdefault1 * (Decaydefault^(year - 1))
    }
    
    # Simulate default event
    if (!defaulted && runif(1) < prob_default) {
      defaulted <- TRUE
      default_year <- year
      break # Stop regular cash flows after default
    } else if (!defaulted) {
      # Regular interest payment
      cash_flows[year + 1] <- rinterest * pricemachine
    }
  }
  
  # Handle repayment or bankruptcy recovery
  if (!defaulted) {
    cash_flows[nlife + 1] <- cash_flows[nlife + 1] + pricemachine # Final repayment + interest
  } else {
    # Bankruptcy recovery occurs 2 years after default
    recovery_year <- default_year + 2
    if (recovery_year <= length(cash_flows)) {
      cash_flows[recovery_year + 1] <- rrecovery * pricemachine
    }
  }
  
  return(cash_flows)
}

# Function to calculate IRR for a single simulation
calculate_irr <- function(cash_flows) {
  tryCatch({
    irr <- uniroot(function(rate) npv(rate, cash_flows), interval = c(-0.9, 1))$root
    return(irr)
  }, error = function(e) {
    return(NA) # Return NA if IRR cannot be calculated
  })
}

# Function to calculate Net Present Value (NPV)
npv <- function(rate, cash_flows) {
  n <- length(cash_flows) - 1
  return(sum(cash_flows / (1 + rate)^(0:n)))
}

# Main simulation loop
results_list <- list()

for (nlife in nlife_options) {
  for (pdefault1 in pdefault1_options) {
    scenario_results <- list()
    for (rinterest in interest_rates) {
      irr_values <- replicate(num_iterations, calculate_irr(simulate_loan(nlife, pdefault1, Decaydefault, pndefault, rrecovery, rinterest)))
      expected_irr <- mean(irr_values, na.rm = TRUE)
      scenario_results[[as.character(rinterest)]] <- expected_irr
    }
    results_list[[paste0("nlife_", nlife, "_pdefault1_", pdefault1)]] <- scenario_results
  }
}

# Convert results to a data frame for easier plotting
results_df <- data.frame(
  nlife = factor(),
  pdefault1 = factor(),
  rinterest = numeric(),
  expected_irr = numeric()
)

for (nlife in nlife_options) {
  for (pdefault1 in pdefault1_options) {
    scenario_key <- paste0("nlife_", nlife, "_pdefault1_", pdefault1)
    scenario_results <- results_list[[scenario_key]]
    for (r_str in names(scenario_results)) {
      r <- as.numeric(r_str)
      irr <- scenario_results[[r_str]]
      results_df <- rbind(results_df, data.frame(nlife = factor(nlife), pdefault1 = factor(pdefault1), rinterest = r, expected_irr = irr))
    }
  }
}

# Visualize the results
ggplot(results_df, aes(x = rinterest, y = expected_irr, color = pdefault1, shape = nlife)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Expected IRR vs. Interest Rate by Loan Life and Initial Default Probability",
    x = "Loan Interest Rate",
    y = "Expected Internal Rate of Return (IRR)",
    color = "Initial Default Probability (pdefault1)",
    shape = "Loan Life (Years)"
  ) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent, breaks = interest_rates) +
  scale_y_continuous(labels = scales::percent)

# Display the results in a table format
print(pivot_wider(results_df, id_cols = c("nlife", "pdefault1"), names_from = "rinterest", values_from = "expected_irr"))

# Conditional Formatting (example using a tibble and printing with a style)
library(knitr)
library(kableExtra)

formatted_results <- results_df %>%
  pivot_wider(id_cols = c("nlife", "pdefault1"), names_from = "rinterest", values_from = "expected_irr") %>%
  mutate_at(vars(-nlife, -pdefault1), scales::percent) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  # Example conditional formatting: highlight IRR > 0
  column_spec(3:ncol(.), background = ifelse(as.matrix(pivot_wider(results_df, id_cols = c("nlife", "pdefault1"), names_from = "rinterest", values_from = "expected_irr")[, -c(1:2)]) > 0, "lightgreen", "lightcoral"))

formatted_results