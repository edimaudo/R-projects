## Machines Manufacturing Captal Budgeting Model (Project 1)

## Environment setup
rm(list = ls())

##### Load Libraries ###### 
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",
              'scales','ggfortify','DT',
              'shiny','shinydashboard','lubridate'
              )
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#####  Input Parameters ##### 
noutput <- 100000    # Number of phones per machine per year
pscrap <- 50000      # Scrap value of machine ($)
pphone <- 500        # Price per phone ($)
cmachine <- 1000000  # Cost per machine or advertising year ($)
cphone <- 250        # Variable cost per phone ($)
nlife <- 10          # Number of years for which the machine produces phones
nmachines_to_buy <- 5 # Number of machines to purchase sequentially
d1 <- 100000         # Quantity of phones demanded in the first year
gd <- 0.2            # Percentage growth in demand for each advertisement (0.2 = 20%)
r <- 0.05            # Interest rate earned on investments (0.05 = 5%)
max_year <- 20       # Maximum number of years for the model

#####  Initialization ##### 
years <- 1:max_year
cash_flow <- numeric(max_year)
demand <- numeric(max_year)
production_capacity <- numeric(max_year)
sales_volume <- numeric(max_year)
revenue <- numeric(max_year)
variable_cost <- numeric(max_year)
scrap_value_received <- numeric(max_year)
investment_or_ad_cost <- numeric(max_year)
active_machines <- numeric(max_year)

# Keep track of when each machine was bought and when it will be scrapped
# machine_details[i, 1] = year bought, machine_details[i, 2] = year scrapped
machine_details <- matrix(NA, nrow = nmachines_to_buy, ncol = 2)
colnames(machine_details) <- c("YearBought", "YearScrapped")

#####  Yearly Simulation Loop ##### 
for (y in years) {
  
  # 1. Investment / Advertising Cost
  # This cost is incurred every year as per the problem statement
  investment_or_ad_cost[y] <- cmachine
  is_machine_purchase_year <- (y <= nmachines_to_buy)
  is_advertising_year <- (y > nmachines_to_buy)
  
  # If it's a machine purchase year, record the purchase and scrap year
  if (is_machine_purchase_year) {
    machine_details[y, "YearBought"] <- y
    machine_details[y, "YearScrapped"] <- y + nlife
  }
  
  # 2. Calculate Active Machines & Production Capacity for the current year 'y'
  current_active_machines <- 0
  if (nmachines_to_buy > 0) {
    for (m in 1:min(y, nmachines_to_buy)) { # Only check machines bought up to year y
      year_bought <- machine_details[m, "YearBought"]
      # Machine is active if current year 'y' is >= year_bought AND < year_scrapped
      if (y >= year_bought && y < machine_details[m, "YearScrapped"]) {
        current_active_machines <- current_active_machines + 1
      }
    }
  }
  active_machines[y] <- current_active_machines
  production_capacity[y] <- active_machines[y] * noutput
  
  # 3. Calculate Demand for the current year 'y'
  if (y == 1) {
    demand[y] <- d1
  } else {
    # Demand grows only in advertising years
    if (is_advertising_year) {
      demand[y] <- demand[y - 1] * (1 + gd)
    } else {
      demand[y] <- demand[y - 1] # Demand stays flat during machine buying years after year 1
    }
  }
  
  # 4. Calculate Sales Volume (limited by demand or capacity)
  sales_volume[y] <- min(demand[y], production_capacity[y])
  
  # 5. Calculate Revenue and Variable Costs
  revenue[y] <- sales_volume[y] * pphone
  variable_cost[y] <- sales_volume[y] * cphone
  
  # 6. Calculate Scrap Value Received
  # Check if any machine is scheduled to be scrapped *at the end* of this year
  current_scrap_value <- 0
  if (nmachines_to_buy > 0) {
    for (m in 1:nmachines_to_buy) {
      # Check if the machine exists (was bought) and if its scrap year is the current year
      if (!is.na(machine_details[m, "YearScrapped"]) && machine_details[m, "YearScrapped"] == y) {
        current_scrap_value <- current_scrap_value + pscrap
      }
    }
  }
  scrap_value_received[y] <- current_scrap_value
  
  # 7. Calculate Net Cash Flow for the year
  # Cash Flow = Revenue - Variable Costs - Investment/Ad Cost + Scrap Value
  cash_flow[y] <- revenue[y] - variable_cost[y] - investment_or_ad_cost[y] + scrap_value_received[y]
}

#####  Calculate Net Present Value (NPV) ##### 
discount_factors <- (1 + r)^years
present_values <- cash_flow / discount_factors
npv <- sum(present_values)

#####  Output ##### 

# Print Yearly Cash Flow
cat("--- Yearly Cash Flow ---\n")
for (y in years) {
  cat(sprintf("Year %d: $%.2f\n", y, cash_flow[y]))
}

# Print Net Present Value
cat("\n--- Net Present Value ---\n")
cat(sprintf("NPV: $%.2f\n", npv))


# --- Optional: Display intermediate calculations for validation ---
# cat("\n--- Detailed Yearly Data (for validation) ---\n")
# yearly_data <- data.frame(
#   Year = years,
#   ActiveMachines = active_machines,
#   Demand = round(demand),
#   ProductionCapacity = production_capacity,
#   SalesVolume = round(sales_volume),
#   Revenue = round(revenue),
#   VariableCost = round(variable_cost),
#   InvestmentAdCost = investment_or_ad_cost,
#   ScrapReceived = scrap_value_received,
#   CashFlow = round(cash_flow)
# )
# print(yearly_data)