library(WDI)
library(highcharter)
library(forecast)

wdi_data <- WDI(
  country = "US",
  indicator = "SP.DYN.TFRT.IN",
  start = 1960,
  end = 2012
)

wdi_data <-
  ts(
    data = wdi_data$SP.DYN.TFRT.IN,
    start = 1960,
    end = 2012,
    frequency = 1
  )

forecast(ets(wdi_data),
         h = 48 / 12,
         level = 95) %>%
  hchart %>%
  hc_tooltip(
    valuePrefix = "There were ",
    valueSuffix = " births per women",
    valueDecimals = 2
  ) %>%
  hc_title(text = "Fertility Forecast for the US")