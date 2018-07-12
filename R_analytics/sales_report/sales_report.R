"""
--------------------------------
sales summary, activity report

The questions I need to answer:
Does more activity net '# of Sold'
Does more activity net 'Total Gross Sales'
Does more activity net 'Initial Payment'
Does more activity net 'Recurring Payment'


I've attached 2 CSV files. One for the activity of everyone in our staff. 
One other with all of our sales. Both are for the month of June. 
Here is a list of the sales reps (listed as Settlement Officers in the exports) 
for our review (which it would be nice to change from the list of 
anyone who had sales listed in the SalesSummary):
  
James Gregory
Robert Humphrey
Joe Rose
Sam Ross
Paul Taylor
Joseph Kelly
Cal Tyler
Bobby Chavez
Sean Porter
Joseph Henry
Marc Anderson
Bruce Allen
Scott Wright
"""

# remove old data
rm(list=ls())

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)
library(ggplot2)



