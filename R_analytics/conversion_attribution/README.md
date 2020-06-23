# Business Intelligence 

This challenge is set in the context of e-commerce or online marketing. Usually when you sell something in the internet, the user/customer 
comes multiple times (e.g. visits) to the site, often also via different channels (e.g. Email, Facebook, Google, etc.),
before he or she buys something (a conversion or transaction).

## Datasets

**table_A_conversions.csv**:
* example list of conversions/ sales/ transaction
* Conv_ID - transaction ID
* Conv_Date - transaction date
* Revenue - value of transaction
* User_ID - an ID of a customer

**table_B_attribution.csv**:
* list of attribution results for conversions
* Conv_ID - transaction ID (link to table A)
* Channel - marketing channel
* IHC_Conv - attributed conversion fraction by the IHC model

Note, that the attributed conversion fraction (IHC_Conv), i.e. it is the fraction of the 
given conversion which is attributed to a certain channel, sums up to 1.0 for every conversion.


## Task
list of points you might consider in your analysis:
* general overview over the time period (interesting KPIs are: revenue, number customers, fraction of return customer)
* performance and impact of different channels and how it changes over time
* most influential channels for every user
* Customer segmentation (RFM, LTV, clustering)
* cohort analysis





