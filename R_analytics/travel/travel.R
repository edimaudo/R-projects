#-------------------
#business background
#-------------------
# One very common marketing problem is where & when to invest in order to drive the most conversions with least cost. 
# In our travel business, We are trying to figure out the same problem. In this challenge you will be given a sample data set. 
# Each row represent a visit to our product, and each row has several columns consist of the behavioral & demographic features 
# such as the route they search within this visit, the departure date and the marketing channel that the visit come from, 
# as well as a column indicating how many conversions have happened(0 means no conversion happened).


#business objective

#data dictionary

#remove old data

#load libraries

#load data

#backup data

#review data

#exploratory analysis

#data transformation

#split data

#modelling

#check for features

#



Please try to answer the following questions:
1) Please rank the importance of each feature based on their level of correlation to the happening of conversions
2) Please identify for route JKT(Jakarta) - DPS(Bali), which channel x device x travel time x any other significant influencing feature we should invest in most in order to drive most bookings out of this route
3) Please build a machine learning model that takes all columns except “conversions” as x and “conversions” as y to predict whether a booking will happen given all column x

Guide
1) Please explain the rationales behind using a particular statistical/ML model for your solution, and why that model is better than other potential models (even if you just do a excel pivot analysis)
2) You may choose to ignore certain features, and if you do, please explain why you ignore those features
3) Please feel free to create additional features or perform any feature transformation for the data. Note that the data might not be 100% clean.

Column explanation:
  data_created_at: Time when the visit to our product(i.e. web, app) happens

device: The device that makes this visit, i.e. desktop, smartphone

Os_type: The operation of the visiting device, i.e. iOS, android, windows

Client_type: product type, either web or app

Airlines: The two letter code of the airlines that the user select to fly with. If it’s round trip, the outbound and inbound airlines are separated by “=”. If the user changes airline within any leg, (i.e. a user’s final destination is hong kong but stopover at KL and changes an airline), then the two segment airline would be separated by “-” 

Stops: With stopover or without stopover

Trip_type: oneway or roundtrip

Trip_category: domestic or international

Cabin_class: EONOMY, PREMIUM ECONOMY OR BUSINESS

Adults_count: Number of adults

Children_count: Number of children

Infants_count: Number of infants

Total_price_usd: Total value of tickets that the user select, with 5 decimal points

User_city: Where the user is when he/she conducts the product visit

Channel: The marketing channel that the user come from

Device_resolution: i.e. 1080 x 1794

Device_version: Model of the visiting device i.e. Samsung G930F

Os_version: Operating system of the device i.e. iOS 8.0

App_version: app version, the value is null if client type = web

Network_type: 4g, wifi or 3g

Network_carrier_name: Telecom company that hosts the internet connection, i.e. Singtel

Conversions: Number of goal happened