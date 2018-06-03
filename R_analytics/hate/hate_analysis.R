# Variables:
#   - (_unit_id) = this is the ID of the comment. Each comment has five ratings from different people.
#   - (_country) = this is the country of the rater.
#   - (how_hateful) = this is the hatefulness rating given by a rater. The scale is from 1-7 (1 = not_hateful_at_all; 7 = very_hateful).
#   
#   Questions:
#     
#- Is country (_country) significantly affecting the hatefulness ratings (how_hateful)?
#==> If yes, what are the major differences between countries?
#- Is continent significantly affecting the hatefulness ratings (how_hateful) -- to answer this, group countries to continents by adding a new column CONTINENT
#==> If yes, what are the major differences between continents?
#- Which countries are most sensitive to hate? (i.e., the share of 'very_hateful' *from all the ratings* (ratio) given by people from that country is the highest)
#- Which countries are least sensitive to hate? (i.e., the share of 'not_hateful_at_all' *from all the ratings* (ratio) given by people from that country is the highest)
#   
# Calculate hate interpretation score as explained below:
   
# For each comment (_unit_id) there are five ratings.
# First, compute average hatefulness rating *for each comment* based on these five ratings.
# Then, compute the *difference for each unique country* from the average rating. For example, if Comment A's average is 3.5 and a Mexican rater would rate it 2, so then the difference from the average is -1.5.
# Do this computation for all comments and then take the sum of each country's differences (this will be used for visualization). The sum is called "hate interpretation score".
# Finally, compute some suitable variation metric to describe how the hatefulness ratings vary *within* each country (again, this will be used for visualization).
#   
# Visualizations:
#     
# -visualize the distribution of raters from different countries (to get an understanding of how biased the sample is) (histogram/bar chart)
# -visualize hate interpretation scores between countries (map visualization)
# -illustrate variation of hate interpretation between countries (table? map? => choose the best way)

#clear old data
rm(list=ls())

