# Load data
?HairEyeColor
str(HairEyeColor)
HairEyeColor

# Get marginal frequencies for eye color
margin.table(HairEyeColor, 2)

# Save eye color to data frame
eyes <- margin.table(HairEyeColor, 2)
eyes
round(prop.table(eyes), 2)  # Show as proportions w/2 digits

# Use Pearson's chi-squared test
# Need one-dimensional goodness-of-fit test
# Default test (assume equal distribution)
chi1 <- chisq.test(eyes)  # Save tests as object "chi1"
chi1  # Check results