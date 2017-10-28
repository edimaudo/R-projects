# PROP TEST
# 98 wins out of 162 games (default settings)
prop.test(98, 162)

# One-tailed test with 90% CI
prop.test(98, 162, alt = "greater", conf.level = .90)