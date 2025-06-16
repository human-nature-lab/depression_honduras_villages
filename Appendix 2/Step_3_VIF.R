#Section 1

# Load the model
load("../Code/Results/model_not_relative_men.rda")

#install.packages("car")
# Load the car package for the VIF function
library(car)

vif_values <- vif(model_men_not_relative)
print(vif_values)

#All values are below 5—most even below 3—suggesting that multicollinearity is not a significant concern.


# Load the model
load("../Code/Results/model_not_relative_women.rda")


vif_values <- vif(model_women_not_relative)
print(vif_values)


#Section 3 - POSTPATRUM

# Load the model
load("../Code/Results/model_not_relative_post_men.rda")

# Compute and display VIF values
vif_values <- vif(model_post_men_not_relative)
print(vif_values)

#All values are below 5—most even below 3—suggesting that multicollinearity is not a significant concern.


# Load the model
load("../Code/Results/model_not_relative_post_women.rda")

# Compute and display VIF values
vif_values <- vif(model_post_women_not_relative)
print(vif_values)

#All values are below 5—most even below 3—suggesting that multicollinearity is not a significant concern.


#Appendix

# Load the model
load("../Code/Results/model_not_relative_men_appendix.rda")

# Compute and display VIF values
vif_values <- vif(model_men_not_relative_appendix)
print(vif_values)

#All values are below 5—most even below 3—suggesting that multicollinearity is not a significant concern.


# Load the model
load("../Code/Results/model_not_relative_women_appendix.rda")


# Compute and display VIF values
vif_values <- vif(model_women_not_relative_appendix)
print(vif_values)

