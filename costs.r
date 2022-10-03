library(plm)
library(readxl)
library(car)

railway_data <- read_excel("railway_data.xlsx")

head(railway_data)
View(railway_data)

# plc - price of labout corrected
# puc - price of utility corrected
# pcc - price of capital corrected
# tc - total cost

model <- plm(log(tc) ~ log(plc) + log(puc)
             + log(pcc) + log(Q) + network_length,
             data = railway_data, index = c("railway_name","quarters"))
summary(model)

stargazer(model, type = "text")

# fail to reject, coefficients are ok
stargazer(linearHypothesis(model, "log(plc) + log(puc) + log(pcc) + log(Q) = 1" ), type="text")

