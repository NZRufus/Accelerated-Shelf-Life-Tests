# Accelerated-Shelf-Life-Tests #
Used for predicting shelf life of products

## Different files are different products ##

Samples were heated at various temperatures (25,35,45 and 55 degrees C) and when samples had dropped by 10% from starting values, they were deemed to have failed. 
Timepoints were collected (Logged) and then plotted against 1/temperature (in Kelvin) and extrapolated to determine shelf life at desired temperatures using Arrhenius Equation 

ln k = ln A - Activation Energy/Gas Constant (8.314) * 1/T (temperature in Kelvin)

The shelf life was calculated along with the 95% confidence interval as lower 95% confidence interval is the value that is used as the shelf life date on these products. This means it is important that there is a small confidence interval to increase the shelf life and thereby meet statutory requirements for shelf life of products.

## Packages used ##
This was using Seaborn, Pandas, Numpy and scistats.


