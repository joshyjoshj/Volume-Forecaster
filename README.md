# Volume-Forecaster
A volume forecast for Small Cap US Equities, with simple attatched Shiny Dashboard.

Models are fitted in `tidymodels` with k-fold cross-validation and a grid search for tuning paramaters. The shiny dashboard allows the input of the pre-market volumes and will output the predictions. Also added is a button to retrieve the current volume for the ticker.


