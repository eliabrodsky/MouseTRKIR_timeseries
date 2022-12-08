##notes

Mouse types considered for analysis: 
For standardization of analysis methods, we considered using recordings from mice (July 2019 vs October 2019) that are matched for age, virus and infection method. The recording also contains separate batches from UCSF and PC cage types. The data were normalized before subjecting it to time-series decomposition analysis methods.


Seasonal-Trend decomposition using LOESS (STL)

Seasonal-Trend decomposition using LOESS (STL) is a robust method of time series decomposition that employs locally fitted regression models to decompose a time series into trend, seasonal, and remainder components. The STL algorithm performs smoothing on the time series using LOESS in two loops; the inner loop iterates between seasonal and trend smoothing and the outer loop minimizes the effect of outliers. During the inner loop, the seasonal component is calculated first and removed to calculate the trend component. The remainder is calculated by subtracting the seasonal and trend components from the time series. It is possible to reconstruct the raw time series by simple addition of the trend, seasonality and the remainder components.

The three components of STL analysis relate to the raw time series as follows:

yi = si + ti + ri

where:
yi = The value of the time series at point i.
si = The value of the seasonal component at point i.
ti = The value of the trend component at point i.
ri = The value of the remainder component at point i.

Smoothing for the time series is performed for each feature (temperature, body speed, head body distance, etc) separately. Time series smoothing based on  a jumping average of 3 hours is performed first on all the features and then subjected to STL decomposition to create trend and seasonal component for all the features. 
