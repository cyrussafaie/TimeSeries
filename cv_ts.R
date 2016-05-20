cv_ts <- function(model, test_data, p = 1)
{
  h_ = length(test_data)
  fcast = forecast(model,h = h_)
  return(abs(fcast[['mean']]-test_data))
}