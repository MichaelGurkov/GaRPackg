test_df = import_dsge_forecasts(
  paste0(file.path(Sys.getenv("USERPROFILE")),
    "\\OneDrive - Bank Of Israel\\",
    "Data\\BoI\\GaR_Data\\DSGE forecasts.xlsx"
  )
)


expect_equal(test_df %>%
               filter(target_var == "gdp") %>%
               filter(date %in% c(zoo::as.yearqtr("1993 Q2"))) %>%
               filter(horizon == 1) %>%
               filter(quantile == 0.25) %>%
               pull(forecast), (0.005042323 - 0.008))
