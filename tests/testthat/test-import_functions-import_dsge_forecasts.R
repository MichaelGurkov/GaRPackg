test_df = import_dsge_forecasts(
  paste0(file.path(Sys.getenv("USERPROFILE")),
    "\\OneDrive - Bank Of Israel\\",
    "Data\\BoI\\GaR_Data\\DSGE forecasts.xlsx"
  )
)


expect_equal(test_df %>%
               dplyr::filter(target_var == "gdp") %>%
               dplyr::filter(date %in% c(zoo::as.yearqtr("1993 Q2"))) %>%
               dplyr::filter(horizon == 1) %>%
               dplyr::filter(quantile == 0.25) %>%
               dplyr::pull(forecast), (0.005042323 - 0.008))
