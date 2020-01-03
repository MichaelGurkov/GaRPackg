library(tidyverse)

library(xts)

library(quantreg)

stress_index = read.csv(paste0("C:\\Users\\Misha\\Documents\\Data",
                               "\\BOI\\Stress_Index.csv"),
                        stringsAsFactors = FALSE)

stress_index = stress_index %>%
  setNames(c("Date","Stress_Index"))

stress_index = stress_index %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  mutate(Stress_Index = str_remove(Stress_Index,"%")) %>%
  mutate(Stress_Index = as.numeric(Stress_Index)) %>%
  filter(complete.cases(.))

raw_data = read.csv(paste0("C:\\Users\\Misha\\Documents\\GaR Python\\",
                           "Israel GaR\\Data\\Raw\\raw_GaR_data.csv"))

raw_data = raw_data %>%
  mutate(Date = as.yearqtr(date, format = "%Y.%q"))


gar_df = inner_join(stress_index %>%
                      group_by(Date = as.yearqtr(Date)) %>%
                      summarise(FCI = mean(Stress_Index)),
                    raw_data %>%
                      select(Date, GDP_real) %>%
                      mutate(GDP_real = lead(GDP_real,4) / GDP_real),
                    by = "Date") %>%
  mutate_at(.vars = vars(-"Date"), .funs = list(scale))


qreg = rq("GDP_real ~ lag(FCI,4)",
          tau = c(0.1,0.5,0.9),data = gar_df)

ggplot(gar_df, aes(x = FCI, y = GDP_real)) +
  geom_point() +
  geom_abline(slope = qreg$coefficients[2,],
              intercept = qreg$coefficients[1,]) +
  theme_bw()

