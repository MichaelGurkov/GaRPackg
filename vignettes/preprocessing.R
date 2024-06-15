## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(GaRPackg)

library(dplyr)

library(ggplot2)

library(tidyr)


## ----load_data----------------------------------------------------------------

data("gar_data")



## ----plot_gdp, fig.width=6----------------------------------------------------

gar_data %>% 
  ggplot(aes(date, gdp)) + 
  geom_line() + 
  xlab(NULL) + ylab(NULL) + ggtitle("Israel GDP (billions ILS)")


## ----preprocessed_df, fig.width=8---------------------------------------------

final_df = gar_data %>% 
  select(date, gdp) %>% 
  preprocess_df(vars_to_yoy = "gdp",
                vars_to_percent_change = "gdp",
                vars_to_diff = "gdp")

final_df %>% 
  select(date, starts_with("gdp_")) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value)) + 
  geom_line() + 
  facet_wrap(~name, scales = "free") + 
  xlab(NULL) + ylab(NULL)




