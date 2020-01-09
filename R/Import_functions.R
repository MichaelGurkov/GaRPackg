#' This function returns the path to Data folder
#'

#' @title Returns Data folder path
#'
#' @import stringr
#'
#' @return string with path to Data folder

get.data.folder.path = function(){

  temp_user = list.dirs(path = "C:\\Users",recursive = FALSE)

  temp_user = gsub(pattern = "C:\\Users/",replacement = "" ,
                   x = temp_user,fixed = TRUE)

  temp_user = temp_user[!temp_user %in% c("All Users","Default",
                                         "Default User","Public" )]

  data_path = paste0("C:\\Users\\", temp_user,"\\Documents\\Data")

  if (dir.exists(data_path)){

    return(data_path)

  } else {

    return(NA)

    }

}



#' This function imports IIP data from xls format
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import zoo

import.iip.data = function(filepath = NULL){


  # Default path for Data folder


  if(is.null(filepath)){filepath = paste0(get.data.folder.path(),
  "\\BOI\\iipsh_past_h.xls")}


   years = 2004:2018

  assets = lapply(years, function(temp_year){

    read_xls(path = filepath, sheet = temp_year - 2003,range = "I8",
             col_names = as.character(temp_year))

  }) %>%
    bind_cols() %>%
    t() %>%
    as.data.frame() %>%
    setNames("Assets") %>%
    mutate(Year = rownames(.))

  liabs = lapply(years, function(temp_year){

    read_xls(path = filepath, sheet = temp_year - 2003,range = "I26",
             col_names = as.character(temp_year))

  }) %>%
    bind_cols() %>%
    t() %>%
    as.data.frame() %>%
    setNames("Liabs") %>%
    mutate(Year = rownames(.))


  df = full_join(assets, liabs, by = "Year") %>%
    mutate(Year = as.yearqtr(paste(Year, "Q4"))) %>%
    select(Year,Assets,Liabs)

  return(df)


}



#' This function imports CPI data from inflation_sa file
#'
#' @import readxl
#'
#' @import dplyr
#'

import.inflation_sa = function(
  filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){filepath = paste0(get.data.folder.path(),
                                          "\\BOI\\DSGE\\inflation_sa.xlsx")}


inflation_sa = read_xlsx(path = filepath,
                         sheet = 1,skip = 4,
                         col_names = c("Date",
                                       "CPI_season_adj",
                                       "CPI_no_house_season_adj",
                                       "CPI_no_house_fruits_season_adj"),
                         col_types = c("date",rep("numeric",3))) %>%
  filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
  mutate(Date = as.yearqtr(Date))

return(inflation_sa)

}


#' This function imports world economic data from inputs_foreign file
#'
#' @import readxl
#'
#' @import dplyr
#'

import.inputs_foreign = function(
  filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){filepath = paste0(get.data.folder.path(),
                                          "\\BOI\\DSGE\\inputs_foreign.xlsx")}


  inputs_foreign = read_xlsx( path = filepath,
    sheet = 1,skip = 4,col_names = c("Date","I_WORLD","GDP_WORLD",
                                     "IMP_OECD", "CPI_WORLD", "OB_FW_R_ROW"),
    col_types = c("date",rep("numeric",5))) %>%
    filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
    mutate(Date = as.yearqtr(Date)) %>%
    select(-OB_FW_R_ROW, -CPI_WORLD, -I_WORLD)

  return(inputs_foreign)

}



#' This function imports  labor market data from Inputs_labor file
#'
#' @import readxl
#'
#' @import dplyr
#'

import.Inputs_labor = function(
  filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){filepath = paste0(get.data.folder.path(),
                                          "\\BOI\\DSGE\\Inputs_labor.xlsx")}


  Inputs_labor = read_xlsx(path = filepath,
    sheet = 1,skip = 5,col_names = c("Date", "Wage","Labor_pop",
                                     "EMP", "Unemp_rate", "Labor_force",
                                     "Vacancies_Rate"),
    col_types = c("date",rep("numeric",6))) %>%
    filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
    mutate(Date = as.yearqtr(Date))

  return(Inputs_labor)

}


#' This function imports monetary data from monetary_variables file
#'
#' @import readxl
#'
#' @import dplyr
#'

import.monetary_variables = function(
  filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\monetary variables.xlsx")}


  monetary_variables = read_xlsx(
    path = paste0(get.data.folder.path(),
                  "\\BOI\\DSGE\\monetary variables.xlsx"),
    sheet = 1,skip = 1,col_names = c("Date","BOI_rate","Forward_10",
                                     "CPI_no_fruits","CPI_no_house",
                                     "CPI", "Export_Prices","Makam_12"),
    col_types = c("text",rep("numeric",7))) %>%
    filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
    mutate(Date = as.yearqtr(Date))


  return(monetary_variables)

}


#' This function imports yields data from Inputs_fw_row file
#'
#' @import readxl
#'
#' @import dplyr
#'

import.Inputs_fw_row = function(
  filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\Inputs_fw_row.xlsx")}


  Inputs_fw_row = read_xlsx(path = filepath,
                            skip = 6,
                            col_names = c("Date",
                                          paste0(rep(c("US","JPN","UK",
                                                       "EU","Ger"),
                                                     each = 2),
                                                 c("_10","_5")))) %>%
    filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
    mutate(Date = as.yearqtr(Date))


  return(Inputs_fw_row)

}


###############################################################################
########################## CSV format import ##################################
###############################################################################


#' This function imports energy prices data from commodities file
#'
#' @import zoo
#'
#' @import dplyr
#'

import.commodities = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\commodities.csv")}


commodities = read.csv(file = filepath, skip = 10,
                       col.names = c("Date","Commodity",
                                     "Commodity_Non_Energy"),
                       stringsAsFactors = FALSE) %>%
  filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
  mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y"))

return(commodities)

}



#' This function imports tourists data from tourists file
#'
#' @import zoo
#'
#' @import dplyr
#'

import.tourists = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\tourists.csv")}


  tourists = read.csv(file = filepath,
                      skip = 8,
                      col.names = c("Date","Visitors","Tourists",""),
                      stringsAsFactors = FALSE) %>%
    select(Date,Visitors,Tourists) %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
    mutate(Date = as.yearqtr(Date))

  return(tourists)

}


#' This function imports current account data from Inputs_CurrentAccount file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.Inputs_CurrentAccount = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\Inputs_CurrentAccount.csv")}


  Inputs_CurrentAccount = read.csv(file = filepath,
                                   skip = 3,col.names = c("Date","CA"),
                                   stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(paste0("01-",Date), format = "%d-%b-%y")) %>%
    mutate(Date = as.yearqtr(Date)) %>%
    mutate(CA = as.numeric(gsub(",","",CA)))

  return(Inputs_CurrentAccount)

}



#' This function imports current account data from Inputs_CurrentAccount file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.Inputs_financial = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\Inputs_financial.csv")}


  Inputs_financial = read.csv(file = filepath,
                              skip = 5, stringsAsFactors = FALSE,
                              col.names = c("Date","ILS_USD",
                                            "ILS_EURO","ILS_Yen",
                                            "ILS_GBP", "Brent")) %>%
    filter_at(.vars = vars(-Date),.vars_predicate = any_vars(!is.na(.))) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    mutate(Date = as.yearqtr(Date))

  return(Inputs_financial)

}



#' This function imports National Accounts data from Inputs_NA_SA file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.Inputs_NA_SA = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\Inputs_NA_SA.csv")}


  Inputs_NA_SA = read.csv(filepath, stringsAsFactors = FALSE)

  names(Inputs_NA_SA)[1] = "Date"

  Inputs_NA_SA = Inputs_NA_SA %>%
    mutate(Date = as.yearqtr(Date, format = "%b-%y")) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(gsub(",","",.))))


  return(Inputs_NA_SA)

}




#' This function imports inflation expectation data from rates_breakeven_inflation
#'
#' @import zoo
#'
#' @import dplyr
#'


import.rates_breakeven_inflation = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\rates and ",
                      "breakeven inflation.csv")}


  rates_breakeven_inflation = read.csv(filepath,
                                       skip = 8, stringsAsFactors = FALSE) %>%
    rename(Date = Name) %>%
    mutate(Date = as.yearqtr(Date, format = "%m/%d/%Y")) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(gsub(",","",.))))



  return(rates_breakeven_inflation)

}




#' This function imports global economic data from global_gdp file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.global_gdp = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\global gdp.csv")}


  global_gdp = read.csv(filepath,
                        skip = 9, stringsAsFactors = FALSE) %>%
    rename(Date = Name) %>%
    mutate(Date = as.yearqtr(Date, format = "%m/%d/%Y")) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(gsub(",","",.))))



  return(global_gdp)

}


#' This function imports global economic data from global_gdp file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.global_inflation = function(
  filepath = NULL){


  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\global inflation.csv")}


  global_inflation = read.csv(filepath,
                              skip = 9, stringsAsFactors = FALSE) %>%
    rename(Date = name) %>%
    mutate(Date = as.yearqtr(Date, format = "%m/%d/%Y")) %>%
    mutate_at(.vars = vars(-Date),
              .funs = list(~replace(., which(. == "#N/A"),NA))) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(.)))





  return(global_inflation)

}


#' This function imports real short rate from real rate 1Y file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.real_rate_1Y = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\",
                      "real rate 1Y.csv")}



  real_rate_1Y = read.csv(filepath,
                              skip = 8, stringsAsFactors = FALSE) %>%
    setNames(c("Date","real_rate_1")) %>%
    mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y"))


  return(real_rate_1Y)

}


#' This function imports real short rate from real rate 1Y file
#'
#' @import zoo
#'
#' @import dplyr
#'


import.gdp_without_startup_tax = function(
  filepath = NULL){


  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\",
                      "GDP without startup or import tax.csv")}


  gdp_without_startup_tax = read.csv(filepath,
                          skip = 48, stringsAsFactors = FALSE) %>%
    setNames(c("Date","GDP_season_adj","GDP_no_startup","GDP_no_tax")) %>%
    mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y")) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~gsub(pattern = ",",
                                                    replacement = "",
                                                    x = .))) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(.)))


  return(gdp_without_startup_tax)

}



#' This function imports all Osnat's data
#'
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @import purrr
#'
#' @import zoo
#'

import.osnat.data = function(){

  raw_data = list()

  raw_data$commodities = import.commodities()

  raw_data$global_inflation = import.global_inflation()

  raw_data$inflation_sa = import.inflation_sa()

  raw_data$Inputs_financial = import.Inputs_financial()

  raw_data$Inputs_fw_row = import.Inputs_fw_row()

  raw_data$Inputs_NA_SA = import.Inputs_NA_SA()

  raw_data$rates_breakeven_inflation = import.rates_breakeven_inflation()

  raw_data$global_gdp = import.global_gdp()

  raw_data$Inputs_CurrentAccount = import.Inputs_CurrentAccount()

  raw_data$inputs_foreign = import.inputs_foreign()

  raw_data$Inputs_labor = import.Inputs_labor()

  raw_data$monetary_variables = import.monetary_variables()

  raw_data$tourists = import.tourists()

  raw_data$Industrial_production = import.indus.prod.data()

  raw_data$VIX = import.vix()

  raw_data$global_monetary_rates = import.global.monetary.rates()

  raw_data$real_rate = import.real_rate_1Y()

  raw_data$gdp_without_startup_tax = import.gdp_without_startup_tax()


   # Join to one data frame and make variables table

  vars_table = lapply(names(raw_data), function(temp_name){

    temp_df = raw_data[[temp_name]]

    var_names = names(temp_df)[!names(temp_df) == "Date"]

    return(data.frame(Variable_Name = var_names, Source_File = temp_name))

  })

  vars_table = rbind_all(vars_table)

  osnat_df = reduce(raw_data,full_join, by = "Date")

  return(list(osnat_df = osnat_df, vars_table = vars_table))



}


#' This function imports all Michael's data
#'
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @import purrr
#'
#' @import zoo
#'

import.michael.data = function(filepath = NULL){



  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\raw_GaR_data.csv")}


  raw_GaR_data = read.csv(filepath) %>%
    rename(Date = date) %>%
    mutate(Date = as.yearqtr(as.character(Date), format = "%Y.%q")) %>%
    select(-ILS_USD, -BOI_rate)

  vars_table = data.frame(
    Variable_Name = names(raw_GaR_data)[!names(raw_GaR_data) == "Date"],
    Source_File = "raw_GaR_data")

  return(list(michael_df = raw_GaR_data, vars_table = vars_table))



}



#' This function imports Industrial Production data from csv format
#'
#'
#' @import dplyr
#'
#' @import zoo

import.indus.prod.data = function(filepath = NULL){

  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\",
                      "Industrial production.csv")}



  Industrial_production = read.csv(file = filepath, skip = 10,
                                   stringsAsFactors = FALSE,
                                   col.names = c("Date","Ind_Prod_advanced",
                                                 "Ind_Prod_Israel")) %>%
    mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y"))


}


#' This function imports VIX data from csv format
#'
#'
#' @import dplyr
#'
#' @import zoo


import.vix = function(
  filepath = NULL){


  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\VIX.csv")}



  VIX = read.csv(file = filepath,skip = 10,col.names = c("Date","VIX"),
                 stringsAsFactors = FALSE) %>%
    mutate(Date = as.yearqtr(Date, format = "%d/%m/%Y"))

  return(VIX)

}


#' This function imports global monetary rates data from csv format
#'
#'
#' @import dplyr
#'
#' @import zoo


import.global.monetary.rates = function(
  filepath = NULL){


  # Default path for Data folder

  if(is.null(filepath)){
    filepath = paste0(get.data.folder.path(),
                      "\\BOI\\DSGE\\global monetary rates.csv")}




  global_monetary_rates = read.csv(file = filepath,skip = 9,
                                   col.names = c("Date","rate_euro",
                                                 "rate_us","rate_uk",
                                                 "rate_japan"),
                 stringsAsFactors = FALSE) %>%
    mutate(Date = as.yearqtr(Date, format = "%m/%d/%Y")) %>%
    mutate_at(.vars = vars(-Date),
              .funs = list(~replace(., which(. == "#N/A"),NA))) %>%
    mutate_at(.vars = vars(-Date),.funs = list(~as.numeric(.)))

  return(global_monetary_rates)

}
