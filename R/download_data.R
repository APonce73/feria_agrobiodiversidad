library(readxl)
library(tidyverse)
library(R.utils)
library(openxlsx)

#Para 2016
root <- c("datos/2016/")
titulo1 <- c("Productores_A_diversidad_")
year <- c(2016)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_A_2016 <- read_xlsx(formula)

root <- c("datos/2016/")
titulo1 <- c("Productores_B_Transferencias_")
year <- c(2016)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_B_2016 <- read_xlsx(formula)


#Para 2017
root <- c("datos/2017/")
titulo1 <- c("Productores_A_diversidad_")
year <- c(2017)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_A_2017 <- read_xlsx(formula)

root <- c("datos/2017/")
titulo1 <- c("Productores_B_Transferencias_")
year <- c(2017)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_B_2017 <- read_xlsx(formula)

#Para 2018
root <- c("datos/2018/")
titulo1 <- c("Productores_A_diversidad_")
year <- c(2018)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_A_2018 <- read_xlsx(formula)

root <- c("datos/2018/")
titulo1 <- c("Productores_B_Transferencias_")
year <- c(2018)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_B_2018 <- read_xlsx(formula)


#Para 2019
root <- c("datos/2019/")
titulo1 <- c("Productores_A_diversidad_")
year <- c(2019)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_A_2019 <- read_xlsx(formula)

root <- c("datos/2019/")
titulo1 <- c("Productores_B_Transferencias_")
year <- c(2019)
type <- c(".xlsx")
formula <- paste0(root, titulo1, year, type)
Prod_B_2019 <- read_xlsx(formula)


uno <- nrow(Prod_A_2016)
dos <- nrow(Prod_A_2017)
tres <- nrow(Prod_A_2018)
cuatro <- nrow(Prod_A_2019)

uno + dos + tres + cuatro

uno <- nrow(Prod_B_2016)
dos <- nrow(Prod_B_2017)
tres <- nrow(Prod_B_2018)
cuatro <- nrow(Prod_B_2019)

uno + dos + tres + cuatro

# Para el 2016 Productores A


