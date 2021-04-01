library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(readr)
library(plotly)

bitcoin <- read_csv("dataset/bitcoin_usd_gwa.csv")
btccash <- read_csv("dataset/btccash_usd_gwa.csv")
cardano <- read_csv("dataset/cardano_usd_gwa.csv")
dash <- read_csv("dataset/dash_usd_gwa.csv")
ethereum <- read_csv("dataset/ethereum_usd_gwa.csv")
iota <- read_csv("dataset/iota_usd_gwa.csv")
litecoin <- read_csv("dataset/litecoin_usd_gwa.csv")
monero <- read_csv("dataset/monero_usd_gwa.csv")
nem <- read_csv("dataset/nem_usd_gwa.csv")
ripple <- read_csv("dataset/ripple_usd_gwa.csv")

##MANIPULANDO OS DATASETS COM INTUITO DE GERAR APENAS 1
##REMOVENDO A PRIMEIRA COLUNA E CRIANDO UMA NOVA COM O NOME DA MOEDA
bitcoin <- bitcoin[ -c(1)]
criptocurrency <- rep("Bitcoin", nrow(bitcoin))
bitcoin <- cbind(bitcoin, criptocurrency)

btccash <- btccash[ -c(1)]
criptocurrency <- rep("Btccash", nrow(btccash))
btccash <- cbind(btccash, criptocurrency)

cardano <- cardano[ -c(1)]
criptocurrency <- rep("Cardano", nrow(cardano))
cardano <- cbind(cardano, criptocurrency)

dash <- dash[ -c(1)]
criptocurrency <- rep("Dash", nrow(dash))
dash <- cbind(dash, criptocurrency)

ethereum <- ethereum[ -c(1)]
criptocurrency <- rep("Ethereum", nrow(ethereum))
ethereum <- cbind(ethereum, criptocurrency)

iota <- iota[ -c(1)]
criptocurrency <- rep("Iota", nrow(iota))
iota <- cbind(iota, criptocurrency)

litecoin <- litecoin[ -c(1)]
criptocurrency <- rep("Litecoin", nrow(litecoin))
litecoin <- cbind(litecoin, criptocurrency)

monero <- monero[ -c(1)]
criptocurrency <- rep("Monero", nrow(monero))
monero <- cbind(monero, criptocurrency)

nem <- nem[ -c(1)]
criptocurrency <- rep("Nem", nrow(nem))
nem <- cbind(nem, criptocurrency)

ripple <- ripple[ -c(1)]
criptocurrency <- rep("Ripple", nrow(ripple))
ripple <- cbind(ripple, criptocurrency)

##UNINDO OS DATAFRAMES
geral <- union_all(ripple, nem)
geral <- union_all(geral, monero)
geral <- union_all(geral, litecoin)
geral <- union_all(geral, iota)
geral <- union_all(geral, ethereum)
geral <- union_all(geral, cardano)
geral <- union_all(geral, dash)
geral <- union_all(geral, btccash)
geral <- union_all(geral, bitcoin)

##DEFININDO O TIPO PARA A COLUNA DATE
geral$Date <- strptime(geral$Date, format='%Y-%m-%d')
cripto_list <- c('Bitcoin', 'Btccash', 'Cardano', 'Dash', 'Ethereum', 'Iota', 'Litecoin', 'Monero', 'Nem', 'Ripple')