library(pdftools)
library(stringr)
library(dplyr)
library(stringr)
library(tabulizer)
library(tidyverse)

options(stringsAsFactors = FALSE)

forfeiture <- "Forfeiture_Set_Aside_10-1-2020_to_9-22-2022.pdf"

name_areas <- locate_areas(file = forfeiture , pages=1)
spn_areas <- locate_areas(file = forfeiture, pages=1)
case_num_areas <- locate_areas(file = forfeiture, pages=1)
charge_literal_areas <- locate_areas(file = forfeiture, pages=1)
disp_date_areas <- locate_areas(file = forfeiture, pages=1)
disp_code_areas <- locate_areas(file = forfeiture, pages=1)
bond_date_areas <- locate_areas(file = forfeiture, pages=1)
bond_num_areas <- locate_areas(file = forfeiture, pages=1)
bond_type_areas <- locate_areas(file = forfeiture, pages=1)
cash_bond_areas <- locate_areas(file = forfeiture, pages=1)

###############################

bond_disp <- function(pg) {
  
  bond_disposition <- data.frame(matrix(ncol = 11, nrow = 0))
  
  name <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = name_areas)
  spn <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = spn_areas)
  case <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = case_num_areas)
  charge <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = charge_literal_areas)
  disp_date <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = disp_date_areas)
  disp_code <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = disp_code_areas)
  bond_date <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = bond_date_areas)
  bond_num <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = bond_num_areas)
  bond_type <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = bond_type_areas)
  cash_bond_applied <- extract_tables(file = forfeiture, pages=pg, guess=FALSE, area = cash_bond_areas)
  
  # NAME #
  
  name <- data.frame(name, stringsAsFactors = FALSE)
  colnames(name) <- "A"
  name_ls <- which(name$A== "Name")
  
  nl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(name_ls)){
    n <- name$A[name_ls[i]+1]
    nl <- rbind(nl, n)
    colnames(nl) <- c('name')
  }
  
  # SPN #
  
  spn <- data.frame(spn, stringsAsFactors = FALSE)
  colnames(spn) <- "A"
  spn_ls <- which(spn$A== "SPN")
  
  sl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(spn_ls)){
    s <- spn$A[spn_ls[i]+1]
    sl <- rbind(sl, s)
    colnames(sl) <- c('spn')
  }
  
  # CASE NUMBER #
  
  case <- data.frame(case, stringsAsFactors = FALSE)
  colnames(case) <- "A"
  case_ls <- which(case$A== "Case #")
  
  cl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(case_ls)){
    c <- case$A[case_ls[i]+1]
    cl <- rbind(cl, c)
    colnames(cl) <- c('case_num')
  }
  
  # CHARGE LITERAL  #
  
  charge <- data.frame(charge, stringsAsFactors = FALSE)
  colnames(charge) <- "A"
  charge_ls <- which(charge$A== "Charge Literal")
  
  chl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(charge_ls)){
    ch <- charge$A[charge_ls[i]+1]
    chl <- rbind(chl, ch)
    colnames(chl) <- c('charge')
  }
  
  # DISPOSITION DATE #
  
  disp_date <- data.frame(disp_date, stringsAsFactors = FALSE)
  colnames(disp_date) <- "A"
  disp_date_ls <- which(disp_date$A== "Date")
  
  dispdtl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(disp_date_ls)){
    dispdt <- disp_date$A[disp_date_ls[i]+1]
    dispdtl <- rbind(dispdtl, dispdt)
    colnames(dispdtl) <- c('disposition_date')
  }
  
  #DISPOSITION CODE #
  
  disp_code <- data.frame(disp_code, stringsAsFactors = FALSE)
  colnames(disp_code) <- "A"
  disp_code_ls <- which(disp_code$A== "Code")
  
  dispcodel <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(disp_code_ls)){
    dispcode <- disp_code$A[disp_code_ls[i]+1]
    dispcodel <- rbind(dispcodel, dispcode)
    colnames(dispcodel) <- c('disposition_code')
  }
  
  # BOND DATE #
  
  bond_date <- data.frame(bond_date, stringsAsFactors = FALSE)
  colnames(bond_date) <- "A"
  bond_date_ls <- which(bond_date$A== "Bond Date")
  
  bonddtl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_date_ls)){
    bonddt <- bond_date$A[bond_date_ls[i]+1]
    bonddtl <- rbind(bonddtl, bonddt)
    colnames(bonddtl) <- c('bond_date')
  }
  
  # BOND NUMBER #
  
  bond_num <- data.frame(bond_num, stringsAsFactors = FALSE)
  colnames(bond_num) <- "A"
  bond_num_ls <- which(bond_num$A== "Bond Number")
  
  bondnuml <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_num_ls)){
    bondnum <- bond_num$A[bond_num_ls[i]+1]
    bondnuml <- rbind(bondnuml, bondnum)
    colnames(bondnuml) <- c('bond_num')
  }
  
  # BOND TYPE #
  
  bond_type <- data.frame(bond_type, stringsAsFactors = FALSE)
  colnames(bond_type) <- "A"
  bond_type_ls <- which(bond_type$A== "Type")
  
  bondtypel <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_type_ls)){
    bondtype <- bond_type$A[bond_type_ls[i]+1]
    bondtypel <- rbind(bondtypel, bondtype)
    colnames(bondtypel) <- c('bond_type')
  }
  
  # CASH BOND APPLIED #
  
  cash_bond_applied <- data.frame(cash_bond_applied, stringsAsFactors = FALSE)
  colnames(cash_bond_applied) <- "A"
  cba_ls <- which(cash_bond_applied $A== "Appl'd to Fine")
  
  cbal <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(cba_ls)){
    cba <- cash_bond_applied$A[cba_ls[i]+1]
    cbal <- rbind(cbal, cba)
    colnames(cbal) <- c('cash_bond_applied_to_fine')
  }
  
  ## Combine all fields together ##
  
  return(cbind(nl, sl, cl, chl, dispdtl, dispcodel, bonddtl, bondnuml, bondtypel, cbal))
}

n = 81
datalist = list()
datalist = vector("list", length = n)

for (i in 1:n) {
  dat <- bond_disp(i)
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}

bond_disposition_full_1_81= do.call(rbind, datalist)

View(bond_disposition_full_1_81)

write.csv(bond_disposition_full_1_81,"forfeiture_bond_disposition.csv", row.names=FALSE)
