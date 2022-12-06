library(pdftools)
library(stringr)
library(dplyr)
library(stringr)
library(tabulizer)

options(stringsAsFactors = FALSE)

#save the pdf file to be scraped
forfeiture <- "Forfeiture_Set_Aside_10-1-2020_to_9-22-2022.pdf"

#for each locate_areas call, highlight the area where on the page where the data for each column is expected 
  #and save that boundary into a variable
#this requires that each column's data is an a consistent area from page to page
#in this example, there are 10 columns we are trying to capture, so there are 10 calls to the locate_area function

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
# This is the primary function that will scrape each page
# It takes a single page number as an input
# The output is a dataframe with 10 columns, with the number of rows dependent on the page
###############################

bond_disp <- function(pg) {
  
  #create an empty dataframe for the 11 columns (10 specified above, and an 11th column for the page number we're pulling from)
  bond_disposition <- data.frame(matrix(ncol = 11, nrow = 0))
  
  #the extract_tables function uses the areas defined above to return the data within the area into a table
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
  
  ### NAME ###
  
  #save the extracted table as a dataframe and name the first column "A"
  name <- data.frame(name)
  colnames(name) <- "A"
  
  #create a list of row index values where the word "Name" shows up
  #the length of this list is the number of rows we will need to scrape for each page
  name_ls <- which(name$A== "Name")
  
  #create an empty dataframe with one column for Name
  nl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  #for each of the row index values in name_ls, return the text from the row index directly below, 
    #add it to our empty dataframe, and name the column as "name"
  for (i in 1:length(name_ls)){
    n <- name$A[name_ls[i]+1]
    nl <- rbind(nl, n)
    colnames(nl) <- c('name')
  }
  
  #we will repeat the same logic for all 10 columns below
  
  ## SPN ##
  
  #see same logic for Name column above
  spn <- data.frame(spn, stringsAsFactors = FALSE)
  colnames(spn) <- "A"
  spn_ls <- which(spn$A== "SPN")
  
  sl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(spn_ls)){
    s <- spn$A[spn_ls[i]+1]
    sl <- rbind(sl, s)
    colnames(sl) <- c('spn')
  }
  
  ### CASE NUMBER ###
  
  #see same logic for Name column above
  case <- data.frame(case, stringsAsFactors = FALSE)
  colnames(case) <- "A"
  case_ls <- which(case$A== "Case #")
  
  cl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(case_ls)){
    c <- case$A[case_ls[i]+1]
    cl <- rbind(cl, c)
    colnames(cl) <- c('case_num')
  }
  
  ### CHARGE LITERAL  ###
  
  #see same logic for Name column above
  charge <- data.frame(charge, stringsAsFactors = FALSE)
  colnames(charge) <- "A"
  charge_ls <- which(charge$A== "Charge Literal")
  
  chl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(charge_ls)){
    ch <- charge$A[charge_ls[i]+1]
    chl <- rbind(chl, ch)
    colnames(chl) <- c('charge')
  }
  
  ### DISPOSITION DATE ###
  
  #see same logic for Name column above
  disp_date <- data.frame(disp_date, stringsAsFactors = FALSE)
  colnames(disp_date) <- "A"
  disp_date_ls <- which(disp_date$A== "Date")
  
  dispdtl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(disp_date_ls)){
    dispdt <- disp_date$A[disp_date_ls[i]+1]
    dispdtl <- rbind(dispdtl, dispdt)
    colnames(dispdtl) <- c('disposition_date')
  }
  
  ### DISPOSITION CODE ###
  
  #see same logic for Name column above
  disp_code <- data.frame(disp_code, stringsAsFactors = FALSE)
  colnames(disp_code) <- "A"
  disp_code_ls <- which(disp_code$A== "Code")
  
  dispcodel <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(disp_code_ls)){
    dispcode <- disp_code$A[disp_code_ls[i]+1]
    dispcodel <- rbind(dispcodel, dispcode)
    colnames(dispcodel) <- c('disposition_code')
  }
  
  ### BOND DATE ###
  
  #see same logic for Name column above
  bond_date <- data.frame(bond_date, stringsAsFactors = FALSE)
  colnames(bond_date) <- "A"
  bond_date_ls <- which(bond_date$A== "Bond Date")
  
  bonddtl <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_date_ls)){
    bonddt <- bond_date$A[bond_date_ls[i]+1]
    bonddtl <- rbind(bonddtl, bonddt)
    colnames(bonddtl) <- c('bond_date')
  }
  
  ### BOND NUMBER ###
  
  #see same logic for Name column above
  bond_num <- data.frame(bond_num, stringsAsFactors = FALSE)
  colnames(bond_num) <- "A"
  bond_num_ls <- which(bond_num$A== "Bond Number")
  
  bondnuml <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_num_ls)){
    bondnum <- bond_num$A[bond_num_ls[i]+1]
    bondnuml <- rbind(bondnuml, bondnum)
    colnames(bondnuml) <- c('bond_num')
  }
  
  ### BOND TYPE ###
  
  #see same logic for Name column above
  bond_type <- data.frame(bond_type, stringsAsFactors = FALSE)
  colnames(bond_type) <- "A"
  bond_type_ls <- which(bond_type$A== "Type")
  
  bondtypel <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(bond_type_ls)){
    bondtype <- bond_type$A[bond_type_ls[i]+1]
    bondtypel <- rbind(bondtypel, bondtype)
    colnames(bondtypel) <- c('bond_type')
  }
  
  ### CASH BOND APPLIED ###
  
  #see same logic for Name column above
  cash_bond_applied <- data.frame(cash_bond_applied, stringsAsFactors = FALSE)
  colnames(cash_bond_applied) <- "A"
  cba_ls <- which(cash_bond_applied $A== "Appl'd to Fine")
  
  cbal <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for (i in 1:length(cba_ls)){
    cba <- cash_bond_applied$A[cba_ls[i]+1]
    cbal <- rbind(cbal, cba)
    colnames(cbal) <- c('cash_bond_applied_to_fine')
  }
  
  ### Combine all fields together ###
  
  return(cbind(nl, sl, cl, chl, dispdtl, dispcodel, bonddtl, bondnuml, bondtypel, cbal))
}

#set n as the number of pages from the pdf we want to scrape
#create a vector of lists with length of n
n = 81
datalist = list()
datalist = vector("list", length = n)

#for each page 1 through n, call the bond_disp function (our primary scraping function) for each page
for (i in 1:n) {
  dat <- bond_disp(i)
  dat$i <- i  #create a new column for the page number of the pdf that is being scraped
  datalist[[i]] <- dat #add each page output to the datalist vector
}

#combine all of the pages into one unified dataframe
bond_disposition_full_1_81 = do.call(rbind, datalist)

#save the unified datafame as a csv, if necessary
write.csv(bond_disposition_full_1_81,"forfeiture_bond_disposition.csv", row.names=FALSE)
