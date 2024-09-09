# Install and load required packages
install.packages("haven")
library(haven)
library(dplyr)

# Base directory setup
base_dir <- ""  # Define the base directory where folders are located

# Initialize an empty list to store data frames
list_data <- list()

# Process data for 2014-15
year_suffix <- 201415
for (block in c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")) {
  file_name <- paste0("blk", block, year_suffix, ".sav")
  file_path <- paste0(base_dir, "ASI201415Study/", file_name)
  if (file.exists(file_path)) {
    data_name <- paste0("ASI201415Study_", toupper(block))
    list_data[[data_name]] <- read_sav(file_path)
  } else {
    cat("File does not exist: ", file_path, "\n")
  }
}

# Process data for 2015-16
year_suffix <- 201516
for (block in LETTERS[1:10]) {
  file_name <- paste0("Block-", block, "-", year_suffix, ".sav")
  file_path <- paste0(base_dir, "ASI201516Study/", file_name)
  if (file.exists(file_path)) {
    data_name <- paste0("ASI201516Study_", block)
    list_data[[data_name]] <- read_sav(file_path)
  } else {
    cat("File does not exist: ", file_path, "\n")
  }
}

# Process data for 2016-2021
folders <- c("ASI201617Study", "ASI201718Study", "ASI201819Study", 
             "ASI201920Study", "ASI202021Study", "ASI202122Study")

for (folder in folders) {
  year_suffix <- substr(folder, 4, 9)
  for (block in LETTERS[1:10]) {
    file_name <- paste0("blk", block, year_suffix, ".sav")
    file_path <- paste0(base_dir, folder, "/", file_name)
    if (file.exists(file_path)) {
      data_name <- paste0(folder, "_", block)
      list_data[[data_name]] <- read_sav(file_path)
    } else {
      cat("File does not exist: ", file_path, "\n")
    }
  }
}

# Manually adding a specific file (exception case)
list_data[["ASI201617Study_J"]] <- read_sav(paste0(base_dir, "ASI201617Study/", "ASI_2016_17_Block_J_rectified.sav"))




### 2014-15 ###

# Define the block letters for the dataset keys
blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

# Dynamically load block data into variables
for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201415Study_", block)]])
}


# Helper function to sum numeric columns with NA handling
sum_numeric <- function(x) sum(x, na.rm = TRUE)

# Combining the data from all blocks
data_frame_2014_15 <- block_a %>%
  select(DSL = DSL, Industry_Code = INC5digit, Direct_Export_Share = Share, Production_Cost = CostofProd,
         Multiplier = Multilplier) %>%
  left_join(block_c %>% 
              filter(SLNO == 10) %>%
              select(DSL, 
                     Total_Gross_Closing = GrossCl,
                     Total_Net_Closing = Netvalcl),
            by = "DSL") %>%
  left_join(block_e %>%
              filter(SNO == 9) %>%
              select(DSL,
                Total_Man_Days = TManDay,
                Average_Person_Worked = AvgPersonWork,
                Total_Mandays_Paid = MandaysPaid
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(Total_Expenses)),
            by = "DSL") %>%
  left_join(block_g %>%
              group_by(DSL) %>%
              summarize(Total_Receipts = sum_numeric(Tot_receipt)),
            by = "DSL") %>%
  left_join(block_i %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(Purvaldel)),
            by = "DSL") %>%
  left_join(block_j %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(Grosssalval),
                Average_Net_Sales_Value = mean(Netsaleval, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(ExfactvalOutput)
              ),
            by = "DSL")

data_frame_2014_15 <- data_frame_2014_15 %>% rename(Plant_ID = DSL)

# Calculate Domestic Purchases as the difference between Production Cost and Total Imported Purchases
data_frame_2014_15 <- data_frame_2014_15 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2014_15 <- data_frame_2014_15 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



### 2015-16 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201516Study_", block)]])
}

data_frame_2015_16 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  select(DSL, Industry_Code = IND_CD, Direct_Export_Share = EXP_SHARE, Production_Cost = PROD_COST,
         Multiplier = MULTIPLIER) %>%
  left_join(block_c %>% 
              filter(S_no == 10) %>%
              select(DSL,
                Total_Gross_Closing = G_Valueclose,
                Total_Net_Closing = N_V_Cl
              ), 
            by = "DSL")%>%
  left_join(block_e %>%
              filter(S_No == 9) %>%
              select(DSL,
                Total_Man_Days = MandaysWorkedTotal,
                Average_Person_Worked = AveNumberPersonwork,
                Total_Mandays_Paid = NoofMandayspaid
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              group_by(DSL) %>%
              summarize(
                Total_Expenses = sum_numeric(Workdoneby) +
                  sum_numeric(Rep_Maint_buldg_othconst) +
                  sum_numeric(Rep_Maint__oth_fixed_asset) +
                  sum_numeric(OP_Expenses) +
                  sum_numeric(ExpensesOnRowmaterials) +
                  sum_numeric(Ins_Charges) +
                  sum_numeric(Rent_paid_Pla_mach_othFixAsst) +
                  sum_numeric(Exp_RD) +
                  sum_numeric(Rent_Paid_Build) +
                  sum_numeric(Rent_land_lease_royalities) +
                  sum_numeric(Interest_paid) +
                  sum_numeric(Purch_val_goods_sold)
              ),
            by = "DSL") %>%
  left_join(block_g %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(Recpt_Manuf_services) +
                  sum_numeric(Recpt_NonManuf_services) +
                  sum_numeric(Value_Elec_Generat_sold) +
                  sum_numeric(Value_own_Const) +
                  sum_numeric(Net_Balan_Goodssold) +
                  sum_numeric(Rent_Rec_Plan_Mach) +
                  sum_numeric(Var_Stok_SemFinGoods) +
                  sum_numeric(Rent_Rec_Bldg) +
                  sum_numeric(Rent_Rec_land_etc) +
                  sum_numeric(Int_Received) +
                  sum_numeric(Sale_Val_Goods) +
                  sum_numeric(Oth_Sub)
              ),
            by = "DSL") %>%
  left_join(block_i %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(Purchase_value)),
            by = "DSL") %>%
  left_join(block_j %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(Gross_salevalue),
                Average_Net_Sales_Value = mean(Per_unit_Netsale_value, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(Ex_FactvalQtyManft)
              ),
            by = "DSL")

data_frame_2015_16 <- data_frame_2015_16 %>% rename(Plant_ID = DSL)

data_frame_2015_16 <- data_frame_2015_16 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2015_16 <- data_frame_2015_16 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



### 2016-17 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201617Study_", block)]])
}

# Rename DSL
block_a <- block_a %>% rename(DSL = dsl)
block_j <- block_j %>% rename(DSL = dsl)

data_frame_2016_17 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  select(DSL, Industry_Code = ind_cd_return, Direct_Export_Share = expshare, Production_Cost = costop,
         Multiplier = mult) %>%
  left_join(block_c %>% 
              mutate(DSL = as.numeric(DSL)) %>%
              filter(sno == 10) %>%
              select(DSL,
                Total_Gross_Closing = GrossValueClose,
                Total_Net_Closing = NVC), 
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(receiptsFromManufServices) +
                  sum_numeric(receiptsFromNonManufServices) +
                  sum_numeric(ValueInElectGenAndSold) +
                  sum_numeric(ValueOfOwnConstruction) +
                  sum_numeric(NetBalofGoodsSoldInSameCond) +
                  sum_numeric(RentRecdForPlantAndMachAndOtherF) +
                  sum_numeric(VarInstockOfsemiFinishedGoods) +
                  sum_numeric(RentRecdForbuildings) +
                  sum_numeric(RentRecdForLandOnLease) +
                  sum_numeric(IntRecd) +
                  sum_numeric(SaleValOfGoodsSoldInTheSameCondi) +
                  sum_numeric(OtherProd_Subsidies)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(PurchValue)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(j_i7),
                Average_Net_Sales_Value = mean(j_i12, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(j_i13)
              ),
            by = "DSL")

data_frame_2016_17 <- data_frame_2016_17 %>% rename(Plant_ID = DSL)

data_frame_2016_17 <- data_frame_2016_17 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2016_17 <- data_frame_2016_17 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



### 2017-18 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201718Study_", block)]])
}

# Rename to DSL
block_a <- block_a %>% rename(DSL = a1)

# List of block names
block_names <- c("b", "c", "d")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("a", block, "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}
block_names <- c("e", "f", "g", "h","i")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("A", toupper(block), "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}
block_j <- block_j %>% rename(DSL = J01)


data_frame_2017_18 <- block_a %>%
  select(DSL, Industry_Code = a5, Direct_Export_Share = expshare, Production_Cost = costop,
         Multiplier = mult) %>%
  mutate(DSL = as.numeric(DSL)) %>%
  left_join(block_c %>% 
              mutate(DSL = as.numeric(DSL)) %>%
              filter(c_11 == 10) %>%
              select(DSL,
                Total_Gross_Closing = c_17,
                Total_Net_Closing = c_113
              ), 
            by = "DSL") %>%
  left_join(block_e %>%
              mutate(DSL = as.numeric(DSL)) %>%
              filter(E11 == 9) %>%
              select(DSL,
                Total_Man_Days = E15,
                Average_Person_Worked = E16,
                Total_Mandays_Paid = E17
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              mutate(DSL = as.numeric(DSL)) %>%
              
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(F1) +
                          sum_numeric(F2A) +
                          sum_numeric(F2B) +
                          sum_numeric(F3) +
                          sum_numeric(F4) +
                          sum_numeric(F5) +
                          sum_numeric(F6) +
                          sum_numeric(F7) +
                          sum_numeric(F8) +
                          sum_numeric(F9) +
                          sum_numeric(F10) +
                          sum_numeric(F11)),
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(G1) +
                  sum_numeric(G2) +
                  sum_numeric(G3) +
                  sum_numeric(G4) +
                  sum_numeric(G5) +
                  sum_numeric(G6) +
                  sum_numeric(G7) +
                  sum_numeric(G8) +
                  sum_numeric(G9) +
                  sum_numeric(G10) +
                  sum_numeric(G11) +
                  sum_numeric(G12)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(I16)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(J17),
                Average_Net_Sales_Value = mean(J112, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(J113)
              ),
            by = "DSL")

data_frame_2017_18 <- data_frame_2017_18 %>% rename(Plant_ID = DSL)

data_frame_2017_18 <- data_frame_2017_18 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2017_18 <- data_frame_2017_18 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



### 2018-19 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201819Study_", block)]])
}

block_a <- block_a %>% rename(DSL = A1)
block_names <- c("b","c","d","e", "f", "g", "h","i","j")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("A", toupper(block), "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}



data_frame_2018_19 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  
  select(DSL, Industry_Code = A5, Direct_Export_Share = EXPSHARE, Production_Cost = COSTOP,
         Multiplier = MULT) %>%
  left_join(block_e %>%
              mutate(DSL = as.numeric(DSL)) %>%
              filter(E11 == 9) %>%
              select(DSL,
                Total_Man_Days = E15,
                Average_Person_Worked = E16,
                Total_Mandays_Paid = E17
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(F1) +
                          sum_numeric(F2A) +
                          sum_numeric(F2B) +
                          sum_numeric(F3) +
                          sum_numeric(F4) +
                          sum_numeric(F5) +
                          sum_numeric(F6) +
                          sum_numeric(F7) +
                          sum_numeric(F8) +
                          sum_numeric(F9) +
                          sum_numeric(F10) +
                          sum_numeric(F11)),
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(G1) +
                  sum_numeric(G2) +
                  sum_numeric(G3) +
                  sum_numeric(G4) +
                  sum_numeric(G5) +
                  sum_numeric(G6) +
                  sum_numeric(G7) +
                  sum_numeric(G8) +
                  sum_numeric(G9) +
                  sum_numeric(G10) +
                  sum_numeric(G11) +
                  sum_numeric(G12)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(I16)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(J17),
                Average_Net_Sales_Value = mean(J112, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(J113)
              ),
            by = "DSL")

data_frame_2018_19 <- data_frame_2018_19 %>% rename(Plant_ID = DSL)

data_frame_2018_19 <- data_frame_2018_19 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2018_19 <- data_frame_2018_19 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))


### 2019-20 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI201920Study_", block)]])
}


block_a <- block_a %>% rename(DSL = a1)

# List of block names
block_names <- c("b", "c", "d")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("a", block, "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}
block_names <- c("e", "f", "g", "h","i","j")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("A", toupper(block), "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}



data_frame_2019_20 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  select(DSL, Industry_Code = a5, Direct_Export_Share = expshare, Production_Cost = costop,
         Multiplier = mult) %>%
  left_join(block_c %>% 
              mutate(DSL = as.numeric(DSL)) %>%
              filter(c_11 == 10) %>%
              select(DSL,
                Total_Gross_Closing = c_17,
                Total_Net_Closing = c_113
              ), 
            by = "DSL") %>%
  left_join(block_e %>%
              mutate(DSL = as.numeric(DSL)) %>%
              filter(E11 == 9) %>%
              select(DSL,
                Total_Man_Days = E15,
                Average_Person_Worked = E16,
                Total_Mandays_Paid = E17
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(F1) +
                          sum_numeric(F2A) +
                          sum_numeric(F2B) +
                          sum_numeric(F3) +
                          sum_numeric(F4) +
                          sum_numeric(F5) +
                          sum_numeric(F6) +
                          sum_numeric(F7) +
                          sum_numeric(F8) +
                          sum_numeric(F9) +
                          sum_numeric(F10) +
                          sum_numeric(F11) +
                          sum_numeric(F12) +
                          sum_numeric(F13)),
                  
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(G1) +
                  sum_numeric(G2) +
                  sum_numeric(G3) +
                  sum_numeric(G4) +
                  sum_numeric(G5) +
                  sum_numeric(G6) +
                  sum_numeric(G7) +
                  sum_numeric(G8) +
                  sum_numeric(G9) +
                  sum_numeric(G10) +
                  sum_numeric(G11) +
                  sum_numeric(G12)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(I16)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(J17),
                Average_Net_Sales_Value = mean(J112, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(J113)
              ),
            by = "DSL")

data_frame_2019_20 <- data_frame_2019_20 %>% rename(Plant_ID = DSL)

data_frame_2019_20 <- data_frame_2019_20 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2019_20 <- data_frame_2019_20 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))

### 2020-21 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI202021Study_", block)]])
}


block_a <- block_a %>% rename(DSL = a1)

# List of block names
block_names <- c("b", "c", "d")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("a", block, "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}
block_names <- c("e", "f", "g", "h","i","j")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("A", toupper(block), "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}



data_frame_2020_21 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  select(DSL, Industry_Code = a5, Direct_Export_Share = expshare, Production_Cost = costop,
         Multiplier = mult) %>%
  left_join(block_c %>% 
              mutate(DSL = as.numeric(DSL)) %>%
              filter(c_11 == 10) %>%
              select(DSL, 
                Total_Gross_Closing = c_17,
                Total_Net_Closing = c_113
              ), 
            by = "DSL") %>%
  left_join(block_e %>%
              mutate(DSL = as.numeric(DSL)) %>%
              filter(E11 == 9) %>%
              select(DSL,
                Total_Man_Days = E15,
                Average_Person_Worked = E16,
                Total_Mandays_Paid = E17
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(F1) +
                          sum_numeric(F2A) +
                          sum_numeric(F2B) +
                          sum_numeric(F3) +
                          sum_numeric(F4) +
                          sum_numeric(F5) +
                          sum_numeric(F6) +
                          sum_numeric(F7) +
                          sum_numeric(F8) +
                          sum_numeric(F9) +
                          sum_numeric(F10) +
                          sum_numeric(F11) +
                          sum_numeric(F12) +
                          sum_numeric(F13)),
            
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(G1) +
                  sum_numeric(G2) +
                  sum_numeric(G3) +
                  sum_numeric(G4) +
                  sum_numeric(G5) +
                  sum_numeric(G6) +
                  sum_numeric(G7) +
                  sum_numeric(G8) +
                  sum_numeric(G9) +
                  sum_numeric(G10) +
                  sum_numeric(G11) +
                  sum_numeric(G12)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(I16)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(J17),
                Average_Net_Sales_Value = mean(J112, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(J113)
              ),
            by = "DSL")

data_frame_2020_21 <- data_frame_2020_21 %>% rename(Plant_ID = DSL)

data_frame_2020_21 <- data_frame_2020_21 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2020_21 <- data_frame_2020_21 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



### 2021-22 ###

blocks <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

for (block in blocks) {
  assign(paste0("block_", tolower(block)), list_data[[paste0("ASI202122Study_", block)]])
}


block_a <- block_a %>% rename(DSL = a1)

# List of block names
block_names <- c("b", "c", "d")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("a", block, "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}
block_names <- c("e", "f", "g", "h","i","j")

# Loop through each block
for (block in block_names) {
  # Get the current block data frame
  current_block <- get(paste0("block_", block))
  original_col_name <- paste0("A", toupper(block), "01")
  current_block <- current_block %>% rename(DSL = !!sym(original_col_name))
  assign(paste0("block_", block), current_block)
}



data_frame_2021_22 <- block_a %>%
  mutate(DSL = as.numeric(DSL)) %>%
  select(DSL, Industry_Code = a5, Direct_Export_Share = expshare, Production_Cost = costop, 
         Multiplier = mult) %>%
  left_join(block_c %>% 
              mutate(DSL = as.numeric(DSL)) %>%
              filter(c_11 == 10) %>%
              select(DSL,
                Total_Gross_Closing = c_17,
                Total_Net_Closing = c_113
              ), 
            by = "DSL") %>%
  left_join(block_e %>%
              mutate(DSL = as.numeric(DSL)) %>%
              filter(E11 == 9) %>%
              select(DSL,
                Total_Man_Days = E15,
                Average_Person_Worked = E16,
                Total_Mandays_Paid = E17
              ),
            by = "DSL") %>%
  left_join(block_f %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Expenses = sum_numeric(F1) +
                          sum_numeric(F2A) +
                          sum_numeric(F2B) +
                          sum_numeric(F3) +
                          sum_numeric(F4) +
                          sum_numeric(F5) +
                          sum_numeric(F6) +
                          sum_numeric(F7) +
                          sum_numeric(F8) +
                          sum_numeric(F9) +
                          sum_numeric(F10) +
                          sum_numeric(F11) +
                          sum_numeric(F12) +
                          sum_numeric(F13)),
            
            by = "DSL") %>%
  left_join(block_g %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Total_Receipts = sum_numeric(G1) +
                  sum_numeric(G2) +
                  sum_numeric(G3) +
                  sum_numeric(G4) +
                  sum_numeric(G5) +
                  sum_numeric(G6) +
                  sum_numeric(G7) +
                  sum_numeric(G8) +
                  sum_numeric(G9) +
                  sum_numeric(G10) +
                  sum_numeric(G11) +
                  sum_numeric(G12)),
            by = "DSL") %>%
  left_join(block_i %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(Total_Imported_Purchases = sum_numeric(I16)),
            by = "DSL") %>%
  left_join(block_j %>%
              mutate(DSL = as.numeric(DSL)) %>%
              group_by(DSL) %>%
              summarize(
                Gross_Sales_Value = sum_numeric(J17),
                Average_Net_Sales_Value = mean(J112, na.rm = TRUE),
                Total_Ex_Factory_Value_Output = sum_numeric(J113)
              ),
            by = "DSL")

data_frame_2021_22 <- data_frame_2021_22 %>% rename(Plant_ID = DSL)

data_frame_2021_22 <- data_frame_2021_22 %>%
  mutate(Domestic_Purchases = Production_Cost - Total_Imported_Purchases)

data_frame_2021_22 <- data_frame_2021_22 %>%
  mutate(across(where(is.numeric) & !c(Plant_ID, Multiplier), ~ . * Multiplier))



## Write Data
data_frames <- list("2014_15" = data_frame_2014_15, "2015_16" = data_frame_2015_16, "2016_17" = data_frame_2016_17, 
                    "2017_18" = data_frame_2017_18, "2018_19" = data_frame_2018_19, "2019_20" = data_frame_2019_20, 
                    "2020_21" = data_frame_2020_21, "2021-22" = data_frame_2021_22)

## Define the directory where you want to save the files
output_directory <- ""

## Loop through the list and write each data frame to a CSV file
for(year in names(data_frames)) {
  file_path <- paste0(output_directory, paste0(year, ".csv")) 
  write.csv(data_frames[[year]], file_path, row.names = TRUE) 
}

