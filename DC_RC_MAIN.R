####Library######
library(tidyverse)
library(tidygeocoder)
library(fuzzyjoin)
library(data.table)
library(lme4)
library(lfe)
library(plm)
library(clipr)
library(stringr)
library(stargazer)
library(rstatix)
library(openxlsx)
library(stringr)
library(sf)
library(tigris)   # for easy shapefile download
library(factoextra)
library(cluster)
library(factoextra)
#setwd("C:/Users/josep/OneDrive/Documents/SCF/OneDrive-2023-10-16")
setwd("C:/Users/jdean/Downloads/OneDrive-2023-10-16")

df <- read.csv("ITSPE.csv", na.strings = c(""))
#test <- read.csv("Merged3.0.csv")
#df2 <- read.csv("CAMA.csv")
C1 <- read.csv("CAMA_res.csv") # Residential Cama
C2 <- read.csv("Computer_Assisted_Mass_Appraisal_Commercial.csv") # Commercial Cama
df2 <- read.csv("Address_RESIDENTIAL_Units.csv") # ARU used to count addresses
df3 <- read.csv("Condo_Regime.csv", na.strings = c("NA")) # Condo
df4 <- read.csv("Condo_Relate_Table.csv") # Condo
Condo_Approval_Lots <- read.csv("Condo_Approval_Lots.csv") # Condo
df6 <- read.csv("Common_Ownership_Lots.csv") # Condo
#gc <- read.csv("final-merged.csv")
C3 <- read.csv("CAMA_CONDO.csv") # Condo CAMA
ADDS <- read.csv("Full_Dataset_Addresses_Formatted.csv") # Formattated Addresses redundant 


ADDS <- ADDS %>%
  rename(PROPERTY_ADDRESS = PREMISEADD,
         OWNER_ADDRESS = ADDRESS1) %>%
  group_by(SSL) %>%
  slice(1) %>%
  ungroup()

#MSSL <- read.csv("MULTIPLE_SSL_APT.csv")

aff_projs <- read.csv("projects.csv") %>%
  select(proj_addre)
colnames(aff_projs) <- toupper(colnames(aff_projs))
aff_projs <- aff_projs %>%
  mutate(across(where(is.character), toupper))

# Tax rolls dataset. creates property type
ITSPE <- df %>%
  mutate(PROPERTY_TYPE_GROUPING = case_when((USECODE == 21 | USECODE == 22 | USECODE == 29) ~ 1,
                                            (USECODE == 15 | USECODE == 16 | USECODE == 17) ~ 2,
                                            (USECODE == 24 | USECODE == 25) ~ 3,
                                            (USECODE == 26 | USECODE == 27 | USECODE == 28 | USECODE == 126 | USECODE == 127) ~ 4,
                                            USECODE == 23 ~ 5,
                                            (USECODE == 216 | USECODE == 217) ~ 6,
                                            (USECODE == 11 | USECODE == 12 | USECODE ==  13 | USECODE == 19) ~ 7,
                                            TRUE ~ 8
  ),
  PROPERTY_TYPE_NAME = case_when(PROPERTY_TYPE_GROUPING == 1 ~ "Apartments",
                                 PROPERTY_TYPE_GROUPING == 2 ~ "Condo",
                                 PROPERTY_TYPE_GROUPING == 3 ~ "Conversion",
                                 PROPERTY_TYPE_GROUPING == 4 ~ "Cooperative",
                                 PROPERTY_TYPE_GROUPING == 5 ~ "Flat",
                                 PROPERTY_TYPE_GROUPING == 6 ~ "Investment Condos",
                                 PROPERTY_TYPE_GROUPING == 7 ~ "Single-family",
                                 PROPERTY_TYPE_GROUPING == 8 ~ "Other"),
  RESIDENTIAL = case_when(PROPERTY_TYPE_GROUPING %in% 1:7 ~ 1,
                          PROPERTY_TYPE_GROUPING == 8 ~ 0)
  #RENTAL_UNIT = case_when((HSTDCODE == 1 | HSTDCODE == 5)~ 0,
  # TRUE ~ 1
  # ),
  # Creates a variable to indicate if owner is an investor
  #investor = ifelse(grepl(paste(c("LLC", "LP", "INC", "L.P."), collapse = "|"), OWNERNAME), 1, 0),
  
  )

# Addresses are not formatted the same way in ITSPE and ARU.
# First we remove the last digits of the full zip code then create a new addy variable NEWADDY
#ITSPE <- ITSPE %>%
#filter(RESIDENTIAL==1) %>%
#mutate(NEWADDY = sub("^(.*\\b\\w{2})$", "\\1", PREMISEADD))

# Separates the citystatezipcode by keeping everything before the "-" becoming "Zip" and the digits become "Unused"
#separate(CITYSTZIP, into = c("Zip", "Unused"), sep = "-", remove = FALSE) %>%
# Adds street address ADDRESS1 with citystatezip "Zip"
#unite(NEWADDY, ADDRESS1, Zip, sep = " ", na.rm = TRUE, remove = FALSE) %>%
# If owner addy is same property addy
#mutate(SameADDY = if_else(PREMISEADD==NEWADDY, 1,0,missing= NULL))

CAMA <- C1 %>% 
  select(NUM_UNITS, SSL, AYB, STORIES, GBA, EYB, YR_RMDL, GBA, GRADE_D) %>%
  rename(LIVING_GBA = GBA)
#%>%  mutate(MSSL = if_else(SSL %in% MSSL$SSL,1,0)) %>%group_by(SSL) %>%filter(MSSL == 1 | (MSSL != 1 & n() == 1)) %>% ungroup()





CAMA_CONDO <- C3 %>%
  select(SSL, ROOMS, BEDRM, AYB, EYB, YR_RMDL, LIVING_GBA)

#%>% mutate(MSSL = if_else(SSL %in% MSSL$SSL,1,0)) %>%group_by(SSL) %>%filter(MSSL == 1 | (MSSL != 1 & n() == 1)) %>%ungroup()


CAMA_COMMERCIAL <- C2 %>%
  select(SSL, NUM_UNITS, AYB, EYB,LIVING_GBA, GRADE_D)
#%>%  mutate(MSSL = if_else(SSL %in% MSSL$SSL,1,0)) %>%group_by(SSL) %>% filter(MSSL == 1 | (MSSL != 1 & n() == 1)) %>% ungroup()



test <- bind_rows(CAMA, CAMA_COMMERCIAL, CAMA_CONDO)


merged_res <-  left_join(ITSPE, test, by = "SSL", copy = FALSE,
                         keep = FALSE)

merged_res <- merged_res %>%
  add_count(SSL) %>%
  filter(n == 1 | (n>1 & PROPERTY_TYPE_GROUPING == 1)) %>%
  select(-n)

############################################
# Format address to be compatible with ARU and Altos
merged_res <- merged_res %>%
  filter(RESIDENTIAL == 1) %>%
  #removes the city and zip code
  mutate(ADDY = sub("^(.*?(NW|NE|SW|SE)).*$", "\\1", PREMISEADD)) %>%
  unite(ADDRESS,LOWNUMBER, STREETNAME, QDRNTNAME , sep = " ", remove = FALSE)


#########################

# We need to replace the street names in Addy to be compatible with later code 
ADDY <- merged_res$ADDY
ADDY <- trimws(ADDY)



# Mapping table of abbreviations and full forms
mapping_table <- data.frame(abbreviation = c("ST", "AVE", "DR", "PL", "RD", "TER", "SQUARE", "BLVD", "CT", "PKWY","CIR", "ALY", "WAY", "LN", "WALK", "ROW"),
                            full_form = c("STREET", "AVENUE", "DRIVE", "PLACE", "ROAD", "TERRACE",  "SQUARE", "BOULEVARD", 
                                          "COURT", "PARKWAY", "CIRCLE", "ALLEY", "WAY", "LANE", "WALK", "ROW"),
                            stringsAsFactors = FALSE)

# Function to replace street type abbreviations
replace_abbreviations <- function(ADDY, mapping_table) {
  # Extract the street type and quadrant using regular expressions
  street_type <- sub(".*\\b(\\w+)\\s+\\w{2}$", "\\1", ADDY)
  quadrant <- sub(".*\\b(\\w{2})$", "\\1", ADDY)
  
  # Check if the street type abbreviation is in the mapping table
  if (street_type %in% mapping_table$abbreviation) {
    # Find the corresponding full form in the mapping table
    full_form <- mapping_table$full_form[mapping_table$abbreviation == street_type]
    
    # Replace the street type abbreviation with the full form
    modified_address <- sub(paste0("\\b", street_type, "\\b"), full_form, ADDY, ignore.case = TRUE)
    
    # Replace the quadrant with capitalized letters
    modified_address <- sub("\\b\\w{2}$", toupper(quadrant), modified_address)
    
    return(modified_address)
  } 
  
  return(ADDY)
}


#test <- as.data.frame(modified_addresses)
# Apply the replace_abbreviations function to the addresses
modified_addresses <- sapply(ADDY, replace_abbreviations, mapping_table = mapping_table)
t <- as.data.frame(modified_addresses)

t1 <- bind_cols(merged_res, t)




################ARU#################################

# ARU contains each housing unit in DC thus we can add up each unit at each address
ARU <- df2 %>%
  group_by(PRIMARY_ADDRESS) %>%
  mutate(Unit_Count = n()) %>%
  ungroup() %>%
  rename(ADDY = PRIMARY_ADDRESS) %>%
  #getting distinct apt value but leaving condo since we only need apt buildings
  filter(UNIT_TYPE != "CONDO") %>%
  distinct(ADDY, .keep_all = TRUE) %>%
  bind_rows(df2 %>% filter(UNIT_TYPE == "CONDO"))

ARU_MULTI <- ARU %>%
  #filter(UNIT_TYPE == "NON CONDO" & ADDY != "") %>%
  select(ADDY, Unit_Count) %>%
  rename(modified_addresses=ADDY)

########Creating master unit variable###############################

# Append open data file to ARU
temp4 <- left_join(t1, ARU_MULTI, by= ("modified_addresses"), copy = FALSE,
                   keep = FALSE)

temp4 <- read.csv("temp4.csv")
# load section 8 dataset
S8 <- read.csv("Section8.csv") 
Section8 <- S8 %>%
  rename(ADDY = PREMISEADD) %>%
  mutate(ADDY = toupper(ADDY),
         SECTION8 = 1)


### we need to create a unit variable.####
#depending on the type of property, we have to use different unit counts that were provided by the 3 CAMA files or created above in the ARU section

temp4 <- temp4 %>%
  mutate(UNITS_MASTER = 0)

temp4 <- temp4 %>%
  mutate(UNITS_MASTER = case_when(PROPERTY_TYPE_GROUPING == 1 ~ coalesce(NUM_UNITS, Unit_Count),
                                  PROPERTY_TYPE_GROUPING == 3 ~ coalesce(NUM_UNITS, Unit_Count),
                                  PROPERTY_TYPE_GROUPING == 2|PROPERTY_TYPE_GROUPING == 6 | PROPERTY_TYPE_GROUPING == 7 ~ 1,
                                  TRUE ~ coalesce(NUM_UNITS, Unit_Count))) 
# Removes multi SSLs for apartment units according to rule and all other multi SSLs
temp4 <- temp4 %>%
  group_by(SSL) %>%
  arrange(desc(UNITS_MASTER)) %>%
  mutate(keep = case_when(PROPERTY_TYPE_GROUPING == 1 & row_number() == 1 ~ 1,
                          PROPERTY_TYPE_GROUPING == 1 & row_number() != 1 ~ 0,
                          TRUE ~ 1)) %>%
  ungroup() %>%
  filter(keep == 1) %>%
  select(-keep) %>%
  group_by(SSL) %>%
  add_count(SSL) %>%
  filter(n == 1) %>%
  ungroup() %>%
  select(-n)


### OLS Model ####

# LM model for apts
# Handling NAs if needed - filtering out rows without unit counts
train_data <- temp4[!is.na(temp4$UNITS_MASTER),]
test_data <- temp4[is.na(temp4$UNITS_MASTER),]

train_data <- temp4[!is.na(temp4$UNITS_MASTER),]
test_data <- temp4[is.na(temp4$UNITS_MASTER),]
train_data <- train_data %>%
  filter(PROPERTY_TYPE_GROUPING == 1)
test_data <- test_data %>%
  filter(PROPERTY_TYPE_GROUPING == 1)
# Building a linear model
model_apts <- lm(UNITS_MASTER ~ LIVING_GBA  , data = train_data)


### LM model for flats
# Handling NAs if needed - filtering out rows without unit counts
train_data <- temp4[!is.na(temp4$UNITS_MASTER),]
test_data <- temp4[is.na(temp4$UNITS_MASTER),]

train_data <- train_data %>%
  filter(PROPERTY_TYPE_GROUPING == 5)
test_data <- test_data %>%
  filter(PROPERTY_TYPE_GROUPING == 5)
# Building a linear model
model_flats <- lm(UNITS_MASTER ~ STORIES + LIVING_GBA, data = train_data)

summary(model_flats)

# Calculate Mean for later
mean_unit_master_apts <- temp4 %>%
  filter(PROPERTY_TYPE_GROUPING == 1, !is.na(UNITS_MASTER)) %>%
  summarise(mean_units = mean(UNITS_MASTER)) %>%
  pull(mean_units)

mean_unit_master_conversions <- temp4 %>%
  filter(PROPERTY_TYPE_GROUPING == 3, !is.na(UNITS_MASTER)) %>%
  summarise(mean_units = mean(UNITS_MASTER)) %>%
  pull(mean_units)

mean_unit_master_flats <- temp4 %>%
  filter(PROPERTY_TYPE_GROUPING == 5, !is.na(UNITS_MASTER)) %>%
  summarise(mean_units = mean(UNITS_MASTER)) %>%
  pull(mean_units)

#### Applies regression to missing data ####
temp4 <- temp4 %>%
  mutate(UNITS_MASTER = case_when(
    # Check for group 1 and NA in UNITS_MASTER
    PROPERTY_TYPE_GROUPING == 1 & is.na(UNITS_MASTER) ~ predict(model_apts, .),
    TRUE ~ UNITS_MASTER)) %>%
  mutate(UNITS_MASTER = case_when(
    # Check for group 1 and NA in UNITS_MASTER
    PROPERTY_TYPE_GROUPING == 1 & (is.na(UNITS_MASTER) | UNITS_MASTER == 0) ~ mean_unit_master_apts,
    TRUE ~ UNITS_MASTER)) %>%
  
  mutate(UNITS_MASTER = case_when(
    # Check for group 1 and NA in UNITS_MASTER
    PROPERTY_TYPE_GROUPING == 5 & is.na(UNITS_MASTER) ~ predict(model_flats, .),
    TRUE ~ UNITS_MASTER)) %>%
  mutate(UNITS_MASTER = case_when(
    # Check for group 5 and NA in UNITS_MASTER
    PROPERTY_TYPE_GROUPING == 5 & (is.na(UNITS_MASTER) | UNITS_MASTER == 0) ~ mean_unit_master_flats,
    TRUE ~ UNITS_MASTER)) %>% 
  mutate(UNITS_MASTER = case_when(
    # Check for group 3 and NA in UNITS_MASTER
    PROPERTY_TYPE_GROUPING == 3 & (is.na(UNITS_MASTER) | UNITS_MASTER == 0) ~ mean_unit_master_conversions,
    TRUE ~ UNITS_MASTER)) 
##### temp5##########

# Formatted addresses for owners
ADDS <- read.csv("Full_Dataset_Addresses_Formatted.csv")
ADDS <- ADDS %>%
  rename(PROPERTY_ADDRESS = PREMISEADD,
         OWNER_ADDRESS = ADDRESS1) %>%
  group_by(SSL) %>%
  slice(1) %>%
  ungroup()


temp5 <- left_join(temp4, ADDS, by = c("SSL"))
temp6 <- left_join(temp5, Section8, by = "ADDY")

# Creates a variable for multi-family buildings sharing the same address
temp7 <- temp6 %>%
  group_by(ADDY) %>%
  mutate(DUP_ADDY = n()) %>%
  ungroup() %>%
  # Flag duplicates based on the given conditions
  mutate(DUPLICATE_ADDRESS = case_when(
    (PROPERTY_TYPE_GROUPING %in% c(1, 3, 5)) & (DUP_ADDY > 1) ~ 1,
    TRUE ~ 0
  ))


merge2 <- temp7 %>%
  # remove multi family buildings with same address
  filter(DUPLICATE_ADDRESS == 0) %>%
  mutate(
    Owners_Street_Number = str_extract(OWNER_ADDRESS, "^\\d+[A-Za-z]?"),
    Property_Street_Number = str_extract(PROPERTY_ADDRESS, "^\\d+[A-Za-z]?"),
    # If owner address and property address are same. street addresss and full address
    MATCH1 = if_else(Owners_Street_Number == Property_Street_Number, 1, 0, missing = 0),
    MATCH2 = if_else(OWNER_ADDRESS == PROPERTY_ADDRESS, 1, 0, missing = 0),
    # Either or
    MATCH = if_else(MATCH1 == 1 | MATCH2 == 1, 1, 0, missing = 0),
    HCODENA = if_else(is.na(HSTDCODE), 1, 0),
    # Homestead exemption code 
    HCODE15 = if_else(HSTDCODE == 1 | HSTDCODE == 5, 1, 0, missing = 0), # Fixed missing '=' here,
    # if owner address and property address arent the same or homestead exemption code given then owner occ 
    # differs from RENTAl which is based on different logic  
    OWNER_OCCUPIED = if_else(HCODE15 == 1 | MATCH == 1, 1, 0, missing = 0),
    # building size
    BUILDINGS_SIZE1 = case_when((UNITS_MASTER > 0 & UNITS_MASTER < 5) ~ "Under 5",
                                (UNITS_MASTER >= 5 & UNITS_MASTER <= 9) ~ "5 to 9",
                                (UNITS_MASTER > 9 & UNITS_MASTER <= 19) ~ "10 to 19",
                                (UNITS_MASTER > 19 & UNITS_MASTER <= 49) ~ "20 to 49",
                                (UNITS_MASTER > 49 & UNITS_MASTER <= 99) ~ "50 to 99",
                                (UNITS_MASTER > 99) ~ "100 or more",
                                TRUE ~ "Zero or Unknown"),
    BUILDINGS_SIZE = case_when(BUILDINGS_SIZE1 == "Under 5" ~ 1,
                               BUILDINGS_SIZE1 == "5 to 9"  ~ 2,
                               BUILDINGS_SIZE1 == "10 to 19" ~ 3,
                               BUILDINGS_SIZE1 == "20 to 49" ~ 4,
                               BUILDINGS_SIZE1 == "50 to 99" ~ 5,
                               BUILDINGS_SIZE1 == "100 or more" ~ 6,
                               BUILDINGS_SIZE1 == "Zero or Unknown" ~ 7),
    # checks if building is in section8 dataset 
    SUBSIDIZED = if_else(SECTION8 == 1, 1, 0, missing = 0)
  ) %>%
  mutate(
    # Rental variable
    RENTAL = case_when(
      PROPERTY_TYPE_GROUPING == 1 ~ 1,
      PROPERTY_TYPE_GROUPING == 4 ~ 0,
      PROPERTY_TYPE_GROUPING == 3 ~ if_else(HCODE15 == 0,1,0),
      PROPERTY_TYPE_GROUPING %in% c(2,5,6,7) ~ if_else(OWNER_OCCUPIED == 0, 1,0),
      TRUE ~ 0),
    MULTIFAMILY = if_else(PROPERTY_TYPE_GROUPING %in% c(1,3,5),1,0,missing = 0),
    
    REG_ERA = case_when(
      PROPERTY_TYPE_GROUPING == 1 ~ case_when(
        AYB < 1976 & AYB != 0 ~ "Built before 1976",
        AYB == 1977 | AYB == 1976 ~ "Built in 1976 or 1977",
        AYB >= 1978 & AYB < 2007 ~ "Unregulated (1978 to 2007)",
        AYB >= 2007 ~ "Subject to IZ requirements (2007-)",
        TRUE ~ "Unknown"
      ),
      PROPERTY_TYPE_GROUPING %in% c(3,5) ~ case_when(
        AYB < 1976 & AYB !=0 ~ "Built before 1976",
        AYB == 1977 | AYB == 1976 ~ "Built in 1976 or 1977",
        AYB >= 1978 & AYB < 2007  ~ "Unregulated (1978 to 2007)",
        AYB >= 2007 ~ "Subject to IZ requirements (2007-)",
        TRUE ~ "Unknown"
      ),
      PROPERTY_TYPE_GROUPING %in% c(7) ~ case_when(
        AYB < 1976 & AYB != 0 ~ "Built before 1976",
        AYB == 1977 | AYB == 1976 ~ "Built in 1976 or 1977",
        AYB >= 1978 & AYB < 2007  ~ "Unregulated (1978 to 2007)",
        AYB >= 2007 ~ "Subject to IZ requirements (2007-)",
        TRUE ~ "Unknown",
      ),
      PROPERTY_TYPE_GROUPING %in% c(2) ~ case_when(
        AYB < 1976 & AYB != 0 ~ "Built before 1976",
        AYB == 1977 | AYB == 1976 ~ "Built in 1976 or 1977",
        AYB >= 1978 & AYB < 2007  ~ "Unregulated (1978 to 2007)",
        AYB >= 2007 ~ "Subject to IZ requirements (2007-)",
        TRUE ~ "Unknown",
      ),
      PROPERTY_TYPE_GROUPING %in% c(6) ~ case_when(
        AYB < 1976 & AYB != 0 ~ "Built before 1976",
        AYB == 1977 | AYB == 1976 ~ "Built in 1976 or 1977",
        AYB >= 1978 & AYB < 2007  ~ "Unregulated (1978 to 2007)",
        AYB >= 2007 ~ "Subject to IZ requirements (2007-)",
        TRUE ~ "Unknown",
      ),
      TRUE ~ "Unknown"
    )
    
  ) %>%
  # publicly owned variable
  mutate(PUBLICLY_OWNED = case_when(
    str_detect(OWNERNAME, "\\b(DISTRICT OF COLUMBIA|UNITED STATES|TRANSIT|TRANSITY)\\b") ~ 1,
    TRUE ~ 0)) %>%
  # Non Taxable
  mutate(NON_TAXABLE = if_else(str_detect(MIX1TXTYPE, "^E[0-9]$") | MIX1TXTYPE == "US" | MIX1TXTYPE == "DC", 1,0, missing = 0)) %>%
  # Count the number of rental units owned by the same owner using owner's address (ADDRESS1)
  group_by(ADDRESS1) %>%
  mutate(Total_Units1 = sum(if_else(RENTAL == 1, UNITS_MASTER, 0), na.rm = TRUE)) %>%
  ungroup()%>% 
  # If owner owns less than 5 units
  mutate(U51 = if_else(Total_Units1 < 5, 1, 0)) %>%
  # Creates natural person variable. searches for the legal identifier in owner name.  if it doesnt include an identifier in list then it is owned by natural person. if owner name is missing then its assumed to be not natural person 
  mutate(NATURALP = if_else(
    str_detect(OWNERNAME, "\\b(CHURCH|ORGANIZATION|ENTERPRISES|ASSOC|UNITED|MINISTRIES|PRTNSHP|CHRUCH|
               INSTUTUTE|\\bLC\\b|ASSOC\\.|GROUP|DEVELOPERS|ESTATES|AMERICAN|AMERICA|\\bCO\\b|MINISTRY|COMPANIES|CITIZEN|JOINT|COMMUNITY|ARMY|
               INVESTMENTS|PARTNERS|ASSOCS|&|VENTURES|VENTURE|EMPLOYEES|ALLIANCE|ENDEAVORS|PROPERTY|MANAGEMENT|INCORPORATED|ESTATE|DEVELOPMENT|HOUSING|CATHOLIC|
               TRUSTEES|COALITION|COMPANY|ASSOCIATES|LLP|US|L\\.P\\.|L P|HOLDINGS|APARTMENTS|PROPERTIES|AND|FOUNDATION|ASSOCIATION|INTERNATIONAL|LEAGUE|CAUCUS|L\\.L\\.C|&|
               LLC|INC|CORPORATION|LTD|LIMITED|LP|CORP|PARTNERSHIP|LIMITED|TRUST|TRUSTEE|COLLEGE|UNIVERSITY|GOVERNMENT|EMBASSY|REPUBLIC|NAVAJO|DISTRICT|CITY|SOCIETY|INSTITUTE|
               COUNCIL|FEDERATION|UNION|COOPERATIVE|FUND|BUREAU|AGENCY|COMMISSION|AUTHORITY|CONSORTIUM|GUILD|CHAMBER|ORDER|SYNDICATE|NGO|NPO)\\b")
    ,0,1, missing = 0)) %>%
  # If owner owns less than 5 units and is natural person then 1
  mutate(U5RULE = if_else(U51 == 1 & NATURALP == 1, 1,0)) %>%
  # Rent control applies if building/unit was built before 1976 OR if year built is unknown AND if No other exemption applies
  mutate(
    RENT_CONTROLLED = if_else(
      (REG_ERA == "Unknown"  | REG_ERA == "Built before 1976") & NON_TAXABLE == 0 & U5RULE == 0 & PUBLICLY_OWNED == 0 & SUBSIDIZED == 0, 1,0, missing = NA
      
    ),
    NOT_RENT_CONTROLLED = if_else(
      REG_ERA != "Built before 1976" | NON_TAXABLE == 1 | U5RULE == 1  | PUBLICLY_OWNED == 1 | SECTION8 == 1, 1,0, missing = 0
    )
  ) %>%
  # Number of units owner owns
  mutate(
    OWNER_SIZE = case_when(
      (Total_Units1 > 0 & Total_Units1 < 5) ~ "Under 5",
      (Total_Units1 >= 5 & Total_Units1 <= 9) ~ "5 to 9",
      (Total_Units1 > 9 & Total_Units1 <= 19) ~ "10 to 19",
      (Total_Units1 > 19 & Total_Units1 <= 49) ~ "20 to 49",
      (Total_Units1 > 49 & Total_Units1 <= 99) ~ "50 to 99",
      (Total_Units1 > 99) ~ "100 or more",
      TRUE ~ "Zero or Unknown"),
    # Year built, differs from Reg era due to 0s
    YEAR_BUILT = AYB) %>%
  # Removes multi-family 
  filter(!(PROPERTY_TYPE_GROUPING %in% c(3,5) & UNITS_MASTER <= 1))


write.csv(merge2, "merge2.csv")

##### Altos Data ####

# single family merged by geocode
mf1 <- read.csv("Finalmerged_file.csv")


mf1 <- mf1 %>%
  select(2,26:54)

ALTOS <- read.csv("Combined_Final_Data.csv")

GEODATA <- read.csv("DC_RC_GEOCODEDALL.csv")

GEODATA <- GEODATA %>%
  select(SSL, X, Y)


GEODATA <- GEODATA %>%
  add_count(SSL) %>%
  filter(n==1) %>%
  select(SSL, X, Y)


######## We need to replace the street####

df <- ALTOS %>%
  mutate(street_address = toupper(street_address),
         ID = row_number())

df <- AIRBNB %>%
  mutate(street_address = toupper(formatted_address),
         ID = row_number())

ADDY <- df$street_address
ADDY <- trimws(ADDY)
ADDY <- df$street_address
ADDY <- trimws(ADDY)


# Mapping table of abbreviations and full forms
mapping_table <- data.frame(full_form = c("ST", "AVE", "DR", "PL", "RD", "TER", "SQUARE", "BLVD", "CT", "PKWY","CIR", "ALY", "WAY", "LN", "WALK", "ROW", "SOUTHWEST", "SOUTHEAST", "NORTHEAST", "NORTHWEST"),
                            abbreviation = c("STREET", "AVENUE", "DRIVE", "PLACE", "ROAD", "TERRACE",  "SQUARE", "BOULEVARD", 
                                             "COURT", "PARKWAY", "CIRCLE", "ALLEY", "WAY", "LANE", "WALK", "ROW", "SW", "SE", "NE", "NW"),
                            stringsAsFactors = FALSE)

# Function to replace street type abbreviations
replace_abbreviations <- function(ADDY, mapping_table) {
  # Extract the street type and quadrant using regular expressions
  street_type <- sub(".*\\b(\\w+)\\s+\\w{2}$", "\\1", ADDY)
  quadrant <- sub(".*\\b(\\w{2})$", "\\1", ADDY)
  
  # Check if the street type abbreviation is in the mapping table
  if (street_type %in% mapping_table$abbreviation) {
    # Find the corresponding full form in the mapping table
    full_form <- mapping_table$full_form[mapping_table$abbreviation == street_type]
    
    # Replace the street type abbreviation with the full form
    modified_address <- sub(paste0("\\b", street_type, "\\b"), full_form, ADDY, ignore.case = TRUE)
    
    # Replace the quadrant with capitalized letters
    modified_address <- sub("\\b\\w{2}$", toupper(quadrant), modified_address)
    
    return(modified_address)
  } 
  
  return(ADDY)
}

#test <- as.data.frame(modified_addresses)
# Apply the replace_abbreviations function to the addresses
modified_addresses <- sapply(ADDY, replace_abbreviations, mapping_table = mapping_table)
t <- as.data.frame(modified_addresses)

t2 <- bind_cols(df, t)


t1 <- t2 %>%
  mutate(ADDY = sub("^(.*?(NW|NE|SW|SE)).*$", "\\1", modified_addresses)) %>%
  rename(UNITNUMBER = unit_name) %>%
  mutate(ID = row_number())


#################Combining Rent Control and Altos#############################
# Setting up the rent control data to be merged 
#merge2 <- read.csv("merge2.csv")
merge2 <- read.csv("Rent_Control.csv")

RC1 <- merge2

RC <- RC1 %>% 
  select(-X) %>%
  mutate(UNIQUEID = row_number())


# Merging apartments, conversions, and flats
RC135 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5))
# Merging condos
RC26 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(2,6) & !is.na(UNITNUMBER)) %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)

# Merging single-family houses
RC7 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 7) %>%
  distinct(ADDY, .keep_all = TRUE)

# We have to merge multi-family structures unit to building so we can merge the 
# entire ALTOS dataset with only use codes for apartment buildings, flats and conversions
# TO merge houses we are doing a building to building merge so we need unique addresses to merge with
# For condos we do both the address and the unit number because this is a unit to unit merge. So address and unit numbers must be unique

t2 <- t1 %>%
  distinct(ADDY, .keep_all = TRUE) %>%
  rename(X = geo_lat,
         Y = geo_long)
t3 <- t1 %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)%>%
  rename(X = geo_lat,
         Y = geo_long)

RC$ADD
# merging separately by property type
ALTOS_MULTI <- inner_join(t1, RC135, by = "ADDY") 
ALTOS_CONDO <- inner_join(t3, RC26, by = c("ADDY", "UNITNUMBER"), relationship = 'one-to-one', keep = FALSE)
ALTOS_HOUSES <- inner_join(t2, RC7, by = "ADDY", relationship = 'one-to-one', keep = FALSE)

# Bind rows
A1 <- bind_rows(ALTOS_MULTI,ALTOS_CONDO, ALTOS_HOUSES)

# Merge single family geocoded merged addresses with main merged file and remove duplicates.
A2 <- A1 %>%
  mutate(DATASET = 1)

ALTOS_SFU <- inner_join(mf1, RC, by = "SSL")
ALTO_SFU <- ALTOS_SFU %>%
  mutate(DATASET = 2)



A3 <- bind_rows(A2, ALTO_SFU)

ALTOS_COMBINED <-  A3 %>%
  group_by(SSL) %>%
  #removes the units that have been merged by geocode that were already merged by address
  filter(!(DATASET == 2 & any(DATASET == 1))) %>%
  ungroup()


# Find rows in A that are not in B based on UNIQUEID
ANTI_ALTOS <- anti_join(t1, ALTOS_COMBINED, by = "ID")
ANTI_RC <- anti_join(RC, ALTOS_COMBINED, by = "UNIQUEID")

# View the result
head(unique_not_in_B)

write.csv(ANTI_RC, "ANTI_RC.csv")

#ANTI_CONDO <- anti_join(t3, RC26, by = c("ADDY", "UNITNUMBER"))

#RC26GEO <- left_join(RC26, GEODATA, by = "SSL")
#ANTI_RC1 <- anti_join(RC26GEO, t3 , by = c("ADDY", "UNITNUMBER"))


# Converting the date column to Date format
A$date <- as.Date(A$date)

# Extracting the year and creating a new column
A$year <- format(A$date, "%Y")
A$year <- as.numeric(A$year)


write.csv(ALTOS_COMBINED, "ALTOS_COMBINED.csv")
A <- read.csv("ALTOS_COMBINED.csv")


A2.5sd <- ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)

A2sd <- ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)

A <- A %>%
  mutate(LIU = case_when(
    laundry_in_unit == "False" ~ 0,
    laundry_in_unit == "True" ~ 1,
    laundry_in_unit == 0 ~ 0,
    laundry_in_unit == 1 ~ 1,
    TRUE ~ 0
    
  ),
  CA = case_when(
    cats_allowed == "False" ~ 0,
    cats_allowed == "True" ~ 1,
    TRUE ~ 0
  ),
  DA = case_when(
    dogs_allowed == "False" ~ 0,
    dogs_allowed == "True" ~ 1,
    TRUE ~ 0
  ),
  
  POB = case_when(
    patio_or_balcony == "False" ~ 0,
    patio_or_balcony == "True" ~ 1,
    TRUE ~ 0
  ),
  
  AC = case_when(
    air_conditioning == "False" ~ 0,
    air_conditioning == "True" ~ 1,
    TRUE ~ 0
  ),
  
  POOL = case_when(
    pool == "False" ~ 0,
    pool == "True" ~ 1,
    TRUE ~ 0
  ),
  
  FC  = case_when(
    fitness_center == "False" ~ 0,
    fitness_center == "True" ~ 1,
    TRUE ~ 0
  ),
  ONSP = case_when(
    on_street_parking == "False" ~ 0,
    on_street_parking == "True" ~ 1,
    TRUE ~ 0
  ),
  
  OFFSP = case_when(
    off_street_parking == "False" ~ 0,
    off_street_parking == "True" ~ 1,
    TRUE ~ 0
  )
  )

write.csv(A , "Altos_RentControl_Merged_Full.csv")

A<- read.csv("Altos_RentControl_Merged_Full.csv")
######## Tables ##############
# Install and load the 'psych' package if you haven't already
install.packages("psych")
library(psych)

A <- ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)
  
  

# Get detailed summary statistics
describe(ALTOS_COMBINED$average_rent)
A %>%
  select(average_rent) %>%
  summary()
summary(A$average_rent)

A %>%
  filter(year == 2023) %>%
  summarise(n())

RC %>%
  filter(DELCODE == "N" & PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  group_by(PRMS_WARD) %>%
  summarise(sum(UNITS_MASTER))

#write.csv(ALTOS_COMBINED,"Altos_RentControl_Merged.csv")

## Altos Coverage
ALTOS_COMBINED %>%
  filter(year == 2023 & beds %in% 0:3 & PROPERTY_TYPE_GROUPING %in% c(3,5)) %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  group_by(PRMS_WARD) %>%
  summarise(n())

# Rent quartiles by ward and unit size
ALTOS_COMBINED %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(n()) 

# Rent quartiles by ward and unit size

A %>%
  filter(year == 2023 & beds %in% 0:3 & PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(PRMS_WARD) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            sum
            .groups = 'drop') 

RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1 & DELCODE == "N") %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(
    sum(UNITS_MASTER),
    .groups = 'drop'
  )

# PSQWardBedsAPT
ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED, beds) %>%
  summarise(mean_rent = mean(PSQ, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = RENT_CONTROLLED, values_from = mean_rent, 
              names_prefix = "RC_") %>%
  rename(RC = RC_1, NRC = RC_0)


#25Q 50Q 75Q Price by Ward and Rent Control Apartments beds
A %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(#mean_rent = mean(average_rent, na.rm = TRUE), 
            Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop')

#25Q 50Q 75Q Price by Ward and Rent Control
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise( 
            Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') 


# Rent Quantiles by Ward and Rent Control Single Family
ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING %in% c(2,6,7)) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(mean_rent = mean(average_rent, na.rm = TRUE), 
            Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') %>%
  pivot_wider(names_from = RENT_CONTROLLED, values_from = c(mean_rent, Q25, Q50, Q75))




# Single Versus Multi Ward 
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  mutate(MFSINGLE = case_when(PROPERTY_TYPE_GROUPING %in% c(2,6,7) ~ "Single",
                              PROPERTY_TYPE_GROUPING %in% c(1,3,5) ~ "Multi",
                              TRUE ~ "Other")) %>%
  group_by(PRMS_WARD, MFSINGLE) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') 
# Apartments by Neighborhood




# Shadow Versus Rental Apts Ward 
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
  group_by(PRMS_WARD, SHADOW) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            median(floor_size, na.rm = TRUE),
            n(),
            n_distinct(property_id),
            
            .groups = 'drop')


# 
FD <- read.csv("Full_Dataset.csv")

FD1 <- FD %>%
  select(SSL, OWNER_SIZE, Total_Units1)
ALTOS_SPECIAL <- left_join(ALTOS_COMBINED, FD1, by = "SSL")

colnames(A)
A %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING %in% c(1,3,5) & beds %in% 0:3) %>%
  mutate(Size = case_when(Total_Units1 < 20 ~ "Small",
                          Total_Units1 %in% 20:50 ~ "Medium",
                          Total_Units1 > 50 ~ "Large")) %>%
  mutate(Size1 = case_when(OWNERSIZE_MASTER < 20 ~ "Small",
                           OWNERNAME_MASTER >= 20 ~ "Not Small")) %>%
  filter(!is.na(Size1)) %>%
  group_by(PROPERTY_TYPE_NAME,Size1) %>%
  summarise(median(average_rent, na.rm = TRUE),
            median(average_rent, na.rm = TRUE)/median(floor_size, na.rm = TRUE),
            median(floor_size, na.rm = TRUE),
            n())

A %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING %in% c(1,3,5) & beds %in% 0:3) %>%
  mutate(Size = case_when(Total_Units1 < 20 ~ "Small",
                           Total_Units1 %in% 20:50 ~ "Medium",
                           Total_Units1 > 50 ~ "Large")) %>%
  mutate(Size1 = case_when(Total_Units1 < 20 ~ "Small",
                          Total_Units1 >= 20 ~ "Not Small")) %>%
  #filter(Size == "Small") %>%
  group_by(RENT_CONTROLLED,Size1) %>%
  summarise(min(average_rent, na.rm = TRUE),
            median(floor_size, na.rm = TRUE),
            n())


RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1 & DELCODE == "N") %>%
  group_by(PRMS_WARD) %>%
  summarise(sum(UNITS_MASTER))


A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  #mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
  mutate(Size = case_when(Total_Units1 < 20 ~ "Small",
  Total_Units1 >= 20 ~ "Not Small")) %>%
  filter(Size == "Small") %>%
  group_by(MULTIFAMILY, beds) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            n(),
            .groups = 'drop')

A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  group_by() %>%
  summarise( 
    min = min(average_rent),
    Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
    Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
    mean = mean(average_rent,na.rm= TRUE),
    Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
    max = max(average_rent),
    std = sd(average_rent),
    n())

A %>%
  filter(year == 2023 & beds %in% 0:3 & YEAR_BUILT != 0) %>%
  #mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
  mutate(Size = case_when(OWNERSIZE_MASTER < 20 ~ "Small",
                          Total_Units1 >= 20 ~ "Not Small")) %>%
  filter(Size == "Small") %>%
  mutate(BUILT = case_when(YEAR_BUILT < 1920 ~ "Before 1920",
                           YEAR_BUILT %in% 1920:1940 ~ "1920:1940",
                           YEAR_BUILT %in% 1940:1960 ~ "1940-1960",
                           YEAR_BUILT %in% 1960:1980 ~ "1960-1980",
                           YEAR_BUILT %in% 1980:2000 ~ "1980-2000",
                           YEAR_BUILT %in% 2000:2010 ~ "2000-2010",
                           YEAR_BUILT > 2010 ~ "2010-")) %>%
  group_by(beds) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            n(),
            .groups = 'drop')

write.table(

  RC %>%
    filter(PROPERTY_TYPE_GROUPING == 1 & DELCODE == "N") %>%
    group_by(PRMS_WARD, RENT_CONTROLLED) %>%
    summarise(
      sum(UNITS_MASTER),
      .groups = 'drop'
    )
  
  
  , "clipboard", sep = "\t")


#A <- read.csv("Altos_RentControl_Merged_Full.csv")

# Calculate the price change for each unit and then compute the average price change #####
average_price_change <- ALTOS %>%
  add_count(unit_id) %>%
  filter(n>1) %>%
  select(-n) %>%
  group_by(unit_id) %>%
  arrange(desc(year)) %>%
  mutate(price_change = first(average_rent) - last(average_rent),
         price_change_percent = 100*price_change/first(average_rent),
         PCHANGEY = first(year) - last(year),
         price_change_adjusted = price_change/PCHANGEY,
         price_change_adjusted_percent = price_change_percent/PCHANGEY) %>%
  ungroup() %>%
  group_by(unit_id) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  ungroup() 

average_price_change %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)%>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(PCHANGEY)%>%
  summarise(n())

average_price_change %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)%>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & price_change_adjusted != 0) %>%
  group_by(RENT_CONTROLLED) %>%
  summarise(mean(price_change_adjusted, na.rm = TRUE),
            mean(PCHANGEY),
            
            n())

average_price_change %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)%>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(year) %>%
  summarise(n())

average_price_change %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500)%>%
  filter(year == 2023) %>%
  group_by(PCHANGEY) %>%
  summarise(n())




################# CRF Units to Condo Entity by assigning entity Regime######

df <- read.csv("Altos_RentControl_Merged.csv")

# df3 is condo regime file
data <- df3 %>%
  select(REGIME, COUNT)


# Group by REGIME and sum the COUNT
CRF <- data %>%
  filter(!is.na(REGIME)) %>%
  group_by(REGIME) %>%
  summarise(COUNT = sum(COUNT))


#CRT is condo relate table
# CRT SSL to Regime and Regime to lot
CRT <- df4 %>%
  select(SSL, REGIME) 


# CRT MAT_SSL to SSL from COL
COL <- df6 %>%
  select(SSL)



tp <- inner_join(CRT, CRF, by = "REGIME")
tp1 <- left_join(tp, CRF, by = "REGIME")

tp2 <- tp1 %>%
  add_count(SSL) %>%
  filter(n == 1) %>%
  select(-n)


# merge temp2 with merged ITSPE/CAMA file AKA merged_res
temp3 <- left_join(df, tp2, by = 'SSL', copy = FALSE, keep = FALSE)

NO_REGIME <- temp3 %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(2,6) & is.na(REGIME)) 



###################
# Building a linear model
model <- lm(average_rent ~ floor_size + beds + baths, data = train_data)

Asub <- A %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1)

model <- lmer(average_rent ~ RENT_CONTROLLED +  floor_size + beds + baths + (1 | zip), data = Asub)


model1 <- lm(average_rent ~ RENT_CONTROLLED + floor_size + beds + baths + LIU + CA +DA+POB+AC+POOL+FC+ONSP+OFFSP, data = Asub)
model <- lm(average_rent ~ RENT_CONTROLLED + floor_size + beds + baths , data = Asub)


summary(model1)
coef(random)

stargazer::stargazer(model1)



fixed <- plm(average_rent ~ RENT_CONTROLLED +  floor_size + beds + baths, data=Asub, index=c("zip"), model="within")  #fixed model
random <- plm(average_rent ~ RENT_CONTROLLED +  floor_size + beds + baths, data=Asub, index=c("zip"), model="random")  #random model
phtest(fixed,random) #Hausman test

summary(fixed)
#######################

NON <- anti_join(ALTOS, A, by = "unit_id")

FD <- read.csv("Full_Dataset.csv")
FD1 <- read.csv("merge2.csv") 

test <- semi_join(FD, FD1, by = "SSL")

########################



write.csv(Apartments, "Apartments.csv", row.names = FALSE)
?write.csv()
geocoded <- Apartments %>%
  tidygeocoder::geocode(
    address = PREMISEADD,
    method = "osm"
  )

##############################

library(tidyverse)
library(tidycensus)

setwd("C:/Users/jdean/Downloads/OneDrive-2023-10-16")


BBL <- read.csv("Basic_Business_Licenses.csv")
BBL <- readxl::read_xlsx("Basic_Business_Licenses.xlsx")
RC <- read.csv("merge2.csv")

BBL_RES <- BBL %>%
  filter(LICENSESTATUS %in% c("Active", "Active-4YR", "Active4Yr", "Active4YR","Active4YRR") & LICENSE_CATEGORY_TEXT == "Housing: Residential") %>%
  select(1:6,8:10,LICENSECATEGORY,ENTITY_NAME,LICENSE_END_DATE,LICENSE_START_DATE,LICENSESTATUS, CITY, STATE, ZIP, LONGITUDE, LATITUDE)

BBL_RES_ALL <- BBL %>%
  filter(LICENSE_CATEGORY_TEXT == "Housing: Residential") %>%
  select(1:6,8:10,LICENSECATEGORY,ENTITY_NAME,LICENSE_END_DATE,LICENSE_START_DATE,LICENSESTATUS, CITY, STATE, ZIP)


colnames(BBL)
colnames(BBL_RES)

print(BBL %>%
        filter(LICENSESTATUS %in% c("Active", "Active-4YR", "Active4Yr", "Active4YR","Active4YRR")) %>%
        distinct(LICENSE_CATEGORY_TEXT), n= 100)

print(
  BBL %>%
    #filter(LICENSESTATUS %in% c("Active", "Active-4YR", "Active4Yr", "Active4YR")) %>%
    distinct(LICENSESTATUS), n = 50)

print(
  BBL_RES %>%
    #filter(LICENSESTATUS %in% c("Active", "Active-4YR", "Active4Yr", "Active4YR")) %>%
    distinct(LICENSESTATUS), n = 50)

print(BBL_RES %>%
        filter(LICENSESTATUS %in% c("Active", "Active-4YR", "Active4Yr", "Active4YR","Active4YRR")) %>%
        distinct(LICENSECATEGORY), n= 100)

BBL_RES %>%
  summarise(n())

BBL_RES %>%
  group_by(LICENSECATEGORY) %>%
  summarise(n())

BBL_RES %>%
  summarise("Unique Addresses" = n_distinct(SITE_ADDRESS),
            "Total Addresses" = n())

print(
  BBL_RES %>%
    select(SITE_ADDRESS, BILLING_ADDRESS), n = 50)

BBL_RES %>%
  group_by(STATE) %>%
  summarise(n())



t1 <- BBL_RES %>%
  select(SITE_ADDRESS) %>%
  rename(ADDY = SITE_ADDRESS) %>%
  add_count(ADDY) %>%
  mutate(keep = if_else(n == 1,1,0,missing = 0)) %>%
  filter(keep == 1) 

RC1 <- RC %>%
  rename(ADDY1 = ADDY,
         ADDY = ADDRESS1)


test <- inner_join(RC1, t1, by = "ADDY", keep = TRUE)

test %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())

test %>%
  group_by(RENTAL) %>%
  summarise(n())

head(
  test %>%
    select(ADDY1, ADDY.x, RENTAL) %>%
    rename("OWNERS ADDRESS" = ADDY1,
           "Property ADDRESS" = ADDY.x),
  n = 50
  
)

BBL_RES_ALL %>%
  add_count(SITE_ADDRESS) %>%
  filter(n > 1) %>%
  summarise(n())

print(
  BBL_RES_ALL %>%
    add_count(SITE_ADDRESS) %>%
    filter(n > 1) %>%
    group_by(n) %>%
    summarise(n()), n = 50)

BBL_RES %>%
  group_by(BBL_CITY_STATE_ZIP) %>%
  summarise(n())


OCC <- read.csv("Certificate_of_Occupancy.csv")
OCC %>%
  summarise(n_distinct(SSL))

test1 <- inner_join(RC, OCC, by = "SSL")

print(
  OCC %>%
    group_by(APPROVED_BUILDING_CODE_USE) %>%
    summarise(n()), n = 380)

test1 %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())


t1 <- BBL_RES %>%
  add_count(SITE_ADDRESS) %>%
  filter(n > 1) %>%
  arrange(desc(SITE_ADDRESS))

write.csv(BBL_RES, "Basic_Business_Licenses_Residential.csv")

write.table

###################
colnames(RC)

ExternalRC <- RC %>%
  select(-1,-2,-Unit_Count, -Unnamed..0, -PROPERTY_ADDRESS, -UNIQUEID,
         OWNER_ADDRESSS8, property_total_unit_count, 
         DUP_ADDY, DUPLICATE_ADDRESS, -MATCH1, -MATCH2, -MATCH,
         -Owners_Street_Number, -Property_Street_Number, -HCODENA, 
         -HCODE15, -BUILDINGS_SIZE, -NOT_RENT_CONTROLLED, -RENTAL_UNIT, -RESIDENTIAL, -investor,
         -modified_addresses, -ROOMS, -GRADE_D,-BEDRM, -YR_RMDL) %>%
  rename(BUILDING_SIZE = BUILDINGS_SIZE1,
         UNITS_OWNED = Total_Units1,
         )

write.csv(ExternalRC, "External_RC_Dataset.csv")

ExternalRC %>% 
  group_by(BUILDING_SIZE) %>%
  summarise(n())
head(
RC %>%
  select(OWNER_ADDRESS, ADDRESS1) %>%
  filter(OWNER_ADDRESS != ADDRESS1), n = 50)



write.table(
  ExternalRC %>% 
    group_by(BUILDING_SIZE) %>%
    summarise(n())
  , "clipboard", sep = "\t")

small_owners <- merge2 %>%
  filter(Total_Units1 %in% 5:19 & PROPERTY_TYPE_GROUPING %in% c(1,3,5) & RENTAL == 1 & DELCODE == "N") %>%
  select(PREMISEADD, OWNERNAME, OWNER_ADDRESS, Total_Units1, PROPERTY_TYPE_NAME) %>%
  rename("Property Address" = PREMISEADD,
         "Owner Name" = OWNERNAME,
         "Owner Address" = OWNER_ADDRESS,
         "Number of Units Owned" = Total_Units1,
         "Property Type" = PROPERTY_TYPE_NAME) %>%
  group_by("Property Type") %>%
  summarise(n())

write.csv(small_owners, "small_owner.csv")

###########Agent############

KNOWN_RENTALS <- read.csv("MergedWithBusinessLicense.csv")
ALL_YEARS_FINAL <- read.csv("AllYears_Final.csv")
FUZZY <- read.csv("FUZZYMATCHEDANDOTHERMATCHED.csv")


RC1 <- KNOWN_RENTALS

RC <- RC1 %>% 
  select(-X) %>%
  mutate(UNIQUEID = row_number())


# Merging apartments, conversions, and flats
RC135 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5))
# Merging condos
RC26 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(2,6) & !is.na(UNITNUMBER)) %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)

RC26A <- RC26

RC26 %>%
  select(ADDY)
  

# Merging single-family houses
RC7 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 7) %>%
  distinct(ADDY, .keep_all = TRUE)


t2 <- t1 %>%
  distinct(ADDY, .keep_all = TRUE) %>%
  rename(X = geo_lat,
         Y = geo_long)
t3 <- t1 %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)%>%
  rename(X = geo_lat,
         Y = geo_long)


# merging separately by property type
ALTOS_MULTI <- inner_join(t1, RC135, by = "ADDY") 
ALTOS_CONDO <- inner_join(t1, RC26, by = c("ADDY"))
ALTOS_HOUSES <- inner_join(t2, RC7, by = "ADDY", relationship = 'one-to-one', keep = FALSE)


# Bind rows
A1 <- bind_rows(ALTOS_MULTI,ALTOS_CONDO, ALTOS_HOUSES)

colnames(KNOWN_RENTALS)

############Kurban###########


KNOWN_RENTALS %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())


A1 %>%
  group_by() %>%
  summarise(n())

KNOWN_RENTALS %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())

Apartments <- KNOWN_RENTALS %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  select(PREMISEADD,OWNER_ADDRESS,BILLING_ADDRESS_CITY_STATE_ZIP, OWNERNAME,AGENT_NAME,ENTITY_NAME) %>%
  rename("Property Address" = PREMISEADD,
         "Owner's Address (from ITSPE)" = OWNER_ADDRESS,
         "Billing Address (from BBL)" = BILLING_ADDRESS_CITY_STATE_ZIP,
         "Owner's Name (from ITSPE)" = OWNERNAME,
         "Agent Name (from BBL)" = AGENT_NAME,
         "Entity Name (from BBL)" = ENTITY_NAME
         ) %>%
  slice(272:278)

Flats <- KNOWN_RENTALS %>%
  filter(PROPERTY_TYPE_GROUPING == 5) %>%
  select(PREMISEADD,OWNER_ADDRESS,BILLING_ADDRESS_CITY_STATE_ZIP, OWNERNAME,AGENT_NAME,ENTITY_NAME) %>%
  rename("Property Address" = PREMISEADD,
         "Owner's Address (from ITSPE)" = OWNER_ADDRESS,
         "Billing Address (from BBL)" = BILLING_ADDRESS_CITY_STATE_ZIP,
         "Owner's Name (from ITSPE)" = OWNERNAME,
         "Agent Name (from BBL)" = AGENT_NAME,
         "Entity Name (from BBL)" = ENTITY_NAME
  ) %>%
  slice(18:28)

write.csv(Apartments, "apartments.csv")
write.csv(Flats, "flats.csv")
  
colnames(test)

merge2 %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())

BBL_RES %>%
  filter(LICENSECATEGORY == "Apartment") %>%
  group_by(LICENSESTATUS) %>%
  summarise(n())

colnames(FUZZY)
##################

FUZZY %>%
  add_count(OWNERNAME_CLEAN) %>%
  filter(n == 1 & PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  summarise(n())


T1 <- T %>%
  filter(RENTAL == 1) %>%
  select(OWNERNAME_CLEAN, NATURALP)

pattern <- "\\b(CHURCH|ORGANIZATION|ENTERPRISES|ASSOC|UNITED|MINISTRIES|PRTNSHP|CHRUCH|INSTUTUTE|\\bLC\\b|ASSOC\\.|GROUP|DEVELOPERS|ESTATES|AMERICAN|AMERICA|\\bCO\\b|MINISTRY|COMPANIES|CITIZEN|JOINT|COMMUNITY|ARMY|INVESTMENTS|PARTNERS|ASSOCS|&|VENTURES|VENTURE|EMPLOYEES|ALLIANCE|ENDEAVORS|PROPERTY|MANAGEMENT|INCORPORATED|ESTATE|DEVELOPMENT|HOUSING|CATHOLIC|TRUSTEES|COALITION|COMPANY|ASSOCIATES|LLP|US|L\\.P\\.|L P|HOLDINGS|APARTMENTS|PROPERTIES|AND|FOUNDATION|ASSOCIATION|INTERNATIONAL|LEAGUE|CAUCUS|L\\.L\\.C|&|LLC|INC|CORPORATION|LTD|LIMITED|LP|CORP|PARTNERSHIP|LIMITED|TRUST|TRUSTEE|COLLEGE|UNIVERSITY|GOVERNMENT|EMBASSY|REPUBLIC|NAVAJO|DISTRICT|CITY|SOCIETY|INSTITUTE|COUNCIL|FEDERATION|UNION|COOPERATIVE|FUND|BUREAU|AGENCY|COMMISSION|AUTHORITY|CONSORTIUM|GUILD|CHAMBER|ORDER|SYNDICATE|NGO|NPO)\\b"

T <- RC %>%
  mutate(NATURALP = if_else(
  str_detect(OWNERNAME, pattern),
  0, 1, missing = 0
)) %>%
  mutate(U5RULE1 = if_else(U51 == 1 & NATURALP == 1, 1,0)) %>%
  mutate(
    RENT_CONTROLLED = if_else(
      (REG_ERA == "Unknown"  | REG_ERA == "Built before 1976") & NON_TAXABLE == 0 & U5RULE1 == 0 & PUBLICLY_OWNED == 0 & SUBSIDIZED == 0, 1,0, missing = NA
      
    )
)

  T %>%
    filter(PROPERTY_TYPE_GROUPING %in% c(3,5)) %>%
    group_by(RENT_CONTROLLED) %>%
    summarise(sum(UNITS_MASTER))

T %>%
  filter(RENTAL == 1) %>%
  add_count(OWNERNAME_CLEAN) %>%
  mutate(morethanone = if_else(n > 1, 1,0, missing = 0)) %>%
  group_by(morethanone) %>%
  summarise(n())



RC <- read.csv("Rent_Control.csv")
COSTAR <- read.csv("CostarMultiFamily.csv")

write.csv(T, "Rent_Control.csv")


te <- RC %>%
  select(OWNERNAME, SSL, OWNER_ADDRESS, UNITS_MASTER, PROPERTY_TYPE_GROUPING)

CE1 <- read_xlsx("Book15.xlsx")
CE <- CE1 %>%
  mutate(CE = 1) 

test <- left_join(te, CE, by = "SSL")

T1 <- test %>%
  filter(PROPERTY_TYPE_GROUPING == 1 & is.na(`Owner Name`))
 

BBL <- read.csv("Basic_Business_Licenses_Residential.csv")
BBL <- read_xlsx("Basic_Business_Licenses.xlsx")

ON <- read.csv("MergedFileMasterOwnerName.csv", na.strings = "" )
ON2 <- read.csv("MergedFileMasterOwnerName1.csv", na.strings = "" )

ON1 <- ON %>%
  select(MasterOwnerName, SSL)

ON2 %>%
  filter(is.na(Owner.Name) & !is.na(MasterOwnerName)) %>%
  #filter(!is.na(MasterOwnerName)) %>%
  summarise(n())

RC1 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5,6))

RC1 <- left_join(RC, ON1, by = "SSL")
write.csv(RC1, "Rent_Control.csv")

RC1 <- RC %>%
  rename(OWNERNAME_COSTAR = MasterOwnerName,
         OWNERNAME_ITSPE = OWNERNAME) %>%
  mutate(OWNERNAME_MASTER = case_when(
    is.na(OWNERNAME_COSTAR) & !is.na(OWNERNAME_ITSPE) ~ OWNERNAME_ITSPE,
    !is.na(OWNERNAME_COSTAR) ~ OWNERNAME_COSTAR)) %>%
  group_by(OWNERNAME_COSTAR) %>%
  mutate(OWNERSIZE_COSTAR = if_else(is.na(OWNERNAME_COSTAR), NA, 
                                    sum(if_else(RENTAL == 1, UNITS_MASTER, 0), na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(OWNERNAME_ITSPE) %>%
  mutate(OWNERSIZE_ITSPE = if_else(is.na(OWNERNAME_ITSPE), NA, 
                                    sum(if_else(RENTAL == 1, UNITS_MASTER, 0), na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(OWNERNAME_MASTER) %>%
  mutate(OWNERSIZE_MASTER = if_else(is.na(OWNERNAME_MASTER), NA, 
                                   sum(if_else(RENTAL == 1, UNITS_MASTER, 0), na.rm = TRUE))) %>%
  
  ungroup()

colnames(ALTOS_COMBINED)

############ Combining Altos and Costar #######
RC <- read.csv("Rent_Control.csv")
Costar1 <- read.csv("CostarMultiFamily.csv")
A<- read.csv("Altos_RentControl_Merged_Full.csv")
CARC <- read.csv("Costar_Altos_RC.csv")
Costar <- Costar1 %>%
  mutate(SSLONE = gsub("-", "    ", SSLONE)) %>%
  rename(SSL = SSLONE)%>%
  group_by(SSL) %>%
  add_count(SSL) %>%
  filter(n == 1) %>%
  ungroup() %>%
  select(-n)


test <- inner_join(Costar, RC, by = "SSL")


test1 <- test %>%
  filter(!is.na(Avg.Effective.Unit) & year == 2023) 

test2 <- test %>%
  mutate(Effective = Avg.Effective.SF * floor_size,
         Asking = Avg.Asking.SF * floor_size)
#write.csv(test2, "Costar_Altos_RC.csv")
CARC <- read.csv("Costar_Altos_RC.csv")

test2 %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(#mean_rent = mean(average_rent, na.rm = TRUE), 
    Q25 = quantile(test, probs = .25, na.rm = TRUE),
    Q50 = quantile(test, probs = .5, na.rm = TRUE),
    Q75 = quantile(test, probs = .75, na.rm = TRUE),
    .groups = 'drop')

write.table(
  
  A %>%
    filter(year == 2023 & PROPERTY_TYPE_GROUPING ==1 & beds %in% 0:3) %>%
    mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
    mutate(Size = case_when(OWNERSIZE_MASTER %in% 1:19 ~ "Small",
                            OWNERSIZE_MASTER %in% 20:50 ~ "Medium",
                            OWNERSIZE_MASTER > 50 ~ "Large",
                            TRUE ~ "NA")) %>%
    mutate(Size1 = case_when(OWNERSIZE_MASTER < 20 ~ "Small",
                             OWNERSIZE_MASTER >= 20 ~ "Not Small")) %>%
    filter(Size != "NA" & !is.na(floor_size)) %>%
    group_by(Size, beds, RENT_CONTROLLED) %>%
    summarise(#Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
              Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
              #Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
              median(floor_size, na.rm = TRUE),
              n(),
              n_distinct(property_id),
              
              .groups = 'drop')
  
  
  
  , "clipboard", sep = "\t")


write.table(

  
  test2 %>%
    filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1  & beds %in% 0:3) %>%
    mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
    mutate(Size = case_when(OWNERSIZE_COSTAR %in% 1:19 ~ "Small",
                            OWNERSIZE_COSTAR %in% 20:50 ~ "Medium",
                            OWNERSIZE_COSTAR > 50 ~ "Large",
                            TRUE ~ "NA")) %>%
    mutate(Size1 = case_when(OWNERSIZE_MASTER < 20 ~ "Small",
                             OWNERSIZE_MASTER >= 20 ~ "Not Small")) %>%
    filter(Size != "NA" & !is.na(floor_size)) %>%
    group_by(beds) %>%
    summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
              Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
              Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
              n(),

              .groups = 'drop')
  
  , "clipboard", sep = "\t")


colnames(CARC)

CARC %>%
  filter(year == 2023  & beds %in% 0:3) %>%
  group_by(Building.Status) %>%
  summarise(n())

########## BBL and Altos ###########
KNOWN_RENTALS1 <- KNOWN_RENTALS %>%
  select(1:26)

test %>%
  group_by(SSL) %>%
  add_count(SSL) %>%
  ungroup() %>%
  group_by(n) %>%
  summarise(n())
ALTOS_RC <- read.csv("Altos_RentControl_Merged_Full.csv")

test <- left_join(KNOWN_RENTALS1, ALTOS_RC, by = "SSL")


##### last rented price ####
last_rented_prices <- read.csv("last_rented_prices.csv")

df <- last_rented_prices %>%
  mutate(street_address = toupper(street_address),
         ID = row_number())

ADDY <- df$street_address
ADDY <- trimws(ADDY)




# Mapping table of abbreviations and full forms
mapping_table <- data.frame(full_form = c("ST", "AVE", "DR", "PL", "RD", "TER", "SQUARE", "BLVD", "CT", "PKWY","CIR", "ALY", "WAY", "LN", "WALK", "ROW"),
                            abbreviation = c("STREET", "AVENUE", "DRIVE", "PLACE", "ROAD", "TERRACE",  "SQUARE", "BOULEVARD", 
                                             "COURT", "PARKWAY", "CIRCLE", "ALLEY", "WAY", "LANE", "WALK", "ROW"),
                            stringsAsFactors = FALSE)

# Function to replace street type abbreviations
replace_abbreviations <- function(ADDY, mapping_table) {
  # Extract the street type and quadrant using regular expressions
  street_type <- sub(".*\\b(\\w+)\\s+\\w{2}$", "\\1", ADDY)
  quadrant <- sub(".*\\b(\\w{2})$", "\\1", ADDY)
  
  # Check if the street type abbreviation is in the mapping table
  if (street_type %in% mapping_table$abbreviation) {
    # Find the corresponding full form in the mapping table
    full_form <- mapping_table$full_form[mapping_table$abbreviation == street_type]
    
    # Replace the street type abbreviation with the full form
    modified_address <- sub(paste0("\\b", street_type, "\\b"), full_form, ADDY, ignore.case = TRUE)
    
    # Replace the quadrant with capitalized letters
    modified_address <- sub("\\b\\w{2}$", toupper(quadrant), modified_address)
    
    return(modified_address)
  } 
  
  return(ADDY)
}

#test <- as.data.frame(modified_addresses)
# Apply the replace_abbreviations function to the addresses
modified_addresses <- sapply(ADDY, replace_abbreviations, mapping_table = mapping_table)
t <- as.data.frame(modified_addresses)

t2 <- bind_cols(df, t)


t1 <- t2 %>%
  mutate(ADDY = sub("^(.*?(NW|NE|SW|SE)).*$", "\\1", modified_addresses)) %>%
  rename(UNITNUMBER = unit_name) %>%
  mutate(ID = row_number())

###

RC1 <- merge2


RC <- RC1 %>% 
  select(-X) %>%
  mutate(UNIQUEID = row_number())


# Merging apartments, conversions, and flats
RC135 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5))
# Merging condos
RC26 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(2,6) & !is.na(UNITNUMBER)) %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)

# Merging single-family houses
RC7 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 7) %>%
  distinct(ADDY, .keep_all = TRUE)

# We have to merge multi-family structures unit to building so we can merge the 
# entire ALTOS dataset with only use codes for apartment buildings, flats and conversions
# TO merge houses we are doing a building to building merge so we need unique addresses to merge with
# For condos we do both the address and the unit number because this is a unit to unit merge. So address and unit numbers must be unique

t2 <- t1 %>%
  distinct(ADDY, .keep_all = TRUE) %>%
  rename(X = geo_lat,
         Y = geo_long)
t3 <- t1 %>%
  distinct(ADDY, UNITNUMBER, .keep_all = TRUE)%>%
  rename(X = geo_lat,
         Y = geo_long)


# merging separately by property type
ALTOS_MULTI <- inner_join(t1, RC135, by = "ADDY") 
ALTOS_CONDO <- inner_join(t3, RC26, by = c("ADDY", "UNITNUMBER"), relationship = 'one-to-one', keep = FALSE)
ALTOS_HOUSES <- inner_join(t2, RC7, by = "ADDY", relationship = 'one-to-one', keep = FALSE)

# Bind rows
A1 <- bind_rows(ALTOS_MULTI,ALTOS_CONDO, ALTOS_HOUSES)

# Converting the date column to Date format
A1$date <- as.Date(A1$date)

# Extracting the year and creating a new column
A1$year <- format(A1$date, "%Y")
A1$year <- as.numeric(A1$year)


###

B <- A1 %>%
  filter(abs(last_rented_price - mean(last_rented_price, na.rm = TRUE)) <= 2.5 * sd(last_rented_price, na.rm = TRUE)) %>%
  filter(last_rented_price > 500) %>%
  rename(average_rent = last_rented_price)

write.csv(B, "ALTO_RC_LASTP.csv")

B <- read.csv("ALTO_RC_LASTP.csv")
#A <- read.csv()
# Get detailed summary statistics
describe(ALTOS_COMBINED$average_rent)
A %>%
  select(average_rent) %>%
  summary()
summary(A$average_rent)

A %>%
  filter(year == 2023) %>%
  summarise(n())

RC %>%
  filter(DELCODE == "N" & PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  group_by(PRMS_WARD) %>%
  summarise(sum(UNITS_MASTER))

#write.csv(ALTOS_COMBINED,"Altos_RentControl_Merged.csv")

## Altos Coverage
ALTOS_COMBINED %>%
  filter(year == 2023 & beds %in% 0:3 & PROPERTY_TYPE_GROUPING %in% c(3,5)) %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  group_by(PRMS_WARD) %>%
  summarise(n())

# Rent quartiles by ward and unit size
ALTOS_COMBINED %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(n()) 

# Rent quartiles by ward and unit size
write.table(
B %>%
  filter(year == 2023 & beds %in% 0:3 & PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(PRMS_WARD , beds) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') 

, "clipboard", sep = "\t")

# PSQWardBedsAPT
ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED, beds) %>%
  summarise(mean_rent = mean(PSQ, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = RENT_CONTROLLED, values_from = mean_rent, 
              names_prefix = "RC_") %>%
  rename(RC = RC_1, NRC = RC_0)


#25Q 50Q 75Q Price by Ward and Rent Control Apartments beds

write.table(
B %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING == 1 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(#mean_rent = mean(average_rent, na.rm = TRUE), 
    Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
    Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
    Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
    .groups = 'drop')

, "clipboard", sep = "\t")


#25Q 50Q 75Q Price by Ward and Rent Control

write.table(
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise( 
    Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
    Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
    Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
    .groups = 'drop') 

, "clipboard", sep = "\t")

# Rent Quantiles by Ward and Rent Control Single Family
ALTOS_COMBINED %>%
  filter(abs(average_rent - mean(average_rent, na.rm = TRUE)) <= 2.5 * sd(average_rent, na.rm = TRUE)) %>%
  filter(average_rent > 500) %>%
  filter(year == 2023 & PROPERTY_TYPE_GROUPING %in% c(2,6,7)) %>%
  group_by(PRMS_WARD, RENT_CONTROLLED) %>%
  summarise(mean_rent = mean(average_rent, na.rm = TRUE), 
            Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') %>%
  pivot_wider(names_from = RENT_CONTROLLED, values_from = c(mean_rent, Q25, Q50, Q75))




# Single Versus Multi Ward 
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  mutate(MFSINGLE = case_when(PROPERTY_TYPE_GROUPING %in% c(2,6,7) ~ "Single",
                              PROPERTY_TYPE_GROUPING %in% c(1,3,5) ~ "Multi",
                              TRUE ~ "Other")) %>%
  group_by(PRMS_WARD, MFSINGLE) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            .groups = 'drop') 



# Shadow Versus Rental Apts Ward 
A %>%
  filter(year == 2023 & beds %in% 0:3) %>%
  mutate(SHADOW = if_else(PROPERTY_TYPE_GROUPING %in% c(2,3,5,6,7),1,0)) %>%
  group_by(PRMS_WARD, SHADOW) %>%
  summarise(Q25 = quantile(average_rent, probs = .25, na.rm = TRUE),
            Q50 = quantile(average_rent, probs = .5, na.rm = TRUE),
            Q75 = quantile(average_rent, probs = .75, na.rm = TRUE),
            median(floor_size, na.rm = TRUE),
            n(),
            n_distinct(property_id),
            
            .groups = 'drop')

################Real-Page###################
RC <- read.csv("Rent_Control.csv")
RP <- read.csv("RealPage.csv")
RP <- as.matrix(RP)

as.matrix()

LL <- RC3 %>%
  #filter(YEAR == 2023) %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  #filter(RENT_CONTROLLED == 1) %>%
  group_by(OWNERNAME_MASTER) %>%
  mutate(Total_Units1 = sum(if_else(RENTAL == 1, BuildingUnits, 0), na.rm = TRUE)) %>%
  ungroup()%>% 
  group_by(Total_Units1) %>%
  distinct(OWNERNAME_MASTER) %>%
  arrange(desc(Total_Units1)) %>%
  select(OWNERNAME_MASTER, Total_Units1)


LL <- RC3 %>%
  filter(YEAR == 2023) %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  filter(RENT_CONTROLLED == 0) %>%
  group_by(OWNERNAME_MASTER) %>%
  summarise(n())

write.csv(LL, "RP.csv")
test<- RC %>%
  select(OWNERNAME_MASTER, PREMISEADD)

#######New RC Altos########

AIRBNB <- read.csv("airbnb_dc_addresses_only.csv")

AIRBNB <- t1 %>%
  mutate(formatted_address = toupper(formatted_address),
         ADDY = sub("^(.*?(NW|NE|SW|SE)).*$", "\\1", formatted_address),
         CITY = "Washington",
         STATE = "DC",
         ZIP = str_extract(formatted_address, "\\d{5}(?=, USA)")) %>%
  select(ADDY, CITY, STATE, ZIP) %>%
  group_by(ADDY) %>%
  summarise(CITY = first(CITY),
            STATE = first(STATE),
            ZIP = first(ZIP))


write.csv(AIRBNB, "AIRBNB_ADDRESSES.csv")

RC1 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  distinct(ADDY, .keep_all = TRUE)



test <- inner_join(AIRBNB, RC1,  by = "ADDY")
test <- inner_join()
test1 <- inner_join(test, t1, by = "ADDY")

test %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())

library(Microsoft365R)
library(readxl)
setwd("C:/Users/josep/OneDrive/Documents/Economics")
setwd("C:/Users/jdean/OneDrive - Nat'l Community Reinvestment Coaltn/Documents/Research")

od <- get_personal_onedrive()
od$list_items("Documents/Rent Control/RC")
# Get the file object
file <- od$get_item("Documents/Rent Control/RC/NewRCAltos.csv")

# Download to a temporary file (won't clutter your PC)
tmp <- tempfile(fileext = ".csv")
file$download(tmp, overwrite = TRUE)

# Read a specific sheet
df <- read.csv(tmp)
# Converting the date column to Date format
A$date <- as.Date(A$date)

# Extracting the year and creating a new column
A$year <- format(A$date, "%Y")
A$year <- as.numeric(A$year)

RCALTOS <- read.csv("NewRCAltos.csv")
RCALTOS %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(n())

RCALTOS$date
# Extracting the year and creating a new column
test <- RCALTOS %>%
  mutate(new_date = ymd(date),
         year = year(new_date))

test %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  filter(year == "2023") %>%
  distinct(unit_id) %>%
  summarise(n())
  
head(RCALTOS %>%select(street_address,ADDY), n =50) 

RCALTOS %>%
  group_by(date)


####

ALTOS <- read.csv("altos_final_unit_all_features.csv")


RC <- read.csv("Rent_Control.csv")

RC1 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  distinct(ADDY, .keep_all = TRUE)
  
RC2 <- inner_join(t1, RC1, by = "ADDY")

#write.csv(RC2, "RC2.csv")
RC2 <- read.csv("RC2.csv")

t <- test1 %>%
  filter(YEAR == 2023 & beds %in% 0:3) %>%
  group_by(zip, beds, RENT_CONTROLLED) %>%
  summarise(
    n(),
    mean(last_listing_price),
    quantile(last_listing_price, .25),
    quantile(last_listing_price, .75),
    max(last_listing_price)
  )

write.table(
  test1 %>%
    filter(YEAR == 2023 & beds %in% 0:3) %>%
    group_by(zip, beds, RENT_CONTROLLED) %>%
    summarise(
      n(),
      mean(last_listing_price),
      quantile(last_listing_price, .25),
      quantile(last_listing_price, .75),
      max(last_listing_price)
    )
  
  
  ,sep= "/", row.names = FALSE
)

# Vector of years
years <- c(2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014)

# Define function
summarize_year <- function(year) {
  RC3 %>%
    filter(YEAR == year) %>%
    group_by(NBHDNAME, beds, RENT_CONTROLLED) %>%
    summarise(
      `25th` = quantile(last_listing_price, 0.25, na.rm = TRUE),
      Median = median(last_listing_price, na.rm = TRUE),
      Mean = mean(last_listing_price, na.rm = TRUE),
      `75th` = quantile(last_listing_price, 0.75, na.rm = TRUE),
      Max = max(last_listing_price, na.rm = TRUE),
      .groups = "drop"
    )
}

# Apply function to each year and store results in a named list
Summary_list <- lapply(years, summarize_year)
names(Summary_list) <- years

#write.xlsx(Summary_list, "NBHD_Beds_RC_Sum_Tables.xlsx")

?pivot_wider
tab <- xtable(RC3_summary, caption = "Summary Statistics", label = "tab:mytable")
print(tab, include.rownames = FALSE, booktabs = TRUE)


write_clip(
  RC3 %>%
    group_by(YEAR,beds, RENT_CONTROLLED) %>%
    summarise(
      n(),
      first(zip),
      first(NBHDNAME),
      "25th" = quantile(last_listing_price, .25),
      "Median" = median(last_listing_price),
      Mean = mean(last_listing_price),
      "75th" = quantile(last_listing_price, .75),
      Max = max(last_listing_price)
    )
  
)
head(test1 %>% select(BUILDINGS_SIZE1, UNITS_MASTER), n =25)

colnames(test1)
test1$YEAR_BUILT
######
options(scipen = 999)
#square foot
#number of units in building
#fixed effect
#census tract/nbhd
#number of weeks listed
rstatix::identify_outliers()
mutate(outlier_var = (var < quantile(var, 0.25, na.rm = TRUE) - 1.5 * IQR(var, na.rm = TRUE)) |
         (var > quantile(var, 0.75, na.rm = TRUE) + 1.5 * IQR(var, na.rm = TRUE)))

RC2 <- read.csv("RC2.csv")

RP <- read.csv("RealPage.csv")
RP <- as.matrix(RP)

OWNER <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(OWNERNAME_MASTER) %>%
  summarise(sum(UNITS_MASTER)
            )

A <- t1 %>%
  mutate(LIU = case_when(
    laundry_in_unit == "False" ~ 0,
    laundry_in_unit == "True" ~ 1,
    laundry_in_unit == 0 ~ 0,
    laundry_in_unit == 1 ~ 1,
    TRUE ~ 0
    
  ),
  CA = case_when(
    cats_allowed == "False" ~ 0,
    cats_allowed == "True" ~ 1,
    TRUE ~ 0
  ),
  DA = case_when(
    dogs_allowed == "False" ~ 0,
    dogs_allowed == "True" ~ 1,
    TRUE ~ 0
  ),
  
  POB = case_when(
    patio_or_balcony == "False" ~ 0,
    patio_or_balcony == "True" ~ 1,
    TRUE ~ 0
  ),
  
  AC = case_when(
    air_conditioning == "False" ~ 0,
    air_conditioning == "True" ~ 1,
    TRUE ~ 0
  ),
  
  POOL = case_when(
    pool == "False" ~ 0,
    pool == "True" ~ 1,
    TRUE ~ 0
  ),
  
  FC  = case_when(
    fitness_center == "False" ~ 0,
    fitness_center == "True" ~ 1,
    TRUE ~ 0
  ),
  ONSP = case_when(
    on_street_parking == "False" ~ 0,
    on_street_parking == "True" ~ 1,
    TRUE ~ 0
  ),
  
  OFFSP = case_when(
    off_street_parking == "False" ~ 0,
    off_street_parking == "True" ~ 1,
    TRUE ~ 0
  )
  )

A <- A %>%
  group_by(ADDY) %>%
  summarise(gym = first(fitness_center),
            pool = first(pool)
  )


RC3 <- RC2 %>%
  filter(beds %in% 0:3) %>%
  #filter(last_listing_price < 10000) %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(beds) %>%
  mutate(Outlier =  if_else(last_listing_price < quantile(last_listing_price, 0.25, na.rm = TRUE) - 1.5 * IQR(last_listing_price, na.rm = TRUE) |
                              (last_listing_price > quantile(last_listing_price, 0.75, na.rm = TRUE) + 1.5 * IQR(last_listing_price, na.rm = TRUE)),1,0, missing = 1)) %>%
  ungroup() %>%
  mutate(DATE = mdy(last_listing_date),
         YEAR = year(DATE),
         AgeOfBuilding = if_else(YEAR_BUILT != 0 & !is.na(YEAR_BUILT), 2024-YEAR_BUILT, NA_real_, missing = NA_real_),
         ppsqf = if_else(is.na(sf) | sf == 0, NA_real_, last_listing_price/sf, missing = NA_real_),
         investor = ifelse(grepl(paste(c("LLC", "LP", "INC", "L.P."), collapse = "|"), OWNERNAME_MASTER), 1, 0),
         RealPage = if_else(OWNERNAME_MASTER %in% RP, 1 ,0, missing = 0)) %>%
  filter(Outlier == 0) %>%
  rename(BuildingUnits = UNITS_MASTER,
         SQFT = sf) %>%
  left_join(., A, by = "ADDY") %>%
  left_join()


#feols(last_listing_price ~ RENT_CONTROLLED + beds + sf + UNITS_MASTER + avg_weeks_per_episode | NBHD + YEAR, data = test1)
#lm(last_listing_price ~ RENT_CONTROLLED + beds + sf + UNITS_MASTER + avg_weeks_per_episode + factor(NBHD) + factor(YEAR), data = test1)
model <- felm(log(last_listing_price) ~ RENT_CONTROLLED + BuildingUnits + RENT_CONTROLLED + factor(beds) +  SQFT + AgeOfBuilding + pool + gym | NBHD  + YEAR, data = RC3)
summary(model)
coef(model)["RENT_CONTROLLED"] # say = -0.08


#model <- plm(last_listing_price ~ RENT_CONTROLLED*beds + SQFT + RENT_CONTROLLED*BuildingUnits + investor +  total_weeks_listed + YEAR_BUILT, data = RC3, index = c("NBHDNAME", "YEAR", model="within"))

stargazer(model, type="text")

# Export main regression results
stargazer(model, type = "text", out = "test.txt")
lfe::felm()

RC3 %>%
  group_by(PROPERTY_TYPE_NAME) %>%
  summarise(first(PROPERTY_TYPE_GROUPING))
# Export fixed effects
fe_df <- as.data.frame(getfe(model))
#write.csv(fe_df, "fe_df.csv")
library(xtable)

tab <- xtable(fe_df, caption = "Nominal Price Fixed Effects", label = "tab:mytable", align = "lrrrrr")
print(tab, include.rownames = FALSE, booktabs = TRUE)

write.table(fe_df, row.names = FALSE, sep = "\t")

clipr::write_clip(fe_df)

print(test1 %>%
  distinct(NBHDNAME) %>%
    arrange(),
  n = 100)

####Clustering#####
OWNER <- RC3 %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(OWNERNAME_MASTER) %>%
  summarise(first(OWNERNAME_MASTER)
            
            )
test <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1)
test %>%
  group_by(MIXEDUSE) %>%
  summarise(n(),
            sum(UNITS_MASTER))
  
RC <- read.csv("Rent_Control.csv")

RC2 <- read.csv("RC2.csv")


Apartments <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  mutate(ID = row_number(),
         Street_Address= str_c(LOWNUMBER, STREETNAME, QDRNTNAME, sep = " "),
         City = "Washington",
         State = "DC",
         Zipcode = str_extract(PREMISEADD, "(?<=DC\\s)\\d{5}")) %>%
  filter(!is.na(Street_Address)) %>%
  select(SSL, ID)

APT_GEOCODE <- read.csv("APT_GEOCODE.csv")

temp8 <- inner_join(Apartments, APT_GEOCODE, by = "ID")

options(digits = 15)
temp9 <- temp8 %>%
  separate(Longitude.Latitude, into = c("X", "Y"), sep = ",", convert = TRUE) %>%
  filter(!is.na(X))

# Convert Coordinates to CT
tracts_DC <- tracts(state = "DC", year = 2020, cb = TRUE)
# 1) make points with a CRS (your coords look like lon/lat WGS84)
points_sf <- st_as_sf(temp9, coords = c("X","Y"), crs = 4326)

# 2) transform points to match tracts
points_sf <- st_transform(points_sf, st_crs(tracts_DC))

# 3) point-in-polygon join (points first, polygons second)
points_with_tract <- st_join(tracts_DC, points_sf, join = st_contains)

RC3 <- RC2 %>%
  filter(beds %in% 0:3) %>%
  #filter(last_listing_price < 10000) %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  group_by(beds) %>%
  mutate(Outlier =  if_else(last_listing_price < quantile(last_listing_price, 0.25, na.rm = TRUE) - 1.5 * IQR(last_listing_price, na.rm = TRUE) |
                              (last_listing_price > quantile(last_listing_price, 0.75, na.rm = TRUE) + 1.5 * IQR(last_listing_price, na.rm = TRUE)),1,0, missing = 1)) %>%
  ungroup() %>%
  mutate(DATE = mdy(last_listing_date),
         YEAR = year(DATE),
         AgeOfBuilding = if_else(YEAR_BUILT != 0 & !is.na(YEAR_BUILT), 2024-YEAR_BUILT, NA_real_, missing = NA_real_),
         ppsqf = if_else(is.na(sf) | sf == 0, NA_real_, last_listing_price/sf, missing = NA_real_),
         investor = ifelse(grepl(paste(c("LLC", "LP", "INC", "L.P."), collapse = "|"), OWNERNAME_MASTER), 1, 0)) %>%
  filter(Outlier == 0) %>%
  rename(BuildingUnits = UNITS_MASTER,
         SQFT = sf)


RC5 <- RC %>%
  filter(PROPERTY_TYPE_GROUPING == 1) %>%
  inner_join(points_with_tract, by = "SSL") %>%
  st_as_sf() %>%   # <- reattach sf class and geometry
  mutate(AgeOfBuilding = if_else(YEAR_BUILT != 0 & !is.na(YEAR_BUILT), 2024-YEAR_BUILT, NA_real_, missing = NA_real_)) %>%
  group_by(GEOID) %>%
  summarise(
    NumberofBuildings = n(),
    BuildingUnits = median(UNITS_MASTER, na.rm = TRUE),
    Density = sum(UNITS_MASTER) / NumberofBuildings,
    ShareRC = mean(RENT_CONTROLLED),
    AGE = median(AgeOfBuilding, na.rm = TRUE)
  )

RC4 <- RC3 %>%
  filter(YEAR == 2023) %>%
  inner_join(., points_with_tract, by = "SSL") %>%
  st_as_sf() %>%   # <- reattach sf class and geometry
  select(GEOID,SSL,RENT_CONTROLLED, SQFT, BuildingUnits, ppsqf, AgeOfBuilding) %>%
  group_by(GEOID) %>%
  summarise(SF = median(SQFT, na.rm = TRUE),
            PPSQF = median(ppsqf, na.rm = TRUE)
         )

RC6 <- inner_join(RC4, st_drop_geometry(RC5), by = "GEOID")


RC7 <- RC6 %>% mutate(.row_id = row_number()) %>% select(-NumberofBuildings)

X <- RC7 %>%
  st_drop_geometry() %>%
  select(.row_id, where(is.numeric)) %>%
  na.omit()

X_mat <- X %>%
  select(-.row_id) %>%
  #mutate(across(everything(), ~replace_na(.x, 0))) %>%
  as.matrix() %>%
  scale()

km <- kmeans(X_mat, centers = 4, nstart = 20)

clusters_df <- tibble(.row_id = X$.row_id, cluster = factor(km$cluster))

FCA_clustered <- RC7 %>%
  left_join(clusters_df, by = ".row_id") %>%
  select(-.row_id) %>%
  na.omit()




#create plot of number of clusters vs total within sum of squares
fviz_nbclust(X_mat, kmeans, method = "wss")

ggplot(FCA_clustered) + geom_sf(aes(fill = cluster)) +
  labs(
    title = "DC Census Tract Clusters",
    fill = "Clusters"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = .5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ClusterSumStats <- FCA_clustered %>%
  st_drop_geometry() %>%
  na.omit() %>%
  group_by(cluster) %>%
  summarise(
    Density = median(Density, na.rm = TRUE),
    BuildingUnits = median(BuildingUnits, na.rm = TRUE),
    PPSQF = median(PPSQF, na.rm = TRUE),
    SF = median(SF, na.rm = TRUE),
    ShareRC = median(ShareRC, na.rm = TRUE),
    AGE = median(AGE, na.rm = TRUE)
  )

ClusterSumStats <- RC3 %>%
  #st_drop_geometry() %>%
  group_by(NBHDNAME) %>%
  summarise(
    #Density = median(Density, na.rm = TRUE),
    BuildingUnits = median(BuildingUnits, na.rm = TRUE),
    PPSQF = median(ppsqf, na.rm = TRUE),
    SF = median(SQFT, na.rm = TRUE),
    ShareRC = mean(RENT_CONTROLLED, na.rm = TRUE),
    AGE = median(AgeOfBuilding, na.rm = TRUE))

write.csv(ClusterSumStats, "ClusterSumStats.csv")

# Sum stats
ggplot(RC5) +
  geom_sf(aes(fill = ShareRC)) +
  scale_fill_viridis_c( na.value = "grey90") +
  labs(fill = "% Rent Control",
       title = "Rent Control Apartments Across DC") +
  theme_minimal()


#####COmbining with Costar#####

RC8 <- RC3 %>%
  filter(YEAR == 2023) %>%
  inner_join(., points_with_tract, by = "SSL") %>%
  group_by(SSL) %>%
  summarise(
    Avg_Listed_Price = mean(last_listing_price, na.rm = TRUE),
    YEAR_BUILT = mean(YEAR_BUILT, na.rm = TRUE),
    AgeOfBuilding = first(AgeOfBuilding),
    RENT_CONTROLLED = first(RENT_CONTROLLED),
    TRACTCE = first(TRACTCE),
    BuildingUnits = first(BuildingUnits)
  )

APTRC <- RC %>% filter(PROPERTY_TYPE_GROUPING == 1) %>% 
  inner_join(., points_with_tract, by = "SSL") %>% 
  select()
Costar_RC <- inner_join(Costar, APTRC, by = "SSL")


RC9 <- left_join()

#####BNB Map####
# Convert Coordinates to CT
options(digits = 20)
BNBCOORD <- read.csv("BNBCOORD.csv")
BNBCOORD<-BNBCOORD %>%
  filter(Coordinates !="") %>%
  separate(Coordinates, into = c("X", "Y"), sep = ",", convert = TRUE)

# 1. Get DC tracts, pick a CRS
tracts_dc <- tracts(state = "DC", year = 2024, cb = TRUE)

# 2. Make points from BNBCOORD
# assuming BNBCOORD$X = longitude, BNBCOORD$Y = latitude (WGS84)
points_sf <- st_as_sf(BNBCOORD,
                      coords = c("X", "Y"),
                      crs = 4326)              # lon/lat

# 3. Reproject points to match tracts
points_sf <- st_transform(points_sf, st_crs(tracts_dc))

# 4. Spatial join: points first, tracts second
points_with_tract <- st_join(points_sf, tracts_dc, join = st_within)

# drop geometry so count doesn't try to union points
tract_counts <- points_with_tract %>%
  st_drop_geometry() %>%
  count(GEOID, name = "bnb_n")   # bnb_n = number of observations per tract

tracts_dc_counts <- tracts_dc %>%
  left_join(tract_counts, by = "GEOID") %>%
  replace_na(list(bnb_n = 0))   # tracts with no listings → 0

ggplot(tracts_dc_counts) +
  geom_sf(aes(fill = bnb_n)) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(fill = "# of listings",
       title = "BNB listings per census tract") +
  theme_minimal()

