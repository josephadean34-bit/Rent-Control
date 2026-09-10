####Library######
library(paletteer)
library(ggthemes)
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
library(tigris)
library(factoextra)
library(cluster)
library(factoextra)


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


pattern <- "\\b(CHURCH|ORGANIZATION|ENTERPRISES|ASSOC|UNITED|MINISTRIES|PRTNSHP|CHRUCH|INSTUTUTE|\\bLC\\b|ASSOC\\.|
GROUP|DEVELOPERS|ESTATES|AMERICAN|AMERICA|\\bCO\\b|MINISTRY|COMPANIES|CITIZEN|JOINT|COMMUNITY|ARMY|INVESTMENTS|PARTNERS|ASSOCS|
&|VENTURES|VENTURE|EMPLOYEES|ALLIANCE|ENDEAVORS|PROPERTY|MANAGEMENT|INCORPORATED|ESTATE|DEVELOPMENT|HOUSING|CATHOLIC|TRUSTEES|COALITION|
COMPANY|ASSOCIATES|LLP|US|L\\.P\\.|L P|HOLDINGS|APARTMENTS|PROPERTIES|AND|FOUNDATION|ASSOCIATION|INTERNATIONAL|LEAGUE|CAUCUS|L\\.L\\.C|&|LLC|INC|CORPORATION|LTD|
LIMITED|LP|CORP|PARTNERSHIP|LIMITED|TRUST|TRUSTEE|COLLEGE|UNIVERSITY|GOVERNMENT|EMBASSY|REPUBLIC|NAVAJO|DISTRICT|CITY|SOCIETY|INSTITUTE|COUNCIL|FEDERATION|UNION|
COOPERATIVE|FUND|BUREAU|AGENCY|COMMISSION|AUTHORITY|CONSORTIUM|GUILD|CHAMBER|ORDER|SYNDICATE|NGO|NPO)\\b"


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
  mutate(
    PUBLICLY_OWNED = if_else(
      OWNERNAME %in% c("DISTRICT OF COLUMBIA", 
                       "DISTRICT OF COLUMBIA HOUSING AUTHORITY", 
                       "UNITED STATES OF AMERICA"), 
      1, 0, missing = 0
    )
  ) %>%
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
    str_detect(OWNERNAME, pattern),
    0, 1, missing = 0
  )) %>%
  mutate(U5RULE1 = if_else(U51 == 1 & NATURALP == 1, 1,0)) %>%
  mutate(
    RENT_CONTROLLED = if_else(
      (REG_ERA == "Unknown"  | REG_ERA == "Built before 1976") & NON_TAXABLE == 0 & U5RULE1 == 0 & PUBLICLY_OWNED == 0 & SUBSIDIZED == 0, 1,0, missing = NA
      
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

  
# Altos RC Merge ####
options(scipen = 15)

G1GEOCODE <- read.csv("G1GEOCODE.csv")
ALTOS <- read.csv("altos_final_unit_all_features.csv")


RC1 <- merge2 %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  distinct(ADDY, .keep_all = TRUE)

RC1temp <- RC1 %>% select(-X)

RC1 <- inner_join(RC1temp,G1GEOCODE, by = "SSL", keep = FALSE)

# Convert Coordinates to CT
tracts_DC <- tracts(state = "DC", year = 2020, cb = TRUE)
# 1) make points with a CRS
points_sf <- st_as_sf(RC1, coords = c("X","Y"), crs = 4326)

# 2) transform points to match tracts
points_sf <- st_transform(points_sf, st_crs(tracts_DC))

# 3) Polygons
RC1Polygons <- st_join(tracts_DC, points_sf, join = st_contains)

# Points
RC1Points <- st_join(points_sf, tracts_DC, join = st_within)

#####RC3Polygon#########

fast_address_standardize <- function(addresses) {
  # Convert to uppercase and normalize spaces
  addresses <- toupper(addresses)
  addresses <- gsub("\\s+", " ", addresses)
  addresses <- trimws(addresses)
  
  # Remove location info (zip codes, city, state)
  addresses <- gsub("\\s*,?\\s*[A-Z]{2,}\\s*,?\\s*[0-9]{5,}.*$", "", addresses)
  addresses <- gsub("\\s*,\\s+WASHINGTON\\s+DC", "", addresses)
  addresses <- gsub(",+", " ", addresses)
  addresses <- gsub("\\s+", " ", addresses)
  addresses <- trimws(addresses)
  
  # Replace abbreviations - order matters (longest first)
  replacements <- c(
    "NORTHWEST" = "NW", "NORTHEAST" = "NE", "SOUTHWEST" = "SW", "SOUTHEAST" = "SE",
    "STREET" = "ST", "AVENUE" = "AVE", "BOULEVARD" = "BLVD", "SQUARE" = "SQ",
    "DRIVE" = "DR", "PLACE" = "PL", "ROAD" = "RD", "TERRACE" = "TER",
    "COURT" = "CT", "PARKWAY" = "PKWY", "CIRCLE" = "CIR", "ALLEY" = "ALY", 
    "LANE" = "LN", "WAY" = "WAY", "WALK" = "WALK", "ROW" = "ROW"
  )
  
  # Apply replacements
  for (i in seq_along(replacements)) {
    pattern <- paste0("\\b", names(replacements)[i], "\\b")
    addresses <- gsub(pattern, replacements[i], addresses)
  }
  
  return(trimws(gsub("\\s+", " ", addresses)))
}

ALTOS <- read.csv("altos_final_unit_all_features.csv")

# Usage
df <- ALTOS %>%
  mutate(
    address = toupper(ADDR_NORM),
    ID = row_number(),
    standardized_address = fast_address_standardize(address),
    ADDY = str_extract(standardized_address, ".*\\b(NW|NE|SW|SE)\\b")
    
  ) %>%
  filter(!is.na(ADDY))

RC2 <- inner_join(df, RC1Polygons, by = "ADDY")


ALTOS <- read.csv("Combined_Final_Data.csv")

Altos <- ALTOS %>%
  mutate(
    address = toupper(street_address),
    ID = row_number(),
    standardized_address = fast_address_standardize(address),
    ADDY = str_extract(standardized_address, ".*\\b(NW|NE|SW|SE)\\b")
    
  ) %>%
  filter(!is.na(ADDY))



A <- Altos %>%
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
            pool = first(pool)  )



RC3Polygons <- RC2 %>%
  filter(beds %in% 0:3) %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  filter(last_listing_price >= 500 & last_listing_price < 10000) %>%
  group_by(PROPERTY_TYPE_GROUPING,beds) %>%
  mutate(Outlier =  if_else(last_listing_price < quantile(last_listing_price, 0.25, na.rm = TRUE) - 1.5 * IQR(last_listing_price, na.rm = TRUE) |
                              (last_listing_price > quantile(last_listing_price, 0.75, na.rm = TRUE) + 1.5 * IQR(last_listing_price, na.rm = TRUE)),1,0, missing = 1)) %>%
  ungroup() %>%
  mutate(DATE = mdy(last_listing_date),
         YEAR = year(DATE),
         AgeOfBuilding = if_else(YEAR_BUILT != 0 & !is.na(YEAR_BUILT), 2024-YEAR_BUILT, NA_real_, missing = NA_real_),
         ppsqf = if_else(is.na(sf) | sf == 0, NA_real_, last_listing_price/sf, missing = NA_real_),
         PROPERTY_TYPE_GROUPING = relevel(factor(PROPERTY_TYPE_GROUPING), ref = "1"),
         beds = relevel(factor(beds), ref = "1")
         #RealPage = if_else(OWNERNAME_MASTER %in% RP, 1 ,0, missing = 0)
  ) %>%
  filter(Outlier == 0) %>%
  rename(BuildingUnits = UNITS_MASTER,
         SQFT = sf) %>%
  left_join(., A, by = "ADDY") %>%
  st_as_sf()
  



##### RC3Points ########
fast_address_standardize <- function(addresses) {
  # Convert to uppercase and normalize spaces
  addresses <- toupper(addresses)
  addresses <- gsub("\\s+", " ", addresses)
  addresses <- trimws(addresses)
  
  # Remove location info (zip codes, city, state)
  addresses <- gsub("\\s*,?\\s*[A-Z]{2,}\\s*,?\\s*[0-9]{5,}.*$", "", addresses)
  addresses <- gsub("\\s*,\\s+WASHINGTON\\s+DC", "", addresses)
  addresses <- gsub(",+", " ", addresses)
  addresses <- gsub("\\s+", " ", addresses)
  addresses <- trimws(addresses)
  
  # Replace abbreviations - order matters (longest first)
  replacements <- c(
    "NORTHWEST" = "NW", "NORTHEAST" = "NE", "SOUTHWEST" = "SW", "SOUTHEAST" = "SE",
    "STREET" = "ST", "AVENUE" = "AVE", "BOULEVARD" = "BLVD", "SQUARE" = "SQ",
    "DRIVE" = "DR", "PLACE" = "PL", "ROAD" = "RD", "TERRACE" = "TER",
    "COURT" = "CT", "PARKWAY" = "PKWY", "CIRCLE" = "CIR", "ALLEY" = "ALY", 
    "LANE" = "LN", "WAY" = "WAY", "WALK" = "WALK", "ROW" = "ROW"
  )
  
  # Apply replacements
  for (i in seq_along(replacements)) {
    pattern <- paste0("\\b", names(replacements)[i], "\\b")
    addresses <- gsub(pattern, replacements[i], addresses)
  }
  
  return(trimws(gsub("\\s+", " ", addresses)))
}

ALTOS <- read.csv("altos_final_unit_all_features.csv")

# Usage
df <- ALTOS %>%
  mutate(
    address = toupper(ADDR_NORM),
    ID = row_number(),
    standardized_address = fast_address_standardize(address),
    ADDY = str_extract(standardized_address, ".*\\b(NW|NE|SW|SE)\\b")
    
  ) %>%
  filter(!is.na(ADDY))

RC2 <- inner_join(df, RC1Points, by = "ADDY")


ALTOS <- read.csv("Combined_Final_Data.csv")

Altos <- ALTOS %>%
  mutate(
    address = toupper(street_address),
    ID = row_number(),
    standardized_address = fast_address_standardize(address),
    ADDY = str_extract(standardized_address, ".*\\b(NW|NE|SW|SE)\\b")
    
  ) %>%
  filter(!is.na(ADDY))



A <- Altos %>%
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
            pool = first(pool)  )



RC3Points <- RC2 %>%
  filter(beds %in% 0:3) %>%
  filter(PROPERTY_TYPE_GROUPING %in% c(1,3,5)) %>%
  filter(last_listing_price >= 500 & last_listing_price < 10000) %>%
  group_by(PROPERTY_TYPE_GROUPING,beds) %>%
  mutate(Outlier =  if_else(last_listing_price < quantile(last_listing_price, 0.25, na.rm = TRUE) - 1.5 * IQR(last_listing_price, na.rm = TRUE) |
                              (last_listing_price > quantile(last_listing_price, 0.75, na.rm = TRUE) + 1.5 * IQR(last_listing_price, na.rm = TRUE)),1,0, missing = 1)) %>%
  ungroup() %>%
  mutate(DATE = mdy(last_listing_date),
         YEAR = year(DATE),
         AgeOfBuilding = if_else(YEAR_BUILT != 0 & !is.na(YEAR_BUILT), 2024-YEAR_BUILT, NA_real_, missing = NA_real_),
         ppsqf = if_else(is.na(sf) | sf == 0, NA_real_, last_listing_price/sf, missing = NA_real_),
         PROPERTY_TYPE_GROUPING = relevel(factor(PROPERTY_TYPE_GROUPING), ref = "1"),
         beds = relevel(factor(beds), ref = "1")
         #RealPage = if_else(OWNERNAME_MASTER %in% RP, 1 ,0, missing = 0)
  ) %>%
  filter(Outlier == 0) %>%
  rename(BuildingUnits = UNITS_MASTER,
         SQFT = sf) %>%
  left_join(., A, by = "ADDY") %>%
  st_as_sf()

###Regression####

reg <- RC3Polygons %>% filter(SUBSIDIZED == 0 & PUBLICLY_OWNED == 0 & NON_TAXABLE == 0)
options(scipen = 999)
model <- felm(log(last_listing_price) ~ RENT_CONTROLLED+BuildingUnits+SQFT+ beds + PROPERTY_TYPE_GROUPING + pool + gym | GEOID  + YEAR, data = reg)
summary(model)


coef(model)["RENT_CONTROLLED"]


#model <- plm(last_listing_price ~ RENT_CONTROLLED*beds + SQFT + RENT_CONTROLLED*BuildingUnits + investor +  total_weeks_listed + YEAR_BUILT, data = RC3, index = c("NBHDNAME", "YEAR", model="within"))

stargazer(model, type="text")

# Export main regression results
stargazer(model, type = "text", out = "test.txt")



