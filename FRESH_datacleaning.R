library(tidyverse)
library(plyr)
library(acs)
library(tigris)
library(sp)
library(janitor)
library(readxl)

#############ALL THE BUSINESS OF DATA################


#####GET DATA FROM CENSUS#####


#####SHAPEFILE#####
#API key
api.key.install(key="ad683b28beddcddedda574c37473e0b948b407f5")

#Allegheny County census tracts
shapefile <- tracts(state='42', county='03')


#####ACS DATA#####

acs_19 <- read_csv("acs_2019.csv")

#clean up acs data....
acs_19 <- acs_19[-1,]
acs_19 <- acs_19 %>%
  remove_empty() %>% #remove empty cols (no empty rows) %>%
  mutate(geo_id_ct = str_remove(`Area Name`,"Census Tract ")) %>%
  select(-c(2, 4:12, 14:16, 19:31, 44:51, 60:67, 69:72, 74, 75, 77:83, 85:89, 92:98, 107, 108,
            112:114, 118:129, 131, 134:142, 149:157)) %>% #remove unneeded columns
  clean_names() #clean up column names

acs_19<-acs_19[,c(1,63,3:62)] #get rid of dup. census tract ID and move to beginning




#####NEIGHBORHOOD AND MUNICIPAL NAMES#####


pitt_tracts <- read_csv("pitt_tracts.csv")
muni_tracts <- read_excel("ac_muni_tracts.xlsx")




#neighborhood & municipality names associated w/ census tracts
#clean up city data
pitt_tracts <- pitt_tracts %>%
  dplyr::rename(geo_id_ct = `Census Tracts`) %>% #rename variable for easier use
  filter(!is.na(Neighborhood)) %>% #remove all neighborhood == NA (this means all census codes != 2010)
  select(Neighborhood, geo_id_ct) %>% #keep neighborhood, tract id
  separate(geo_id_ct, into = c("1", "2", "3", "4", "5"), sep=", ") %>% #separate out diff tracts that share a cell
  pivot_longer(c("1", "2", "3", "4", "5"), #go back to long data after ^ widened it. 1 row per tract id
               names_to="tract", 
               values_to="geo_id_ct",
               values_drop_na=T)

#clean up other AC municipalities
muni_tracts <- muni_tracts %>%
  dplyr::rename(geo_id_ct = `2010 Census Tracts`) %>% #rename variable for easier use
  select(Municipality, geo_id_ct) %>% #keep municipality, tract id
  filter(Municipality != "Pittsburgh City") %>% #get rid of city of pgh row (has no data, inc. in pitt_tracts)
  mutate(num_ct = str_count(geo_id_ct, ",")) %>% #find the muni w/ the most tracts by counting commas & arranging
  arrange(desc(num_ct)) %>% #9 commas is highest - so, 10 diff. tract IDs per muni at MOST
  separate(geo_id_ct, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), sep =",") %>%
  pivot_longer(c("1","2","3","4","5","6","7","8","9","10"), #sep out each tract ID to own col.
               names_to="tract",                            #then back to long data w/ 1 row
               values_to="geo_id_ct",                       #per tract
               values_drop_na=T) %>%
  arrange(Municipality) %>% #arrange by muni name
  select(Municipality, geo_id_ct) #keep only municipality, census tract ID


#merge df's w/ name-geocode for pgh neigh & muni
all_tracts <- join(muni_tracts, pitt_tracts, type="full") %>%  
  mutate(muni_nei = (ifelse(is.na(Municipality),
                            paste(Neighborhood," (Pittsburgh)"),
                            Municipality)),
         geo_id_ct = str_squish(geo_id_ct)) %>%
  select(muni_nei, geo_id_ct) %>%
  mutate(muni_nei = str_squish(muni_nei))

all_tracts[179,2] <- "4592.02" #simplest way to fix a little weirdness w/ this tract ID
all_tracts[404,1] <- "Spring Hill City View (Pittsburgh)"

all_tracts <- all_tracts %>%
  distinct(geo_id_ct, .keep_all=T) #only one row per census tract



#####MERGE TRACT MUNI/NEI NAMES WITH GEO and ACS DF#####

acs_tracts <- merge(acs_19, all_tracts, by="geo_id_ct", all=T)

#move muni_nei name column to 2nd column
acs_tracts <- acs_tracts %>%
  relocate(63, .after=1)

#remove rows with no census data
acs_tracts <- acs_tracts %>%
  filter(!is.na(total_population))

#assign muni_nei names to rows w/o them
acs_tracts[49,2] <- "Beechview (Pittsburgh)"
acs_tracts[220,2] <- "Upper St. Clair Twp"
acs_tracts[311,2] <- "Monroeville"
acs_tracts[402,2] <- "Oakland (Pittsburgh)"



#ACS column names are a mess, make them nicer...
acs_tracts <- acs_tracts %>%
  dplyr::rename(total_pop = total_population)

acs_tracts <- acs_tracts %>%
  dplyr::rename(hhincome = median_household_income_in_2019_inflation_adjusted_dollars)

map <- map %>%
  dplyr::rename(Male = percent_total_population_male,
                Female = percent_total_population_female)

acs_tracts <- acs_tracts %>%
  rename(Male = percent_total_population_male,
         Female = percent_total_population_female,
         
         `Under 5 years` = percent_total_population_under_5_years,
         `5 to 9 years` = percent_total_population_5_to_9_years,
         `10 to 14 years` = percent_total_population_10_to_14_years,
         `15 to 17 years` = percent_total_population_15_to_17_years,
         `18 to 24 years` = percent_total_population_18_to_24_years,
         `25 to 34 years` = percent_total_population_25_to_34_years,
         `35 to 44 years` = percent_total_population_35_to_44_years,
         `45 to 54 years` = percent_total_population_45_to_54_years,
         `55 to 64 years` = percent_total_population_55_to_64_years,
         `65 to 74 years` = percent_total_population_65_to_74_years,
         `75 to 84 years` = percent_total_population_75_to_84_years,
         `85 years and over` = percent_total_population_85_years_and_over,
         
         White = percent_total_population_white_alone,
         Black = percent_total_population_black_or_african_american_alone,
         Asian = percent_total_population_asian_alone,
         `Indigenous/Native American` = percent_total_population_american_indian_and_alaska_native_alone,
         `Native Hawaiian/Pacific Islander` = percent_total_population_native_hawaiian_and_other_pacific_islander_alone,
         `Two or More Races` = percent_total_population_two_or_more_races,
         
         `Family Households` = percent_households_family_households,
         `Nonfamily Households` = percent_households_nonfamily_households,
         `Households with Children` = percent_occupied_housing_units_with_related_children_of_the_householder_under_18,
         `Households with No Children` = percent_occupied_housing_units_no_related_children_of_the_householder_under_18,
         
         over_25_dem = population_25_years_and_over,
         `Less than High School` = percent_population_25_years_and_over_less_than_high_school,
         `High Degree or Equivalent` = percent_population_25_years_and_over_high_school_graduate_includes_equivalency,
         `Some College` = percent_population_25_years_and_over_some_college,
         `Bachelor's Degree` = percent_population_25_years_and_over_bachelors_degree
         )



acs_tracts <- acs_tracts %>%
  rename(`Master's Degree` = percent_population_25_years_and_over_masters_degree)

acs_tracts <- acs_tracts %>%
  mutate(`Doctoral/Professional Degree` = as.numeric(percent_population_25_years_and_over_doctorate_degree) + as.numeric(percent_population_25_years_and_over_professional_school_degree))


acs_tracts <- acs_tracts %>%
  relocate(`Doctoral/Professional Degree`, .after = `Master's Degree`)


acs_tracts <- acs_tracts %>%
  rename(civ_lf_denom = civilian_population_in_labor_force_16_years_and_over,
         Employed = percent_civilian_population_in_labor_force_16_years_and_over_employed,
         Unemployed = percent_civilian_population_in_labor_force_16_years_and_over_unemployed,
         `Owner-Occupied Housing Units` = percent_occupied_housing_units_owner_occupied,
         `Renter-Occupied Housing Units` = percent_occupied_housing_units_renter_occupied,
         workers_16up_denom = workers_16_years_and_over,
         `Public Transportation` = percent_workers_16_years_and_over_public_transportation_includes_taxicab,
         `Bicycle` = percent_workers_16_years_and_over_bicycle,
         `Walk` = percent_workers_16_years_and_over_walked,
         `No Vehicle` = percent_occupied_housing_units_no_vehicle_available,
         `One Vehicle` = percent_occupied_housing_units_1_vehicle_available)

acs_tracts <- acs_tracts %>%
  mutate(`Two or More Vehicles` = as.numeric(percent_occupied_housing_units_2_vehicles_available) +
           as.numeric(percent_occupied_housing_units_3_vehicles_available) + 
           as.numeric(percent_occupied_housing_units_4_vehicles_available) +
           as.numeric(percent_occupied_housing_units_5_or_more_vehicles_available)) %>%
  relocate(`Two or More Vehicles`, .after = `One Vehicle`,
           White, .after = `Native Hawaiian/Pacific Islander`)

acs_tracts <- acs_tracts %>%
  relocate(White, .after = `Native Hawaiian/Pacific Islander`)

map <- map %>%
  relocate(White, .after = `Native Hawaiian/Pacific Islander`)

acs_tracts <- acs_tracts %>%
  rename(`High School Degree or Equivalent` = `High Degree or Equivalent`)

#select ACS variables we want to use
acs_tracts1 <- acs_tracts %>%
  select(1:23,25:28,30:38,41:53)

#make sure they are numeric
acs_tracts1 <- acs_tracts1 %>%
  mutate_all(type.convert, as.is=T)

acs_tracts1 <- acs_tracts1 %>%
  mutate(geo_id_ct = as.character(geo_id_ct),
         fips = as.character(fips))



#merge spatial data with other data
map <- geo_join(shapefile, acs_tracts1, "GEOID", "fips")

map <- map %>%
  st_transform('+proj=longlat +datum=WGS84')



#####CLEAN BUSINESS DATA#####

#read business data
bus_geo <- read_csv("/Users/jennajabaut/Dropbox/PIA 2096 Class use/Xiaohong/business_geoid.csv") #business directory

bus_geo <- bus_geo[,3:18] #remove columns of row numbers
bus_geo <- bus_geo %>%
  filter(str_detect(formatted_address, "PA")) #remove non-PA businesses

#remove duplicates
bus_geo <- bus_geo %>%
  distinct(name, .keep_all=T)


bus_geo <- bus_geo %>%
  filter(!is.na(lat)) %>% #filter out NAs
  mutate(formatted_address = str_remove(formatted_address, ", United States")) 
#& remove country from business address



bus_geo <- bus_geo[-c(13,25,73,95,120),]


acs_tracts1 <- acs_tracts1 %>%
  rename(Walking = "Walk")


####HOW TO WRITE SHAPEFILE???? using st_write
st_write(shapefile, "demo_map/data/shapefile.shp")

###WELL. WRITE acs_tracts1 and bus_geo anyway###
write_csv(acs_tracts1, "demo_map/data/acs_tracts1.csv")
write_csv(bus_geo, "demo_map/data/bus_geo.csv")

###Will need to merge them in app script###





