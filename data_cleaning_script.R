## Data Cleaning Script ##


# load libraries
library(tidyverse)
library(ggmap)


# load in and prep data
opened <- read_csv("data/opened_restaurants.csv") %>% 
  select(-X5, -X6) %>% 
  unite("key", c("Name", "Address"), remove = FALSE)


closed <- read_csv("data/closed_restaurants.csv") %>% 
  unite("key", c("Name", "Address"), remove = FALSE) 


# join the open and closed data by the new key column
master_data <- opened %>% 
  full_join(closed)

  
# check for repeates in the data
mismatches <- master_data %>% 
  group_by(Name) %>% 
  add_tally()
# note budda bowls opened twice at same place so did old town coffee overall pretty clean data though



## geocode section ##

# set api
#myAPI <- read_file("api.txt") # this lives on my computer sorry 
register_google(key = "AIzaSyABEJ6H1VtH5itry5bO5j1Ba6hjweKPBzw")


geo_code_ref <- master_data %>% 
  distinct(Address) %>% 
  mutate(state = "California") %>% 
  unite("address_state", c("Address", "state"), sep = ", ", remove = FALSE) %>% 
  mutate_geocode(address_state)

# idk why these didnt all work the first time I think it was some account info updating
check_missings <- geo_code_ref %>% 
  filter(is.na(lat)) %>% 
  select(-lat, -lon) %>% 
  mutate_geocode(address_state)

# google maps didn't like the "#" for whatever reason
check_missings2 <- check_missings %>% 
  filter(is.na(lat)) %>% 
  select(-lat, -lon) %>% 
  mutate(address_state = str_remove(address_state, "#")) %>% 
  mutate_geocode(address_state)


# attaching the different segments to a master sheet
geocode_ref <- geo_code_ref %>% 
  filter(!is.na(lat)) %>% 
  bind_rows(check_missings) %>% 
  filter(!is.na(lat)) %>% 
  bind_rows(check_missings2) %>% 
  select(-address_state, -state)


write_csv(master_data, "data/master_data.csv")
write_csv(geocode_ref, "data/geocode_ref.csv")
