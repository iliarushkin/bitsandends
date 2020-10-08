# #The data stored in this package was created here
#
# library(tidyverse)
# library(sf)
# library(geojsonio)
# library(maps)
#
# load('./data/us_state_codes.RData')
#
# us_counties_geojson=geojson_sf('https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json')
# us_counties_geojson$name=paste0(us_counties_geojson$NAME, ', ', us_state_codes$code[match(us_counties_geojson$STATE, us_state_codes$fips)])
# us_counties_geojson$state_code=us_state_codes$code[match(us_counties_geojson$STATE, us_state_codes$fips)]
# us_counties_geojson$state_name=us_state_codes$name[match(us_counties_geojson$STATE, us_state_codes$fips)]
# us_counties_geojson$name=paste0(us_counties_geojson$NAME, ', ', us_counties_geojson$state_code)
# us_states_geojson=geojson_sf('https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json')
# countries_geojson=geojson_sf('https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/countries.geojson')
#
# save(us_counties_geojson, file='./data/us_counties_geojson.RData')
# save(us_states_geojson, file='./data/us_states_geojson.RData')
# save(countries_geojson, file='./data/countries_geojson.RData')

# us_counties_sp=sf::as(us_counties_geojson,'Spatial')
# us_states_sp=sf::as(us_states_geojson,'Spatial')
# countries_sp=sf::as(countries_geojson,'Spatial')
# save(us_counties_sp, file='./data/us_counties_sp.RData')
# save(us_states_sp, file='./data/us_states_sp.RData')
# save(countries_sp, file='./data/countries_sp.RData')
