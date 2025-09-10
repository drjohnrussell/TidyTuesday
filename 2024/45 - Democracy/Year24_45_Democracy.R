library(tidytuesdayR)
library(tidyverse)

dat <- tidytuesdayR::tt_load(2024, week = 45)
democracy <- dat$democracy_data
democracy2 <- democracy |> 
  ## removing colonies as much as possible to get down to countries
  filter(!is.na(regime_category_index),
         ## British Virgin Islands oddly keeps showing up in spite of status as colony
         country_code!="VGB") |> 
  ## adding in parameters to use geom_rect
  mutate(country_name=as.factor(country_name),
         yearend=year+1)

## exploration 1
democracyfilter <- democracy2 |> 
  janitor::tabyl(country_name,has_free_and_fair_election) |> 
  filter(`TRUE` > 0 & `FALSE` > 0)

library(countrycode)
library(ggflags)

democracy3 <- democracy2 |> 
  filter(country_name %in% democracyfilter$country_name) |> 
  left_join(democracyfilter) |> 
  mutate(iso2=countrycode(country_name,"country.name","iso2c"),
         continent = countrycode(iso2, "iso2c", "continent")) |> 
  arrange(continent,`FALSE`) |> 
         mutate(country_name=fct_inorder(country_name))

y_lab <- democracy3 |> 
  filter(continent=="Europe") |> 
  distinct(country_name,iso2,continent)  |> 
  mutate(y_mid = as.numeric(country_name),
         name=country_name)
democracy3 |> 
  filter(continent=="Europe") |> 
  ggplot(aes(xmin=year,
                         xmax=yearend,
                         ymin=as.numeric(country_name)-.3,
                         ymax=as.numeric(country_name)+.3,
                         fill=has_free_and_fair_election)) +
  geom_rect() +
  ggflags::geom_flag(data=y_lab,mapping=aes(y=y_mid,country=tolower(iso2),x=1945),inherit.aes=FALSE) +
  scale_y_continuous(breaks=y_lab$y_mid,labels=y_lab$country_name) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.text=element_text(size=7),
        legend.title=element_text(size=7)) +
  labs(x="",
       y="",
       fill="Has Free and Fair Elections") +
  scale_fill_brewer(palette="Set1") +
  facet_wrap(~continent,ncol=1,scales="free",strip.position="right")

library(rnaturalearth)

europe <- ne_countries(type="countries",continent="europe",returnclass="sf")

ggsave("flags.png",bg="white",width=4,height=4)

## exploration 2
library(gganimate)

coord <- read_csv("https://gist.github.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv") |> 
  mutate(`Alpha-2 code`=replace_na(`Alpha-2 code`,"NA")) |> 
  rename(code=`Alpha-2 code`,
         lat=`Latitude (average)`,
         lon=`Longitude (average)`)

  
democracymap <- democracy2 |> 
  mutate(iso2=countrycode(country_name,"country.name","iso2c"),
         continent = countrycode(iso2, "iso2c", "continent")) |> 
  select(country_name,year,has_free_and_fair_election,iso2,continent) |> 
  left_join(coord |>
              select(code,lat,lon),by=c("iso2"="code"))

world <- ne_countries(type="countries",returnclass="sf")

democracymap2 <- democracymap |> 
  select(country_name,year,has_free_and_fair_election,iso2,lat,lon) |> 
  left_join(world, by=c("iso2"="iso_a2_eh")) |> 
  filter(!is.na(geometry))

library(ggthemes)
democracymap2 <- democracymap2 |> 
  mutate(year=as.integer(year))

map <- democracymap2 |> 
  filter(year==2020, !is.na(geometry)) |> 
  select(-year)

democracymap3 <- democracymap2 |> 
  filter(has_free_and_fair_election==TRUE)

#World
map |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry),fill="white",color="black") +
  geom_sf(data=democracymap3,
          mapping=aes(geometry=geometry),color=NA,fill="blue",
          show.legend=FALSE)+
  scale_x_longitude() +
  scale_y_latitude() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.text=element_text(size=8,
                                margin = margin(0,0,0,0, "cm")))


#Europe
library(gganimate)
library(metR)
map |> 
  filter(continent=="Europe") |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry),fill="white",color="black") +
  geom_sf(data=democracymap2 |> 
            filter(continent=="Europe"),
          mapping=aes(geometry=geometry,fill=has_free_and_fair_election,
                      alpha=has_free_and_fair_election),color=NA,
          show.legend=FALSE) +
  scale_fill_manual(values=c("black","blue")) +
  scale_alpha_manual(values=c(0,1)) +
  ggflags::geom_flag(data=democracymap2 |> 
                       filter(continent=="Europe",
                              has_free_and_fair_election==TRUE),
                     mapping=aes(y=lat,x=lon,country=tolower(iso2)),inherit.aes=FALSE) +
  labs(title="Fair and Free Elections by Year (current country boundaries)",
       subtitle = 'Year: {closest_state}') +
  scale_x_longitude() +
  scale_y_latitude() +
  transition_states(year) +
  coord_sf(xlim=c(-20,40),
           ylim=c(30,70),
           default_crs=sf::st_crs(4326))

#Africa
map |> 
  filter(continent=="Africa") |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry),fill="white",color="black") +
  geom_sf(data=democracymap2 |> 
            filter(continent=="Africa"),
          mapping=aes(geometry=geometry,fill=has_free_and_fair_election,
                      alpha=has_free_and_fair_election),color="black",
          show.legend=FALSE) +
  scale_fill_manual(values=c("black","blue")) +
  scale_alpha_manual(values=c(0,1)) +
  scale_x_longitude() +
  scale_y_latitude() +
  ggflags::geom_flag(data=democracymap2 |> 
                       filter(continent=="Africa",
                              has_free_and_fair_election==TRUE),
                     mapping=aes(y=lat,x=lon,country=tolower(iso2)),inherit.aes=FALSE) +
  labs(title = 'Year: {closest_state}') +
  theme_bw() +
  transition_states(year) +
  coord_sf(default_crs=sf::st_crs(4326))


  
