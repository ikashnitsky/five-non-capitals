#===============================================================================
# 2022-04-10 -- twitter
# Five non-capital European cities
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

# Data gathered here
# https://twitter.com/ikashnitsky/status/1510260422884081674


library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(academictwitteR)
library(rtweet)
library(tidytext)
library(stringi)
library(textclean)
library(sf)
library(ggmap)
library(countrycode)
library(ggdark)

##------ Sun Apr  3 21:27:23 2022 ------##
replies <- get_all_tweets(
    n = 200,
    start_tweets = "2022-04-02T00:00:00Z",
    end_tweets = "2022-04-09T00:00:00Z",
    conversation_id = "1510260422884081674"
)

quote_tweets <- search_tweets(
    q = "https://twitter.com/ikashnitsky/status/1510260422884081674",
    include_rts = TRUE, type = "mixed"
) %>%
    transmute(author_id = user_id, text = text %>% replace_non_ascii)

save(replies, quote_tweets, file = "replies.rda")


# package maps contains a nice dataset of world cities
library(maps)

cities <- world.cities %>%
    mutate(name = name %>% str_to_lower)

# exceptions for double words
cities[32185, 1] <- "petersburg"
cities[43644, 1] <- "hague"

boo <- replies %>%
    filter(in_reply_to_user_id == "739773414316118017") %>%
    transmute(
        author_id,
        text = text %>% replace_non_ascii
    ) %>%
    bind_rows(quote_tweets) %>%
    unnest_tokens(output = name, input = text) %>%
    inner_join(cities) %>%
    distinct() %>%
    filter(
        -20 < long & long < 120, lat > 30,
        ! name %in% c("of", "are", "come", "pop", "as", "has", "hard", "many"),
        # manually  remove duplicate city names
        ! pop == 64389, ! pop == 15563, ! capital == 1
    )

# view # cities by country
cntr <- boo %>%
    group_by(country = country.etc) %>%
    summarise(n = n()) %>%
    arrange(n %>% desc) %>%
    mutate(iso_a2 = country %>% countrycode(origin = "country.name", destination = "iso2c"))

# tally # of city mentions
coo <- boo %>%
    group_by(name) %>%
    summarise(
        n = n(),
        lat = lat %>% first,
        long = long %>% first
    ) %>%
    ungroup() %>%
    arrange(n %>% desc)

# turn cities coordinates into sf
coo_geo <- coo %>%
    st_as_sf(
        coords = c("long", "lat"),
        crs = 4326
    )


# get world map outline (you might need to install the package)
world_outline <- spData::world %>%
    st_as_sf()

# let's use a fancy projection
world_outline_laea <- world_outline %>%
    st_transform(crs = 3035)

country_borders <- world_outline_robinson %>%
    rmapshaper::ms_innerlines()

# match countries existing in the data
cntr_sf <- cntr %>%
    left_join(world_outline_laea) %>%
    st_as_sf()

st_bbox(cntr_sf)


# map!
world_outline_laea %>%
    filter(!iso_a2 == "AQ") %>%
    ggplot()+
    geom_sf(fill = "#222222", color = NA)+
    geom_sf(data = cntr_sf, aes(fill = n, geometry = geom), color = NA)+
    geom_sf(data = country_borders, size = .25, color = "#BD92B7FF")+
    geom_sf(
        data = coo_geo, aes(size = n),
        fill = "#ffeb3b", color = "#c6a700", shape = 21
    )+
    scale_fill_viridis_c(option = "G", begin = .1)+
    scale_size_area(guide = NULL)+
    dark_theme_minimal(base_family = font_rc)+
    coord_sf(xlim = c(27e5, 72e5), ylim = c(15e5, 55e5),  datum = NA)+
    labs(
        title = "Which 5 non-capital cities in Europe\nfirst come to your mind?",
        caption = "Data: twitter.com/ikashnitsky/status/1510260422884081674 / Design: @ikashnitsky",
        fill = "Mentions\nper country"
    )+
    theme(
        plot.background = element_rect(fill = "#e5ffff", color = NA),
        axis.text = element_blank(),
        legend.position = c(.8, .75),
        plot.title = element_text(family = "Roboto Slab", face = 2, size = 16),
        text = element_text(color = "#c6a700")
    )

ggsave(
    "the-map.png",
    width = 4.5, height = 4.5, type = "cairo-png"
)



# interactive map with leaflet
library(leaflet)

out <- leaflet(coo_geo) %>%
    setView(25, 55, 4) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    # addPolygons() %>%
    addCircleMarkers(coo$long, coo$lat,
                     radius = coo$n %>% divide_by(2),
                     color = "#fbc02d", opacity = .9,
                     popup = coo$name,
                     label = coo$name)

htmlwidgets::saveWidget(out, "index.html", selfcontained = TRUE)


# the users who participated
uoo <- boo %>% pull(author_id) %>% lookup_users()

# copy screen names for tagging
uoo %>% pull(screen_name) %>% paste0("@", .) %>% clipr::write_clip()
