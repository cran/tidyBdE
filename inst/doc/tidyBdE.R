## ---- include = FALSE----------------------------------------------------------------------------

start_time <- Sys.time()

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = "styler",
  warning = FALSE,
  message = FALSE,
  dpi = 300,
  dev = "ragg_png",
  fig.path = "./",
  out.width = "100%"
)



## ----search, message = FALSE---------------------------------------------------------------------

library(tidyBdE)

# Load tidyverse for better handling
library(tidyverse)


# Search GBP on "TC" (exchange rate) catalog
XR_GBP <- bde_catalog_search("GBP", catalog="TC")

XR_GBP %>% 
  select(Numero_secuencial, Descripcion_de_la_serie) %>%
  # To table on document 
  knitr::kable()
  



## ----find----------------------------------------------------------------------------------------

seq_number <- XR_GBP %>%
  # First record
  slice(1) %>%
  # Get id
  select(Numero_secuencial) %>%
  # Convert to num
  as.double()


seq_number

# We can check the metadata of the series
seq_number %>% 
  bde_series_load(extract_metadata = TRUE) %>%
 # To nice table for the vignette
  knitr::kable()


time_series <- bde_series_load(seq_number, series_label = "EUR_GBP_XR") %>%
  filter(Date >= "2010-01-01" & Date <= "2020-12-31") %>%
  drop_na()



## ----chart, fig.asp=0.7--------------------------------------------------------------------------

ggplot(time_series, aes(x = Date, y = EUR_GBP_XR)) +
  geom_line(colour = bde_vivid_pal()(1)) +
  geom_smooth(method = "gam", colour = bde_vivid_pal()(2)[2]) +
  labs(title = "EUR/GBP Exchange Rate (2010-2020)",
       subtitle = "%",
       caption = "Source: BdE") +
  geom_vline(xintercept = as.Date("2016-06-23"),
             linetype = "dotted") +
  geom_label(aes(
    x = as.Date("2016-06-23"),
    y = .95,
    label = "Brexit"
  )) +
  coord_cartesian(ylim = c(0.7, 1)) +
  theme_bde()



## ----macroseries, fig.asp=0.7--------------------------------------------------------------------

gdp <- bde_ind_gdp_var("values")
gdp$label <- "GDP YoY"

UnempRate <- bde_ind_unemployment_rate("values")
UnempRate$label <- "Unemployment Rate"

plotseries <- bind_rows(gdp, UnempRate) %>%
  drop_na() %>%
  filter(Date >= "2010-01-01" & Date <= "2019-12-31")

ggplot(plotseries, aes(x = Date, y = values)) +
  geom_line(aes(color = label)) +
  labs(title = "Spanish Economic Indicators (2010-2019)",
       subtitle = "%",
       caption = "Source: BdE") +
  theme_bde() +
  scale_color_bde_d() # Custom palette on the package



## ----gdp-----------------------------------------------------------------------------------------
# Load GDP Series

GDP <- bde_series_load(
  series_code = c(3777251,
                  3777265,
                  3777259,
                  3777269,
                  3777060),
  series_label = c("Agriculture",
                   "Industry",
                   "Construction",
                   "Services",
                   "Total")
)


# Manipulate data - tidyverse style

GDP_all <- GDP %>%
  # Filter dates
  filter(Date <= "2020-12-31") %>%
  # Create 'Other' column and convert Date to year
  mutate(Other = Total -  rowSums(across(Agriculture:Services)),
         Date = as.numeric(format(Date, format = "%Y"))) %>%
  # Sum by year
  group_by(Date) %>%
  summarise_at(vars(-group_cols()), sum) %>%
  # Create percentage
  relocate(Total, .after = Other) %>%
  mutate(across(Agriculture:Other, ~ .x * 100 / Total)) %>%
  # Move cols to rows for plotting
  select(-Total) %>%
  pivot_longer(Agriculture:Other,
               names_to = "serie",
               values_to = "value")



ggplot(data = GDP_all, aes(x = Date,
                           y = value, 
                           fill = serie)) +
  geom_bar(position = "stack",
           stat = "identity",
           alpha = 0.8) +
  scale_fill_bde_d(palette = "bde_rose_pal") + # Custom palette on the package
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bde() +
  labs(title = "Spain: Gross domestic product by industry",
       subtitle = "%",
       caption = "Source: BdE")



## ---- eval=FALSE---------------------------------------------------------------------------------
#> options(bde_cache_dir = "./path/to/location")


## ---- eval=FALSE---------------------------------------------------------------------------------
#> bde_catalog_update()
#> 
#> # On most of the functions using the option update_cache = TRUE
#> 
#> bde_series_load("SOME ID", update_cache = TRUE)

