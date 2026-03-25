#Script i prosjekt assignment 1. Starter med å laste tidyverse

library(tidyverse)
library(readr)
library(dplyr)

#Part 1
#Read in the data owid-energy-data.csv and call this dataset energy. 
#Note: if you get parsing issues, you need to resolve them. 
#Show the steps you take and argue for the choices you make. 
#All changes to the data must be explicitly discussed.

energy <- read_csv("owid-energy-data.csv")  #reading file downloaded to the project-file

energy # gives warning with several parsing issues

problems(energy) #gives tibble with problem in colomn 14 of row 12 and 18 with "semicolon";" when expecting "."

energy <- energy |> 
  mutate(across(14, ~as.numeric(gsub(";", ".", .))))  #had to search in google/AI for advise how to mutate
  #along column 14 changing ";" to ".", leading to gsub function.

#Part 2
#Read in the data countries.xlsx and call this dataset countries. 
#Print only the first few lines and columns of this dataset.

library(readxl)

countries <- read_xlsx("countries.xlsx") #reading file downloaded to the project-file

countries #prints first few lines of both columns in the dataset 


#Part 3
#The dataset energy contains many missing values, especially in early years. 
#For the purpose of later continent-level analysis and plotting, identify a reasonable filtering strategy and justify it. 
#Hint: Your strategy should be based on the variables you will later use (e.g., consumption variables, gdp, population)

# Reading at https://github.com/owid/energy-data/blob/master/README.md data processing is made
#for regions aggregating from different sources EI, EIA, EMBER, SHIFT - we`re using aggregating datas in the following
#Datas for different regions grouped in different ways, aggregations fo Africa, Asia, Europe, North America
#South America, Oceania and Asia match Excel file and seem resonable to use. 
# EI registered from 1965, EMBER 2000, EIA 1980 and SHIFT 1900. Starting point 1965.
#Strategy to filter into new energy_filtered with pipeoperators...


### don`t really understand the question, filter in data modification lecture....
# user filter function with year after 1965, then select columns with gdp, popolation and containng consumpt

energy_filtered <- energy |> 
  filter( year >= 1965 )|> 
          select(contains("cons"),gdp,population)

energy_filtered #Gives an answear with planty of NA, these can be removed laterwith na.rm=TRUE


#Part4
#Utilize the countries dataset to create a bar chart displaying the number of countries per continent. 
#This will require you to count the number of occurrences of each continent in the dataset. 
#Plot these counts in a bar chart where each continent is represented by a different color. 
#Calculate the percentage share of each continent and include the percentage as text labels on the bars.

#Strategy is to careate count withhin group continent

countries_per_continent <- countries |> 
  count(Continent) |> 
  mutate(share = n / sum(n)) # count(Conuntry) gave n=1 per country, count(Continent) give countries in each continent
                             # i was not able to add percentage after creating graph - from chat gpt to add variable share
countries_per_continent

library(scales). #package added for percentage

ggplot(
  data = countries_per_continent ,
  mapping = aes(x = Continent , y = n, color = Continent)) + geom_bar(stat = "identity") +  #stat=identity from https://r-graph-gallery.com/218-basic-barplots-with-ggplot2.html
  geom_text(aes(label = percent(share)))         #creating percent from share in last section

#Part 5
#Filter the observations in energy to only contain the countries present in countries then add the 
#Continent variable to the energy dataset. 
#From energy select the columns year, continent, country, population, gdp and those with data on consumption. 
#Change the variable year to an integer variable and country and continent to factors. 
#Note: You should not get warnings here, if you do, then you did not do 1 correctly. 
#Print the first few lines and columns of the dataset only.

energy_filtered |>
  semi_join(countries, by = c("country" = "Country")) |> #matches the sets energy and countries through variable Country with capital letter corrected and keeps only columns the have in common
  left_join(countries, by = c("country" = "Country")) |> #adds variable Country from countries
  relocate(Continent,.after = country) |>
  mutate(year = as.integer(year), 
         country = as.factor(country), 
         Continent = as.factor(Continent))
#Part 8 
#Create a copy of energy and call it energy_loop. 
#Define a function to calculate the share of energy from each energy source defined above. 
#Using a for-loop where you loop over a vector of column names, transform each of the consumption variables for each energy source into a share of the total. 
#After transforming the variables into shares, verify that the shares sum to 1 (or approximately 1) for at least three randomly selected countries and years. 
#Show your verification.

energy_loop <- energy #Lager kopi

#Strategi er å summere kolonner med consumption variables for så å regne ut en brøk av dette

energy_sources <- c(
  "biofuel_consumption",
  "coal_consumption",
  "gas_consumption",
  "hydro_consumption",
  "nuclear_consumption",
  "oil_consumption",
  "other_renewable_consumption",
  "solar_consumption",
  "wind_consumption"
)
#lager en ny kolonne i energy_loop med summen at relevante kolonner for konsum: 
energy_loop$total_energy <- rowSums(     
  energy_loop[, energy_sources],
  na.rm = TRUE
)

# Hjelp fra chat gpt med funksjon som kalkulerer fraksjon

calc_share <- function(x, total) {
  return(x / total)
}

#gjør deretter iterasjon gjennom kolonnene hvor det kalkuleres brøk av total energy consumption

for (col in energy_sources) {
  energy_loop[[col]] <- calc_share(
    energy_loop[[col]],
    energy_loop$total_energy
  )
}

#Lager en kontroll hvor brøkene i sum skal bli omtrent 1, tung chat gpr bruk her
# Suppose your data has columns named "country" and "year"
# Select three countries and a given year, e.g., 2020
control_countries <- c("Norway", "Germany", "Brazil")
selected_year <- 2020

# Filter the rows
verification <- energy_loop[energy_loop$country %in% control_countries &
                              energy_loop$year == selected_year, ]

# Calculate row sums for the energy shares (should be ≈ 1)
verification$share_sum <- rowSums(verification[, energy_sources], na.rm = TRUE)

# Show the verification table
verification[, c("country", "year", "share_sum")]

# Dette løser oppgaven med mye chat gpt

#oppg9
##Create a new data set that is a subset of energy. This new dataset should contain the share of consumption from renewables and GDP per capita by continent and year.
#Print out the first few lines and columns of the new dataset. Hint: Create a new function to reduce code duplication. 
#Furthermore, think through the order of operations, they matter for the end result. 
#Explain why calculating GDP per capita before grouping by continent would lead to a different result than calculating it after grouping.

#strategi å bruke funksjon fra oppgave 5 for å hennte land og kontinent kopieres fra arbeidskrav. Det er bedre med inner join enn først semi og så left join. 

energy_renewables <- energy |>  #inner join hentet fra chat gpt - får ut meningsfull tabell
  #filter(!is.na(gdp)) |> #viser kun de med registereringer for gdp #forsøker å fjerne denne linjen da jeg trenger tallene i neste oppgave
  inner_join(countries, by = c("country" = "Country")) |> #beholder felles kolonner i begge filer
  relocate(Continent, .after = country) |> #legger inn continent etter country filen
  mutate(
    year = as.integer(year),
    country = as.factor(country),
    Continent = as.factor(Continent))

energy


energy_renewables %>% #fra chat, gir summering per kontinent videre, fjernes om ale skjærer seg
  group_by(Continent)

energy_renewables <- energy_renewables %>%
  mutate(gdp_per_capita = gdp / population, .after = gdp) # ny kolonne med gdp/capita legges til til høyre for gdp


energy_sources_renewable <- c( #lager vektor med fornybare kilder. Alle kilder i vektor fra forrige oppgave
  "biofuel_consumption",
  "hydro_consumption",
  "other_renewable_consumption",
  "solar_consumption",
  "wind_consumption"
)

energy_renewables$total_energy_consumption_allsource <- rowSums(     #henter funksjon fra forrige oppgave som summerer over kildene der, legges til som kolonnen total_energy_consumption_allsource
  energy_renewables[, energy_sources],
  na.rm = TRUE, 
)

energy_renewables$total_energy_consumption_renewable <- rowSums(     #henter funksjon fra forrige oppgave som summerer over kildene der, legges til som kolonnen total_energy_consumption_renewable
  energy_renewables[, energy_sources_renewable],
  na.rm = TRUE
)

energy_renewables <- energy_renewables %>% #legger til en kolonne med brøken av fornybar delt på allsource
  mutate(renewable_fraction = total_energy_consumption_renewable / 
           total_energy_consumption_allsource) |>
           filter(!is.na(renewable_fraction))

energy_renewables %>% select(renewable_fraction) #sjekker at den ikke er tom

energy_renewables <- energy_renewables |>
  relocate(renewable_fraction, .after = gdp_per_capita) #flytter til ved siden av gdp_per_capita så det er lettere å få oversikt

#det er kalkulert totalt forbruk i oppgaven over. Biofuel, hydo, sol, wind og other renewabler er fornybare. 
# må lage brøk hvor summen av disse deles på totale. Videre kolonne hvor gdp deles på befolkning. Filtrere vekk rader hvor det ikke er registerer gdp



energy_renewables

#NB regarding quastion about GDP/capita calculated before and after grouping by continent, 
#small and large countries will count the same regardless of size/population

#Using the data you created above in question 8, create a lineplot with the following properties/characteristics
#Only uses data after 1960
#Shows year on the x-axis, share of renewables consumption on the y-axis and has separate lines for each continent. Make sure that the legend is ordered based on the order of the lines in 2021.
#Customize the labels on the axis and legend
#Show a tic for every 5 years on the x-axis
#Change the theme of the plot.
#Highlight the continent with the largest renewable share in 2021 by increasing line thickness or annotating the final point.

#!! Renewables are in task 9 ...

energy_renewables_continent <- energy_renewables |> #lager et gjennomsnitt som vel er helt meningsløst, men som gir trend over tid
  group_by(Continent, year) |>  #skal være grupert fra tidliere
  summarise(mean_renewable_fraction = mean(renewable_fraction, na.rm = TRUE))

energy_renewables_continent #sjekker at jeg får ut tabell med snitt per kontinent per år

#tung chat gpt bruk for order 2021, men leser dette som at grafen legges i høyde etter 2021
order_2021 <- energy_renewables_continent |> 
  filter(year == 2021) |> 
  arrange(mean_renewable_fraction) |> 
  pull(Continent)

top_continent <- energy_renewables_continent |> #kartlegger hvilket kontinent som ligger høyset, legger dette til i aes for geom line senere
  filter(year == 2021) |> 
  slice_max(mean_renewable_fraction, n = 1) |> 
  pull(Continent)


#fortsatt chat bruk, legger til kolonne i erergy_renewables_continent

energy_renewables_continent <- energy_renewables_continent %>%
  mutate(Continent = factor(Continent, levels = order_2021))


ggplot(energy_renewables_continent |> filter(year>=1960), aes(x = year, y = mean_renewable_fraction, color = Continent)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2021, by = 5)) + #setter sekvenseringen til 2021, skjønner ikke maksfunksjonen
  labs(title = "Mean Renewable Energy Fraction by Continent",
       y = "Mean Renewable Fraction") +
  theme_classic() # theme endres her, classic, modern, minimal etc

# det er ikke markert for top_kontinent - sliter med å finne elegant måte å adressere dette