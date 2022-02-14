library(tidyverse)


######################################################

# Graph of Social democratic parliamentary strength in western Europe

days <- c('01','02','03','04','05','06','07','08','09',10:31)
months <- c('01','02','03','04','05','06','07','08','09','10','11','12')
yearmonths_country <- tibble(date = character())

for (i in 1901:2021) {
  for (x in 1:12) {
    for (y in 1:31) {
      temp <- tibble(date = paste0(i,'-',months[x],'-',days[y]))
      yearmonths_country <- bind_rows(yearmonths_country, as.tibble(temp)) 
    }
  }
}
rm(temp)

cab_party_share <- read_csv("https://raw.githubusercontent.com/hdigital/parlgov-snippets/master/cabinet-party-family/cabinet-party-family.csv")

cab_party_share %>%
  group_by(country_name) %>%
  mutate(end_date = lead(start_date)) -> cab_party_share
cab_party_share$end_date <- replace_na(cab_party_share$end_date, "2021-12-31")
cab_party_share$end_date <- as.character(cab_party_share$end_date)
cab_party_share$start_date <- as.character(cab_party_share$start_date)

cab_party_share <- cab_party_share[cab_party_share$soc > 0 | cab_party_share$com > 0,]
cab_party_share$weight <- cab_party_share$com + cab_party_share$soc

countries <- unique(cab_party_share$country_name_short)
for (i in 1:length(countries)) {
  yearmonths_country[, paste0(countries[[i]])] <- ifelse(yearmonths_country$date %in% cab_party_share$start_date[cab_party_share$country_name_short == countries[i]], "start",
                                                         ifelse(lead(yearmonths_country$date) %in% cab_party_share$end_date[cab_party_share$country_name_short == countries[i]], "end", NA))
}

for (i in 2:length(yearmonths_country)) {
  names(yearmonths_country)[i] <- unique(cab_party_share$country_name_short[cab_party_share$country_name_short == names(yearmonths_country)[i]])
}

for (i in 2:length(yearmonths_country)) {
  start_ind <- which(yearmonths_country[,i] == "start")
  end_ind <- which(yearmonths_country[,i] == "end")
  for (x in 1:length(start_ind)) {
    score <- cab_party_share$weight[cab_party_share$country_name_short == names(yearmonths_country[,i]) & cab_party_share$start_date == yearmonths_country[[start_ind[[x]],1]]]
    yearmonths_country[start_ind[[x]]:end_ind[[x]],i] <- as.character(score)
  }
}

yearmonths_country[is.na(yearmonths_country)] <- '0'

yearmonths_country %>%
  pivot_longer(-date, names_to = 'Countries', values_to = 'Value') %>%
  filter(Countries %in% c('AUT', 'BEL','CHE','CZE','DEU','DNK','ESP','FIN','FRA','GBR',
                          'GRC','IRL','ISL','ITA','LUX','NLD','NOR','PRT','SWE')) %>%
  mutate(year = as.numeric(substr(date,1,4))) %>%
  group_by(Countries, year) %>%
  summarise(Value = sum(as.numeric(Value))) -> graphdata


graphdata$Countries <- factor(graphdata$Countries, levels = c('DNK','NOR','SWE','FIN','ISL',
                                                              'AUT','DEU','CZE',
                                                              'NLD','BEL','LUX','CHE','FRA',
                                                              'ITA','PRT','ESP','GRC',
                                                              'GBR','IRL'))

ggplot(graphdata, aes(x = year, y = Countries, fill = Value)) + geom_tile(color = "transparent") + 
  scale_fill_gradient(low = "transparent", high = 'pink') +
  xlab(NULL) + ylab(NULL) + labs(caption = "Source: ParlGov database. Days spent under communist and social democratic cabinets (weighted by proportion of seats).") +
  scale_x_continuous(limits = c(1910, 2015)) +
  scale_y_discrete(limits = rev(levels(graphdata$Countries))) +
  theme_base() + theme(legend.position = "none",
                       panel.border = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks = element_blank()) -> fig1

graphdata %>%
  group_by(year) %>%
  summarise(Value = sum(as.numeric(Value), na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = Value)) + geom_line(color = "pink") + 
  xlab(NULL) + ylab(NULL) + labs(title = "Left Governments in Western Europe (1900-2021)") +
  scale_x_continuous(limits = c(1910, 2015))  +
  scale_y_continuous(breaks = c(150000), labels = c('TOTAL')) +
  theme_base() + theme(legend.position = "none",
                       panel.border = element_blank(),
                       axis.line = element_blank(), 
                       axis.ticks = element_blank(), 
                       axis.title.x = element_blank(),
                       axis.text.x = element_blank()) -> fig2

gridExtra::grid.arrange(fig2, fig1, nrow=2, heights=c(1.5, 4)) -> final

graphdata %>%
  group_by(year) %>%
  summarise(leftgovs = sum(as.numeric(Value), na.rm = TRUE)) %>%
  mutate(leftgovs = leftgovs/(365*19)) -> control # turn into 0 - 100 index

rm(cab_party_share, graphdata, yearmonths_country, countries, days, end_ind,i, months, score, start_ind,x,y)

######################################################
## ParlGov data set - add CMP identifyer

cabs <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv")
parties <- read_csv("https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv")

cabs <- merge(cabs, parties[names(parties) %in% c("party_id","cmp", "party_name", "family_name")],
              by = "party_id", all = TRUE, suffixes = c('','_ParlGovParties'))

rm(parties)



######################################################
## Party Facts data set - alternative CMP identifyer

# download and read Party Facts mapping table
file_name <- 'partyfacts-mapping.csv'
if( ! file_name %in% list.files())
  url <- 'https://partyfacts.herokuapp.com/download/external-parties-csv/'
download.file(url, file_name)
partyfacts <- read.csv(file_name, as.is=TRUE)  # maybe conversion of character encoding

# show and select available datasets
cat(paste(unique(partyfacts$dataset_key), collapse='\n'))
dataset_1 <- 'manifesto'
dataset_2 <- 'parlgov'

# merge two datasets selected
first <- partyfacts[partyfacts$dataset_key == dataset_1, ]
second <- partyfacts[partyfacts$dataset_key == dataset_2, ]
merge_table <- merge(first, second, by='partyfacts_id', all=TRUE)

# write results into file with dataset names in file name
file_out <- sprintf('partyfacts-%s-%s.csv', dataset_1, dataset_2)

cabs <- merge(cabs, merge_table[names(merge_table) %in% c("name.y", "dataset_party_id.y", "dataset_party_id.x")],
              by.x = "party_id", by.y = "dataset_party_id.y", all = TRUE, suffixes = c('','_PartyFacts'))

names(cabs)[names(cabs) == "cmp"] <- "cmp_ParlGovParties"
names(cabs)[names(cabs) == "dataset_party_id.x"] <- "cmp_PartyFacts"
names(cabs)[names(cabs) == "name.y"] <- "party_name_PartyFacts"
cabs$cmp_ParlGovParties <- as.character(cabs$cmp_ParlGovParties)

rm(first, merge_table, partyfacts, second, dataset_1, dataset_2, file_name, file_out)



######################################################
## Subset by membership of "UN regional group: Western European and Others" & Left parties only

cabs <- cabs[!(is.na(cabs$party_id)),]
cabs <- cabs[!(is.na(cabs$country_id)),]

country_codes <- c(59, 64, 21, 67, 43, 54, 41, 56, 37, 26, 7, 72, 8, 9, 63, 27, 35, 40, 44)
# UN regional group: Western European and Others: Andorra (NA), Austria, Belgium, Denmark,
# Finland, France, Germany, Greece, Iceland, Ireland, Italy, Liechtenstein (NA), Luxembourg,
# Malta, Monaco (NA), Netherlands, Norway, Portugal, San Marino (NA), Spain, Sweden,
# Switzerland, United Kingdom
cabs <- cabs[cabs$country_id %in% country_codes,]

######################################################
## Checking CMP identifyer

cabs$cmp_ParlGovParties <- replace_na(cabs$cmp_ParlGovParties, "Not found")
cabs$cmp_PartyFacts <- replace_na(cabs$cmp_PartyFacts, "Not found")

cabs$cmp_PartyFacts <- ifelse(cabs$cmp_PartyFacts == "Not found", cabs$cmp_ParlGovParties, cabs$cmp_PartyFacts)
cabs$cmp_ParlGovParties <- ifelse(cabs$cmp_ParlGovParties == "Not found", cabs$cmp_PartyFacts, cabs$cmp_ParlGovParties)

cabs$match <- (cabs$cmp_PartyFacts == cabs$cmp_ParlGovParties)

#install.packages("manifestoR")
library(manifestoR)
mp_setapikey("/Users/matteo/Code/API Keys/manifesto_apikey.txt")
cmp <- mp_maindataset()

country_codes <- c(11,12,13,14,15,21,22,23,31,32,33,34,35,41,42,43,51,52,53,54)
cmp <- cmp[cmp$country %in% country_codes,]

cabs[cabs$match == FALSE & cabs$family_name %in% c("Social democracy","Communist/Socialist"),] -> notmatching
unique(notmatching$party_name)
unique(notmatching$party_name_PartyFacts)
unique(notmatching$party_name_ParlGovParties)

## Issues:
# 1
# The Flemish Socialist Party in the 2003 and 2007 elections ran as part of a cartel
# with the liberal Spirit Party.
# CMP codes those two elections as a different party (with two entries for them in 2007):
# 21321 for the long running Flemish Socialist Party which exists before and after 2003-7
# 21221 for the two years it ran as part of larger cartel
# => Keep separate but record government presence, don't compare new manifesto to cartel manifesto
cabs$cmp_ParlGovParties[cabs$party_name == "Socialistische Partij Anders / Sociaal-Liberale Partij"] <- "Not found"

# 2
# L'Unione-Prodi was direct hier of the Olive Tree coalition coalition which
# represented the centre-left in the 1996 and 2001 general elections. However,
# The Union also included parties of the radical left, which were not affiliated
# with The Olive Tree. CMP has L'Unione on its own for 2006 election (32955), alongside
# Oliver Tree (32329) - which was a smaller federation of parties which merged
# to form the Democratic Party in October 2007
# => Code both L'Unione and Olive Tree in 2006 as having been in gov, don't compute change
cabs$cmp_ParlGovParties[cabs$party_name == "L'Unione-Prodi"] <- "Not found"

# 3
# CMP divids Sinn Fein in Ireland in 1980s as Workers Party (53220), from nationalist
# party afterwards (53951)
# => keep separate but recode both as Left parties [might need to check other parties which have been
# erroneously dropped fom party family... No obvious candidates]
cmp$parfam[cmp$party == 53951] <- 30

# 4
# CMP separates Die Linke pre/post 2007 election
# => Default to CMP coding as two separate parties [wikipedia says new party formed in 2007]
# Never in government
rm(notmatching)


######################################################
## Create cmp$ingov variable (IV)

cmp <- cmp[cmp$parfam %in% c(20,30),] # subset by party family
cabs <- cabs[cabs$cabinet_party == 1,] # subset by only parties of government

cmp <- merge(cmp, unique(cabs[names(cabs) %in% c('cmp_ParlGovParties','country_name','election_date','cabinet_party')]),
             by.x = c('party', 'countryname','edate'),
             by.y = c('cmp_ParlGovParties','country_name','election_date'),
             all.x = TRUE, all.y = FALSE) # merge to create indicator variable in cmp of whether they were in gov or not after that election

cmp %>%
  group_by(party) %>%
  arrange(edate) %>%
  mutate(wasingov = lag(cabinet_party)) -> cmp # take lag to indicate if they were in gov in the previous period
cmp$wasingov <- replace_na(cmp$wasingov, 0)


######################################################
## Create cmp$ideological_change variable (DV)

cmp$sd_rile <- (cmp$rile - mean(cmp$rile, na.rm = TRUE))/sd(cmp$rile, na.rm = TRUE)

cmp %>%
  group_by(party) %>%
  arrange(date) %>%
  mutate(ideological_change = sd_rile - lag(sd_rile)) -> cmp


######################################################
## Data corrections

# need to correct for cases where the lag between cmp data is more than
# one election cycle and reset the IV and DV to 0/NA

countries <- unique(cmp$countryname)
lookup <- tibble(countryname = character(), date = numeric(), real_date_diff = numeric())
for (i in 1:length(countries)){
  bind_rows(lookup, tibble(countryname = countries[[i]],
                           date = unique(cmp$date[cmp$countryname == countries[[i]]]),
                           real_date_diff = c(NA, diff(unique(cmp$date[cmp$countryname == countries[[i]]]))))) -> lookup
}

cmp %>%
  group_by(party) %>%
  arrange(date) %>%
  mutate(date_diff = date - lag(date)) -> cmp
cmp$date_diff <- replace_na(cmp$date_diff, 0)

cmp <- merge(cmp, lookup, by = c('countryname', 'date'), all.x = TRUE, all.y = FALSE)
cmp$real_date_diff <- replace_na(cmp$real_date_diff, 99999999)

cmp[cmp$date_diff - cmp$real_date_diff != 0,] %>%
  filter(!(is.na(.$countryname))) %>%
  select(countryname, date, partyname, date_diff, real_date_diff) %>%
  filter(date_diff != 0) -> test # 14 cases to look up

rm(lookup, test, countries, country_codes, i)

######################################################
## Prepare for analysis

cmp$wasingov <- as.factor(cmp$wasingov)

cmp$five_year <- paste0(substr(cmp$date,1,3),ifelse(substr(cmp$date,4,4)>4,5,0))
cmp$decade <- paste0(substr(cmp$date,1,3), 0)
cmp$year <- as.numeric(substr(cmp$date,1,4))

control %>%
  mutate(five_year = paste0(substr(control$year,1,3),ifelse(substr(control$year,4,4)>4,5,0))) %>%
  group_by(five_year) %>%
  summarise(leftpower = sum(Value)/5) -> control2

cmp <- merge(cmp, control, by = 'year', all.x = TRUE)
cmp <- merge(cmp, control2, by = 'five_year', all.x = TRUE)

rm(control, control2)

######################################################
## Analysis

results <- tibble(sample = character(), model = character(), coef = numeric(),
                  low = numeric(), high = numeric())
samples <- list('Full sample', 'Ever in power')

for (i in c(0,1)){
  governingparties <- unique(cmp$party[cmp$wasingov %in% c(i,1)])
  sample <- samples[[i+1]]
  cmp %>%
    filter(party %in% governingparties) -> temp
  pooled <- glm(ideological_change ~ wasingov,
                family = "gaussian", data = temp)
  fe <- glm(ideological_change ~ wasingov + factor(party),
            family = "gaussian", data = temp)
  twfe_decade <- glm(ideological_change ~ wasingov + factor(party) + factor(decade),
                     family = "gaussian", data = temp)
  output <- tibble(sample = sample, model = c('pooled','fe','twfe_decade'),
                   coef = c(pooled$coefficient['wasingov1'], fe$coefficient['wasingov1'], twfe_decade$coefficient['wasingov1']),
                   low = c(confint(pooled, 'wasingov1')[1], confint(fe, 'wasingov1')[1], confint(twfe_decade, 'wasingov1')[1]),
                   high = c(confint(pooled, 'wasingov1')[2], confint(fe, 'wasingov1')[2], confint(twfe_decade, 'wasingov1')[2]))
  results <- bind_rows(results, output)
}

ggplot(results, aes(y = model, x = coef, xmin = low, xmax = high)) +
  geom_point() +
  geom_linerange() +
  geom_vline(xintercept = 0, colour =  'black') +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(limits = c(-0.25,0.35)) +
  theme_bw() +
  facet_grid(facets = vars(sample), rows = 3, scales = "fixed", space = 'fixed')

Results <- tibble(Year = numeric(), wasingov0 = numeric(), wasingov1 = numeric(),
                  p.value = numeric(), n_ingov = numeric(), n_outgov = numeric())

for (i in 1925:2021) {
  cmp %>%
    filter(year >= (i-3) & year <= (i+3)) -> temp
  n_ingov = nrow(temp[temp$wasingov == 1,])
  n_outgov = nrow(temp[temp$wasingov == 0,])
  tryCatch({t.test(x = temp$rile[temp$wasingov == 0],
                  y = temp$rile[temp$wasingov == 1],
                  alternative = "two.sided", var.equal = FALSE, mu = 0) %>%
             broom::tidy() -> temp
           bind_rows(Results, tibble(Year = i,
                                     wasingov0 = temp$estimate1,
                                     wasingov1 = temp$estimate2,
                                     p.value = temp$p.value,
                                     n_ingov = n_ingov,
                                     n_outgov = n_outgov)) -> Results},
  error = function(cond){bind_rows(Results, tibble(Year = i,
                                             wasingov0 = mean(temp$rile[temp$wasingov == 0], na.rm = TRUE),
                                             wasingov1 = mean(temp$rile[temp$wasingov == 1], na.rm = TRUE),
                                             p.value = NA,
                                             n_ingov = n_ingov,
                                             n_outgov = n_outgov)) -> Results})
}

meansovertime <- tibble(Year = numeric(), wasingov0 = numeric(), wasingov1 = numeric())

for (i in 1945:2021) {
  cmp %>%
    filter(year >= (i-3) & year <= (i+3)) -> temp
  temp$pervote <- replace_na(temp$pervote, 0)
  temp$cabinet_party <- replace_na(temp$cabinet_party, 0)
  bind_rows(meansovertime, tibble(Year = i,
                            wasingov0 = weighted.mean(x = temp$rile[temp$cabinet_party == 0], 
                                                      w = temp$pervote[temp$cabinet_party == 0],
                                                      na.rm = TRUE),
                            wasingov1 = weighted.mean(x = temp$rile[temp$cabinet_party == 1],
                                                      w = temp$pervote[temp$cabinet_party == 1],
                                                      na.rm = TRUE))) -> meansovertime
}


meansovertime %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value, colour = Variable)) + geom_line() + theme_base() +
  scale_colour_discrete(name = NULL, breaks = c('wasingov1','wasingov0'), 
                        labels = c('Going into power', 'Not going into power')) +
  xlab(NULL) + ylab(NULL) + labs(title = 'Mean ideological position of socialist and communist party manifestos in western Europe',
                                               caption = 'Source: Comparative Manifesto Project & ParlGov. Lower scores are more economically left wing. Weighted by party vote share.') +
  theme(legend.position = c(0.89,0.9),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

Results %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>%
  filter(Variable %in% c('n_ingov')) %>%
  ggplot(aes(x = Year, y = Value)) + geom_line() + theme_bw()

summary(glm(ideological_change ~ wasingov*leftpower,
              family = "gaussian", data = cmp))
summary(glm(ideological_change ~ wasingov*leftpower + factor(party),
          family = "gaussian", data = cmp))

### Nothing statistically significant