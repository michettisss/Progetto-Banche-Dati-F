#
#
#
#
#
#
#
#
#
install.packages("psych")
library(psych)
install.packages("jsonlite")
library(jsonlite)
install.packages("ggplot2")
library(ggplot2)
hdi.ssa <- fromJSON("hdr-data.json")
hdi.ssa <- hdi.ssa[, c(2,8,11)]
names(hdi.ssa)[names(hdi.ssa) == "value"] <- "HDI"
hdi.ssa$year <- as.integer(hdi.ssa$year)
hdi.ssa$HDI <- as.numeric(hdi.ssa$HDI)

hdi.w <- fromJSON("hdi.world.json")
hdi.w <- hdi.w[, c(2,8,11)]
names(hdi.w)[names(hdi.w) == "value"] <- "HDI"
hdi.w$year <- as.integer(hdi.w$year)
hdi.w$HDI <- as.numeric(hdi.w$HDI)



hdi <- rbind(hdi.ssa, hdi.w)
ggplot(hdi, aes(x = year, y = HDI, color = country)) +
  geom_line(size = 1.2) +
  labs(
    title = "HDI Sub-Saharan Africa vs World",
    x = "Anno",
    y = "HDI",
    color = "Legenda"
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  scale_y_continuous(breaks = seq(0.4, 0.8, by = 0.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#
#
#
#
#
#
install.packages("gt")
library(gt)
library(dplyr)
library(tidyr)

hdi %>%
  group_by(country) %>% 
  summarise(
    Media = mean(HDI, na.rm = TRUE),
    SD = sd(HDI, na.rm = TRUE),
    Min = min(HDI, na.rm = TRUE),
    Max = max(HDI, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche HDI per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max), 
    decimals = 3
  ) %>%
  cols_label(
    country = "Paese/Area",
    SD = "Dev. Standard",
  )
#
#
#
#
#
#
#
#
#
#
#SENEGAL
hdi.sen <- fromJSON("hdi.sen.json")
hdi.sen <- hdi.sen[, c(2,8,11)]
names(hdi.sen)[names(hdi.sen) == "value"] <- "HDI"
hdi.sen$year <- as.integer(hdi.sen$year)
hdi.sen$HDI <- as.numeric(hdi.sen$HDI)

#NIGERIA
hdi.nga <- fromJSON("hdi.NGA.json")
hdi.nga <- hdi.nga[, c(2,8,11)]
names(hdi.nga)[names(hdi.nga) == "value"] <- "HDI"
hdi.nga$year <- as.integer(hdi.nga$year)
hdi.nga$HDI <- as.numeric(hdi.nga$HDI)

#GHANA
hdi.gha <- fromJSON("hdi.gha.json")
hdi.gha <- hdi.gha[, c(2,8,11)]
names(hdi.gha)[names(hdi.gha) == "value"] <- "HDI"
hdi.gha$year <- as.integer(hdi.gha$year)
hdi.gha$HDI <- as.numeric(hdi.gha$HDI)

#SUDAFRICA
hdi.zaf <- fromJSON("hdi.zaf.json")
hdi.zaf <- hdi.zaf[, c(2,8,11)]
names(hdi.zaf)[names(hdi.zaf) == "value"] <- "HDI"
hdi.zaf$year <- as.integer(hdi.zaf$year)
hdi.zaf$HDI <- as.numeric(hdi.zaf$HDI)

#GRAFICO 3 paesi a confronto
hdi_3 <- rbind(hdi.nga, hdi.gha, hdi.sen, hdi.zaf, hdi.ssa)
ggplot(hdi_3, aes(x = year, y = HDI, color = country)) +
  geom_line(size = 1.2) +
  labs(
    title = "HDI Nigeria, Ghana, Senegal, Sudafrica",
    x = "Anno",
    y = "HDI",
    color = "Paese"
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  scale_y_continuous(breaks = seq(0.4, 0.8, by = 0.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
d_hdi_3 <- (describe(hdi_3)) 
View(d_hdi_3)
#
#
#
#
#
#
#
#
hdi_3 %>%
  group_by(country) %>% 

  summarise(
    Media = mean(HDI, na.rm = TRUE),
    SD = sd(HDI, na.rm = TRUE),
    Min = min(HDI, na.rm = TRUE),
    Max = max(HDI, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche HDI per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max), 
    decimals = 3
  ) %>%
  cols_label(
    country = "Paese/Area",
    SD = "Dev. Standard",
  )
#
#
#
#
#
install.packages("readxl")
library(readxl)
library(tidyr)
library(dplyr)
gdp <- read_excel("gdp.xlsx")
gdp <- gdp[c(1:5),c(-1,-2,-4)]

names(gdp)
if ("Country Name" %in% names(gdp)) {
  names(gdp)[names(gdp) == "Country Name"] <- "Country.Name"
}

gdp[ , -1] <- lapply(gdp[ , -1], function(x) as.numeric(as.character(x)))
gdp_long <- gdp %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "gdp"
  )
gdp_long$year <- as.integer(sub("\\[.*", "", gdp_long$year))



ggplot(gdp_long, aes(x = year, y = gdp, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "GDP per capita (current US$)",
    x = "Anno",
    y = "GDP per capita (current US$)",
    color = "Paese"
  ) +
  theme_minimal()
d_gdp <- (describe(gdp)) 
View(d_gdp)

#
#
#
#
gdp_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(gdp, na.rm = TRUE),
    SD = sd(gdp, na.rm = TRUE),
    Min = min(gdp, na.rm = TRUE),
    Max = max(gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche GDP per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
fert <- read_excel ("fert.xlsx")
fert <- fert[c(1:4),c(-2,-3,-4)]
names(fert)
if ("Country Name" %in% names(fert)) {
  names(fert)[names(fert) == "Country Name"] <- "Country.Name"
}

fert[ , -1] <- lapply(fert[ , -1], function(x) as.numeric(as.character(x)))
fert_long <- fert %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "Fertility"
  )
fert_long$year <- as.integer(sub("\\[.*", "", fert_long$year))

ggplot(fert_long, aes(x = year, y = Fertility, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Fertility rate",
    x = "Anno",
    y = "Fertility rate",
    color = "Paese"
  ) +
  theme_minimal()
d_fert <- describe(fert)
View(d_fert)
#
#
#
fert_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Fertility, na.rm = TRUE),
    SD = sd(Fertility, na.rm = TRUE),
    Min = min(Fertility, na.rm = TRUE),
    Max = max(Fertility, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche Fecondità per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
#
wat <- read_excel ("Acqua.xlsx")
wat <- wat[c(1:4),c(-2,-3,-4)]
wat <- wat[,c(1,12:ncol(wat))]
names(wat)
if ("Country Name" %in% names(wat)) {
  names(wat)[names(wat) == "Country Name"] <- "Country.Name"
}

wat[ , -1] <- lapply(wat[ , -1], function(x) as.numeric(as.character(x)))
wat_long <- wat %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "Acqua"
  )
wat_long$year <- as.integer(sub("\\[.*", "", wat_long$year))

ggplot(wat_long, aes(x = year, y = Acqua, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Acqua",
    x = "Anno",
    y = "Acqua",
    color = "Paese"
  ) +
  theme_minimal()
d_wat <- describe(wat)
View(d_wat)

#
#
#
wat_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Acqua, na.rm = TRUE),
    SD = sd(Acqua, na.rm = TRUE),
    Min = min(Acqua, na.rm = TRUE),
    Max = max(Acqua, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche % pop con accesso ad acqua potabile di base per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
mort <- read_excel ("tasso di mort.xlsx")
mort <- mort[c(1:8),c(-2,-3,-4)]
library(dplyr)

names(mort)
if ("Country Name" %in% names(mort)) {
  names(mort)[names(mort) == "Country Name"] <- "Country.Name"
}

mort[ , -1] <- lapply(mort[ , -1], function(x) as.numeric(as.character(x)))

mort_long <- mort %>%
  pivot_longer(cols = -Country.Name, names_to = "year", values_to = "Mortality")

mort_long$year <- as.integer(sub("\\[.*", "", mort_long$year))

mort_final <- mort_long %>%
  group_by(Country.Name, year) %>%
  summarise(Mortality = mean(Mortality, na.rm = TRUE))

ggplot(mort_final, aes(x = year, y = Mortality, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Tasso di mortalità",
    x = "Anno",
    y = "Tasso di mortalità",
    color = "Paese"
  ) +
  theme_minimal()
d_mort <- describe(mort)
View(d_mort)

#
#
#
mort_final %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Mortality, na.rm = TRUE),
    SD = sd(Mortality, na.rm = TRUE),
    Min = min(Mortality, na.rm = TRUE),
    Max = max(Mortality, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche tasso di mortalità (%) per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
pop <- read_excel ("pop.xlsx")
pop <- pop[c(1:4),c(-2,-3,-4)]
names(pop)
if ("Country Name" %in% names(pop)) {
  names(pop)[names(pop) == "Country Name"] <- "Country.Name"
}

pop[ , -1] <- lapply(pop[ , -1], function(x) as.numeric(as.character(x)))
pop_long <- pop %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "Population"
  )
pop_long$year <- as.integer(sub("\\[.*", "", pop_long$year))

ggplot(pop_long, aes(x = year, y = Population, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Popolazione",
    x = "Anno",
    y = "Popolazione",
    color = "Paese"
  ) +
  theme_minimal()
d_pop <- describe(pop)
View(d_pop)
#
#
#
pop_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Population, na.rm = TRUE),
    SD = sd(Population, na.rm = TRUE),
    Min = min(Population, na.rm = TRUE),
    Max = max(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche popolazione per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
elet <- read_excel ("elet.xlsx")
elet <- elet[c(1:4),c(-2,-3,-4)]
elet <- elet[,c(1,8:ncol(elet))]
names(elet)
if ("Country Name" %in% names(elet)) {
  names(elet)[names(elet) == "Country Name"] <- "Country.Name"
}

elet[ , -1] <- lapply(elet[ , -1], function(x) as.numeric(as.character(x)))
elet_long <- elet %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "Electricity"
  )
elet_long$year <- as.integer(sub("\\[.*", "", elet_long$year))

ggplot(elet_long, aes(x = year, y = Electricity , color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Electricità",
    x = "Anno",
    y = " Accesso all'elettricità",
    color = "Paese"
  ) +
  theme_minimal()
d_elet <- describe(elet)
View(d_elet)
#
#
#
#
#
elet_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Electricity, na.rm = TRUE),
    SD = sd(Electricity, na.rm = TRUE),
    Min = min(Electricity, na.rm = TRUE),
    Max = max(Electricity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche accesso all'elettricità (% di popolazione) per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
#
invst <- read_excel("investimenti stranieri.xlsx")
invst <- invst[c(1:4), c(-2, -3, -4)]

if ("Country Name" %in% names(invst)) {
  names(invst)[names(invst) == "Country Name"] <- "Country.Name"
}

invst[ , -1] <- lapply(invst[ , -1], function(x) as.numeric(as.character(x)))

invst_long <- invst %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    values_to = "Investimenti_Stranieri"   # safe name (no spaces)
  )

invst_long$year <- as.integer(sub("\\[.*", "", invst_long$year))

ggplot(invst_long, aes(x = year, y = Investimenti_Stranieri, color = Country.Name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Investimenti stranieri",
    x = "Anno",
    y = "Investimenti stranieri",
    color = "Paese"
  ) +
  theme_minimal()
d_invst <- describe(invst)
View(d_invst)
#
#
#
invst_long %>%
  group_by(Country.Name) %>%
  summarise(
    Media = mean(Investimenti_Stranieri, na.rm = TRUE),
    SD = sd(Investimenti_Stranieri, na.rm = TRUE),
    Min = min(Investimenti_Stranieri, na.rm = TRUE),
    Max = max(Investimenti_Stranieri, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Statistiche FDI netti(% PIL) per Paese",
    subtitle = "Confronto tra aree geografiche"
  ) %>%
  fmt_number(
    columns = c(Media, SD, Min, Max),
    decimals = 3
  ) %>%
  cols_label(
    Country.Name = "Paese/Area",
    SD = "Dev. Standard"
  )
#
#
#
#
#
#
#
mort.gha <- mort_final[c(1:34),]
mort.nga <- mort_final[c(35:68),]
mort.sen <- mort_final[c(69:102),]
mort.zaf <- mort_final[c(103:136),]
elet.gha <- elet_long[c(1:28),]
elet.nga <- elet_long[c(57:84),]
elet.sen <- elet_long[c(85:112),]
elet.zaf <- elet_long[c(29:56),]
gdp.gha <- gdp_long[c(1:34),]
gdp.nga <- gdp_long[c(35:68),]
gdp.sen <- gdp_long[c(69:102),]
gdp.zaf <- gdp_long[c(103:136),]
invst.gha <- invst_long[c(1:34),]
invst.nga <- invst_long[c(35:68),]
invst.sen <- invst_long[c(69:102),]
invst.zaf <- invst_long[c(103:136),]
pop.gha <- pop_long[c(35:68),]
pop.nga <- pop_long[c(103:136),]
pop.sen <- pop_long[c(1:34),]
pop.zaf <- pop_long[c(69:102),]
wat.gha <- wat_long[c(25:48),]
wat.nga <- wat_long[c(1:24),]
wat.sen <- wat_long[c(49:72),]
wat.zaf <- wat_long[c(73:96),]
fert.gha <- fert_long[c(35:68),]
fert.nga <- fert_long[c(69:102),]
fert.sen <- fert_long[c(1:34),]
fert.zaf <- fert_long[c(103:136),]

gha <- Reduce(function(x, y) merge(x, y, by="year"),
              list(
                hdi.gha,
                mort.gha[,c(2,3)],
                elet.gha[,c(2,3)],
                fert.gha[,c(2,3)],
                gdp.gha[,c(2,3)],
                pop.gha[,c(2,3)],
                wat.gha[,c(2,3)],
                invst.gha[,c(2,3)]
              ))

sen <- Reduce(function(x, y) merge(x, y, by="year"),
              list(
                hdi.sen,
                mort.sen[,c(2,3)],
                elet.sen[,c(2,3)],
                fert.sen[,c(2,3)],
                gdp.sen[,c(2,3)],
                pop.sen[,c(2,3)],
                wat.sen[,c(2,3)],
                invst.sen[,c(2,3)]
              ))

nga <- Reduce(function(x, y) merge(x, y, by="year"),
              list(
                hdi.nga,
                mort.nga[,c(2,3)],
                elet.nga[,c(2,3)],
                fert.nga[,c(2,3)],
                gdp.nga[,c(2,3)],
                pop.nga[,c(2,3)],
                wat.nga[,c(2,3)],
                invst.nga[,c(2,3)]
              ))

zaf <- Reduce(function(x, y) merge(x, y, by="year"),
              list(
                hdi.zaf,
                mort.zaf[,c(2,3)],
                elet.zaf[,c(2,3)],
                fert.zaf[,c(2,3)],
                gdp.zaf[,c(2,3)],
                pop.zaf[,c(2,3)],
                wat.zaf[,c(2,3)],
                invst.zaf[,c(2,3)]
              ))

data <- rbind(gha, sen, nga, zaf)
#
#
#
#
#
#
install.packages("plm")
library(plm)

pdata <- pdata.frame(data, index = c("country", "year"))
model <- plm(HDI ~ Mortality + Electricity + Fertility + gdp + Population + Acqua + Investimenti_Stranieri, data = pdata, model = "within")
summary(model)

#CONSIGLIATO DA POSITRON
# R
library(dplyr)

vars <- c("Mortality","Electricity","Fertility","gdp","Population","Acqua","Investimenti_Stranieri","HDI")
df_p <- as.data.frame(pdata)

# 1) per-country distinct counts
within_counts <- df_p %>%
  group_by(country) %>%
  summarize(across(all_of(vars), ~n_distinct(.x)), .groups = "drop")

# 2) predictors that are constant within every country
const_within <- vars[ vapply(vars, function(v) all(within_counts[[v]] == 1), logical(1)) ]

# 3) linear dependence via QR on model matrix (predictors only)
mm <- model.matrix(~ Mortality + Electricity + Fertility + gdp + Population + Acqua + Investimenti_Stranieri, data = df_p)
qr_mm <- qr(mm)
dependent_columns <- setdiff(colnames(mm), colnames(mm)[seq_len(qr_mm$rank)])

# return diagnostics
list(within_counts = within_counts, constant_within = const_within, qr_rank = qr_mm$rank, dependent_columns = dependent_columns)


summary(model)
install.packages("modelsummary")
library(modelsummary)

modelsummary(model)
#
#
#
#
#
