setwd("/Users/kelly/Desktop/customer/cbc/")

# load the libraries (add the packages you require that are missing)
pacman::p_load(reshape2, ggplot2, psych, lavaan, mlogit, gmnl, MASS, tidyverse, dplyr)

# load the data
choiceData_all = read.csv("choiceData.csv")
indivData_all = read.csv("indivData.csv")

# This is the full dataset
dim(indivData_all)  # 697  29
dim(choiceData_all) # 33456    23

head(indivData_all)
head(choiceData_all)

# Use your student id number as the seed and run the code to
# get a subsample of 400 respondents
all = unique(indivData_all$id)

set.seed(620146) # INPUT YOUR STUDENT ID NUMBER INSTEAD!!!
subsample = sample(all, 400, replace = FALSE)

# subset the full data files
indivData = subset(indivData_all, id %in% subsample)
choiceData = subset(choiceData_all, id %in% subsample)

dim(indivData)  # 400  29
dim(choiceData) # 19200    23

head(indivData)
head(choiceData)

# Investigate data and explore data ============================================
#missing value inspect
any(is.na(indivData)) # Check if one or more values in the data frame are missing
any(is.na(choiceData))
#univariate analysis----------
summary(indivData)
owngroup<- indivData %>% group_by(Own) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
Intentgroup<- indivData %>% group_by(IntentToBuy) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
Brandgroup<- indivData %>% group_by(BrandAwareness) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
gendergroup <- indivData %>% group_by(GenderLabel) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
agegroup<- indivData %>% group_by(AgeLabel) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
occupationgroup<- indivData %>% group_by(OccupationLabel) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
educationgroup<- indivData %>% group_by(EducationLabel) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
incomegroup<- indivData %>% group_by(IncomeLabel) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
residencegroup<- indivData %>% group_by(Residence) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
residencegroup<-residencegroup[order(residencegroup$count,decreasing = TRUE),]

SubjKnow_r1group <-indivData %>% group_by(SubjKnow_r1) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
SubjKnow_r2group <-indivData %>% group_by(SubjKnow_r2) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
SubjKnow_r3group <-indivData %>% group_by(SubjKnow_r3) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
SubjKnow_r4group <-indivData %>% group_by(SubjKnow_r4) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
SubjKnow_r5group <-indivData %>% group_by(SubjKnow_r5) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
mean(c(indivData$SubjKnow_r1,indivData$SubjKnow_r2,indivData$SubjKnow_r3,indivData$SubjKnow_r4,indivData$SubjKnow_r5))

PII_1group <-indivData %>% group_by(PII_1) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
PII_2group <-indivData %>% group_by(PII_2) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
PII_3group <-indivData %>% group_by(PII_3) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
PII_4group <-indivData %>% group_by(PII_4) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
PII_5group <-indivData %>% group_by(PII_5) %>% summarise(count=n())%>%mutate(Percentage=paste0(round(count/sum(count)*100,2),"%"))
mean(c(indivData$PII_1,indivData$PII_2,indivData$PII_3,indivData$PII_4,indivData$PII_5))

#relative importance plot
head(indivData)
importanceacc = indivData[, c(1,15,16,17,18)]

ia_long = melt(importanceacc, id.vars = "id", variable.name = "attribute",
               value.name = "ia")

ia_long$attribute = sapply(strsplit(as.character(ia_long$attribute), "_"), "[",2)

ggplot(data = ia_long, aes(x = attribute, y = ia)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Relative importance Evaluation") +
  theme_bw()

ggsave("RI_eva.jpg", device = 'jpg', width=6, height = 5)

#bivariate analysis---------
#regroup income into 4 groups，和price importance
head(indivData)
indivincome <- indivData %>% 
  mutate(incomeregroup = case_when(
    Income <=2 ~"<1000",
    Income >2 & Income<= 4~"1001-2000",
    Income >4 & Income<=6~"2001-3000",
    Income==7~"more than 3000",
    Income == 8 ~"notsay"
  ))
aggregate(indivincome$RelImp_sound, list(indivincome$incomeregroup),FUN=mean)
aggregate(indivincome$RelImp_battery, list(indivincome$incomeregroup),FUN=mean)
aggregate(indivincome$RelImp_price, list(indivincome$incomeregroup),FUN=mean)
aggregate(indivincome$RelImp_weight, list(indivincome$incomeregroup),FUN=mean)

ggplot(data = indivincome, aes(x = incomeregroup, y = RelImp_price)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", 
               col = "darkred", fill = "darkred",
               size = 2) +
  labs(x = "", y = "Relative importance of price", title = "Relative importance of price for different incomegroups") +
  theme_classic()

ggsave("price_income.jpg", device = 'jpg', width=6, height = 5)

#x = IncomeLabel, y = RelImp_price
#x = AgeLabel, y = RelImp_price
#x = OccupationLabel, y = RelImp_battery no difference
#x = OccupationLabel, y = RelImp_sound no difference
#x = EducationLabel, y = RelImp_sound

ggplot(data = indivData, aes(x = GenderLabel, y = SubjKnow_r1)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", 
               col = "darkred", fill = "darkred",
               size = 2) +
  labs(x = "", y = "Know about portable Bluetooth speakers") +
  theme_classic()

ggplot(data = indivData, aes(x = AgeLabel, y = PII_5)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", 
               col = "darkred", fill = "darkred",
               size = 2) +
  labs(x = "", y = "Portable Bluetooth speakers importance") +
  theme_classic()

# choice model ===========================================================
head(choiceData)
dim(choiceData)

I = length(unique(choiceData$id)) #400
T = length(unique(choiceData$cs))#12
J = length(unique(choiceData$alt))#4

I * T * J#19200

# Create a unique chid counter and index the data object
chid <- unique(choiceData[, c("id", "cs")])
dim(chid)
chid$chid <- 1:nrow(chid)
head(chid, 20)

choiceData <- merge(chid, choiceData, by = c("id", "cs"))
dim(choiceData)
head(choiceData)
str(choiceData)
# sort
choiceData = choiceData[order(choiceData$id, choiceData$chid, choiceData$alt), ]

# Indexing using dfidx() this will be the input into mlogit function 
# Note that the input into dfidx() SHOULD BE a data.frame object
pacman::p_load(reshape2, ggplot2, fastDummies, mlogit, gmnl, MASS)
choiceData_ml <- dfidx(choiceData, 
                       shape = "long", 
                       choice = "choice", 
                       idx = list(c("chid", "id")), 
                       idnames = c(NA, "alt"))
head(choiceData_ml)


# LL of Null model
LL0 = T * I * log(1/J) 
LL0 # -6654.213

# Model: Main effect model with linear price effect and effect-coding for other attributes
# MNL -------------------------------------------------------------------------
mnl <- mlogit(choice ~ none + priceL + battery_8h + battery_10h +
                battery_10h + battery_12h + battery_14h + weight_400g+
                weight_500g + weight_600g + sound_3.5s + sound_4s + sound_4.5s| 0, 
              data = choiceData_ml)
summary(mnl)

# same can be achieved using gmnl package
choiceData_mlogit = mlogit.data(choiceData, 
                                choice = "choice", 
                                shape = "long",
                                id.var = "id", 
                                alt.var = "alt",
                                chid = "chid")

mnl_gmnl <- gmnl(choice ~ none + priceL + battery_8h +
                   battery_10h + battery_12h + battery_14h + weight_400g+
                   weight_500g + weight_600g + sound_3.5s + sound_4s + sound_4.5s| 0, 
                 data = choiceData_mlogit,
                 model = "mnl")
summary(mnl_gmnl)

# MXL -------------------------------------------------------------------------
# Number of draws R
set.seed(125)
hist(rnorm(100,0, 1))
hist(rnorm(500,0, 1))
hist(rnorm(1000,0, 1))

# Trade-off: Higher R - more precision, higher estimation time
mxl.rpar <- rep("n", length = length(mnl$coef))
names(mxl.rpar) <- names(mnl$coef)
mxl.rpar

# Code for mxl with mlogit
mxl <-  mlogit(choice ~ none + priceL + battery_8h + battery_10h +
                 battery_12h + battery_14h + weight_400g+
                 weight_500g + weight_600g + sound_3.5s + sound_4s + sound_4.5s| 0, 
               data = choiceData_ml,
               R = 100, panel = TRUE, correlation = FALSE,# correlation:false-diagonal, true-full
               seed = 125,       # set seed for replicability
               rpar = mxl.rpar)
summary(mxl)

#We will switch to gmnl package for convenience, as there is already 
# a function to compute individual-level estimates
r = c(100, 500, 1000) # vary the number of draws
mxl_gmnl = NULL # initialize

for(i in 1:length(r)){
  R = r[i]
  mxl.temp <-  gmnl(choice ~ none + priceL + battery_8h + battery_10h +
                      battery_10h + battery_12h + battery_14h + weight_400g+
                      weight_500g + weight_600g + sound_3.5s + sound_4s + sound_4.5s| 0, 
                    data = choiceData_mlogit,
                    model = "mixl", correlation = FALSE, haltons = NULL,
                    R = R, panel = TRUE, 
                    seed = 125,       # set seed for replicability
                    ranp = mxl.rpar)#specify distribution and which attributes
  
  save(mxl.temp, file = paste0("mxl_diag", R, ".RData"))
  
  mxl_gmnl[[i]] = mxl.temp
}

# load the results
mxl_diag100 = get(load("mxl_diag100.RData"))
mxl_diag500 = get(load("mxl_diag500.RData"))
mxl_diag1000 = get(load("mxl_diag1000.RData"))

summary(mxl_diag100)
summary(mxl_diag500)
summary(mxl_diag1000)

#make a df to compare coeffients and likelihood
res = cbind(mxl_diag100$coefficients, 
            mxl_diag500$coefficients,
            mxl_diag1000$coefficients)
res = rbind(res, c(as.numeric(mxl_diag100$logLik$maximum),
                   as.numeric(mxl_diag500$logLik$maximum),
                   as.numeric(mxl_diag1000$logLik$maximum)))

rownames(res) = c(rownames(res)[-nrow(res)], "LL")

round(res, 2)

# Population-level estimates 
mxl_diag500$coefficients[1:12]
mxl_diag500$coefficients[13:24]
beta = mxl_diag500$coefficients[1:12]
sigma = mxl_diag500$coefficients[13:24]

names(sigma) = names(beta)

beta
sigma

# Draw from the population-level estimates
set.seed(158)
pop = mvrnorm(n = 500, mu = beta, Sigma = diag(sigma^2))
pop = data.frame(pop)
pop$id = 1:nrow(pop)
head(pop)
dim(pop)

# reshape to long format
pop_long = melt(pop, id.vars = "id", variable.name = "param")
head(pop_long)

# Compute individual-level estimates
betai = data.frame(effect.gmnl(mxl_diag500)$mean)
betai$id = unique(choiceData_mlogit$id)
head(betai)
dim(betai)

# Reshape to long format
betai_long = melt(betai, id.vars = "id",
                  variable.name = "param")
head(betai_long)

# Plot the distribution of individual- and population-level estimates
ggplot(data = betai_long, aes(x = value)) +
  geom_histogram(bins = 40, aes(y=..density..)) +
  geom_histogram(data = pop_long, bins = 40, aes(y=..density..), alpha = 0.2) +
  facet_wrap(.~ param, scales = "free") +
  labs(x = "") +
  theme_bw()
dev.off()

# Relative Importance of Attributes ---------------------------
# First based on Individual-level Estimates
# Recover left-out part-worth utilities
dim(betai)
#set the the threshold of priceL
betai1 = subset(betai, priceL < -0.015) #361  13
dim(betai1)
#find the id of priceL < threhold
exclude = subset(betai, priceL > -0.015)
excludeid =exclude$id 

#delete the individual observation where priceL < threhold
dim(indivData)
"%nin%" <- function(x, table) match(x, table, nomatch = 0L) == 0L
indivdatamerge  = subset(indivData, id %nin% excludeid)
dim(indivdatamerge)

betai1['battery_16h'] = -(betai1['battery_8h'] + betai1['battery_10h']+betai1['battery_12h']+betai1['battery_14h'])
betai1["weight_700g"] = -(betai1["weight_400g"] + betai1["weight_500g"] + betai1["weight_600g"])
betai1["sound_5s"] = -(betai1["sound_3.5s"] + betai1["sound_4s"]+ betai1["sound_4.5s"])
dim(betai1)

betai_long = melt(betai1, id.vars = "id", variable.name = "param")

betai_long$param = as.character(betai_long$param)
head(betai_long)
unique(betai_long$param)

# Compute Relative importance (RI) of attributes
# Delete the None
betai_long = subset(betai_long, param != "none")

# add attribute column
betai_long$attr = sapply(strsplit(betai_long$param, "_"), `[`, 1)

# Compute range of attributes 
Range = tapply(betai_long$value, list(betai_long$id, betai_long$attr), 
               FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = betai_long[betai_long$param == "priceL", ]$value
price = c(70, 90, 110, 130, 150) # price levels

# compute Range for price
Range[, "priceL"] = abs(coef_price)*(max(price) - min(price))

# save as a data frame
Range = data.frame(Range)
rownames(Range) = NULL

# Compute RI
RI = Range / rowSums(Range) * 100
RI$id = unique(indivdatamerge$id)
head(RI)

# reshape to long format
RI_long = melt(RI, id.vars = "id", variable.name = "attribute",
               value.name = "RI")

summary(RI)
# plot
ggplot(data = RI_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Individual-level Estimates") +
  theme_bw()


# Based on population-level estimates 
# omit those draws that result in a positive price coefficient
dim(pop)
pop = subset(pop, priceL < -0.015)
dim(pop)

# Recover left-out part-worth utilities
pop['battery_16h'] = -(pop['battery_8h'] + pop['battery_10h']+pop['battery_12h']+pop['battery_14h'])
pop["weight_700g"] = -(pop["weight_400g"] + pop["weight_500g"] + pop["weight_600g"])
pop["sound_5s"] = -(pop["sound_3.5s"] + pop["sound_4s"]+ pop["sound_4.5s"])

# Compute Relative importance (RI) of attributes
# reshape to long format
pop_long = melt(pop, id.vars = "id", variable.name = "param")
head(pop_long)

# Delete the None
pop_long = subset(pop_long, param != "none")

# get rid off the E_ before the attribute-level labels
pop_long$param = as.character(pop_long$param)

# add attribute column
pop_long$attr = sapply(strsplit(pop_long$param, "_"), `[`, 1)
head(pop_long)

# Compute range of attributes 
Range = tapply(pop_long$value, list(pop_long$id, pop_long$attr), 
               FUN=function(x){diff(range(x))})

# for price we need to compute range as abs(beta)*(maxPrice - minPrice)
coef_price = pop_long[pop_long$param == "priceL", ]$value
price = c(70, 90, 110, 130, 150)# price levels

# compute Range for price
Range[, "priceL"] = abs(coef_price)*(max(price) - min(price))

# save as a data frame
Range = data.frame(Range)
rownames(Range) = NULL

# Compute RI
RI_pop = Range / rowSums(Range) * 100
RI_pop$id = 1:nrow(RI_pop)
head(RI_pop)

# reshape to long format
RI_pop_long = melt(RI_pop, id.vars = "id", variable.name = "attribute",
                   value.name = "RI")
head(RI_pop_long)

# plot
ggplot(data = RI_pop_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Population-level Estimates") +
  theme_bw()

# Let's also investigate the individual difference in RI patterns
RI$pattern = apply(as.matrix(RI[, -5]), 1, which.max)
RI$plot = ifelse(duplicated(RI$pattern), 0, 1) #id the pattern duplicated
head(RI)

indiv_ids_plot = subset(RI, plot == 1) # 4distinct pattern
indiv_ids_plot = melt(indiv_ids_plot, id.vars = "id", 
                      variable.name = "attribute",
                      value.name = "RI")
indiv_ids_plot = subset(indiv_ids_plot, !attribute %in% c("pattern", "plot"))

ggplot(data = RI_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  geom_point(data = indiv_ids_plot, alpha = 0.3) + 
  geom_line(data = indiv_ids_plot, aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Individual-level Estimates") +
  theme_bw()

ggsave("RI_indiv_patterns", device = "jpg", width = 6, height = 5)

RI_pop$pattern = apply(as.matrix(RI_pop[, -5]), 1, which.max)
RI_pop$plot = ifelse(duplicated(RI_pop$pattern), 0, 1)
head(RI_pop)

pop_ids_plot = subset(RI_pop, plot == 1)
pop_ids_plot = melt(pop_ids_plot, id.vars = "id", 
                    variable.name = "attribute",
                    value.name = "RI")
pop_ids_plot = subset(pop_ids_plot, !attribute %in% c("pattern", "plot"))
summary(RI_pop)

ggplot(data = RI_pop_long, aes(x = attribute, y = RI)) +
  geom_boxplot() +
  geom_point(data = pop_ids_plot, alpha = 0.3) + 
  geom_line(data = pop_ids_plot, aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 3, 
               color = "darkred", fill = "darkred") +
  labs(x = "", y = "Relative importance", title = "Based on Population-level Estimates") +
  theme_bw()

ggsave("RI_pop_patterns", device = "jpg", width = 6, height = 5)

# Willigness-to-Pay (based on individual estimates) ----------------------------
pricei = subset(betai_long, attr == "priceL")[, c("id", "value")]
names(pricei) = c("id", "UM")
head(pricei)

betai_long = merge(betai_long, pricei, by = "id")
head(betai_long)

betai_long$UM = betai_long$value / (-betai_long$UM)
head(betai_long)

UM = subset(betai_long, param != "priceL")[, c("id", "attr", "param", "UM")] 
head(UM)

base = subset(UM, param %in% c("battery_16h", "weight_700g", "sound_5s")) 
base = base[, c("id", "attr", "UM")]
names(base) = c("id", "attr", "base")
head(base)

UM = merge(UM, base, by = c("id", "attr"))
UM$WTP = UM$UM - UM$base
head(UM)

wtpweight = subset(UM, WTP != 0)
wtpweight = wtpweight %>% filter(attr == 'weight')

# Plot WTP
ggplot(data = subset(wtpweight, WTP != 0), aes(x = WTP)) +
  geom_histogram(bins = 40, alpha = 0.8) +
  geom_vline(xintercept = 0) +
  facet_wrap(.~param, scales = "free_x", nrow = 1) +
  labs(x = "Willingness-to-pay", y = "Frequency") +
  theme_bw()

ggsave("WTP", device = "jpg", width = 5, height = 2)

a <-subset(UM, WTP != 0)
head(a)
a <-a[, c(3,6)]

a %>%
  group_by(param) %>%
  dplyr::summarize(Mean = mean(WTP, na.rm=TRUE))

a %>%
  group_by(param) %>%
  dplyr::summarize(min = min(WTP, na.rm=TRUE))

a %>%
  group_by(param) %>%
  dplyr::summarize(max = max(WTP, na.rm=TRUE))


# cluster analysis -------------------------------------------------------------
head(RI)
datari=RI %>% select(1:5)
dim(indivdatamerge)
dim(datari)
head(datari)
head(indivdatamerge)
cldata = merge(datari,indivdatamerge,by="id")
head(cldata)

#encode variables(income, gender, occupation, residence)
library('fastDummies')
cldata <- dummy_cols(cldata, select_columns = 'OccupationLabel')
cldata <- dummy_cols(cldata, select_columns = 'GenderLabel')
#regroup income groups into 4 groups
cldata <- cldata %>% 
  mutate(incomeregroup = case_when(
    Income <=2 ~"<1000",
    Income >2 & Income<= 5~"1001-2500",
    Income >5 & Income<=7~">2501",
    Income == 8 ~"notsay"
  ))
cldata <- dummy_cols(cldata, select_columns = 'incomeregroup')
#regroup residence into 2 groups(Germany or other)
cldata <- cldata %>% 
  mutate(residence = case_when(
    Residence =='Germany' ~"Germany",
    Residence !='Germany' ~"other",
  ))
cldata <- dummy_cols(cldata, select_columns = 'residence')

cldata= cldata[, !names(cldata) %in% c('Gender','GenderLabel', 'AgeLabel','Occupation', 'OccupationLabel','EducationLabel','Income', 'IncomeLabel', 'Residence','residence', 'incomeregroup','RelImp_weight','RelImp_price','RelImp_sound','RelImp_battery')]
head(cldata)
dim(cldata) #33

# Decide on Basis and Descriptor Variables
basis = names(cldata)[2:5]

# rest as descriptors
descriptors = setdiff(names(cldata), c(basis, "id"))

# Clustering 
# Standardize the data
cldata.sd <- cldata[, basis]
cldata.sd <- scale(cldata.sd)

# Compute proximity measure
# Compute Euclidean distance
dist.eucl <- dist(cldata.sd)
as.matrix(dist.eucl)[1:5, 1:5]

# Try out various clustering methods
cl.single <- hclust(dist.eucl, method = "single")     # single linkage method
cl.complete <- hclust(dist.eucl, method = "complete") # complete linkage method
cl.average <- hclust(dist.eucl, method = "average")   # average linkage method
cl.centroid <- hclust(dist.eucl, method = "centroid") # centroid linkage method
cl.median <- hclust(dist.eucl, method = "median")     # median linkage method
cl.ward <- hclust(dist.eucl, method = "ward.D2")      # ward's methods

# Investigate solutions
k <- 3  # initial solution to check

# single linkage
table(cutree(cl.single, k))
#359   1   1

# complete linkage
plot(as.dendrogram(cl.complete), leaflab = "none")
plot(as.dendrogram(cl.complete), leaflab = "none", ylim = c(0, 4.5))
table(cutree(cl.complete, k))
#293  29  39 

# average linkage
plot(as.dendrogram(cl.average), leaflab = "none")
table(cutree(cl.average, k))
#346  13   2 

# centroid linkage
plot(as.dendrogram(cl.centroid), leaflab = "none")
table(cutree(cl.centroid, k))
#353   6   2 

#median linkage
table(cutree(cl.median, k))
#119 240   2

# Ward method
plot(as.dendrogram(cl.ward), leaflab = "none")
plot(as.dendrogram(cl.ward), leaflab = "none", ylim = c(0, 30))
rect.hclust(cl.ward, k = 3, border = "darkblue") 
table(cutree(cl.ward, 3))
#179 107  75 

#variance ratio index
# Variance ratio criterion or Calinski-Harabasz (CH) index 
library(fpc)
VRC.ward = rep(0, length(2:8)) # initialize
for(k in 2:8){
  VRC.ward[k] <- cluster.stats(d = dist.eucl, 
                               clustering = cutree(cl.ward, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward#128.7387 140.7751 141.0496 133.2639 125.7639 124.6469 119.3970

# save as a data frame
VRC = data.frame(K = 2:8, ward = VRC.ward)

# reshape to long
VRC = melt(VRC, id.vars = "K")

# plot
ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

ggsave("VRC", device = "jpg", width = 4, height = 3)

# 3 clusters seem reasonable, we proceed with 3
k <- 3

# k-means clustering
set.seed(185) 
cl.kmeans <- kmeans(cldata.sd, centers = k)

library(cluster)
# Investigate how well clusters are separate with ward and k-means
clusplot(cldata.sd, cutree(cl.ward, k), color = TRUE , shade = TRUE ,
         labels = 2, lines = 0, main = "Ward's cluster plot")

clusplot(cldata.sd, cl.kmeans$cluster, color = TRUE , shade = TRUE ,
         labels = 2, lines = 0, main = "K-means cluster plot")


# Describe the clusters
# combine with the original data
cldata$cluster_kmeans <- cl.kmeans$cluster
cldata$cluster_ward <- cutree(cl.ward, k)
head(cldata)
ncol(cldata) # number of columns

# ward solution --go with ward
t(aggregate(cldata[, -c(1, 34, 35)], 
            by = list(cluster = cldata$cluster_ward), 
            function(x)c(mean = round(mean(x), 2))))

# cluster sizes
round(prop.table(table(cldata$cluster_ward)), 3)

# Market Simulation ------------------------------------------------------------
head(betai)
dim(betai)
predict.mxl = function(X, beta) {
  
  counter = X[, c("Task", "Alt")]
  
  # as id counter initial may not be sequential, 
  # we create a new one that is sequential
  ids = data.frame(id_new = 1:length(beta$id), 
                   id = beta$id)
  
  param = names(beta)[names(beta) != "id"]
  beta = as.matrix(beta[, param])
  
  I = nrow(beta)
  
  data = NULL
  for (i in 1:I) {
    xi = as.matrix(X[, param])
    data[[i]] = X[, param]
    
    # add counters
    data[[i]]$id_new = i 
    data[[i]] = cbind(counter, data[[i]])
    
    # Compute V_ijt (utility)
    data[[i]]$u = c(xi %*% beta[i,]) 
  }
  data = do.call(rbind, data) 
  
  # merge with ids
  data = merge(ids, data, by = "id_new", all = TRUE)
  
  # compute exp(V_ijt)
  data$exp_u = exp(data$u)
  
  # compute sum of exp(V_ijt) across J alternatives for each id and Task
  data$sum_exp_u = rep(c(tapply(data$exp_u, list(data$Task, data$id), sum)), each = J) 
  
  # compute choice probs
  data$p_hat = data$exp_u/data$sum_exp_u
  
  # max util rule: predict chosen alternative
  data$choice_hat = rep(c(tapply(data$p_hat, list(data$Task, data$id), which.max)), each = J)
  data$choice_hat = ifelse(data$choice_hat == data$Alt, 1, 0)
  
  return(data)
}

# market scenario1--------------
# alt 1: sound 5.0, weight 600g, 12hours battery
# alt 2: sound 4.0, weight 400g, 16hours battery
# alt 3: none

x = data.frame(none = c(0, 0, 1),
               priceL = c(NA, NA, 0),
               battery_8h = c(0, -1, 0),
               battery_10h = c(0, -1, 0),
               battery_12h = c(1, -1, 0),
               battery_14h = c(0, -1, 0),
               weight_400g = c(0, 1, 0),
               weight_500g = c(0, 0, 0),
               weight_600g = c(1, 0, 0),
               sound_3.5s = c(-1, 0, 0),
               sound_4s = c(-1, 1, 0),
               sound_4.5s = c(-1, 0, 0))
x

# set the marginal costs
mc = c(75, 70, 0) # 40 for firm 1, 50 for firm 2, 0 for none

# set the market size
N = 400

# set initial prices
init_price = c(75, NA, 0) 

# which one is the focal firm?
focal_firm = which(is.na(init_price) == TRUE)

# Let's generate sets with varying prices
# the price for the focal firm changes (we generate sequence of prices)
# and the price for the other firm is fixed
J = nrow(x)
price = seq(75, 150, by = 1)
T = length(price)

X = NULL
for(t in 1:T){
  temp = x
  temp[, "priceL"] = init_price # set prices based on the init_price vector
  temp[focal_firm, "priceL"] = price[t] # update price value for focal firm
  temp$Alt = 1:J # create Alt variable
  temp$Task = t # create Task variable
  X[[t]] = temp
}
X = do.call(rbind, X)

head(X)
dim(X) # J*T

# predict choices and choice probs based on individual-level estimates
# vector of parameters
pred_indiv = predict.mxl(X, betai)
head(pred_indiv)
dim(pred_indiv)

# Note each task indicates a specific price for the focal firm with price for the other fixed
# we can compute market shares of each alternative (aggregate across respondents) for each 
# Task, i.e., price of the focal firm
ms = aggregate(pred_indiv$choice_hat, 
               by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
               mean)
names(ms) = c("Alt", "Task", "ms")

# let's add the price of the focal firm
price_df = data.frame(Task = 1:T,
                      price_seq = price)

ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)

# define the prices of all alternatives: 1, 2, and 3 (the none)
ms$price = init_price
ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)

# add the marginal costs
ms$mc = mc

# compute profit
ms$profit = N * ms$ms * (ms$price - ms$mc)

# At what price is the profit of the focal firm maximized?
maximum = subset(ms, Alt == focal_firm)
maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)

subset(maximum, price_star == 1)
maximum = subset(maximum, price_star == 1)$price_seq
maximum

optimal = subset(ms, price_seq == maximum)
optimal


# to make things more optimal we can write the upper part as a function, so we can change 
# the focal firm, initial prices, marginal costs, and market size
findOptimum = function(x, mc, N = 1, init_price, price_seq, beta){
  # set the the focal firm
  focal_firm = which(is.na(init_price) == TRUE)
  J = nrow(x)
  T = length(price_seq)
  
  X = NULL
  for(t in 1:T){
    temp = x
    temp[, "priceL"] = init_price # set prices based on the init_price vector
    temp[focal_firm, "priceL"] = price_seq[t] # update price value for focal firm
    temp$Alt = 1:J # create Alt variable
    temp$Task = t # create Task variable
    X[[t]] = temp
  }
  X = do.call(rbind, X)
  
  # predict choices and choice probs based on individual-level estimates
  # vector of parameters
  pred_indiv = predict.mxl(X, beta)
  
  # Note each task indicates a specific price for the focal firm with price for the other fixed
  # we can compute market shares of each alternative (aggregate across respondents) for each 
  # Task, i.e., price of the focal firm
  ms = aggregate(pred_indiv$choice_hat, 
                 by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
                 mean)
  names(ms) = c("Alt", "Task", "ms")
  
  # let's add the price of the focal firm
  price_df = data.frame(Task = 1:T,
                        price_seq = price_seq)
  
  ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)
  
  # define the prices of all alternatives: 1, 2, and 3 (the none)
  ms$price = init_price
  ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)
  ms$mc = mc # add the marginal costs
  ms$profit = N * ms$ms * (ms$price - ms$mc) # compute profit
  
  # At what price is the profit of the focal firm maximized?
  maximum = subset(ms, Alt == focal_firm)
  maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)
  maximum = subset(maximum, price_star == 1)$price_seq
  
  optimal = subset(ms, price_seq == maximum)
  rownames(optimal) = NULL
  return(optimal)
}


N = 400                  # define market size
mc = c(75, 70, 0)       # define the marginal costs
price_vec = c(75, NA, 0) # initial prices

# initial iteration (r = 1) ---
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter1 = findOptimum(x, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter1

#iteration (r = 2) ---
price_vec = c(NA, iter1[iter1$Alt == focal, "price"], 0)
price_vec
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter2 = findOptimum(x, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter2

# We can do this using a for loop 
# Note: this is for 2 + none alternatives, you need to adjust the code to 
# work for more alternatives
res = NULL        # initialize
diff = NA          # initialize
max_iter = 100     # you can change this
min_diff = 5       # what should be the minimum difference between 
price_max = 150    # the maximum value the price can be (when generating sequences of prices)
price_interval = 1 # the interval by which price sequence should be built 

N = 400                  # define market size
mc = c(75, 70, 0)       # define the marginal costs
price_vec = c(75, NA, 0) # initial prices

# the loop will stop when max_iter is reached or earlier 
# when the difference between prices in r and r-1 is 0
for(r in 1:max_iter){
  if(r == 1){
    price_temp = price_vec
  }else{
    # update price_vec
    if(r %in% seq(3, max_iter, by = 2)){ # for these iterations alt 2 is the focal option
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"], NA, 0)
      
    }else if(r %in% seq(2, max_iter, by = 2)){ # for these iterations alt 1 is the focal option
      price_temp = c(NA, res[[r-1]][res[[r-1]]$Alt == focal, "price"], 0)
      
    }
  }
  
  # update focal and min in the price sequence
  focal = which(is.na(price_temp) == TRUE) 
  price_seq = seq(mc[focal], price_max, by = price_interval) 
  
  res[[r]] = findOptimum(x, mc, N, 
                         init_price = price_temp, 
                         price_seq = price_seq,
                         beta = betai)
  
  if(r == 1){
    diff[r] = NA
  }else{
    diff[r] = abs(res[[r]][res[[r]]$Alt == focal, "price"] - 
                    res[[r-1]][res[[r-1]]$Alt == focal, "price"])
  }
  
  if(r > 1 & diff[r] == min_diff){ break}
}

# how many iterations did it take?
r

# what are the prices we reach convergence, i.e., Nash equilibrium?
res

# market senario2----------------
# market scenario2: 
# alt 1: sound 5.0, weight 600g, 12hours battery
# alt 2: sound 4.0, weight 400g, 16hours battery
# alt 3: sound 5.0, weight 600g, 16hours battery
# alt 4: none

x = data.frame(none = c(0, 0, 0, 1),
               priceL = c(NA, NA, NA, 0),
               battery_8h = c(0, -1, -1, 0),
               battery_10h = c(0, -1, -1, 0),
               battery_12h = c(1, -1, -1, 0),
               battery_14h = c(0, -1, -1, 0),
               weight_400g = c(0, 1, 0, 0),
               weight_500g = c(0, 0, 0, 0),
               weight_600g = c(1, 0, 1, 0),
               sound_3.5s = c(-1, 0, -1, 0),
               sound_4s = c(-1, 1,-1, 0),
               sound_4.5s = c(-1, 0,-1, 0))
x

# set the marginal costs
mc = c(75, 70, 75, 0) 

# set the market size
N = 400

# set initial prices
init_price = c(119, 103, NA, 0) 

# which one is the focal firm?
focal_firm = which(is.na(init_price) == TRUE)

J = nrow(x)
price = seq(75, 150, by = 1)
T = length(price)

X = NULL
for(t in 1:T){
  temp = x
  temp[, "priceL"] = init_price # set prices based on the init_price vector
  temp[focal_firm, "priceL"] = price[t] # update price value for focal firm
  temp$Alt = 1:J # create Alt variable
  temp$Task = t # create Task variable
  X[[t]] = temp
}
X = do.call(rbind, X)

head(X)
dim(X) # J*T

# predict choices and choice probs based on individual-level estimates
# vector of parameters
pred_indiv = predict.mxl(X, betai)
head(pred_indiv)
dim(pred_indiv)

ms = aggregate(pred_indiv$choice_hat, 
               by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
               mean)
names(ms) = c("Alt", "Task", "ms")

# let's add the price of the focal firm
price_df = data.frame(Task = 1:T,
                      price_seq = price)

ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)

ms$price = init_price
ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)

# add the marginal costs
ms$mc = mc

# compute profit
ms$profit = N * ms$ms * (ms$price - ms$mc)

# At what price is the profit of the focal firm maximized?
maximum = subset(ms, Alt == focal_firm)
maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)

subset(maximum, price_star == 1)
maximum = subset(maximum, price_star == 1)$price_seq
maximum

optimal = subset(ms, price_seq == maximum)
optimal


# to make things more optimal we can write the upper part as a function, so we can change 
# the focal firm, initial prices, marginal costs, and market size
findOptimum = function(x, mc, N = 1, init_price, price_seq, beta){
  # set the the focal firm
  focal_firm = which(is.na(init_price) == TRUE)
  J = nrow(x)
  T = length(price_seq)
  
  X = NULL
  for(t in 1:T){
    temp = x
    temp[, "priceL"] = init_price # set prices based on the init_price vector
    temp[focal_firm, "priceL"] = price_seq[t] # update price value for focal firm
    temp$Alt = 1:J # create Alt variable
    temp$Task = t # create Task variable
    X[[t]] = temp
  }
  X = do.call(rbind, X)
  
  # predict choices and choice probs based on individual-level estimates
  # vector of parameters
  pred_indiv = predict.mxl(X, beta)
  
  # Note each task indicates a specific price for the focal firm with price for the other fixed
  # we can compute market shares of each alternative (aggregate across respondents) for each 
  # Task, i.e., price of the focal firm
  ms = aggregate(pred_indiv$choice_hat, 
                 by = list(Alt = pred_indiv$Alt, Task = pred_indiv$Task),
                 mean)
  names(ms) = c("Alt", "Task", "ms")
  
  # let's add the price of the focal firm
  price_df = data.frame(Task = 1:T,
                        price_seq = price_seq)
  
  ms = merge(ms, price_df, by = c("Task"), all.x = TRUE)
  
  # define the prices of all alternatives: 1, 2, and 3 (the none)
  ms$price = init_price
  ms$price = ifelse(ms$Alt == focal_firm, ms$price_seq, ms$price)
  ms$mc = mc # add the marginal costs
  ms$profit = N * ms$ms * (ms$price - ms$mc) # compute profit
  
  # At what price is the profit of the focal firm maximized?
  maximum = subset(ms, Alt == focal_firm)
  maximum$price_star = ifelse(maximum$profit == max(maximum$profit), 1, 0)
  maximum = subset(maximum, price_star == 1)$price_seq
  
  optimal = subset(ms, price_seq == maximum)
  rownames(optimal) = NULL
  return(optimal)
}


N = 400                  # define market size
mc = c(75,70,75,0)        # define the marginal costs
price_vec = c(119, 103, NA, 0) # initial prices

# initial iteration (r = 1) ---
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter1 = findOptimum(x, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter1

# iteration 2 ---
price_vec = c(NA, 103, iter1[iter1$Alt == focal, "price"], 0)
price_vec
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter2 = findOptimum(x, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter2


# iteration 3 ---
price_vec = c(iter2[iter2$Alt == focal, "price"], NA, iter2[iter2$Alt == 3, "price"], 0)
price_vec
focal = which(is.na(price_vec) == TRUE) # focal firm
price_seq = seq(mc[focal], 150, by = 1) # the min in the price sequence is the marginal cost

iter3 = findOptimum(x, mc, N, 
                    init_price = price_vec, 
                    price_seq = price_seq,
                    beta = betai)
iter3

# Firms will continue reacting to until both get to point where changing the price 
# doesn't make sense. That is we will find that point by iterating and getting no 
# change in price from one to the next iteration. What would this point be?
res = NULL     # initialize
diff = NA      # initialize
max_iter = 200 # you can change this

N = 400                  # define market size
mc = c(75,70, 75, 0)        # define the marginal costs
price_vec = c(119, 103, NA, 0) # initial prices

# the loop will stop when max_iter is reached or earlier 
# when the difference between prices in r and r-1 is 0
for(r in 1:max_iter){
  if(r == 1){
    price_temp = price_vec
  }else{
    # update price_vec(rewrite here)
    if(r %in% seq(1, 200, by = 3)){ 
      price_temp = c(res[[r-1]][res[[r-1]]$Alt == 1, "price"], res[[r-1]][res[[r-1]]$Alt == focal, "price"], NA, 0 )
      
    }else{
      if(r %in% seq(2, 200, by = 3)){ 
        price_temp = c(NA, res[[r-1]][res[[r-1]]$Alt == 2, "price"], res[[r-1]][res[[r-1]]$Alt == focal, "price"], 0)
        
      }else if(r %in% seq(3, 200, by = 3)){ 
        price_temp = c(res[[r-1]][res[[r-1]]$Alt == focal, "price"], NA, res[[r-1]][res[[r-1]]$Alt == 3, "price"],  0)
        
      }
    }
  }   
  # update focal and min in the price sequence
  focal = which(is.na(price_temp) == TRUE) 
  price_seq = seq(mc[focal], 150, by = 1) 
  
  res[[r]] = findOptimum(x, mc, N, 
                         init_price = price_temp, 
                         price_seq = price_seq,
                         beta = betai)
  
  if(r == 1){
    diff[r] = NA
  }else{
    diff[r] = res[[r]][res[[r]]$Alt == focal, "price"] - 
      res[[r-1]][res[[r-1]]$Alt == focal, "price"]
  }
  
  if(r > 1 & diff[r] == 2){ break} #threshold as 2
}

# how many iterations did it take?
r

# what are the prices we reach convergence, i.e., Nash equilibrium?
res

