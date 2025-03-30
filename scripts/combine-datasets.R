# combine EE data sets for nuclear energy questions 
## yrs 16-17 had question wording differences for nrsk2; exclude 
## environ not asked in 06-07
## yr 2017 had question wording difference for new_reac; exclude 

# packages 
## need car package for recodes
library(plyr)
library(dplyr)

## load EE data  
ee08 <- read.csv("data/eeData/EE08_Web+Phone_comb_wtd.csv")
ee09 <- read.csv("data/eeData/EE09_Public_Web_wtd.csv")
ee10 <- read.csv("data/eeData/EE10_Web+Phone_comb_wtd.csv")
ee11 <- read.csv("data/eeData/EE11_Web+Phone_comb_wtd.csv")
ee12 <- read.csv("data/eeData/EE12_Web_wtd.csv")
ee13 <- read.csv("data/eeData/EE13_Web_wtd.csv")
ee14 <- read.csv("data/eeData/EE14_data_wtd.csv")
ee15 <- read.csv("data/eeData/EE15_data_wtd.csv")
# ee16 <- read.csv("data/eeData/EE16_data_wtd.csv")
# ee17 <- read.csv("data/eeData/EE17_data_wtd.csv")
ee18 <- read.csv("data/eeData/EE18_data_wtd.csv")
ee19 <- read.csv("data/eeData/EE19_data_wtd.csv")
ee20 <- read.csv("data/eeData/EE20_data_wtd.csv")
ee21 <- read.csv("data/eeData/EE21_data_wtd.csv")
ee22 <- read.csv("data/eeData/EE22_data_wtd.csv")
ee23 <- read.csv("data/eeData/EE23_data_wtd.csv")

## variables
### demographics 
age <- c(ee08$e2_age,
         ee09$E1_age,
         ee10$e1_age,
         ee11$E1_age,
         ee12$e1_age,
         ee13$E1_age,
         ee14$e1_age,
         ee15$e1_age,
#         ee16$e1_age,
#         ee17$age,
         ee18$age,
         ee19$age,
         ee20$age,
         ee21$age,
         ee22$age,
         ee23$age)

gender <- c(ee08$e3_gend,
            ee09$E3_gend,
            ee10$e3_gend,
            ee11$E3_gend,
            ee12$e3_gend,
            ee13$E3_gend,
            ee14$e3_gend,
            ee15$e3_gend,
#            ee16$e3_gend,
#            ee17$gend,
            ee18$gend,
            ee19$gend,
            ee20$gend,
            ee21$gend,
            ee22$gend,
            ee23$gend) # 1 = male

race <- c(ee08$e125b_race_rec_wt,
          ee09$E129b_race_rec_wt,
          ee10$e115a_race_rec_wt,
          ee11$E111a_race_rec_wt,
          ee12$e106a_race_rec,
          ee13$E92b_race_rec,
          ee14$e5b_race_rec,
          ee15$race_recode,
#          ee16$e5_race,
#          ee17$race,
          ee18$race,
          ee19$race,
          ee20$race,
          ee21$race,
          ee22$race,
          ee23$race) # 1 = white 
white <- ifelse(race==1,1,0)

#### recode edu to college dummy - 06-15 >= 4; 16-23 >= 6 college graduate
ee08$e1b_edu_rec <- ifelse(ee08$e1b_edu_rec>=4,1,0)
ee09$E2a_edu_rec <- ifelse(ee09$E2a_edu_rec>=4,1,0)
ee10$e2b_edu_rec <- ifelse(ee10$e2b_edu_rec>=4,1,0)
ee11$E2a_edu_rec <- ifelse(ee11$E2a_edu_rec>=4,1,0)
ee12$e2b_edu_rec7 <- ifelse(ee12$e2b_edu_rec7>=4,1,0)
ee13$E2a_edu_rec <- ifelse(ee13$E2a_edu_rec>=4,1,0)
ee14$e2b_edu_rec <- ifelse(ee14$e2b_edu_rec>=4,1,0)
ee15$e2b_edu_rec <- ifelse(ee15$e2b_edu_rec>=4,1,0)
#ee16$e2_edu <- ifelse(ee16$e2_edu>=6,1,0)
#ee17$edu <- ifelse(ee17$edu>=6,1,0)
ee18$edu <- ifelse(ee18$edu>=6,1,0)
ee19$edu <- ifelse(ee19$edu>=6,1,0)
ee20$edu <- ifelse(ee20$edu>=6,1,0)
ee21$edu <- ifelse(ee21$edu>=6,1,0)
ee22$edu <- ifelse(ee22$edu>=6,1,0)
ee23$edu <- ifelse(ee23$edu>=6,1,0)

college <- c(ee08$e1b_edu_rec, 
             ee09$E2a_edu_rec, 
             ee10$e2b_edu_rec,
             ee11$E2a_edu_rec, 
             ee12$e2b_edu_rec7, 
             ee13$E2a_edu_rec, 
             ee14$e2b_edu_rec, 
             ee15$e2b_edu_rec, 
#             ee16$e2_edu, 
#             ee17$edu, 
             ee18$edu,
             ee19$edu, 
             ee20$edu, 
             ee21$edu, 
             ee22$edu, 
             ee23$edu) 
college <- car::recode(college, "2:8=1")

### worry
energyWorry <- c(ee08$e6_worry3,
                 ee09$E6_worry3,
                 ee10$e6_worry3,
                 ee11$E6_worry3,
                 ee12$e10_worry3,
                 ee13$E10_worry3,
                 ee14$e11_worry3,
                 ee15$e11_worry3,
#                 ee16$e10_worry2,
#                 ee17$worry_enrgy_a,
                 ee18$worry_enrgy,
                 ee19$worry_enrgy,
                 ee20$worry_enrgy,
                 ee21$worry_enrgy,
                 ee22$worry_enrgy,
                 ee23$worry_enrgy)

### environmental orientation 
nature <- c(ee08$e11_nature,
            ee09$E12_nature,
            ee10$e11_nature,
            ee11$E11_nature,
            ee12$e15_nature,
            ee13$E15_nature,
            ee14$e14_nature,
            ee15$e12_nature,
#            ee16$e12_nature,
#            ee17$nature,
            ee18$nature,
            ee19$nature,
            ee20$nature,
            ee21$nature,
            ee22$nature,
            ee23$nature)

environ <- c(ee08$e84_environ,
             ee09$E84_environ,
             ee10$e96_environ,
             ee11$E89_environ,
             ee12$e82_environ,
             ee13$E82_environ,
             ee14$e90_environ,
             ee15$e83_environ,
#             ee16$e96_environ,
#             ee17$environ,
             ee18$environ,
             ee19$environ,
             ee20$environ,
             ee21$environ,
             ee22$environ,
             ee23$environ)
envt_oren <- round((nature+environ)/2,0)

### nuclear energy 
ee18$nrsk1 <- ee18$nrsk1_a
ee18$nrsk1[!is.na(ee18$nrsk1_b)] = ee18$nrsk1_b[!is.na(ee18$nrsk1_b)]

nrsk1 <- c(ee08$e67_nrsk1,
           ee09$E67_nrsk1,
           ee10$e58_nrsk1,
           ee11$E43_nrsk1,
           ee12$e31_nrsk1,
           ee13$E28_nrsk1,
           ee14$e21_nrsk1,
           ee15$e19_nrsk1,
#           ee16$e19_nrsk1,
#           ee17$nrsk1,
           ee18$nrsk1,
           ee19$nrsk1,
           ee20$nrsk1,
           ee21$nrsk1,
           ee22$nrsk1,
           ee23$nrsk1) 

ee18$nrsk2 <- ee18$nrsk2_a
ee18$nrsk2[!is.na(ee18$nrsk2_b)] = ee18$nrsk2_b[!is.na(ee18$nrsk2_b)]

nrsk2 <- c(ee08$e68_nrsk2,
           ee09$E68_nrsk2,
           ee10$e59_nrsk2,
           ee11$E44_nrsk2,
           ee12$e32_nrsk2,
           ee13$E29_nrsk2,
           ee14$e22_nrsk2,
           ee15$e20_nrsk2,
           ee18$nrsk2,
           ee19$nrsk2,
           ee20$nrsk2,
           ee21$nrsk2,
           ee22$nrsk2,
           ee23$nrsk2) 

ee18$nrsk3 <- ee18$nrsk3_a
ee18$nrsk3[!is.na(ee18$nrsk3_b)] = ee18$nrsk1_b[!is.na(ee18$nrsk3_b)]

nrsk3 <- c(ee08$e69_nrsk3,
           ee09$E69_nrsk3,
           ee10$e60_nrsk3,
           ee11$E45_nrsk3,
           ee12$e33_nrsk3,
           ee13$E30_nrsk3,
           ee14$e23_nrsk3,
           ee15$e21_nrsk3,
#           ee16$e21_nrsk3,
#           ee17$nrsk3,
           ee18$nrsk3,
           ee19$nrsk3,
           ee20$nrsk3,
           ee21$nrsk3,
           ee22$nrsk3,
           ee23$nrsk3)

ee18$nrsk4 <- ee18$nrsk4_a
ee18$nrsk4[!is.na(ee18$nrsk4_b)] = ee18$nrsk4_b[!is.na(ee18$nrsk4_b)]

nrsk4 <- c(ee08$e70_nrsk4,
           ee09$E70_nrsk4,
           ee10$e61_nrsk4,
           ee11$E46_nrsk4,
           ee12$e34_nrsk4,
           ee13$E31_nrsk4,
           ee14$e24_nrsk4,
           ee15$e22_nrsk4,
#           ee16$e22_nrsk4,
#           ee17$nrsk4,
           ee18$nrsk4,
           ee19$nrsk4,
           ee20$nrsk4,
           ee21$nrsk4,
           ee22$nrsk4,
           ee23$nrsk4)

nben1 <- c(ee08$e71_nben1,
           ee09$E71_nben1,
           ee10$e62_nben1,
           ee11$E47_nben1,
           ee12$e35_nben1,
           ee13$E32_nben1,
           ee14$e25_nben1,
           ee15$e23_nben1,
#           ee16$e23_nben1,
#           ee17$nben1,
           ee18$nben1,
           ee19$nben1,
           ee20$nben1,
           ee21$nben1,
           ee22$nben1,
           ee23$nben1)

nben2 <- c(ee08$e72_nben2,
           ee09$E72_nben2,
           ee10$e63_nben2,
           ee11$E48_nben2,
           ee12$e36_nben2,
           ee13$E33_nben2,
           ee14$e26_nben2,
           ee15$e24_nben2,
#           ee16$e24_nben2,
#           ee17$nben2,
           ee18$nben2,
           ee19$nben2,
           ee20$nben2,
           ee21$nben2,
           ee22$nben2,
           ee23$nben2)

nben3 <- c(ee08$e73_nben3,
           ee09$E73_nben3,
           ee10$e64_nben3,
           ee11$E49_nben3,
           ee12$e37_nben3,
           ee13$E34_nben3,
           ee14$e27_nben3,
           ee15$e25_nben3,
#           ee16$e25_nben3,
#           ee17$nben3,
           ee18$nben3,
           ee19$nben3,
           ee20$nben3,
           ee21$nben3,
           ee22$nben3,
           ee23$nben3)

nben4 <- c(ee08$e74_nben4,
           ee09$E74_nben4,
           ee10$e65_nben4,
           ee11$E50_nben4,
           ee12$e38_nben4,
           ee13$E35_nben4,
           ee14$e28_nben4,
           ee15$e26_nben4,
#           ee16$e26_nben4,
#           ee17$nben4,
           ee18$nben4,
           ee19$nben4,
           ee20$nben4,
           ee21$nben4,
           ee22$nben4,
           ee23$nben4)

rskben <- c(ee08$e75_rskben,
            ee09$E75_rskben,
            ee10$e66_rskben,
            ee11$E51_rskben,
            ee12$e39_rskben,
            ee13$E36_rskben,
            ee14$e29_rskben,
            ee15$e27_rskben,
#            ee16$e27_rskben,
#            ee17$rskben,
            ee18$rskben,
            ee19$rskben,
            ee20$rskben,
            ee21$rskben,
            ee22$rskben,
            ee23$rskben)

plantSite <- c(ee08$e76_new1,
               ee09$E76_new1,
               ee10$e67_new1,
               ee11$E52_new1,
               ee12$e40_new1,
               ee13$E37_new1,
               ee14$e30_new1,
               ee15$e28G_new1,
#               ee16$e28_new1,
#               ee17$new_reac,
               ee18$new_reac,
               ee19$new_reac,
               ee20$new_reac,
               ee21$new_reac,
               ee22$new_reac,
               ee23$new_reac)

plantNew <- c(ee08$e77_new2,
              ee09$E77_new2,
              ee10$e68_new2,
              ee11$E53_new2,
              ee12$e41_new2,
              ee14$e31_new2,
              ee13$E38_new2,
              ee15$e29G_new2,
#              ee16$e29_new2,
              ee18$new_npp,
              ee19$new_npp,
              ee20$new_npp,
              ee21$new_npp,
              ee22$new_npp,
              ee23$new_npp)

morePlants <- (plantSite+plantNew)/2

### partisanship
party <- c(ee08$e122_party,
           ee09$E126_party,
           ee10$e112_party,
           ee11$E108_party,
           ee12$e103_party,
           ee13$E88_party,
           ee14$e97_party,
           ee15$e90_party,
#          ee16$e103_party,
#          ee17$party,
           ee18$party,
           ee19$party,
           ee20$party,
           ee21$party,
           ee22$party,
           ee23$party) # 1 = Dem; 2 = Rep; 3 = Indep; 4 = Other
democrat <- ifelse(party==1,1,0)
republican <- ifelse(party==2,1,0)

### ideology 
ideology <- c(ee08$e124_ideol,
              ee09$E128_ideol,
              ee10$e114_ideol,
              ee11$E110_ideol,
              ee12$e105_ideol,
              ee13$E90_ideol,
              ee14$e99_ideol,
              ee15$e92_ideol,
#              ee16$e105_ideol,
#              ee17$ideol,
              ee18$ideol,
              ee19$ideol,
              ee20$ideol,
              ee21$ideol,
              ee22$ideol,
              ee23$ideol)

con <- ifelse(ideology>=5,1,0)
liberal <- ifelse(ideology<=3,1,0)

### year 
ee08$year <- c(2008)
ee09$year <- c(2009)
ee10$year <- c(2010)
ee11$year <- c(2011)
ee12$year <- c(2012)
ee13$year <- c(2013)
ee14$year <- c(2014)
ee15$year <- c(2015)
#ee16$year <- c(2016)
#ee17$year <- c(2017)
ee18$year <- c(2018)
ee19$year <- c(2019)
ee20$year <- c(2020)
ee21$year <- c(2021)
ee22$year <- c(2022)
ee23$year <- c(2023)

year <- c(ee08$year,
          ee09$year,
          ee10$year,
          ee11$year,
          ee12$year,
          ee13$year,
          ee14$year,
          ee15$year,
#          ee16$year,
#          ee17$year,
          ee18$year,
          ee19$year,
          ee20$year,
          ee21$year,
          ee22$year,
          ee23$year)

fuk <- ifelse(year==2012 | 
                year==2013 |
                year==2014 |  
                year==2015 |
#                year==2016 |
#                year==2017 |
                year==2018 |
                year==2019 |
                year==2020 |
                year==2021 |
                year==2022 |
                year==2023,1,0)

## make data set 
nukeData <- data.frame(age, gender, white, nature, environ, envt_oren,
                       nrsk1, nrsk2, nrsk3, nrsk4, nben1, nben2, nben3,
                       nben4, rskben, plantSite, plantNew, morePlants, 
                       college, ideology, democrat, republican, con, 
                       liberal, year, fuk, energyWorry)
write.csv(nukeData, "data/nukeData.csv")
