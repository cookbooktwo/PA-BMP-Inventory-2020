
# Shorten farmclean to farm
farm <- farmclean

# Create a shorthand for basic farm characteristics such as ID, SRCID, status, Acres
info <- c("ID", "SRCID", "status", "Acres")


#### Acreage characteristics ####

#Is it a farm? Should we include it in our sample?

# 99 farms report no acreage or do not specify acreage
# Diagnostic:
farm %>%
  filter(Acres == 0 | is.na(Acres)) %>%
  select(ID, SRCID, status, Acres, CropAcr, CropTyp, AEU, AnmTyp) %>%
  print(n=100)

# Many of these have neither crops nor livestock. Should these be removed? What should
# the denominator be when calculating proportions?

# 141 farms do not report any crop acres or animals
# Diagnostic:
farm %>%
  filter(CropTyp == "None" & AnmTyp == "None") %>%
  select(ID, SRCID, status, Acres, CropAcr, CropTyp, AnmTyp, PoulAEU, HorsAEU, AEU) %>% 
  print(n = 150)
# As determined below, ID 332 for example is under "CREP"
# Does it therefore make sense to consider this as "nutrient-managed acreage"?

# Although 90 of these report positive farm acreage
# Diagnostic:
farm %>%
  filter(CropTyp == "None" & AnmTyp == "None" & Acres > 0) %>%
  select(ID, SRCID, status, Acres, CropAcr, CropTyp, AnmTyp, PoulAEU, HorsAEU, AEU) %>%
  print(n = 100)

# Replace Acres with CropAcr and a binary variable to indicate that the Acres value
# was imputed?











#### Q5 Nutrients ####

# Create a shorthand for all the variables in Q5
Q5 <- c("Nutr", "Mnur", "Cfert", "Bios", "FPR", "Mush", "OthNutr", "OthNutr_text")

# How many farms apply nutrients to their land?
# Diagnostic:
table(farm %>% select(Nutr), useNA = "always")
# 1406 do apply nutrients
# 267 do not
# 156 did not say

# Check what kind of variables are in Q5
# Diagnostic:
farm %>% select(all_of(info), all_of(Q5))

# Find all the respondents who answered "no" to Q5 or left it blank but answered "yes" to
# one of the nutrient types in Q5a (what type of nutrients do you apply to your land?) or
# wrote something in the "other nutrient" text box
# Diagnostic:
farm %>%
  filter((Nutr == 1 | is.na(Nutr)) & ((Mnur != 0 & !is.na(Mnur)) | (Cfert != 0 & !is.na(Cfert)) | (Bios != 0 & !is.na(Bios)) | (FPR != 0 & !is.na(FPR)) | (Mush != 0 & !is.na(Mush)) | (OthNutr != 0 & !is.na(OthNutr)) | !is.na(OthNutr_text))) %>%
  select(all_of(info), all_of(Q5)) %>%
  print(n = 130)
# There are 126 such cases

# A small subset indicate no nutrients in Q5 but put comments in the "other
# nutrient" text box (most check the bubble in Q5a but some don't):

# ID 613 (No Q5a) I USE A SOL U GRO FERTILIZER THROUGH TRICKLE TUBE
# ID 765 ORGANIC FERTILIZER
# ID 1188 NOTHING ON PERSONAL USE UNKNOWN ON RENTED GROUND
# ID 1316 GROUND WAS TRENTED TO FARMER LIVING NEARBY NOT SURE OF WHAT IF ANY NUTRIENTS WERE ADDED
# ID 1538 SEA WATER FERTILIZER MYCORR PLUS
# ID 1641 CHICKEN COMPOST
# ID 1723 (No Q5a) I FARM 15 ACRES  THE REST OF THE LAND IS LEASED OUT
# ID 1808 MANURE AS PASTURED ANIMALS
# ID 1817 TENANT FARM APPLIES LADD MUMMERT
# ID 1858 (No Q5a) I APPLY MANURE TO 15 FEET OF RASPBERRIES ONLY I GIVE HORSE MANURE TO LOCAL GARDNERS
# ID 1889 CHICKEN MANURE
# ID 1930 THIS IS A HORSE AND CHICKEN RANCH A NEIGHBOR HAULS THE MANURE TO HIS LAND EXCEPT WHAT ANIMALS NATURALLY DROP ON PASTURES I HAVE NOT PUT ANY OTHER NUTR

# Collect the ID's for these cases
idQ5 <- (farm %>% filter((Nutr == 1 | is.na(Nutr)) & ( (Mnur != 0 & !is.na(Mnur)) | (Cfert != 0 & !is.na(Cfert)) | (Bios != 0 & !is.na(Bios)) | (FPR != 0 & !is.na(FPR)) | (Mush != 0 & !is.na(Mush)) | (OthNutr != 0 & !is.na(OthNutr)) | !is.na(OthNutr_text))))$ID

# Replace Nutr with "2" for each of the cases
# Add a column to track the rows that get modified in the "impute Nutr" loop below
farm <- farm %>% mutate(imputeNutr = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$Nutr[i] == 1 | is.na(farm$Nutr[i])) & ( (farm$Mnur[i] != 0 & !is.na(farm$Mnur[i])) | (farm$Cfert[i] != 0 & !is.na(farm$Cfert[i])) | (farm$Bios[i] != 0 & !is.na(farm$Bios[i])) | (farm$FPR[i] != 0 & !is.na(farm$FPR[i])) | (farm$Mush[i] != 0 & !is.na(farm$Mush[i])) | (farm$OthNutr[i] != 0 & !is.na(farm$OthNutr[i])) | !is.na(farm$OthNutr_text[i])) ) {
    farm[i, "Nutr"] <- "2"
    farm$imputeNutr[i] <- 1
  }
}
# There are a few respondents with Nutr == 2 but no indication of which type of
# nutrients (NA for all Q5a). 23 Others such as 442, 516, 613 have Nutr == 2 but explictly
# zeros for Q5a)

# Diagnostic: 
farm %>% filter(ID %in% idQ5) %>% select(all_of(info), all_of(Q5)) %>% print(n = 130)
# Diagnostic:
farm %>% filter(imputeNutr == 1) %>% select(all_of(info), all_of(Q5)) %>% print(n = 130)

# Check if the set of ID's that were modified in the "impute Nutr" loop are the same as
# the ID's that were collected above whenever the farm had Nutr == 1 or missing Nutr
# but responded affirmatively to one of the Q5a subquestions
# Diagnostic:
setequal((farm %>% filter(imputeNutr == 1))$ID, (farm %>% filter(ID %in% idQ5))$ID)
# The sets are equal


# What about those with Nutr == 2, but only wrote something in "OthNutr_text"
# Diagnostic:
farm %>% filter(Nutr == 2 & !is.na(OthNutr_text) & imputeNutr == 0) %>% select(all_of(info), all_of(Q5)) %>% print(n = 120)
# The following ID's made no Q5a selection except to write in the text box
# ID 1197 ONLY TO PEACH TREES
# ID 1227 CHICKEN LITTER
# ID 1319 A LIMITED AMOUNT OF ORGANIC COMPOST BASED ON FERTILIZERS TO VEGETABLE GROUND
# ID 1536 FISH
# ID 1676 GREEN MANURE GRASS CLIPPING BRUSH IS MOWED IN ORCHARD
# ID 1769 APPLY PLANTS IN A POT OR BALL AND BURLAP
# ID 1789 SOME COMPOST SOME MANURE
# ID 1866 CERTIFIED ORGANIC OPERATION 0 MANURE RMP


# Post-modification summary
# How many farms apply nutrients to their land?
# Diagnostic:
table(farm %>% select(Nutr), useNA = "always")
# 1532 do apply nutrients
# 251 do not 
# 46 did not say


# Of those not applying nutrients, what kinds of farming operations do they have?
# Diagnostic:
farm %>%
  filter(Nutr == 1 | is.na(Nutr)) %>%
  select(all_of(info), CropAcr, CropTyp, AnmTyp, AEU, all_of(Q5)) %>%
  print(n = 300)











#### Q6 Injection or Incorporation ####

# Create a shorthand for all the variables in Q6
Q6 <- c("Inj", "LowFast", "LowSlow", "HiFast", "HiSlow", "Immed")

# How many farms inject or incorporate manure?
# Diagnostic:
table(farm %>% select(Inj), useNA = "always")
# 315 do inject or incorporate manure
# 1253 do not
# 261 did not say

# Check what kind of variables are in Q6
# Diagnostic:
farm %>% select(all_of(info), all_of(Q6))
# Inj is numeric
# LowFast is numeric
# LowSlow is numeric
# HiFast is numeric
# HiSlow is a string
# Immed is a string

# Find all the respondents who answered "no" to Q6 or left it blank but indicated
# positive acres in one or more of Q6a
# Diagnostic:
farm %>%
  filter( (Inj == 1 | is.na(Inj)) & ((LowFast > 0 & !is.na(LowFast)) | (LowSlow > 0 & !is.na(LowSlow)) | (HiFast > 0 & !is.na(HiFast)) | (HiSlow > 0 & !is.na(HiSlow)) | (Immed > 0 & !is.na(Immed))) ) %>%
  select(all_of(info), all_of(Q6)) %>%
  print(n = 40)
# There are 33 such cases

# Collect the ID's for these cases
idQ6 <- (farm %>% filter((Inj == 1 | is.na(Inj)) & ((LowFast > 0 & !is.na(LowFast)) | (LowSlow > 0 & !is.na(LowSlow)) | (HiFast > 0 & !is.na(HiFast)) | (HiSlow > 0 & !is.na(HiSlow)) | (Immed > 0 & !is.na(Immed)))))$ID

# Replace Inj with "2" for each of the cases
# Add a column to track the rows that get modified in the "impute Inj" loop below
farm <- farm %>% mutate(imputeInj = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$Inj[i] == 1 | is.na(farm$Inj[i])) & ((farm$LowFast[i] > 0 & !is.na(farm$LowFast[i])) | (farm$LowSlow[i] > 0 & !is.na(farm$LowSlow[i])) | (farm$HiFast[i] > 0 & !is.na(farm$HiFast[i])) | (farm$HiSlow[i] > 0 & !is.na(farm$HiSlow[i])) | (farm$Immed[i] > 0 & !is.na(farm$Immed[i]))) ) {
    farm[i, "Inj"] <- 2
    farm$imputeInj[i] <- 1
  }
}
# There are a few respondents with Inj == 2 but no indication of which type of
# injection, e.g. ID 318, 330

# Diagnostic:
farm %>% select(all_of(info), all_of(Q6), imputeInj) %>% filter(imputeInj == 1) %>% print(n = 50)
# Diagnostic:
farm %>% select(all_of(info), all_of(Q6), imputeInj) %>% filter(ID %in% idQ6) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute Inj" loop is the
# same as the set of ID's for which the respondents answered "no" to Q6 or left it
# blank but indicated positive acres in one or more of Q6a
# Diagnostic:
setequal((farm %>% filter(imputeInj == 1))$ID, (farm %>% filter(ID %in% idQ6))$ID)

# Post-modification summary
# How many farms inject or incorporate manure?
# Diagnostic:
table(farm %>% select(Inj), useNA = "always")
# 348 do inject or incorporate manure
# 1244 do not
# 237 did not say









#### Q7 Nutrient/Manure Management Plan ####

# Create a shorthand for all the variables in Q7
Q7 <- c("Plan", "PlanTyp", "OthPlan_text", "PlanMo", "PlanYr", "PlanAcr", "PlanGov", "PlanNP", "PlanFolw", "PlanRec")

# How many farms have a nutrient or manure management plan?
# Diagnostic:
table(farm %>% select(Plan), useNA = "always")
# 974 do have a nutrient or manure management plan
# 690 do not
# 165 did not say

# Check what kind of variables are in Q7
# Diagnostic:
farm %>% select(all_of(info), all_of(Q7))
# Plan is numeric
# PlanTyp is numeric
# OthPlan_text is a string
# PlanMo is numeric
# PlanYr is numeric
# PlanAcr is numeric
# PlanGov is a string
# PlanNP is a string
# PlanFolw is numeric
# PlanRec is a string

# Examine all the open-ended responses describing "Other" plan
# Diagnostic:
farm %>%
  filter(!is.na(OthPlan_text)) %>%
  select(all_of(info), Plan, PlanTyp, OthPlan_text) %>%
  print(n = 100)

# Identify respondents that selected "Yes" for nutrient/manure management plan but
# don't have a legitimate plan based on their description in "OthPlan_text"
# Diagnostic:
farm %>%
  filter(Plan == 2 & !is.na(OthPlan_text)) %>%
  select(all_of(info), Plan, PlanTyp, OthPlan_text) %>%
  print(n = 80)
# There are 71 such cases

# Change Plan value from "Yes" to "No" for any of the following?

# ID 39  Not sure
# ID 56  BioArc software following PSU Agronomy Guide
# ID 109 removal from property (10 Acres, 0 CropAcr, 0 AEU)
# ID 152 Soil samples every 2years agronomy planning (92 Acres, 95 CropAcr, 6.3 AEU)
# ID 154 Garden and Pasure (21.9 Acres, 8 CropAcr, 15.1 AEU)
# ID 184 Custom On- site equine manure plan
# ID 275 DEP CHAPTER 91 MANURE MANAGEMENT PLAN
# ID 308 PAG 12CAFO
# ID 310 DEP CHAPTER 91 MANURE MANAGEMENT PLAN PREPARE BY TEAM AG
# ID 329 ACT 6 NUTRIENT MANAGEMENT PLAN
# ID 335 CAFO AUG 2022
# ID 341 COMMON SENSE I TRY TO SPREAD MANURE THEN TAKE ACROP OFF DOUBLE CROPPING
# ID 353 AGRICULTURAL E AND S PLAN
# ID 370 E AND S PLAN BY LANC CO CONSERVATION DISTRICT
# ID 377 PER USDA REQUIREMENTS
# ID 452 WE HAVE VERY LITTLE MANURE 7 SPREADER LOADS A YEAR
# ID 493 A BOOK MANURE MANAGEMENT PLAN GUIDANCE 361 0300 002
# ID 537 LOCAL EXTENSION SERVICE HAD MEETING TO ASSIST US DRAW UP OUR OWN INDIVIDUAL MANURE PLANS
# ID 572 VERSION 4 3 NUTRIENT MANAGE PLAN
# ID 636 DEP CHAPTER 91 MANURE MANAGEMENT PLAN
# ID 641 AG E AND S CONTROL PLAN
# ID 654 ALL NUTRIENTS APPLED ARE RECORDED FOR CERTIFIED ORGANIC PUSPOSES
# ID 675 CAFO
# ID 702 P AND R CREP
# ID 721 COHRANVILLE AG SERVICE SUPPLIES A SHEET WHERE WE KEEP RECORD OF LIQUID AND DRY MANURE APPLICATIONS
# ID 766 I GET NOT SURE WHAT WAS WRITTEN NO OFFICIAL PLAN
# ID 771 WE THE FARMERS DECIDE WHICH FIELD AND HOW MUCH TO APPLY
# ID 820 AG E AND S PLAN
# ID 824 SOIL TESTS
# ID 882 MANURE MANAGEMENT PLAN IS BEING WORKED ON
# ID 920 MANURE MANAGEMENT PLAN AND NUTRIENT BALANCE SHEETS FOR IMPORTED MANURE
# ID 925 PLAN BASED ON MARYLAND NUTRIENT MANAGEMENT PLAN
# ID 929 I AM WORKING ON A NUTRIENT MANAGEMENT PLAN
# ID 975 CAFO
# ID 978 PASTURED ANIMALS
# ID 982 AG E AND S NUTRIENT MANAGEMENT PLAN
# ID 1091 EGS PLAN
# ID 1052 MANURE MANAGEMENT PLAN AND NUTRIENT BALANCE SHEETS FOR IMPORTED MANURE
# ID 1121 AGRICULTURAL EROSION AND SEDIMENTATION CONTROL PLAN
# ID 1147 NITROGEN BASED PLAN                                                                                  
# ID 1190 DONT KNOW DENNIS EBY DID IT
# ID 1214 CROP MANAGEMENT PLAN
# ID 1215 AG E AND S PLAN REPORT
# ID 1227 CMZ65C
# ID 1230 I TOOK A MANURE MANAGEMENT PLAN WORKSHOP IN 2012 AT JUST UNDER 2 ANIMAL UNITS PER ACRE OUT MANURE NEEDS ARE NOT MET I AM DOCUMENTING WHEN AND WHER
# ID 1248 WE DO SOIL TEST EVERY YEAR
# ID 1261 PLAN WAS WRITTEN BY CERTIFIED PLAN WRITER TO APPLY CHICKEN MANURE
# ID 1289 SELF MANAGEMENT PLAN PENPACK MANURE
# ID 1302 MANURE IS REMOVED BY MUSHROOM FARM SUPPLIER
# ID 1322 NOT SURE ITS THROUGH NRCS MAYBE NRCS 590
# ID 1326 FOOD PROCESSING LAND APPLICATION SYSTEM PLAN
# ID 1381 SOIL TESTING PLAN
# ID 1426 AGRICULTURAL  SEDIMENTATION CONTROL PLAN
# ID 1450 AG ELS
# ID 1568 CONSERVATION AG EROSION SEDIMENT PLAN
# ID 1595 ANNUAL SOIL TEST COMPOST TEST EVERY 3 YEARS APPLICATIONS OF COMPOST FERTILIZER MANURE BASED ON AGRONOMIST RECOMMENDATIONS
# ID 1635 EROSION AND SEDIMENTATION PLAN
# ID 1637 WILL HAVE ONE SOON
# ID 1645 WE APPLY FERTILIZER TO OUR FRUIT AND VEGETABLE CROPS BASED OFF OF RECOMMENDATIONS OF OUR FERTILIZER CONSULTANTS  DID HYGROUND MAPPING IN 2018
# ID 1731 NUTRIENT MANAGEMENT PLAN
# ID 1777 PA DEP PLAN
# ID 1781 NOT MY PLAN BUT MY ACREAGE IS UNDER A HOG GROWERS NUTRIENT MANAGEMENT PLAN
# ID 1810 MULCH MANUFACTURER HAULS IT OUR EVERY 2 WEEKS
# ID 1820 NUTRIENT MAMGEMEN PLAN
# ID 1826 TOP DRESSING HAY FIELDS AND PASTURE AT 3 TONS 25 15 15 ANNUALLY
# ID 1827 MANURE GIVEN TO A NEIGHBOR WHO HAS NO LIVESTOCK  HE HAS MORE THAN 100 ACRES
# ID 1902 AG E AND S PLANS UPDATED AUGUST 2019
# ID 1904 E AND S PLAN
# ID 1912 OUR CATTLE DONT PRODUCE ENOUGH MANURE TO MAKE A DIFFERENCE IN PRODUCTIVITY
# ID 1922 PLAN IS MANAGED BY MY FARMER RICK JONES
# ID 1927 ACT 38 BUT COVERED UNDER FATHERS ADJOINING FA

# Find all the respondents who answered "no" to Q7 or left it blank but indicated a
# type of plan in Q7a, or a date in Q7b, or a number of acres covered by the plan in
# Q7c, or that the county, state, or federal government funds were used to develop
# the plan in Q7d, or that the plan is nitrogen-based or nitrogen- and phosphorus-based
# in Q7e

# Diagnostic:
farm %>%
  filter( (Plan == 1 | is.na(Plan)) & (!is.na(PlanTyp) | !is.na(OthPlan_text) | (PlanMo > 0 & !is.na(PlanMo)) | !is.na(PlanYr) | (PlanAcr > 0 & !is.na(PlanAcr)) | PlanGov == 2) ) %>%
  select(all_of(info), all_of(Q7)) %>%
  print(n = 125)
# There are 88 such cases

# Yes? ID 332 CREP
# Yes? ID 385 THE PLANS ARE BEING WRITTEN IN THE OFFICES AT THIS TIME WITH TEAM AG
# No?  ID 488 NOT SURE HAD CROP SCOUTING SERVICE
# Yes? ID 525 SOIL AND WATER CONSERVATION PLAN 12 1992 103 ACRES 84 LOIS LANE FARM CONSERVATION PLAN 12 1981 159 ACRES 162 BIG ROCK ROAD FARM
# No?  ID 616 REGISTERED ORGANIC
# No?  ID 992 BUILD PIT IN PLAN
# No?  ID 1002 PLAN GROWB AS GRASS TOLD PLAN NOT NECESSARY
# Yes? ID 1057 AGRONOMIST DIRECTED
# No?  ID 1379 AG E AND S
# No?  ID 1186 AS NEEDED
# No?  ID 1379 AG E AND S
# Yes? ID 1469 WORKING ON BOTH
# Yes? ID 1475 I AM WORKING WITH THE TEAM AG TO GET PLANS IN PLACE
# Yes? ID 1499 SOIL TEST REGULAR AND FOLLOW RECOMMENDATION
# No? ID 1637 WILL HAVE ONE SOON
# Yes? ID 1741 I DO THE SAME AS MY MARYLAND FARMS I HAVE A MARYLAND PLAN
# No?  ID 1808 VERY BASIC WE HAVE 20 ACRES PASTURE WITH VERY LITTLE EXTRA INPUTS
# No?  ID 1858 WHEN I CLEAN STALLS THE MANURE GOES INTO A PILE OR IS USED TO FILL HOLES

# Key question, should Ag E&S Plans count toward "Plan"?
# My gut says no (since these will be counted under "ConsPlan")
# Change to "No" those responses that indicate Ag E&S plan in "OthPlan_text"

# Collect the ID's for these cases
idQ7 <- (farm %>% filter((Plan == 1 | is.na(Plan)) & (!is.na(PlanTyp) | !is.na(OthPlan_text) | (PlanMo > 0 & !is.na(PlanMo)) | !is.na(PlanYr) | (PlanAcr > 0 & !is.na(PlanAcr)) | PlanGov == 2)))$ID

#********************************************************************************************
# This code can eventually be tightened up by having the for loop look for ID %in% idQ7     *
# No need to write out the complicated conditional statement twice                          *
#********************************************************************************************

# Replace Plan with "2" for each of the cases
# Add a column to track the rows that get modified in the "impute Plan" loop below
farm <- farm %>% mutate(imputePlan = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$Plan[i] == 1 | is.na(farm$Plan[i])) & (!is.na(farm$PlanTyp[i]) | !is.na(farm$OthPlan_text[i]) | (farm$PlanMo[i] > 0 & !is.na(farm$PlanMo[i])) | !is.na(farm$PlanYr[i]) | (farm$PlanAcr[i] > 0 & !is.na(farm$PlanAcr[i])) | (farm$PlanGov[i] == 2 & !is.na(farm$PlanGov[i])) ) ) {
    farm[i, "Plan"] <- 2
    farm$imputePlan[i] <- 1
  }
}
# There are a few respondents with Plan == 2 but plan may not be legitimate
# see above, e.g. ID 39, 675

# Diagnostic:
farm %>% filter(imputePlan == 1) %>% select(all_of(info), all_of(Q7)) %>% print(n = 90)
# Diagnostic:
farm %>% select(all_of(info), all_of(Q7), imputePlan) %>% filter(ID %in% idQ7) %>% print(n = 90)

# Test whether the set of ID's that were modified in the "impute Plan" loop is the
# same as the set of ID's for which the respondents answered "no" to Q7 or left it
# blank but indicated something in one or more of Q7a, Q7b, Q7c, Q7d, Q7e, Q7f, Q7g
# Diagnostic:
setequal((farm %>% filter(imputePlan == 1))$ID, (farm %>% filter(ID %in% idQ7))$ID)


# Post-modification summary
# How many farms have a manure management plan?
# Diagnostic:
table(farm %>% select(Plan), useNA = "always")
# 1062 do have a plan
# 674 do not
# 93 did not say

# Check against Q7e, Q7f, and Q7g
# Ask Matt what to do with these
# Dianostic:
farm %>% filter( (Plan == 1 | is.na(Plan)) & !is.na(PlanNP) ) %>% select(all_of(info), all_of(Q7))
# There are 7 respondents that indicated having a nitrogen-based or nitrogen- and
# phosphorus-based plan but did not select "Yes" for Q7

# Diagnostic:
farm %>% filter( (Plan == 1 | is.na(Plan)) & (PlanFolw == 2 & !is.na(PlanFolw)) ) %>% select(all_of(info), all_of(Q7))
# There are 5 respondents that indicated following their plan when they apply nutrients to
# their land but they did not select "Yes" for Q7

# Diagnostic:
farm %>% filter( (Plan == 1 | is.na(Plan)) & (!is.na(PlanRec)) ) %>% select(info, Q7)
# There are 4 respondents that indicated that they do or don't keep nutrient application
# records in accordance with their plan but they did not select "Yes" for Q7










#### Q8A Low Nitrogen Application Rates ####

# Create a shorthand for the variables in Q8
Q8 <- c("NitrLow", "NitrLowAcr", "NitrSpl8", "NitrSpl8Acr", "NitrVar", "NitrVarAcr")

# Q8A
# NitrLow
# Applying nitrogen at rates lower than those recommended in the Penn State
# Agronomy Guide and basic nutrient balance recommendations for nitrogen

# How many farms had total nitrogen application rates lower than those recommended
# in the Penn State Agronomy Guide and basic nutrient balance recommendations for
# nitrogen?
# Diagnostic:
table(farm$NitrLow, useNA = "always")
# 583 do apply nitrogen at a lower rate
# 752 do not
# 494 did not say

# Check what kind of variables are in Q8
# Diagnostic:
farm %>% select(all_of(info), all_of(Q8))
# NitrLow is numeric
# NitrLowAcr is a string
# NitrSpl8 is a string
# NitrSpl8Acr is numeric
# NitrVar is numeric
# NitrVarAcr is a string

# Check (visually) whether NitrLowAcr has any non-numeric characters
# Diagnostic:
table(farm$NitrLowAcr, useNA = "always")

# Fill in the missing NitrLowAcr value for the respondent that indicated "all" for Q8A
# ID 227, SRCID 224224
farm[which(farm$NitrLowAcr == "all"), "NitrLowAcr"] <- as.character(farm$CropAcr[which(farm$NitrLowAcr == "all")])

# Then, convert NitrLowAcr to a numeric variable
farm$NitrLowAcr <- as.numeric(farm$NitrLowAcr)

# Find all the respondents who answered "no" to Q8A or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrLow == 1 | is.na(NitrLow)) & (NitrLowAcr > 0 & !is.na(NitrLowAcr)) ) %>%
  select(all_of(info), all_of(Q8)) %>% print(n = 50)
# There are 37 such cases (after adjusting the one respondent that wrote "all" for NitrLowAcr, and
# changing all the "0.0" strings to numerical values)

# Look at just the respondents who answered "no" to Q8A but indicated some acreage
# under the practice
# Diagnostic:
farm %>%
  filter( NitrLow == 1 & (NitrLowAcr > 0 & !is.na(NitrLowAcr)) ) %>%
  select(all_of(info), all_of(Q8)) %>% print(n = 50)
# There are 28 cases

# Collect the ID's for the cases where the respondent answered "no" to Q8A or left it
# blank but indicated some acreage under the practice
idQ8a <- (farm %>% filter( (NitrLow == 1 | is.na(NitrLow)) & (NitrLowAcr > 0 & !is.na(NitrLowAcr)) ))$ID

# Replace NitrLow with "2" for each case where NitrLow is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old NitrLow, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute NitrLow" loop below
farm <- farm %>% mutate(imputeNitrLow = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrLow[i] == 1 | is.na(farm$NitrLow[i])) & (farm$NitrLowAcr[i] > 0 & !is.na(farm$NitrLowAcr[i])) ) {
    farm[i, "NitrLow"] <- 2
    farm$imputeNitrLow[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ8a) %>% select(all_of(info), all_of(Q8), imputeNitrLow) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrLow == 1) %>% select(all_of(info), all_of(Q8), imputeNitrLow) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrLow" loop is the
# same as the set of ID's for which the respondents answered "No" to Q8A or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrLow == 1))$ID, (farm %>% filter(ID %in% idQ8a))$ID)

# Post-modification summary
# How many farms had total nitrogen application rates lower than those recommended
# in the Penn State Agronomy Guide and basic nutrient balance recommendations for
# nitrogen?
# Diagnostic:
table(farm %>% select(NitrLow), useNA = "always")
# 620 do apply nitrogen at a lower rate
# 724 do not
# 485 did not say









#### Q8B Multiple Lower Rate Split Nitrogen Applications ####

# Q8B
# NitrSpl8
# Applying nitrogen by crop by multiple lower rate split applications throughout
# the growing year

# How many farms applied nitrogen by crop by multiple lower split applications made
# throughout the growing year?
# Diagnostic:
table(farm$NitrSpl8, useNA = "always")
# 712 do apply nitrogen by crop by multiple lower split applications
# 684 do not
# 433 did not say

# Check again what kind of variables are in Q8
# Diagnostic:
farm %>% select(all_of(info), all_of(Q8))

# Convert NitrSpl8 to a numeric variable
# farm$NitrSpl8 <- as.numeric(farm$NitrSpl8)
# why do this? doesn't seem necessary since it's just an indicator

# Find all the respondents who answered "no" to Q8B or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrSpl8 == "1" | is.na(NitrSpl8)) & (NitrSpl8Acr > 0 & !is.na(NitrSpl8Acr)) ) %>%
  select(all_of(info), all_of(Q8)) %>% print(n = 50)
# There are 27 such cases

# Collect the ID's for these cases
idQ8b <- (farm %>% filter( (NitrSpl8 == 1 | is.na(NitrSpl8)) & (NitrSpl8Acr > 0 & !is.na(NitrSpl8Acr)) ))$ID

# Replace NitrSpl8 with "2" for each case where NitrSpl8 is "No" or missing yet the
# respondent indicated having some acres under the practice

# Convert NitrSpl8 to a numeric variable
farm$NitrSpl8 <- as.numeric(farm$NitrSpl8)

# Add a column to track the rows that get modified in the "impute NitrSpl8" loop below
farm <- farm %>% mutate(imputeNitrSpl8 = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrSpl8[i] == 1 | is.na(farm$NitrSpl8[i])) & (farm$NitrSpl8Acr[i] > 0 & !is.na(farm$NitrSpl8Acr[i])) ) {
    farm[i, "NitrSpl8"] <- 2
    farm$imputeNitrSpl8[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ8b) %>% select(all_of(info), all_of(Q8), imputeNitrSpl8) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrSpl8 == 1) %>% select(all_of(info), all_of(Q8), imputeNitrSpl8) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrSpl8" loop is the
# same as the set of ID's for which the respondents answered "No" to Q8B or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrSpl8 == 1))$ID, (farm %>% filter(ID %in% idQ8b))$ID)

# Post-modification summary
# How many farms applied nitrogen by crop by multiple lower split applications made
# throughout the growing year?
# Diagnostic:
table(farm$NitrSpl8, useNA = "always")
# 739 do apply nitrogen by crop by multiple lower split applications
# 669 do not
# 421 did not say











#### Q8C Nitrogen Application at Variable Rates ####

# Q8C
# NitrVar
# Applying nitrogen at variable rates at the sub-field level based on variable crop
# response data from historical records

# How many farms applied nitrogen at variable rates at the sub-field level based on
# variable crop response data from historical records
# Diagnostic:
table(farm$NitrVar, useNA = "always")
# 172 do apply nitrogen at variable rates at the sub-field level
# 1095 do not
# 562 did not say

# Check again what kind of variables are in Q8
# Diagnostic:
farm %>% select(all_of(info), all_of(Q8))
# NitrVar is numeric
# NitrVarAcr is a string

# Check (visually) if there are any non-numeric characters in NitrVarAcr
table(farm$NitrVarAcr, useNA = "always")
# There appear to be only numbers 

# Convert NitrVarAcr to a numeric variable
farm$NitrVarAcr <- as.numeric(farm$NitrVarAcr)

# Find all the respondents who answered "no" to Q8C or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrVar == 1 | is.na(NitrVar)) & (NitrVarAcr > 0 & !is.na(NitrVarAcr)) ) %>%
  select(all_of(info), all_of(Q8)) %>% print(n = 50)
# There are 21 such cases

# Collect the ID's for these cases
idQ8c <- (farm %>% filter( (NitrVar == 1 | is.na(NitrVar)) & (NitrVarAcr > 0 & !is.na(NitrVarAcr)) ))$ID

# Replace NitrVar with "2" for each case where NitrVar is "No" or missing yet the
# respondent indicated having some acres under the practice

# Add a column to track the rows that get modified in the "impute NitrVar" loop below
farm <- farm %>% mutate(imputeNitrVar = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrVar[i] == 1 | is.na(farm$NitrVar[i])) & (farm$NitrVarAcr[i] > 0 & !is.na(farm$NitrVarAcr[i])) ) {
    farm[i, "NitrVar"] <- 2
    farm$imputeNitrVar[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ8c) %>% select(all_of(info), all_of(Q8), imputeNitrVar) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrVar == 1) %>% select(all_of(info), all_of(Q8), imputeNitrVar) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrVar" loop is the
# same as the set of ID's for which the respondents answered "No" to Q8C or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrVar == 1))$ID, (farm %>% filter(ID %in% idQ8c))$ID)

# Post-modification summary
# How many farms applied nitrogen at variable rates at the sub-field level based on
# variable crop response data from historical records?
# Diagnostic:
table(farm$NitrVar, useNA = "always")
# 193 do apply nitrogen at variable rates at the sub-field level
# 1077 do not
# 559 did not say



# Collect "repeat offenders" for writing acres after checking "no"
# or leaving the yes/no part of Q8 blank
Q8prob <- union(union(intersect(idQ8a, idQ8b), intersect(idQ8b, idQ8c)), intersect(idQ8a, idQ8c))

a <- union(union(Q8prob, idQ9a), intersect(Q8prob, idQ9b))
b <- union(intersect(Q8prob, idQ9a), union(Q8prob, idQ9b))
union(a, b)
# There are a zillion possible ways to do this










#### Q9A Nitrogen Injection or Incorporation ####

# Create a shorthand for the variables in Q9
Q9 <- c("NitrInj", "NitrInjAcr", "NitrSet", "NitrSetAcr")


# Q9A
# NitrInj
# Injecting or incorporating inorganic nitrogen fertilizer within 24 hours
# of application

# How many farms injected or incorporated inorganic nitrogen fertilizer only (only?)
# within 24 hours of application
# Diagnostic:
table(farm$NitrInj, useNA = "always")
# 202 do inject or incorporate inorganic nitrogen fertilzer only within 24 hours
# 1188 do not
# 439 did not say

# Check what kind of variables are in Q9
# Diagnostic:
farm %>% select(all_of(info), all_of(Q9))
# NitrInj is a string
# NitrInjAcr is numeric
# NitrSet is a string
# NitrSetAcr is numeric

# Convert NitrInj to a numeric variable
# farm$NitrInj <- as.numeric(farm$NitrInj)
# Why? Maybe just leave this alone for now?

# Find all the respondents who answered "no" to Q9A or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrInj == 1 | is.na(NitrInj)) & (NitrInjAcr > 0 & !is.na(NitrInjAcr)) ) %>%
  select(all_of(info), all_of(Q9)) %>% print(n = 50)
# There are 24 such cases

# Collect the ID's for these cases
idQ9a <- (farm %>% filter( (NitrInj == 1 | is.na(NitrInj)) & (NitrInjAcr > 0 & !is.na(NitrInjAcr)) ))$ID

# Replace NitrInj with "2" for each case where NitrInj is "No" or missing yet the
# respondent indicated having some acres under the practice

# Add a column to track the rows that get modified in the "impute NitrInj" loop below
farm <- farm %>% mutate(imputeNitrInj = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrInj[i] == 1 | is.na(farm$NitrInj[i])) & (farm$NitrInjAcr[i] > 0 & !is.na(farm$NitrInjAcr[i])) ) {
    farm[i, "NitrInj"] <- 2
    farm$imputeNitrInj[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ9a) %>% select(all_of(info), all_of(Q9), imputeNitrInj) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrInj == 1) %>% select(all_of(info), all_of(Q9), imputeNitrInj) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrInj" loop is the
# same as the set of ID's for which the respondents answered "No" to Q9A or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrInj == 1))$ID, (farm %>% filter(ID %in% idQ9a))$ID)

# Post-modification summary
# How many farms injected or incorporated inorganic nitrogen fertilizer only (only?)
# within 24 hours of application
# Diagnostic:
table(farm$NitrInj, useNA = "always")
# 226 do inject or incorporate inorganic nitrogen fertilzer only within 24 hours
# 1167 do not
# 436 did not say











#### Q9B Setbacks (Nitrogen) ####

# Q9B
# NitrSet
# Maintaining a setback of at least 100 feet from any wellheads or springs used
# for drinking water

# How many farms maintained a setback of at least 100 feet from any wellheads or
# springs used for drinking water?
# Diagnostic:
table(farm$NitrSet, useNA = "always")
# 762 do maintain a setback of 100 feet from wellheads or springs
# 612 do not
# 455 did not say

# Check again what kind of variables are in Q9
# Diagnostic:
farm %>% select(all_of(info), all_of(Q9))
# NitrSet is a string
# NitrSetAcr is numeric

# Convert NitrSet to a numeric variable
# farm$NitrSet <- as.numeric(farm$NitrSet)
# Maybe don't need to (yet, at least)

# Find all the respondents who answered "no" to Q9B or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrSet == 1 | is.na(NitrSet)) & (NitrSetAcr > 0 & !is.na(NitrSetAcr)) ) %>%
  select(all_of(info), all_of(Q9)) %>% print(n = 50)
# There are 23 such cases

# Collect the ID's for these cases
idQ9b <- (farm %>% filter( (NitrSet == 1 | is.na(NitrSet)) & (NitrSetAcr > 0 & !is.na(NitrSetAcr)) ))$ID

# Replace NitrSet with "2" for each case where NitrSet is "No" or missing yet the
# respondent indicated having some acres under the practice

# Add a column to track the rows that get modified in the "impute NitrSet" loop below
farm <- farm %>% mutate(imputeNitrSet = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrSet[i] == 1 | is.na(farm$NitrSet[i])) & (farm$NitrSetAcr[i] > 0 & !is.na(farm$NitrSetAcr[i])) ) {
    farm[i, "NitrSet"] <- 2
    farm$imputeNitrSet[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ9b) %>% select(all_of(info), all_of(Q9), imputeNitrSet) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrSet == 1) %>% select(all_of(info), all_of(Q9), imputeNitrSet) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrSet" loop is the
# same as the set of ID's for which the respondents answered "No" to Q9B or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrSet == 1))$ID, (farm %>% filter(ID %in% idQ9b))$ID)

# Post-modification summary
# How many farms maintained a setback of at least 100 feet from any wellheads or
# springs used for drinking water?
# Diagnostic:
table(farm$NitrSet, useNA = "always")
# 785 do maintain a setback of 100 feet from wellheads or springs
# 602 do not
# 442 did not say










#### Q10A Multiple Lower Rate Split Nitrogen Applications ####

# Create a shorthand for the variables in Q10
Q10 <- c("NitrSpl10", "NitrSpl10Acr", "NitrMult", "NitrMultAcr")

# Q10A
# NitrSpl10
# Applying nitrogen by crop by multiple lower rate split applications throughout
# the growing year

# How many farms applied nitrogen by crop by multiple lower rate split applications
# throughout the growing year?
# Diagnostic:
table(farm$NitrSpl10, useNA = "always")
# 671 do apply nitrogen by crop by multiple lower rate split applications
# 767 do not
# 391 did not say

# Check what kind of variables are in Q10
# Diagnostic:
farm %>% select(all_of(info), all_of(Q10))
# NitrSpl10 is numeric
# NitrSpl10Acr is a string
# NitrMult is a string
# NitrMultAcr is numeric

# Tabulate NitrSpl10
# Diagnostic:
table(farm$NitrSpl10, useNA = "always")

# Tabulate NitrSpl10Acr
# Diagnostic:
table(farm$NitrSpl10Acr, useNA = "always")
# There do not appear to be any non-numeric characters in NitrSpl10Acr

# Convert NitrSpl10Acr from a string to a numeric
farm$NitrSpl10Acr <- as.numeric(farm$NitrSpl10Acr)

# Find all the respondents who answered "no" to Q10A or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrSpl10 == 1 | is.na(NitrSpl10)) & (NitrSpl10Acr > 0 & !is.na(NitrSpl10Acr)) ) %>%
  select(all_of(info), all_of(Q10)) %>% print(n = 50)
# There are 28 such cases

# Collect the ID's for these cases
idQ10a <- (farm %>% filter( (NitrSpl10 == 1 | is.na(NitrSpl10)) & (NitrSpl10Acr > 0 & !is.na(NitrSpl10Acr)) ))$ID

# Replace NitrSpl10 with "2" for each case where NitrSpl10 is "No" or missing yet the
# respondent indicated having some acres under the practice

# Add a column to track the rows that get modified in the "impute NitrSpl10" loop below
farm <- farm %>% mutate(imputeNitrSpl10 = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrSpl10[i] == 1 | is.na(farm$NitrSpl10[i])) & (farm$NitrSpl10Acr[i] > 0 & !is.na(farm$NitrSpl10Acr[i])) ) {
    farm[i, "NitrSpl10"] <- 2
    farm$imputeNitrSpl10[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ10a) %>% select(all_of(info), all_of(Q10), imputeNitrSpl10) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrSpl10 == 1) %>% select(all_of(info), all_of(Q10), imputeNitrSpl10) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrSpl10" loop is the
# same as the set of ID's for which the respondents answered "No" to Q10A or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrSpl10 == 1))$ID, (farm %>% filter(ID %in% idQ10a))$ID)

# Post-modification summary
# How many farms applied nitrogen by crop by multiple lower rate split applications
# throughout the growing year?
# Diagnostic:
table(farm$NitrSpl10, useNA = "always")
# 699 do apply nitrogen by crop by multiple lower rate split applications
# 751 do not
# 379 did not say











#### Q10B Multiple Nitrogen Applications ####

# Q10B
# NitrMult
# Applying nitrogen through multiple applications based on recommendations from
# Pre-side dress Nitrate Test (PSNT), chlorophyll meter, NDVI sensor, plant sampling,
# nitrogen modeling, etc.

# How many farms applied nitrogen through multiple applications based on recommendations
# from Pre-side dress Nitrate Test (PSNT), chlorophyll meter, NDVI sensor, plan
# sampling, nitrogen modeling, etc.?
# Diagnostic:
table(farm$NitrMult, useNA = "always")
# 126 do apply nitrogen through multiple applications
# 1171 do not
# 532 did not say

# Check what kind of variables are in Q10
# Diagnostic:
farm %>% select(all_of(info), all_of(Q10))
# NitrMult is a string
# NitrMultAcr is numeric

# Tabulate NitrMult
# Diagnostic:
table(farm$NitrMult, useNA = "always")
# There are no non-numeric characters in NitrMult

# Tabulate NitrMultAcr
# Diagnostic:
table(farm$NitrMultAcr, useNA = "always")

# Convert NitrMult to a numeric variable
# farm$NitrMultAcr <- as.numeric(farm$NitrMultAcr)
# No, don't do this yet

# Find all the respondents who answered "no" to Q10B or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (NitrMult == 1 | is.na(NitrMult)) & (NitrMultAcr > 0 & !is.na(NitrMultAcr)) ) %>%
  select(all_of(info), all_of(Q10)) %>% print(n = 50)
# There are 22 such cases

# Collect the ID's for these cases
idQ10b <- (farm %>% filter( (NitrMult == 1 | is.na(NitrMult)) & (NitrMultAcr > 0 & !is.na(NitrMultAcr)) ))$ID

# Replace NitrMult with "2" for each case where NitrMult is "No" or missing yet the
# respondent indicated having some acres under the practice

# Add a column to track the rows that get modified in the "impute NitrMult" loop below
farm <- farm %>% mutate(imputeNitrMult = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$NitrMult[i] == 1 | is.na(farm$NitrMult[i])) & (farm$NitrMultAcr[i] > 0 & !is.na(farm$NitrMultAcr[i])) ) {
    farm[i, "NitrMult"] <- 2
    farm$imputeNitrMult[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ10b) %>% select(all_of(info), all_of(Q10), imputeNitrMult) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputeNitrMult == 1) %>% select(all_of(info), all_of(Q10), imputeNitrMult) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute NitrMult" loop is the
# same as the set of ID's for which the respondents answered "No" to Q10B or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputeNitrMult == 1))$ID, (farm %>% filter(ID %in% idQ10b))$ID)

# Post-modification summary
# How many farms applied nitrogen through multiple applications based on recommendations
# from Pre-side dress Nitrate Test (PSNT), chlorophyll meter, NDVI sensor, plan
# sampling, nitrogen modeling, etc.?
# Diagnostic:
table(farm$NitrMult, useNA = "always")
# 148 do apply nitrogen through multiple applications
# 1153 do not
# 528 did not say









#### Q11A Low Phosphorus Application Rates ####

# Create a shorthand for the variables in Q11
Q11 <- c("PhosLow", "PhosLowAcr", "PhosRmvl", "PhosRmvlAcr", "PhosVar", "PhosVarAcr")

# Q11A
# PhosLow
# Applying phosphorus at rates lower than those recommended in the Penn State
# Agronomy Guide and basic nutrient balance recommendations for phosphorus

# How many farms had total phosphorus application rates lower than those recommended
# in the Penn State Agronomy Guide and basic nutrient balance recommendations for
# phosphorus?
# Diagnostic:
table(farm$PhosLow, useNA = "always")
# 549 do apply phosphorus at a lower rate
# 791 do not
# 489 did not say

# Check what kind of variables are in Q11
# Diagnostic:
farm %>% select(all_of(info), all_of(Q11))
# PhosLow is numeric
# PhosLowAcr is a string
# PhosRmvl is numeric
# PhosRmvlAcr is numeric
# PhosVar is numeric
# PhosVarAcr is a string

# Check (visually) whether PhosLowAcr has any non-numeric characters
# Diagnostic:
table(farm$PhosLowAcr, useNA = "always")
# It does not appear to

# Convert PhosLowAcr to a numeric variable
farm$PhosLowAcr <- as.numeric(farm$PhosLowAcr)

# Find all the respondents who answered "no" to Q11A or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosLow == 1 | is.na(PhosLow)) & (PhosLowAcr > 0 & !is.na(PhosLowAcr)) ) %>%
  select(all_of(info), all_of(Q11)) %>% print(n = 50)
# There are 21 such cases

# Collect the ID's for these cases
idQ11a <- (farm %>% filter( (PhosLow == 1 | is.na(PhosLow)) & (PhosLowAcr > 0 & !is.na(PhosLowAcr)) ))$ID

# Replace PhosLow with "2" for each case where PhosLow is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosLow, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosLow" loop below
farm <- farm %>% mutate(imputePhosLow = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosLow[i] == 1 | is.na(farm$PhosLow[i])) & (farm$PhosLowAcr[i] > 0 & !is.na(farm$PhosLowAcr[i])) ) {
    farm[i, "PhosLow"] <- 2
    farm$imputePhosLow[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ11a) %>% select(all_of(info), all_of(Q11), imputePhosLow) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosLow == 1) %>% select(all_of(info), all_of(Q11), imputePhosLow) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosLow" loop is the
# same as the set of ID's for which the respondents answered "No" to Q11A or left it
# blank but indicated having some positive acres in the practice
# Diagnostic:
setequal((farm %>% filter(imputePhosLow == 1))$ID, (farm %>% filter(ID %in% idQ11a))$ID)

# Post-modification summary
# How many farms had total phosp application rates lower than those recommended
# in the Penn State Agronomy Guide and basic nutrient balance recommendations for
# nitrogen?
# Diagnostic:
table(farm %>% select(PhosLow), useNA = "always")
# 620 do apply nitrogen at a lower rate
# 724 do not
# 485 did not say

# How many farms had total phosphorus application rates lower than those recommended
# in the Penn State Agronomy Guide and basic nutrient balance recommendations for
# phosphorus?
# Diagnostic:
table(farm$PhosLow, useNA = "always")
# 570 do apply phosphorus at a lower rate
# 776 do not
# 483 did not say











#### Q11B Manure Application Based on Annual Crop Removal of Phosphorus ####

# Create a shorthand for the variables in Q11
Q11 <- c("PhosLow", "PhosLowAcr", "PhosRmvl", "PhosRmvlAcr", "PhosVar", "PhosVarAcr")

# Q11B
# PhosRmvl
# Applying manure based on annual crop removal of phosphorus rather than nitrogen

# How many farms based their manure applications on annual crop removal of
# phosphorus rather than nitrogen?
# Diagnostic:
table(farm$PhosRmvl, useNA = "always")
# 402 do apply manure based on annual crop removal of phosphorus
# 917 do not
# 510 did not say

# Check what kind of variables are in Q11
# Diagnostic:
farm %>% select(all_of(info), all_of(Q11))
# PhosVarAcr is a string (others are numeric)

# Find all the respondents who answered "no" to Q11B or left it blank but indicated
# some acreage under this practice
# Diagnostic:
farm %>%
  filter( (PhosRmvl == 1 | is.na(PhosRmvl)) & (PhosRmvlAcr > 0 & !is.na(PhosRmvlAcr)) ) %>%
  select(all_of(info), all_of(Q11)) %>% print(n = 50)
# There are 23 such cases

# Collect the ID's for these cases
idQ11b <- (farm %>% filter( (PhosRmvl == 1 | is.na(PhosRmvl)) & (PhosRmvlAcr > 0 & !is.na(PhosRmvlAcr)) ))$ID

# Replace PhosRmvl with "2" for each case where PhosRmvl is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosRmvl, create a completely new variable? At least temporarily?  *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosRmvl" loop below
farm <- farm %>% mutate(imputePhosRmvl = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosRmvl[i] == 1 | is.na(farm$PhosRmvl[i])) & (farm$PhosRmvlAcr[i] > 0 & !is.na(farm$PhosRmvlAcr[i])) ) {
    farm[i, "PhosRmvl"] <- 2
    farm$imputePhosRmvl[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ11b) %>% select(all_of(info), all_of(Q11), imputePhosRmvl) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosRmvl == 1) %>% select(all_of(info), all_of(Q11), imputePhosRmvl) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosRmvl" loop is the
# same as the set of ID's for which the respondents answered "No" to Q11B or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosRmvl == 1))$ID, (farm %>% filter(ID %in% idQ11b))$ID)

# Post-modification summary
# How many farms based their manure applications on annual crop removal of
# phosphorus rather than nitrogen?
# Diagnostic:
table(farm$PhosRmvl, useNA = "always")
# 425 do apply manure based on annual crop removal of phosphorus
# 899 do not
# 505 did not say











#### Q11C Phosphorus Application at Variable Rates ####

# Create a shorthand for the variables in Q11
Q11 <- c("PhosLow", "PhosLowAcr", "PhosRmvl", "PhosRmvlAcr", "PhosVar", "PhosVarAcr")

# Q11C
# PhosVar
# Applying phosphorus at variable rates at the sub-field level based on variable
# crop response data from historical records or tools like optical crop sensors

# How many farms applied phosphorus as variable rates at the sub-field level based
# on variable crop response data from historical records or tools like optical crop
# sensors?
# Diagnostic:
table(farm$PhosVar, useNA = "always")
# 136 do apply nitrogen at a lower rate
# 1136 do not
# 557 did not say

# Check what kind of variables are in Q11
farm %>% select(all_of(info), all_of(Q11))
# PhosVarAcr is a string (others are numeric)

# Check (visually) whether PhosVarAcr has any non-numeric characters
table(farm$PhosVarAcr, useNA = "always")
# It does not appear so

# Convert PhosVarAcr to a numeric variable
farm$PhosVarAcr <- as.numeric(farm$PhosVarAcr)

# Find all the respondents who answered "no" to Q11C or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosVar == 1 | is.na(PhosVar)) & (PhosVarAcr > 0 & !is.na(PhosVarAcr)) ) %>%
  select(all_of(info), all_of(Q11)) %>% print(n = 50)
# There are 25 such cases

# Collect the ID's for these cases
idQ11c <- (farm %>% filter( (PhosVar == 1 | is.na(PhosVar)) & (PhosVarAcr > 0 & !is.na(PhosVarAcr)) ))$ID

# Replace PhosVar with "2" for each case where PhosVar is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosVar, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosVar" loop below
farm <- farm %>% mutate(imputePhosVar = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosVar[i] == 1 | is.na(farm$PhosVar[i])) & (farm$PhosVarAcr[i] > 0 & !is.na(farm$PhosVarAcr[i])) ) {
    farm[i, "PhosVar"] <- 2
    farm$imputePhosVar[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ11c) %>% select(all_of(info), all_of(Q11), imputePhosVar) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosVar == 1) %>% select(all_of(info), all_of(Q11), imputePhosVar) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosVar" loop is the
# same as the set of ID's for which the respondents answered "No" to Q11C or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosVar == 1))$ID, (farm %>% filter(ID %in% idQ11C))$ID)

# Post-modification summary
# How many farms applied phosphorus as variable rates at the sub-field level based
# on variable crop response data from historical records or tools like optical crop
# sensors?
Diagnostic: table(farm$PhosVar, useNA = "always")
# 136 do apply nitrogen at a lower rate
# 1136 do not
# 557 did not say











#### Q12A Phosphorus Injection or Incorporation ####

# Create a shorthand for the variables in Q12
Q12 <- c("PhosInj", "PhosInjAcr", "PhosSet", "PhosSetAcr")

# Q12A
# PhosInj
# Injection or incorporation of inorganic phosphorus fertilizer only within 24 hours of application

# How many farms injected or incorporated inorganic phosphorus fertilizer only within
# 24 hours of application?
# Diagnostic:
table(farm$PhosInj, useNA = "always")
# 105 do inject or incorporate phosphorus within 24 hours of application
# 1233 do not
# 494 did not say

# Check what kind of variables are in Q12
# Diagnostic:
farm %>% select(all_of(info), all_of(Q12))
# PhosInj is numeric
# PhosInjAcr is a string
# PhosSet is numeric
# PhosSetAcr is numeric

# Check (visually) whether PhosInjAcr has any non-numeric characters
# Diagnostic:
table(farm$PhosInjAcr, useNA = "always")
# It does not appear to

# Convert PhosInjAcr to a numeric variable
# Diagnostic:
farm$PhosInjAcr <- as.numeric(farm$PhosInjAcr)

# Find all the respondents who answered "no" to Q12A or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosInj == 1 | is.na(PhosInj)) & (PhosInjAcr > 0 & !is.na(PhosInjAcr)) ) %>%
  select(all_of(info), all_of(Q12)) %>% print(n = 50)
# There are 19 such cases

# Collect the ID's for these cases
idQ12a <- (farm %>% filter( (PhosInj == 1 | is.na(PhosInj)) & (PhosInjAcr > 0 & !is.na(PhosInjAcr)) ))$ID

# Replace PhosInj with "2" for each case where PhosInj is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosInj, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosInj" loop below
farm <- farm %>% mutate(imputePhosInj = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosInj[i] == 1 | is.na(farm$PhosInj[i])) & (farm$PhosInjAcr[i] > 0 & !is.na(farm$PhosInjAcr[i])) ) {
    farm[i, "PhosInj"] <- 2
    farm$imputePhosInj[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ12a) %>% select(all_of(info), all_of(Q12), imputePhosInj) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosInj == 1) %>% select(all_of(info), all_of(Q12), imputePhosInj) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosInj" loop is the
# same as the set of ID's for which the respondents answered "No" to Q12A or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosInj == 1))$ID, (farm %>% filter(ID %in% idQ12a))$ID)

# Post-modification summary
# How many farms injected or incorporated inorganic phosphorus fertilizer only within
# 24 hours of application?
# Diagnostic:
table(farm$PhosInj, useNA = "always")
# 583 do inject or incorporate phosphorus within 24 hours of application
# 752 do not
# 494 did not say











#### Q12B Setbacks (Phosphorus) ####

# Create a shorthand for the variables in Q12
Q12 <- c("PhosInj", "PhosInjAcr", "PhosSet", "PhosSetAcr")

# Q12B
# PhosSet
# Maintaining a setback of 100 feet from any wellheads or springs used for drinking
# water and 100 feet from any streams, lakes, ponds, or sinkholes.

# How many farms maintained a setback of 100 feet from any wellheads or springs
# used for drinking water and 100 feet from any streams, lakes, ponds, or sinkholes?
# Diagnostic:
table(farm$PhosSet, useNA = "always")
# 587 do maintain a setback of 100 feet
# 721 do not
# 521 did not say

# First, check what kind of variables are in Q12
farm %>% select(all_of(info), all_of(Q12))
# PhosSet is numeric
# PhosSetAcr is numeric

# Convert PhosSetAcr to a numeric variable
# farm$PhosSetAcr <- as.numeric(farm$PhosSetAcr)
# PhosSetAcr is already a string

# Find all the respondents who answered "no" to Q12B or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosSet == 1 | is.na(PhosSet)) & (PhosSetAcr > 0 & !is.na(PhosSetAcr)) ) %>%
  select(all_of(info), all_of(Q12)) %>% print(n = 50)
# There are 25 such cases

# Collect the ID's for these cases
idQ12b <- (farm %>% filter( (PhosSet == 1 | is.na(PhosSet)) & (PhosSetAcr > 0 & !is.na(PhosSetAcr)) ))$ID

# Replace PhosSet with "2" for each case where PhosSet is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosSet, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosSet" loop below
farm <- farm %>% mutate(imputePhosSet = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosSet[i] == 1 | is.na(farm$PhosSet[i])) & (farm$PhosSetAcr[i] > 0 & !is.na(farm$PhosSetAcr[i])) ) {
    farm[i, "PhosSet"] <- 2
    farm$imputePhosSet[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ12b) %>% select(all_of(info), all_of(Q12), imputePhosSet) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosSet == 1) %>% select(all_of(info), all_of(Q12), imputePhosSet) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosSet" loop is the
# same as the set of ID's for which the respondents answered "No" to Q12b or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosSet == 1))$ID, (farm %>% filter(ID %in% idQ12b))$ID)

# Post-modification summary
# How many farms maintained a setback of 100 feet from any wellheads or springs
# used for drinking water and 100 feet from any streams, lakes, ponds, or sinkholes?
# Diagnostic:
table(farm$PhosSet, useNA = "always")
# 583 do maintain a setback of 100 feet
# 752 do not
# 494 did not say











#### Q13A Phosphorus Application During Lower Risk Seasons ####

# Create a shorthand for the variables in Q13
Q13 <- c("PhosRisk", "PhosRiskAcr", "PhosIdx", "PhosIdxAcr", "PhosSpl", "PhosSplAcr")

# Q13A
# PhosRisk
# Applying phosphorus in seasons of lower risk for phosphorus loss

# How many farms applied phosphorus in seasons of lower risk for phosphorus loss?
# Diagnostic:
table(farm$PhosRisk, useNA = "always")
# 435 do apply phosphorus in seasons of lower risk for phosphorus loss
# 874 do not
# 520 did not say

# Check what kind of variables are in Q13
farm %>% select(all_of(info), all_of(Q13))
# PhosRisk is numeric
# PhosRiskAcr is numeric
# PhosIdx is a string
# PhosIdxAcr is a string
# PhosSpl is a string
# PhosSplAcr is numeric

# Check whether PhosRiskAcr has any non-numeric characters
table(farm$PhosRiskAcr, useNA = "always")
# PhosRiskAcr is a numeric vector so by definition it does not

# Convert PhosRiskAcr to a numeric variable
# farm$PhosRiskAcr <- as.numeric(farm$PhosRiskAcr)
# PhosRiskAcr is already numeric

# Find all the respondents who answered "no" to Q13A or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosRisk == 1 | is.na(PhosRisk)) & (PhosRiskAcr > 0 & !is.na(PhosRiskAcr)) ) %>%
  select(all_of(info), all_of(Q13)) %>% print(n = 50)
# There are 12 such cases

# Correct the acres entry in 13A for SRCID 94676, change from 44 to 4
farm[[which(farm$ID == 1034 & farm$SRCID == "94676"), "PhosRiskAcr"]] <- 4

# Diagnostic:
farm %>%
  filter( (PhosRisk == 1 | is.na(PhosRisk)) & (PhosRiskAcr > 0 & !is.na(PhosRiskAcr)) ) %>%
  select(all_of(info), all_of(Q13)) %>% print(n = 50)
# There are 12 such cases

# Collect the ID's for these cases
idQ13a <- (farm %>% filter( (PhosRisk == 1 | is.na(PhosRisk)) & (PhosRiskAcr > 0 & !is.na(PhosRiskAcr)) ))$ID

# Replace PhosRisk with "2" for each case where PhosRisk is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosRisk, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosRisk" loop below
farm <- farm %>% mutate(imputePhosRisk = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosRisk[i] == 1 | is.na(farm$PhosRisk[i])) & (farm$PhosRiskAcr[i] > 0 & !is.na(farm$PhosRiskAcr[i])) ) {
    farm[i, "PhosRisk"] <- 2
    farm$imputePhosRisk[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ13a) %>% select(all_of(info), all_of(Q13), imputePhosRisk) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosRisk == 1) %>% select(all_of(info), all_of(Q13), imputePhosRisk) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosRisk" loop is the
# same as the set of ID's for which the respondents answered "No" to Q13A or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosRisk == 1))$ID, (farm %>% filter(ID %in% idQ13a))$ID)

# Post-modification summary
# How many farms applied phosphorus in seasons of lower risk for phosphorus loss?
# Diagnostic:
table(farm$PhosRisk, useNA = "always")
# 583 do apply phosphorus in seasons of lower risk for phosphorus loss
# 752 do not
# 494 did not say












#### Q13B Phosphorus Index Assessment was Followed ####

# Create a shorthand for the variables in Q13
Q13 <- c("PhosRisk", "PhosRiskAcr", "PhosIdx", "PhosIdxAcr", "PhosSpl", "PhosSplAcr")

# Q13B
# PhosIdx
# Following phosphorus index assessment to change manure application to a time of year
# when there is a lower risk for phosphorus loss

# How many farms followed a phosphorus index assessment to change manure application
# to a time of year when there is a lower risk for phosphorus loss?
# Diagnostic:
table(farm$PhosIdx, useNA = "always")
# 224 do follow a phosphorus index assessment
# 1018 do not
# 587 did not say

# First, check what kind of variables are in Q13
farm %>% select(all_of(info), all_of(Q13))
# PhosIdx is a string
# PhosIdxAcr is a string

# Check (visually) whether PhosIdxAcr has any non-numeric characters
table(farm$PhosIdxAcr, useNA = "always")
# It does not appear so

# Convert PhosIdxAcr to a numeric variable
farm$PhosIdxAcr <- as.numeric(farm$PhosIdxAcr)

# Find all the respondents who answered "no" to Q13B or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosIdx == 1 | is.na(PhosIdx)) & (PhosIdxAcr > 0 & !is.na(PhosIdxAcr)) ) %>%
  select(all_of(info), all_of(Q11), all_of(Q13)) %>% print(n = 50)
# There are 22 such cases

# Correct the yes/no entry in 11A for SRCID 42711, change from missing to "yes"
farm[[which(farm$ID == 589 & farm$SRCID == "42711"), "PhosLow"]] <- 2

# Diagnostic:
farm %>%
  filter( (PhosIdx == 1 | is.na(PhosIdx)) & (PhosIdxAcr > 0 & !is.na(PhosIdxAcr)) ) %>%
  select(all_of(info), all_of(Q11), all_of(Q13)) %>% print(n = 50)
# There are 22 such cases

# Collect the ID's for these cases
idQ13b <- (farm %>% filter( (PhosIdx == 1 | is.na(PhosIdx)) & (PhosIdxAcr > 0 & !is.na(PhosIdxAcr)) ))$ID

# Replace PhosIdx with "2" for each case where PhosIdx is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosIdx, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosIdx" loop below
farm <- farm %>% mutate(imputePhosIdx = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosIdx[i] == 1 | is.na(farm$PhosIdx[i])) & (farm$PhosIdxAcr[i] > 0 & !is.na(farm$PhosIdxAcr[i])) ) {
    farm[i, "PhosIdx"] <- 2
    farm$imputePhosIdx[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ13b) %>% select(all_of(info), all_of(Q13), imputePhosIdx) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosIdx == 1) %>% select(all_of(info), all_of(Q13), imputePhosIdx) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosIdx" loop is the
# same as the set of ID's for which the respondents answered "No" to Q13B or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosIdx == 1))$ID, (farm %>% filter(ID %in% idQ13b))$ID)

# Post-modification summary
# How many farms followed a phosphorus index assessment to change manure application
# to a time of year when there is a lower risk for phosphorus loss?
# Diagnostic:
table(farm$PhosIdx, useNA = "always")
# 583 do follow a phosphorus index assessment
# 752 do not
# 494 did not say












#### Q13C Split Applications of Phosphorus Made Throughout Growing Year ####

# Create a shorthand for the variables in Q13
Q13 <- c("PhosRisk", "PhosRiskAcr", "PhosIdx", "PhosIdxAcr", "PhosSpl", "PhosSplAcr")

# Q13C
# PhosSpl
# Making split applications of phosphorus throughout the growing year

# How many farms made split applications of phosphorus throught the growing
# year?
# Diagnostic:
table(farm$PhosSpl, useNA = "always")
# 200 do make split applications of phosphorus
# 1066 do not
# 563 did not say

# Check what kind of variables are in Q13
farm %>% select(all_of(info), all_of(Q13))
# PhosSpl is a string
# PhosSplAcr is numeric

# Check whether PhosSpl has any non-numeric characters
table(farm$PhosSpl, useNA = "always")
# No, it only has 1's, 2's, and missing

# Find all the respondents who answered "no" to Q13C or left it blank but indicated
# some acreage under this practice

# Diagnostic:
farm %>%
  filter( (PhosSpl == 1 | is.na(PhosSpl)) & (PhosSplAcr > 0 & !is.na(PhosSplAcr)) ) %>%
  select(all_of(info), all_of(Q13)) %>% print(n = 50)
# There are 20 such cases

# Correct the acres entry in 13C for SRCID 177618, change from 6 to 0
farm[[which(farm$ID == 1164 & farm$SRCID == "177618"), "PhosSplAcr"]] <- 0

# Diagnostic:
farm %>%
  filter( (PhosSpl == 1 | is.na(PhosSpl)) & (PhosSplAcr > 0 & !is.na(PhosSplAcr)) ) %>%
  select(all_of(info), all_of(Q13)) %>% print(n = 50)
# After making the change to ID 1164, there are now 19 such cases

# Collect the ID's for these cases
idQ13c <- (farm %>% filter( (PhosSpl == 1 | is.na(PhosSpl)) & (PhosSplAcr > 0 & !is.na(PhosSplAcr)) ))$ID

# Replace PhosSpl with "2" for each case where PhosSpl is "No" or missing yet the
# respondent indicated having some acres under the practice

#********************************************************************************************************
# Maybe instead of replacing the old PhosSpl, create a completely new variable? At least temporarily?   *
# This way, the modifications can be compared side by side against the original                         *
#********************************************************************************************************

# Add a column to track the rows that get modified in the "impute PhosSpl" loop below
farm <- farm %>% mutate(imputePhosSpl = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set
for (i in 1:dim(farm)[1]) {
  if ( (farm$PhosSpl[i] == 1 | is.na(farm$PhosSpl[i])) & (farm$PhosSplAcr[i] > 0 & !is.na(farm$PhosSplAcr[i])) ) {
    farm[i, "PhosSpl"] <- 2
    farm$imputePhosSpl[i] <- 1
  }
}

# Diagnostic:
farm %>% filter(ID %in% idQ13c) %>% select(all_of(info), all_of(Q13), imputePhosSpl) %>% print(n = 50)
# Diagnostic:
farm %>% filter(imputePhosSpl == 1) %>% select(all_of(info), all_of(Q13), imputePhosSpl) %>% print(n = 50)

# Test whether the set of ID's that were modified in the "impute PhosSpl" loop is the
# same as the set of ID's for which the respondents answered "No" to Q13C or left it
# blank but indicated having some positive acres in the practice
setequal((farm %>% filter(imputePhosSpl == 1))$ID, (farm %>% filter(ID %in% idQ13c))$ID)

# Post-modification summary
# How many farms made split applications of phosphorus throught the growing
# year?
# Diagnostic:
table(farm$PhosSpl, useNA = "always")
# 583 do make split applications of phosphorus
# 752 do not
# 494 did not say











#### Q14 Animal Waste Storage Systems ####

# How many farms have animal waste storage systems?
# Diagnostic:
table(farm$Sto, useNA = "always")
# 674 do
# 931 do not
# 224 did not say

# What kinds of variables are in Q14?
# Diagnostic:
farm %>% select(Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1)
# Sto is numeric
# StoLiv1 is numeric
# StoTyp1 is numeric
# StoDate1 is a string
# StoMo1 is a string
# StoGov1 is a string
# StoEngr1 is numeric
# StoCtrl1 is numeric

# What do the responses to the manure storage subquestions look like, among
# those who indicated having a manure storage unit?
farm %>% filter(Sto == 2) %>% select(Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1)


# Correct the StoTyp2 entry for SRCID 363479, change from 1 to missing
farm[[which(farm$ID == 1365 & farm$SRCID == "363479"), "StoTyp2"]] <- NA
# What was the diagnostic search that led to finding this? maybe in the scraps below?


# How many farms who indicated having a storage unit did not specify
# the manure type? 
farm %>%
  filter(Sto == 2 & is.na(StoLiv1) & is.na(StoTyp1)) %>%
  select(all_of(info), AEU, AnmTyp, Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1) %>%
  print(n = 30)
# There are 28 such cases
# Eventually look at the animal holdings of these farms to perhaps guess at the manure type


# How many farms did not respond affirmatively to the question about having a manure
# storage unit but indicated a response to any of the subitems in section 1 of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv1) | !is.na(StoTyp1) | StoDate1 != "/" | !is.na(StoMo1) | !is.na(StoGov1) | !is.na(StoEngr1) | !is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1) %>%
  print(n = 30)
# There are 139 such cases if we just include responses from the first section of Q14a
idQ14one <- (farm %>% filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv1) | !is.na(StoTyp1) | StoDate1 != "/" | !is.na(StoMo1) | !is.na(StoGov1) | !is.na(StoEngr1) | !is.na(StoCtrl1))))$ID

# How many farms did not respond affirmatively to the question about having a manure
# storage unit but indicated a response to any of the subitems in section 2 of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2))) %>%
  select(all_of(info), Sto, StoLiv2, StoTyp2, StoDate2, StoMo2, StoGov2, StoEngr2, StoCtrl2) %>%
  print(n = 30)
# There are 55 such cases if we just include responses from the second section of Q14a
idQ14two <- (farm %>% filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2))))$ID

# How many farms did not respond affirmatively to the question about having a manure
# storage unit but indicated a response to any of the subitems in section 3 of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv3) | !is.na(StoTyp3) | StoDate3 != "/" | !is.na(StoMo3) | !is.na(StoGov3) | !is.na(StoEngr3) | !is.na(StoCtrl3))) %>%
  select(all_of(info), Sto, StoLiv3, StoTyp3, StoDate3, StoMo3, StoGov3, StoEngr3, StoCtrl3) %>%
  print(n = 30)
# There are 21 such cases if we just include responses from the third section of Q14a
idQ14three <- (farm %>% filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv3) | !is.na(StoTyp3) | StoDate3 != "/" | !is.na(StoMo3) | !is.na(StoGov3) | !is.na(StoEngr3) | !is.na(StoCtrl3))))$ID

# How many farms did not respond affirmatively to the question about having a manure
# storage unit but indicated a response to any of the subitems in section 4 of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv4) | !is.na(StoTyp4) | StoDate4 != "/" | !is.na(StoMo4) | !is.na(StoGov4) | !is.na(StoEngr4) | !is.na(StoCtrl4))) %>%
  select(all_of(info), Sto, StoLiv4, StoTyp4, StoDate4, StoMo4, StoGov4, StoEngr4, StoCtrl4) %>%
  print(n = 30)
# There are 16 such cases if we just include responses from the fourth section of Q14a
idQ14four <- (farm %>% filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv4) | !is.na(StoTyp4) | StoDate4 != "/" | !is.na(StoMo4) | !is.na(StoGov4) | !is.na(StoEngr4) | !is.na(StoCtrl4))))$ID

# How many farms did not respond affirmatively to the question about having a manure
# storage unit but indicated a response to any of the subitems in section 5 of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv5) | !is.na(StoTyp5) | StoDate5 != "/" | !is.na(StoMo5) | !is.na(StoGov5) | !is.na(StoEngr5) | !is.na(StoCtrl5))) %>%
  select(all_of(info), Sto, StoLiv5, StoTyp5, StoDate5, StoMo5, StoGov5, StoEngr5, StoCtrl5) %>%
  print(n = 30)
# There are 14 such cases if we just include responses from the fifth section of Q14a
idQ14five <- (farm %>% filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv5) | !is.na(StoTyp5) | StoDate5 != "/" | !is.na(StoMo5) | !is.na(StoGov5) | !is.na(StoEngr5) | !is.na(StoCtrl5))))$ID

# Create a variable to collect the ID's for the union of all 5 sets above
id14<- union(idQ14one, c(idQ14two, idQ14three, idQ14four, idQ14five))

# Create shorthands for the sets of variables in Q14
Q14a1 <- c("StoLiv1", "StoTyp1", "StoDate1", "StoMo1", "StoGov1", "StoEngr1", "StoCtrl1")
Q14a2 <- c("StoLiv2", "StoTyp2", "StoDate2", "StoMo2", "StoGov2", "StoEngr2", "StoCtrl2")
Q14a3 <- c("StoLiv3", "StoTyp3", "StoDate3", "StoMo3", "StoGov3", "StoEngr3", "StoCtrl3")
Q14a4 <- c("StoLiv4", "StoTyp4", "StoDate4", "StoMo4", "StoGov4", "StoEngr4", "StoCtrl4")
Q14a5 <- c("StoLiv5", "StoTyp5", "StoDate5", "StoMo5", "StoGov5", "StoEngr5", "StoCtrl5")

# Inspect observations that did not respond affirmatively to the question about
# having a manure storage unit but indicated a response to any of the subitems in
# Q14a (for any of the five sections)
farm %>% filter(ID %in% id14) %>% select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2)) %>% print(n = 150)

# Correct the values of StoLiv2 and beyond for SRCID 12939, where the respondent
# started filling out sections 2, 3, and 4 but then scribbled out 2, 3, and 4 and
# wrote “we only have one” [storage unit]
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoLiv2"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoMo2"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoGov2"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoEngr2"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoCtrl2"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoLiv3"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoGov4"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoEngr4"]] <- NA
farm[[which(farm$ID == 300 & farm$SRCID == "12939"), "StoCtrl4"]] <- NA

# It seems like all the respondents who left Q14 yes/no blank but indicated
# details about their storage system in Q14a ought to all have their Sto values
# changed to "yes"

# It is less clear is what to do about the respondents that indicated "no" in 
# Q14 but then listed details about manure storage in Q14a

# Inspect observations that explicitly answered "no" to the question about
# having a manure storage unit but indicated a response to any of the subitems in
# Q14a (for any of the five sections)
farm %>% filter(ID %in% id14 & Sto == 1) %>% select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2)) %>% print(n = 150)
# There a 29 such observations

# Correct the values of StoLiv1 and StoTyp1 for SRCID 19397, where the respondent
# filled in the bubble for "beef" and "dry" but then crossed them out
farm[[which(farm$ID == 366 & farm$SRCID == "19397"), "StoLiv1"]] <- NA
farm[[which(farm$ID == 366 & farm$SRCID == "19397"), "StoTyp1"]] <- NA

# Correct the values of StoLiv1 for SRCID 35598, where the respondent selected "no"
# for Q14 and there was a smudge in the "dairy" bubble that got picked up
farm[[which(farm$ID == 518 & farm$SRCID == "35598"), "StoLiv1"]] <- NA


# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left section 1 of 14a blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoTyp1) & is.na(StoMo1) & is.na(StoGov1) & is.na(StoEngr1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# There appears to be only one respondent (SRCID 155073) that has details for
# storage units in section 2 of 14a but no details for units in section 1 of
# 14a. In this case the respondent scribbled out the section 1 responses and
# the scanner must have accurately picked up on that because all the section 1
# info registered as missing


# Check the respondents that indicated details for a storage unit in section 2 of
# the Q14a subitems
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# There are 313 such cases, so it appears there are about 313 farms that have at
# least 2 storage units (maybe less if there are any others like SRCID 155073 who
# crossed out their entire section 1 responses and wrote their first storage unit
# info in section 2



# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoEngr1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 15 cases of leaving StoEngr1 and StoCtrl1 blank
# I looked at these in depth and documented my findings in the Word document

# Correct the values of StoGov1 and StoCtrl1 for SRCID 49364, where the respondent
# missed the bubbles
farm[[which(farm$ID == 642 & farm$SRCID == "49364"), "StoGov1"]] <- "1"
farm[[which(farm$ID == 642 & farm$SRCID == "49364"), "StoCtrl1"]] <- 2

# Correct the values of StoCtrl1 SRCID 74727, where the respondent missed the
# bubble
farm[[which(farm$ID == 872 & farm$SRCID == "74727"), "StoCtrl1"]] <- 1

# Correct the values of StoGov2 SRCID 679182, where the respondent filled in
# the bubble but then put an X through sections 2-5
farm[[which(farm$ID == 1700 & farm$SRCID == "679182"), "StoGov2"]] <- NA

# Correct the values of StoGov1, StoEngr1, StoCtrl1, StoEngr2 for SRCID 721987,
# where the respondent missed some of the bubbles or made a correction
# the bubble but then put an X through sections 2-5
farm[[which(farm$ID == 1746 & farm$SRCID == "721987"), "StoGov1"]] <- "1"
farm[[which(farm$ID == 1746 & farm$SRCID == "721987"), "StoEngr1"]] <- 1
farm[[which(farm$ID == 1746 & farm$SRCID == "721987"), "StoCtrl1"]] <- 2
farm[[which(farm$ID == 1746 & farm$SRCID == "721987"), "StoEngr2"]] <- 1








# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoMo1) & is.na(StoGov1) & is.na(StoEngr1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 2 cases of leaving everything but StoTyp1 blank
# ID 514
# ID 1147

# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoTyp1) & is.na(StoMo1) & is.na(StoGov1) & is.na(StoEngr1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 2 cases of leaving everything but StoLiv1 blank
# ID 888
# ID 1147

# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoTyp1) & is.na(StoMo1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 6 cases of leaving StoLiv1, StoTyp1, and StoMo1 blank
# ID 731 check
# ID 1147 
# ID 1264 check
# ID 1375 check
# ID 1414 check
# ID 1849 check

# Correct the values of StoEngr1 and StoCtrl1 for SRCID 372280, where the respondent
# looked like they were trying to white out everything in section 1
farm[[which(farm$ID == 1375 & farm$SRCID == "372280"), "StoEngr1"]] <- NA
farm[[which(farm$ID == 1375 & farm$SRCID == "372280"), "StoCtrl1"]] <- NA




# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoTyp1) & is.na(StoGov1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 3 cases of leaving StoLiv1, StoTyp1, and StoGov1 blank
# ID 1080 check
# ID 1147
# ID 1375

# Correct the values of StoTyp1 for SRCID 99623, where the respondent checked
# both dry and liquid, change this to "3" to indicate "both"
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoTyp1"]] <- 3
# Correct the values of StoTyp, StoDate, StoMo, StoEngr, StoCtrl in sections 2-5
# for SRCID 99623, where the respondent checked some of the bubbles but then put
# 4 different X's over the dry/liquid bubbles in sections 2-5
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoTyp2"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoDate2"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoMo2"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoEngr2"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoCtrl2"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoTyp3"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoTyp4"]] <- NA
farm[[which(farm$ID == 1080 & farm$SRCID == "99623"), "StoTyp5"]] <- NA



# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoMo1) & is.na(StoGov1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 4 cases of leaving StoLiv1, StoMo1, and StoGov1 blank
# ID 514
# ID 1106 check
# ID 1147
# ID 1375

# Correct the values of StoLiv1, StoGov1 for SRCID 125247, where the respondent checked
# dairy and "no" but the checkmarks did not make it in the bubbles. Also change the
# StoGov2 to missing since it doesn't appear that there is actually a second manure
# storage unit
farm[[which(farm$ID == 1106 & farm$SRCID == "125247"), "StoLiv1"]] <- 1
farm[[which(farm$ID == 1106 & farm$SRCID == "125247"), "StoGov1"]] <- "1"
farm[[which(farm$ID == 1106 & farm$SRCID == "125247"), "StoGov2"]] <- NA



# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoLiv1) & is.na(StoMo1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 18 cases of leaving StoLiv1 and StoMo1 blank
# ID 514
# ID 548 check
# ID 599 check
# ID 731
# ID 1040 check
# ID 1106
# ID 1129 check
# ID 1147
# ID 1264
# ID 1305 check
# ID 1375
# ID 1414
# ID 1539 check
# ID 1544 check
# ID 1845 check
# ID 1849
# ID 1874 check

# Correct the values of StoGov, StoEngr, and StoCtrl for SRCID 44110 in sections 2-5,
# where the respondent drew vertical lines through these bubbles
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoGov2"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoEngr2"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoCtrl2"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoGov3"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoEngr3"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoCtrl3"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoGov4"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoEngr4"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoCtrl4"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoGov5"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoEngr5"]] <- NA
farm[[which(farm$ID == 599 & farm$SRCID == "44110"), "StoCtrl5"]] <- NA

# Correct the values of StoTyp1, StoGov1, StoEngr1, and StoCtrl1 for SRCID 95456
# where the respondent scribbled out section 1 and listed 4 other units in section 2-5
farm[[which(farm$ID == 1040 & farm$SRCID == "95456"), "StoTyp1"]] <- NA
farm[[which(farm$ID == 1040 & farm$SRCID == "95456"), "StoGov1"]] <- NA
farm[[which(farm$ID == 1040 & farm$SRCID == "95456"), "StoEngr1"]] <- NA
farm[[which(farm$ID == 1040 & farm$SRCID == "95456"), "StoCtrl1"]] <- NA



# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoGov1) & is.na(StoEngr1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 5 cases of leaving StoGov1, StoEngr1, and StoCtrl1 blank
# ID 514
# ID 549
# ID 642
# ID 888
# ID 1147

# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoGov1) & is.na(StoCtrl1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 5 cases of leaving StoGov1 and StoCtrl1 blank
# ID 514
# ID 549
# ID 642
# ID 888
# ID 1147

# Check the respondents that indicated details for a storage unit in section 2 of
# the 14a subitems but left various combinations of section 1 blank
farm %>%
  filter((!is.na(StoLiv2) | !is.na(StoTyp2) | StoDate2 != "/" | !is.na(StoMo2) | !is.na(StoGov2) | !is.na(StoEngr2) | !is.na(StoCtrl2)) & (is.na(StoGov1) & is.na(StoEngr1))) %>%
  select(all_of(info), Sto, all_of(Q14a1), all_of(Q14a2))
# 10 cases of leaving StoGov1 and StoEngr1 blank
# ID 327 check
# ID 335 check
# ID 514
# ID 549
# ID 642
# ID 867
# ID 888
# ID 1125 check
# ID 1147
# ID 1871 check

# Correct the values of StoGov1 for SRCID 15391 where the respondent
# missed the "no" bubble
farm[[which(farm$ID == 327 & farm$SRCID == "15391"), "StoGov1"]] <- "1"

# Correct the values of StoLiv1, StoCtrl1 for SRCID 823540 where the respondent
# checked these bubbles but then put a giant X through section 2
farm[[which(farm$ID == 1871 & farm$SRCID == "823540"), "StoLiv2"]] <- NA
farm[[which(farm$ID == 1871 & farm$SRCID == "823540"), "StoCtrl2"]] <- NA







# Q14 Code Scraps

# How many farms did not respond affirmatively to the question about having a
# manure storage unit but indicated manure type or months of storage in the
# first or second section of Q14a?
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (!is.na(StoLiv1) | !is.na(StoTyp1) | !is.na(StoMo1) | !is.na(StoLiv2) | !is.na(StoTyp2) | !is.na(StoMo2))) %>%
  select(all_of(info), Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1) %>%
  print(n = 30)
# There are 133 cases if we include manure type and storage months responses
# from the first and second sections of Q14a

# Find where the extra two cases come from
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (is.na(StoLiv1) & is.na(StoTyp1) & is.na(StoMo1)) & (!is.na(StoLiv2) | !is.na(StoTyp2) | !is.na(StoMo2))) %>%
  select(all_of(info), Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1, StoLiv2, StoTyp2, StoDate2, StoMo2) %>%
  print(n = 30)

# Find the respondents who did not select "yes" for having animal waste storage
# systems and did not indicate anything for manure type or months of storage in
# section 1 but indicated something for gov't funds, engineer design, or runoff
# control in section 1 or indicated something for manure type or months of
# storage in sections 2 through 5
farm %>%
  filter((Sto == 1 | is.na(Sto)) & (is.na(StoLiv1) & is.na(StoTyp1) & is.na(StoMo1)) & (!is.na(StoGov1) | !is.na(StoEngr1) | !is.na(StoCtrl1) | !is.na(StoLiv2) | !is.na(StoTyp2) | !is.na(StoMo2) | !is.na(StoLiv3) | !is.na(StoTyp3) | !is.na(StoMo3) | !is.na(StoLiv4) | !is.na(StoTyp4) | !is.na(StoMo4)| !is.na(StoLiv5) | !is.na(StoTyp5) | !is.na(StoMo5) )) %>%
  select(all_of(info), Sto, StoLiv1, StoTyp1, StoDate1, StoMo1, StoGov1, StoEngr1, StoCtrl1, StoLiv2, StoTyp2, StoDate2, StoMo2, StoGov2, StoEngr2, StoCtrl2) %>%
  print(n = 30)
# There are 8 such cases






#### Q15 Barnyards ####

Q15 <- c("Barn", "BarnCtrl", "BarnDvrt", "BarnDvrtDate", "BarnDvrtGov", "BarnConc", "BarnConcDate", "BarnConcGov", "BarnCatch", "BarnCatchDate", "BarnCatchGov")

# what kind of variables are in Q15
farm %>% select(all_of(info), all_of(Q15))
# Barn is a string
# BarnCtrl is numeric
# BarnDvrt is numeric
# BarvDvrtDate is a string
# BarnDvrtGov is a string
# BarnConc is numeric
# BarnConcDate is a string
# BarnConcGov is a string
# BarnCatch is numeric
# BarnCatchDate is a string
# BarnCatchGov is numeric


# How many farms have barnyards where animals are kept?
table(farm %>% select(Barn), useNA = "always")
# 709 do have barnyards
# 922 do not
# 198 did not say (surprisingly low number of missing values)

# How many farms have barnyard runoff controls?
table(farm %>% select(BarnCtrl), useNA = "always")
# 435 do have barnyard runoff controls
# 150 do not
# 1244 did not say

# How many farms have diversions to direct clean water runoff away from the
# barnyard (such as roof gutters, downspouts, and outlets)?
table(farm %>% select(BarnDvrt), useNA = "always")
# 526 do have diversions
# 69 do not
# 1234 did not say

# How many respondents indicated "no" for barnyard diversions or did not
# say whether they had a barnyard diversion but wrote something in for
# "date constructed" or checked one of the bubbles for "gov't funds"?
farm %>% filter((BarnDvrt == 1 | is.na(BarnDvrt)) & (BarnDvrtDate != "/" | !is.na(BarnDvrtGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 80 such cases

# How many respondents indicated "no" for barnyard diversions but wrote
# something in for "date constructed" or checked one of the bubbles for
# "gov't funds"?
farm %>% filter(BarnDvrt == 1 & (BarnDvrtDate != "/" | !is.na(BarnDvrtGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 29 such cases but there isn't anything substantive for
# "date constructed" nor are there any who answered "yes" to gov't funds

# How many respondents did not answer the barnyard diversions question
# but wrote something in for "date constructed" or checked one of the
# bubbles for "gov't funds"?
farm %>% filter(is.na(BarnDvrt) & (BarnDvrtDate != "/" | !is.na(BarnDvrtGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 51 such cases

# How many respondents did not answer the barnyard diversions question
# but wrote something in for "date constructed" or checked "yes" for
# "gov't funds"
farm %>% filter(is.na(BarnDvrt) & (BarnDvrtDate != "/" | BarnDvrtGov == "2")) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 37 such cases
# These seem like the ones that should have BarnDvrt converted from
# "missing" to "yes"
# The one exception seems to be ID 1677, SRCID 658604 whose “date
# constructed” is “/ /” and gov’t funds is “no”. This does not seem
# to be a legitimate date constructed and therefore this observation
# ought not be included among those having a diversion system.

#*********************************************************************
# As before, perhaps it would be best to create new variables rather *
# than just modifying the olds ones. That way it is always possible  *
# to compare the adjusted yes/no's with the raw variable             *
#*********************************************************************


# I can't figure out why this loop doesn't work, the if statement seems
# to be doing the same thing as what the filter is doing in the example
# right below. Come back to this with fresh eyes and will.
# Add a column to track the rows that get modified in the "impute BarnDvrt" loop below
farm <- farm %>% mutate(imputeBarnDvrt = numeric(dim(farm)[1]))
# Loop through all rows of the farm data set and change BarnDvrt from
# "missing" to "yes" if the respondent wrote something in for "date
# constructed" or checked "yes" for "gov't funds"
for (i in 1:dim(farm)[1]) {
  if ( is.na(farm$BarnDvrt[i]) & ((farm$BarnDvrtDate[i] != "/" & farm$BarnDvrtDate[i] != "/ /" & !is.na(farm$BarnDvrtDate[i]) ) | farm$BarnDvrtGov[i] == "2") ) {
    farm[i, "imputeBarnDvrt"] <- 2
#    farm$imputeBarnDvrt[i] <- 1
  }
}
farm %>% filter( is.na(BarnDvrt) & ((BarnDvrtDate != "/" & BarnDvrtDate != "/ /" & !is.na(BarnDvrtDate)) | BarnDvrtGov == "2" )) %>% select(all_of(info), all_of(Q15)) %>% print(n = 50)
farm %>% filter( is.na(BarnDvrt) & (BarnDvrtDate != "/" | BarnDvrtGov == "2" )) %>% select(all_of(info), all_of(Q15)) %>% print(n = 50)





# How many farms have a stabilized barnyard surface with concrete, stone
# aggregate or other suitable materials?
table(farm %>% select(BarnConc), useNA = "always")
# 506 do have a stabilized surface with concrete
# 69 do not
# 1254 did not say

# How many farms responded "no" to having a stabilized barnyard surface
# with concrete or did not say whether they had a stabilized barnyard
# surface with concrete but wrote something in for "date constructed" or
# checked one of the bubbles for "gov't funds"?
farm %>% filter((BarnConc == 1 | is.na(BarnConc)) & (BarnConcDate != "/" | !is.na(BarnConcGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 52 such cases

# How many farms responded "no" to having a stabilized barnyard surface
# with concrete but wrote something in for "date constructed" or checked
# one of the bubbles for "gov't funds"?
farm %>% filter(BarnConc == 1 & (BarnConcDate != "/" | !is.na(BarnConcGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 13 such cases but there is only 1 that has anything substantive
# for "date constructed" (ID 81, SRCID 867370 answered "2000"). None answered
# "yes" to gov't funds

# How many farms did not respond to the stabilized barnyard surface
# with concrete question but wrote something in for "date constructed"
# or checked one of the bubbles for "gov't funds"
farm %>% filter(is.na(BarnConc) & (BarnConcDate != "/" | !is.na(BarnConcGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 39 such cases

# How many farms did not respond to the stabilized barnyard surface
# with concrete question but wrote something in for "date constructed"
# or checked "yes" for "gov't funds"
farm %>% filter(is.na(BarnConc) & (BarnConcDate != "/" | BarnConcGov == "2")) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 33 such cases (which is almost the same set of farms as
# in the filter directly above, except the farms with no date that also
# checked "no" for gov't funds have been omitted)
# The set under this filter seem like the ones that should have BarnConc
# converted from "missing" to "yes"

# Adapt the loop from 15A (the one I'm having trouble getting to work)
# to make a modified version of BarnConc











# How many farms have a system to catch barnyard runoff and discharge it
# to storage or stabilized vegetated filter area?
table(farm %>% select(BarnCatch), useNA = "always")
# 390 do have a system to catch barnyard runoff
# 179 do not
# 1260 did not say

# How many farms responded "no" to having a system to catch barnyard
# runoff and discharge it to storage or stabilized vegetated filter area
# or did not say whether they had a system to catch barnyard runoff and
# discharge it to storage or stabilized vegetated filter area but wrote
# something in for "date constructed" or checked one of the bubbles for
# "gov't funds"?
farm %>% filter((BarnCatch == 1 | is.na(BarnCatch)) & (BarnCatchDate != "/" | !is.na(BarnCatchGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 76 such cases

# How many farms responded "no" to having a system to catch barnyard
# runoff and discharge it to storage or stabilized vegetated filter area
# but wrote something in for "date constructed" or checked one of the
# bubbles for "gov't funds"?
farm %>% filter(BarnCatch == 1 & (BarnCatchDate != "/" | !is.na(BarnCatchGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 40 such cases but there is only 1 that has anything substantive
# for "date constructed" (ID 1601, SRCID 587212 answered "08/2016") and 1
# that answered "yes" to gov't funds (ID 149, SRCID 69610)

# How many farms did not respond to the question about having a system
# to catch barnyard runoff and discharge it to storage or stabilized
# vegetated filter area but wrote something in for "date constructed" or
# checked one of the bubbles for "gov't funds"?
farm %>% filter(is.na(BarnCatch) & (BarnCatchDate != "/" | !is.na(BarnCatchGov))) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 36 such cases

# How many farms did not respond to the question about having a system
# to catch barnyard runoff and discharge it to storage or stabilized
# vegetated filter area but wrote something in for "date constructed"
# or checked "yes" for "gov't funds"
farm %>% filter(is.na(BarnCatch) & (BarnCatchDate != "/" | BarnCatchGov == "2")) %>% select(all_of(info), all_of(Q15)) %>% print(n = 100)
# There are 25 such cases (which is 11 fewer thatn the set of farms as
# in the filter directly above, 11 farms with no date that also
# checked "no" for gov't funds have been omitted)
# The set under this filter seem like the ones that should have BarnConc
# converted from "missing" to "yes"

# Adapt the loop from 15A (the one I'm having trouble getting to work)
# to make a modified version of BarnCatch

# Once the 15a-A, 15a-B, 15a-C parts are modified, these can be aggregated
# up to modify the 15a question about having any barnyard runoff controls

# Once 15a is modified, it will probably be possible to use this
# information to back out the true number of farms that have baryards
# where animals are kept, in case there are respondents who did not
# answer Q15 but indicated having some kind of runoff control practice
# (could there also be farms that have barnyards without animals and
# therefore they could have runoff control practices, but no barnyards
# where animals are kept?)






#### Q16 Pastures ####

Q16 <- c("Graze", "GrazePlan", "GrazePlanDate", "GrazePlanGov", "GrazePlanImpl", "GrazePlanAcr")

# what kind of variables are in Q16
farm %>% select(all_of(info), all_of(Q16))
# Graze is numeric
# GrazePlan is numeric
# GrazePlanDate is a string
# GrazePlanGov is numeric
# GrazePlanImpl is numeric
# GrazePlanAcr is numeric


# How many farms have pastures where you graze animals?
table(farm %>% select(Graze), useNA = "always")
# 1018 do have pastures
# 639 do not
# 172 did not say (like Q15 Barn, surprisingly few missing values)

# How many farms have and follow a grazing management plan?
table(farm %>% select(GrazePlan), useNA = "always")
# 216 do have pastures
# 707 do not
# 906 did not say


# How many farms responded "no" to having and following a grazing
# management plan or did not say whether they had or followed a
# grazing management plan but wrote something in for "date constructed"
# or checked one of the bubbles for "gov't funds" or checked one of
# the bubbles for implementing the grazing management plan or wrote
# something in for acres of pasture that the farm is implementing
# their grazing management plan on?
farm %>% filter((GrazePlan == 1 | is.na(GrazePlan)) & (GrazePlanDate != "/" | !is.na(GrazePlanGov) | !is.na(GrazePlanImpl) | !is.na(GrazePlanAcr))) %>% select(all_of(info), all_of(Q16)) %>% print(n = 150)
# There are 141 such cases

# ID 278 SRCID 10799 indicated "no" for 16a grazing plan but claimed
# to be implementing a grazing management plan on 6 acres. Looking at
# the survey image it seems like a real 6 in the acres boxes


# Correct the values of StoGov1 for SRCID 13982 where the respondent
# filled in both the "yes" and "no" bubble but eventually scribbled
# out "yes". This value for StoGov1 should be changed from "2" to "1"
farm[[which(farm$ID == 313 & farm$SRCID == "13982"), "StoGov1"]] <- "1"

# How many farms responded "no" to having and following a grazing
# management plan or did not say whether they had or followed a
# grazing management plan but wrote something in for "date constructed"
# or checked "yes" for "gov't funds" or checked "yes" for implementing
# the grazing management plan or wrote something for acres of pasture
# that the farm is implementing their grazing management plan on?
farm %>% filter((GrazePlan == 1 | is.na(GrazePlan)) & (GrazePlanDate != "/" | GrazePlanGov == "2" | GrazePlanImpl == "2" | !is.na(GrazePlanAcr))) %>% select(all_of(info), all_of(Q16)) %>% print(n = 150)
# There are 106 such cases after removing checking "no" for gov't funds
# or checking "no" for plan implementation as criteria for inclusion

# How many farms did not say whether they had or followed a
# grazing management plan but wrote something in for "date constructed"
# or checked "yes" for "gov't funds" or checked "yes" for implementing
# the grazing management plan or wrote something for acres of pasture
# that the farm is implementing their grazing management plan on?
farm %>% filter((GrazePlan == 1) & (GrazePlanDate != "/" | GrazePlanGov == "2" | GrazePlanImpl == "2" | !is.na(GrazePlanAcr))) %>% select(all_of(info), all_of(Q16)) %>% print(n = 150)
# There are 13 such cases after removing checking "no" for gov't funds
# or checking "no" for plan implementation as criteria for inclusion and
# just considering farms that explicitly checked "no" for 16a GrazePlan

# These are basically all the farms that I recorded as anomalies in the
# project notes Word document







#### Q17 Erosion and Sedimentation Control Plans ####

# Q17 "ConsPlan"
Q17_1 <- c("ConsTyp1", "ConsDate1", "ConsRowAcr1", "ConsHayAcr1", "ConsPasAcr1", "ConsGov1", "ConsSchd1")
Q17_2 <- c("ConsTyp2", "ConsDate2", "ConsRowAcr2", "ConsHayAcr2", "ConsPasAcr2", "ConsGov2", "ConsSchd2")
Q17_3 <- c("ConsTyp3", "ConsDate3", "ConsRowAcr3", "ConsHayAcr3", "ConsPasAcr3", "ConsGov3", "ConsSchd3")
Q17_4 <- c("ConsTyp4", "ConsDate4", "ConsRowAcr4", "ConsHayAcr4", "ConsPasAcr4", "ConsGov4", "ConsSchd4")
Q17_5 <- c("ConsTyp5", "ConsDate5", "ConsRowAcr5", "ConsHayAcr5", "ConsPasAcr5", "ConsGov5", "ConsSchd5")

# what kind of variables are in Q17
farm %>% select(all_of(info), ConsPlan, all_of(Q17_1))
# ConsPlan is numeric

# ConsTyp1 is numeric
# ConsDate1 is a string
# ConsRowAcr1 is a string
# ConsHayAcr1 is numeric
# ConsPasAcr1 is numeric
# ConsGov1 is numeric
# ConsSchd1 is numeric

farm %>% select(all_of(info), ConsPlan, all_of(Q17_2))
# ConsTyp2 is numeric
# ConsDate2 is a string
# ConsRowAcr2 is a string
# ConsHayAcr2 is a string
# ConsPasAcr2 is numeric
# ConsGov2 is numeric
# ConsSchd2 is numeric

farm %>% select(all_of(info), ConsPlan, all_of(Q17_3))
# ConsTyp3 is numeric
# ConsDate3 is a string
# ConsRowAcr3 is a string
# ConsHayAcr3 is numeric
# ConsPasAcr3 is numeric
# ConsGov3 is numeric
# ConsSchd3 is numeric

farm %>% select(all_of(info), ConsPlan, all_of(Q17_4))
# ConsTyp4 is numeric
# ConsDate4 is a string
# ConsRowAcr4 is a string
# ConsHayAcr4 is numeric
# ConsPasAcr4 is a string
# ConsGov4 is numeric
# ConsSchd4 is numeric

farm %>% select(all_of(info), ConsPlan, all_of(Q17_5))
# ConsTyp5 is numeric
# ConsDate5 is a string
# ConsRowAcr5 is numeric
# ConsHayAcr5 is a string
# ConsPasAcr5 is numeric
# ConsGov5 is numeric
# ConsSchd5 is numeric

# How many farms have any Agricultural Erosion & Sedimentation
# Control Plans or NRCS Conservation Plans for your farming
# operations?
table(farm %>% select(ConsPlan), useNA = "always")
# 716 do have any erosion & sedimentation plans
# 830 do not
# 283 did not say


# Plan #1
# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something in for plan type, date, row crop acres, hay acres,
# pasture acres, gov't funds or being on schedule in SECTION 1 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp1) | ConsDate1 != "/" | !is.na(ConsRowAcr1) | !is.na(ConsHayAcr1) | !is.na(ConsPasAcr1) | !is.na(ConsGov1) | !is.na(ConsSchd1))) %>%
  select(all_of(info), ConsPlan, all_of(Q17_1)) %>%
  print(n = 100)
# There are 90 such cases

# Correct the values of ConsTyp1 for SRCID 29420 where the respondent
# tried to check the "NRCE Conservation Plan" bubble but missed the
# inside of the bubble. This value for ConsTyp1 should be changed from
# missing to 2
farm[[which(farm$ID == 455 & farm$SRCID == "29420"), "ConsTyp1"]] <- 2

# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something for plan type, date, row crop acres, hay acres, pasture
# acres, or responded "yes" to gov't funds in SECTION 1 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp1) | ConsDate1 != "/" | !is.na(ConsRowAcr1) | !is.na(ConsHayAcr1) | !is.na(ConsPasAcr1) | ConsGov1 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_1)) %>%
  print(n = 100)
# There are 81 such cases

# How many farms responded "no" to having an erosion control plan
# but wrote something for plan type, date, row crop acres, hay acres,
# pasture acres, or responded "yes" to gov't funds in SECTION 1 of Q17
farm %>%
  filter(ConsPlan == 1 & (!is.na(ConsTyp1) | ConsDate1 != "/" | !is.na(ConsRowAcr1) | !is.na(ConsHayAcr1) | !is.na(ConsPasAcr1) | ConsGov1 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_1)) %>%
  print(n = 20)
# There are 6 such cases



# Plan #2
# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something in for plan type, date, row crop acres, hay acres,
# pasture acres, gov't funds or being on schedule in SECTION 2 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp2) | ConsDate2 != "/" | !is.na(ConsRowAcr2) | !is.na(ConsHayAcr2) | !is.na(ConsPasAcr2) | !is.na(ConsGov2) | !is.na(ConsSchd2))) %>%
  select(all_of(info), ConsPlan, all_of(Q17_2)) %>%
  print(n = 20)
# There are 18 such cases
# ID 283 SRCID 11324  
# ID 455 SRCID 29420
# ID 489 SRCID 32805
# ID 519 SRCID 35715
# ID 731 SRCID 59610
# ID 777 SRCID 64217
# ID 800 SRCID 67387
# ID 1016 SRCID 91503
# ID 1146 SRCID 154485
# ID 1215 SRCID 229611
# ID 1226 SRCID 235805
# ID 1314 SRCID 314103
# ID 1360 SRCID 361086
# ID 1414 SRCID 422833
# ID 1743 SRCID 718918
# ID 1777 SRCID 743234
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something for plan type, date, row crop acres, hay acres, pasture
# acres, or responded "yes" to gov't funds in SECTION 2 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp2) | ConsDate2 != "/" | !is.na(ConsRowAcr2) | !is.na(ConsHayAcr2) | !is.na(ConsPasAcr2) | ConsGov2 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_2)) %>%
  print(n = 20)
# There are 15 such cases
# ID 283 SRCID 11324
# ID 455 SRCID 29420
# ID 489 SRCID 32805
# ID 519 SRCID 35715
# ID 731 SRCID 59610
# ID 777 SRCID 64217
# ID 800 SRCID 67387
# ID 1146 SRCID 154485
# ID 1215 SRCID 229611
# ID 1226 SRCID 235805
# ID 1314 SRCID 314103
# ID 1743 SRCID 718918
# ID 1777 SRCID 743234
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# but wrote something for plan type, date, row crop acres, hay acres,
# pasture acres, or responded "yes" to gov't funds in SECTION 2 of Q17
farm %>%
  filter(ConsPlan == 1 & (!is.na(ConsTyp2) | ConsDate2 != "/" | !is.na(ConsRowAcr2) | !is.na(ConsHayAcr2) | !is.na(ConsPasAcr2) | ConsGov2 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_2)) %>%
  print(n = 20)
# There is 1 such case, ID 489 SRCID 32805


# Plan #3
# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something in for plan type, date, row crop acres, hay acres,
# pasture acres, gov't funds or being on schedule in SECTION 3 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp3) | ConsDate3 != "/" | !is.na(ConsRowAcr3) | !is.na(ConsHayAcr3) | !is.na(ConsPasAcr3) | !is.na(ConsGov3) | !is.na(ConsSchd3))) %>%
  select(all_of(info), ConsPlan, all_of(Q17_3)) %>%
  print(n = 20)
# There are 12 such cases
# ID 283 SRCID 11324
# ID 489 SRCID 32805
# ID 519 SRCID 35715
# ID 731 SRCID 59610
# ID 1016 SRCID 91503 
# ID 1215 SRCID 229611
# ID 1333 SRCID 338928
# ID 1360 SRCID 361086
# ID 1414 SRCID 422833
# ID 1777 SRCID 743234
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something for plan type, date, row crop acres, hay acres, pasture
# acres, or responded "yes" to gov't funds in SECTION 3 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp3) | ConsDate3 != "/" | !is.na(ConsRowAcr3) | !is.na(ConsHayAcr3) | !is.na(ConsPasAcr3) | ConsGov3 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_3)) %>%
  print(n = 20)
# There are 9 such cases
# ID 283 SRCID 11324
# ID 489 SRCID 32805
# ID 519 SRCID 35715
# ID 731 SRCID 59610
# ID 1215 SRCID 229611
# ID 1333 SRCID 338928
# ID 1777 SRCID 743234
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# but wrote something for plan type, date, row crop acres, hay acres,
# pasture acres, or responded "yes" to gov't funds in SECTION 3 of Q17
farm %>%
  filter(ConsPlan == 1 & (!is.na(ConsTyp3) | ConsDate3 != "/" | !is.na(ConsRowAcr3) | !is.na(ConsHayAcr3) | !is.na(ConsPasAcr3) | ConsGov3 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_3)) %>%
  print(n = 20)
# There are 2 such cases, ID 489 SRCID 32805 and ID 1333 SRCID 338928 (distorted survey image guy)
# ID 489 SRCID 32805
# ID 1333 SRCID 338928


# Plan #4
# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something in for plan type, date, row crop acres, hay acres,
# pasture acres, gov't funds or being on schedule in SECTION 4 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp4) | ConsDate4 != "/" | !is.na(ConsRowAcr4) | !is.na(ConsHayAcr4) | !is.na(ConsPasAcr4) | !is.na(ConsGov4) | !is.na(ConsSchd4))) %>%
  select(all_of(info), ConsPlan, all_of(Q17_4)) %>%
  print(n = 20)
# There are 11 such cases
# ID 489 SRCID 32805   
# ID 731 SRCID 59610
# ID 1016 SRCID 91503
# ID 1215 SRCID 229611
# ID 1360 SRCID 361086
# ID 1412 SRCID 419351
# ID 1414 SRCID 422833
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418
# ID 1804 SRCID 764085
# ID 1894 SRCID 847422

# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something for plan type, date, row crop acres, hay acres, pasture
# acres, or responded "yes" to gov't funds in SECTION 4 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp4) | ConsDate4 != "/" | !is.na(ConsRowAcr4) | !is.na(ConsHayAcr4) | !is.na(ConsPasAcr4) | ConsGov4 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_4)) %>%
  print(n = 20)
# There are 6 such cases
# ID 489 SRCID 32805 
# ID 731 SRCID 59610 
# ID 1215 SRCID 229611
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418
# ID 1804 SRCID 764085

# How many farms responded "no" to having an erosion control plan
# but wrote something for plan type, date, row crop acres, hay acres,
# pasture acres, or responded "yes" to gov't funds in SECTION 4 of Q17
farm %>%
  filter(ConsPlan == 1 & (!is.na(ConsTyp4) | ConsDate4 != "/" | !is.na(ConsRowAcr4) | !is.na(ConsHayAcr4) | !is.na(ConsPasAcr4) | ConsGov4 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_4)) %>%
  print(n = 20)
# There are 2 such cases, ID 489 SRCID 32805 and ID 1804 SRCID 764085
# ID 489 SRCID 32805
# ID 1804 SRCID 764085


# Plan #5
# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something in for plan type, date, row crop acres, hay acres,
# pasture acres, gov't funds or being on schedule in SECTION 5 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp5) | ConsDate5 != "/" | !is.na(ConsRowAcr5) | !is.na(ConsHayAcr5) | !is.na(ConsPasAcr5) | !is.na(ConsGov5) | !is.na(ConsSchd5))) %>%
  select(all_of(info), ConsPlan, all_of(Q17_5)) %>%
  print(n = 20)
# There are 7 such cases
# ID 489 SRCID 32805
# ID 731 SRCID 59610
# ID 1016 SRCID 91503
# ID 1360 SRCID 361086
# ID 1414 SRCID 422833
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# or did not say whether they had an erosion control plan but wrote
# something for plan type, date, row crop acres, hay acres, pasture
# acres, or responded "yes" to gov't funds in SECTION 5 of Q17
farm %>%
  filter((ConsPlan == 1 | is.na(ConsPlan)) & (!is.na(ConsTyp5) | ConsDate5 != "/" | !is.na(ConsRowAcr5) | !is.na(ConsHayAcr5) | !is.na(ConsPasAcr5) | ConsGov5 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_5)) %>%
  print(n = 20)
# There are 3 such cases
# ID 731 SRCID 59619
# ID 1778 SRCID 743306
# ID 1796 SRCID 759418

# How many farms responded "no" to having an erosion control plan
# but wrote something for plan type, date, row crop acres, hay acres,
# pasture acres, or responded "yes" to gov't funds in SECTION 5 of Q17
farm %>%
  filter(ConsPlan == 1 & (!is.na(ConsTyp5) | ConsDate5 != "/" | !is.na(ConsRowAcr5) | !is.na(ConsHayAcr5) | !is.na(ConsPasAcr5) | ConsGov5 == 2)) %>%
  select(all_of(info), ConsPlan, all_of(Q17_5)) %>%
  print(n = 20)
# There are 0 such cases




#### Q18 No Till or Minimum Till ####

Q18 <- c("Notill", "HiResAcr", "MedResAcr", "LowResAcr")

# What kind of variables are in Q18?
farm %>% select(all_of(info), all_of(Q18))
# Notill is numeric
# HiResAcr is numeric
# MedResAcr is numeric
# LowResAcr is numeric

# How many farms practiced no till or minimum till in calendar
# year 2019?
table(farm %>% select(Notill), useNA = "always")
# 1119 practiced no till or minimum till in calendar year 2019
# 511 did not
# 199 did not say

# How many farms had some acres in which they left at least 60% of
# crop residue in the field at the time of [next?] planting?
farm %>% filter(HiResAcr > 0) %>% summarize(farms = n())
farm %>% filter(HiResAcr == 0) %>% summarize(farms = n())
farm %>% filter(is.na(HiResAcr)) %>% summarize(farms = n())
# 589 farms reported some acres with at least 60% residue
# 1240 farms did not report a number acres

# Check that this code properly takes missing values in HiResAcr
# and converts them to zeros
table(farm %>% mutate(HiResAcr = case_when(!is.na(HiResAcr) ~ HiResAcr, is.na(HiResAcr) ~ 0)) %>% select(HiResAcr), useNA = "always")

# Modify the farm data set to convert the HiResAcr missing values to zeros
farm <- farm %>% mutate(HiResAcr = case_when(!is.na(HiResAcr) ~ HiResAcr, is.na(HiResAcr) ~ 0))

# Tabulate HiResAcr after transformation
table(farm %>% select(HiResAcr), useNA = "always")




# How many farms had some acres in which they left between 30% and
# 60% of crop residue in the field at the time of [next?] planting?
farm %>% filter(MedResAcr > 0) %>% summarize(farms = n())
farm %>% filter(MedResAcr == 0) %>% summarize(farms = n())
farm %>% filter(is.na(MedResAcr)) %>% summarize(farms = n())
# 396 farms reported some acres with 30-60% residue
# 3 farms reported zero acres with 30-60% residue
# 1430 farms did not report a number of acres

# Check that this code properly takes missing values in MedResAcr
# and converts them to zeros
table(farm %>% mutate(MedResAcr = case_when(!is.na(MedResAcr) ~ MedResAcr, is.na(MedResAcr) ~ 0)) %>% select(MedResAcr), useNA = "always")

# Modify the farm data set to convert the MedResAcr missing values to zeros
farm <- farm %>% mutate(MedResAcr = case_when(!is.na(MedResAcr) ~ MedResAcr, is.na(MedResAcr) ~ 0))

# Tabulate MedResAcr after transformation
table(farm %>% select(MedResAcr), useNA = "always")



# How many farms had some acres in which they left between 15% and
# 30% of crop residue in the field at the time of [next?] planting?
farm %>% filter(LowResAcr > 0) %>% summarize(farms = n())
farm %>% filter(LowResAcr == 0) %>% summarize(farms = n())
farm %>% filter(is.na(LowResAcr)) %>% summarize(farms = n())
# 332 farms reported some acres with 30-60% residue
# 13 farms reported zero acres with 30-60% residue
# 1484 farms did not report a number of acres

# Check that this code properly takes missing values in LowResAcr
# and converts them to zeros
table(farm %>% mutate(LowResAcr = case_when(!is.na(LowResAcr) ~ LowResAcr, is.na(LowResAcr) ~ 0)) %>% select(LowResAcr), useNA = "always")

# Modify the farm data set to convert the LowResAcr missing values to zeros
farm <- farm %>% mutate(LowResAcr = case_when(!is.na(LowResAcr) ~ LowResAcr, is.na(LowResAcr) ~ 0))

# Tabulate LowResAcr after transformation
table(farm %>% select(LowResAcr), useNA = "always")






#### Q19 Cover Crops ####

# Q19 "Cov"
Q19_1 <- c("CovTyp1", "OthCov1_text", "Drill1", "Broadw1", "Broadwo1", "Aerial1", "OthMthd1", "OthMthd1_text", "CovDate1", "CovAcr1", "FalMnur1", "SprNutr1", "SprHarv1", "SprHarvAcr1")
Q19_2 <- c("CovTyp2", "OthCov2_text", "Drill2", "Broadw2", "Broadwo2", "Aerial2", "OthMthd2", "OthMthd2_text", "CovDate2", "CovAcr2", "FalMnur2", "SprNutr2", "SprHarv2", "SprHarvAcr2")
Q19_3 <- c("CovTyp3", "OthCov3_text", "Drill3", "Broadw3", "Broadwo3", "Aerial3", "OthMthd3", "OthMthd3_text", "CovDate3", "CovAcr3", "FalMnur3", "SprNutr3", "SprHarv3", "SprHarvAcr3")
Q19_4 <- c("CovTyp4", "OthCov4_text", "Drill4", "Broadw4", "Broadwo4", "Aerial4", "OthMthd4", "OthMthd4_text", "CovDate4", "CovAcr4", "FalMnur4", "SprNutr4", "SprHarv4", "SprHarvAcr4")
Q19_5 <- c("CovTyp5", "OthCov5_text", "Drill5", "Broadw5", "Broadwo5", "Aerial5", "OthMthd5", "OthMthd5_text", "CovDate5", "CovAcr5", "FalMnur5", "SprNutr5", "SprHarv5", "SprHarvAcr5")

# What kind of variables are in Q19?
farm %>% select(all_of(info), Cov, all_of(Q19_1))
# Cov is a string
# CovTyp1 is a string
# OthCov1_text is a string
# Drill1 is a string
# Broadw1 is numeric
# Broadwo1 is numeric
# Aerial1 is a string
# OthMthd1 is numeric
# OthMthd1_text is a string
# CovDate1 is a string
# CovAcr1 is numeric
# FalMnur1 is numeric
# SprNutr1 is a string
# SprHarv1 is numeric
# SprHarvAcr1 is a string

farm %>% select(Cov, all_of(Q19_2))
# CovTyp2 is numeric
# OthCov2_text is a string
# Drill2 is a string
# Broadw2 is numeric
# Broadwo2 is numeric
# Aerial2 is numeric
# OthMthd2 is a string
# OthMthd2_text is a string
# CovDate2 is a string
# CovAcr2 is numeric
# FalMnur2 is a string
# SprNutr2 is a string
# SprHarv2 is numeric
# SprHarvAcr2 is numeric

farm %>% select(Cov, all_of(Q19_3))
# CovTyp3 is a string
# OthCov3_text is a string
# Drill3 is a string
# Broadw3 is numeric
# Broadwo3 is a string
# Aerial3 is numeric
# OthMthd3 is a string
# OthMthd3_text is a string
# CovDate3 is a string
# CovAcr3 is numeric
# FalMnur3 is numeric
# SprNutr3 is numeric
# SprHarv3 is numeric
# SprHarvAcr3 is numeric

farm %>% select(Cov, all_of(Q19_4))
# CovTyp4 is a string
# OthCov4_text is a string
# Drill4 is a string
# Broadw4 is numeric
# Broadwo4 is numeric
# Aerial4 is numeric
# OthMthd4 is numeric
# OthMthd4_text is logical
# CovDate4 is a string
# CovAcr4 is numeric
# FalMnur4 is numeric
# SprNutr4 is numeric
# SprHarv4 is numeric
# SprHarvAcr4 is numeric

farm %>% select(Cov, all_of(Q19_5))
# CovTyp5 is numeric
# OthCov5_text is a string
# Drill5 is a string
# Broadw5 is numeric
# Broadwo5 is numeric
# Aerial5 is numeric
# OthMthd5 is numeric
# OthMthd5_text is logical
# CovDate5 is a string
# CovAcr5 is numeric
# FalMnur5 is a string
# SprNutr5 is numeric
# SprHarv5 is numeric
# SprHarvAcr5 is a string


# How many farms planted cover crops or winter crops in calendar
# year 2019?
table(farm %>% select(Cov), useNA = "always")
# 1035 planted cover crops or winter crops in calendar year 2019
# 546 did not
# 248 did not say

# Cover Crop #1
# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 1 of Q19 for cover crop type, planting
# method, date planted, acres planted, fall manure nutient
# application, spring nutrient application before March 1, spring
# harvesting, or acres harvested in the spring?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/" | !is.na(CovAcr1) | !is.na(FalMnur1) | !is.na(SprNutr1) | !is.na(SprHarv1) | !is.na(SprHarvAcr1))) %>%
  select(all_of(info), Cov, all_of(Q19_1)) %>%
  print(n = 101)
# There are 101 such cases

# Save the ID's for this set
# id1 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/" | !is.na(CovAcr1) | !is.na(FalMnur1) | !is.na(SprNutr1) | !is.na(SprHarv1) | !is.na(SprHarvAcr1))))$ID
  

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 1 of Q19 for cover crop type, planting
# method, date planted, or acres planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/" | !is.na(CovAcr1))) %>%
  select(all_of(info), Cov, all_of(Q19_1)) %>%
  print(n = 100)
# There are 99 such cases (only 2 cases where the respondent
# ONLY indicated something about their fall or spring nutrient
# application, or harvesting in spring, or acres harvested
# in spring, ID 1285 SRCID 284730 and ID 1414 SRCID 422833)

# Go throught this 99x19 table just to check for anamolies
# and verify that it makes sense to change most of these to "yes"
# for growing a cover crop

# Save the ID's for this set
# id2 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/" | !is.na(CovAcr1))))$ID

# Identify the two observations that responded "no" to planting
# a cover crop or did not say whether they planted a cover crop
# and then only checked bubbles for fall/spring nutrient application
# or harvesting in spring, or acres harvested in spring
# setdiff(id1, id2)

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop and did not
# indicate any cover crop acres but wrote
# something in SECTION 1 of Q19 for cover crop type, planting
# method, or date planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (is.na(CovAcr1)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/")) %>%
  select(all_of(info), Cov, all_of(Q19_1)) %>%
  print(n = 50)
# There are 33 such cases
# This list seems to be where we are most likely to find farms that
# don't belong in the "yes" category for cover crops

# Verify that the set of farms in the filter immediately above is a
# subset of those from the 99 and 101 cases a little further above
id3 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (is.na(CovAcr1)) & (!is.na(CovTyp1) | !is.na(OthCov1_text) | (!is.na(Drill1) & Drill1 != "0") | (!is.na(Broadw1) & Broadw1 != 0) | (!is.na(Broadwo1) & Broadwo1 != 0) | (!is.na(Aerial1) & Aerial1 != "0") | (!is.na(OthMthd1) & OthMthd1 != 0) | !is.na(OthMthd1_text) | CovDate1 != "/")))$ID
intersect(id1, id3)
id3
# This will show that intersect(id1, id3) is the same as id3








# Cover Crop #2
# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 2 of Q19 for cover crop type, planting
# method, date planted, acres planted, fall manure nutient
# application, spring nutrient application before March 1, spring
# harvesting, or acres harvested in the spring?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/" | !is.na(CovAcr2) | !is.na(FalMnur2) | !is.na(SprNutr2) | !is.na(SprHarv2) | !is.na(SprHarvAcr2))) %>%
  select(ID, SRCID, Cov, all_of(Q19_2)) %>%
  print(n = 50)
# There are 43 such cases

# Save the ID's for this set
id1 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/" | !is.na(CovAcr2) | !is.na(FalMnur2) | !is.na(SprNutr2) | !is.na(SprHarv2) | !is.na(SprHarvAcr2))))$ID


# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 2 of Q19 for cover crop type, planting
# method, date planted, or acres planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/" | !is.na(CovAcr2))) %>%
  select(ID, SRCID, Cov, all_of(Q19_2)) %>%
  print(n = 50)
# There are 41 such cases (only 2 cases where the respondent
# ONLY indicated something about their fall or spring nutrient
# application, or harvesting in spring, or acres harvested
# in spring, ID 1414 SRCID 422833, ID 1459 SRCID 467105)

# Go throught the 43x17 table just to check for anamolies
# and verify that it makes sense to change most of these to "yes"
# for growing a cover crop

# Save the ID's for this set
id2 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/" | !is.na(CovAcr2))))$ID

# Identify the two observations that responded "no" to planting
# a cover crop or did not say whether they planted a cover crop
# and then only checked bubbles for fall/spring nutrient application
# or harvesting in spring, or acres harvested in spring
setdiff(id1, id2)

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop and did not
# indicate any cover crop acres but wrote
# something in SECTION 2 of Q19 for cover crop type, planting
# method, or date planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr2) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/")) %>%
  select(ID, SRCID, Cov, all_of(Q19_2)) %>%
  print(n = 50)
# There are 28 such cases
# This list seems to be where we are most likely to find farms that
# don't belong in the "yes" category for cover crops (based on section
# 2 responses)

# Verify that the set of farms in the filter immediately above is a
# subset of those from the 41 and 43 cases a little further above
id3 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr2) & (!is.na(CovTyp2) | !is.na(OthCov2_text) | (!is.na(Drill2) & Drill2 != "0") | (!is.na(Broadw2) & Broadw2 != 0) | (!is.na(Broadwo2) & Broadwo2 != 0) | (!is.na(Aerial2) & Aerial2 != 0) | (!is.na(OthMthd2) & OthMthd2 != "0") | !is.na(OthMthd2_text) | CovDate2 != "/")))$ID
intersect(id1, id3)
id3
# This will show that intersect(id1, id3) is the same as id3 (that is,
# all the ID's that appear in id3 also appear in id1)








# Cover Crop #3
# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 3 of Q19 for cover crop type, planting
# method, date planted, acres planted, fall manure nutient
# application, spring nutrient application before March 1, spring
# harvesting, or acres harvested in the spring?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/" | !is.na(CovAcr3) | !is.na(FalMnur3) | !is.na(SprNutr3) | !is.na(SprHarv3) | !is.na(SprHarvAcr3))) %>%
  select(ID, SRCID, Cov, all_of(Q19_3)) %>%
  print(n = 50)
# There are 23 such cases

# Save the ID's for this set
id1 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/" | !is.na(CovAcr3) | !is.na(FalMnur3) | !is.na(SprNutr3) | !is.na(SprHarv3) | !is.na(SprHarvAcr3))))$ID

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 3 of Q19 for cover crop type, planting
# method, date planted, or acres planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/" | !is.na(CovAcr3))) %>%
  select(ID, SRCID, Cov, all_of(Q19_3)) %>%
  print(n = 50)
# There are 22 such cases (only 1 case where the respondent
# ONLY indicated something about their fall or spring nutrient
# application, or harvesting in spring, or acres harvested
# in spring, ID 1414 SRCID 422833)

# Go through the 23x17 table just to check for anamolies
# and verify that it makes sense to change most of these to "yes"
# for growing a cover crop

# Save the ID's for this set
id2 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/" | !is.na(CovAcr3))))$ID
id1
id2
# Use this to check where the 1 discrepancy is (ID 1414)

# Identify the two observations that responded "no" to planting
# a cover crop or did not say whether they planted a cover crop
# and then only checked bubbles for fall/spring nutrient application
# or harvesting in spring, or acres harvested in spring
setdiff(id1, id2)

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop and did not
# indicate any cover crop acres but wrote
# something in SECTION 3 of Q19 for cover crop type, planting
# method, or date planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr3) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/")) %>%
  select(ID, SRCID, Cov, all_of(Q19_3)) %>%
  print(n = 50)
# There are 13 such cases
# This list seems to be where we are most likely to find farms that
# don't belong in the "yes" category for cover crops (based on section
# 3 responses)

# Verify that the set of farms in the filter immediately above is a
# subset of those from the 22 and 23 cases a little further above
id3 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr3) & (!is.na(CovTyp3) | !is.na(OthCov3_text) | (!is.na(Drill3) & Drill3 != "0") | (!is.na(Broadw3) & Broadw3 != 0) | (!is.na(Broadwo3) & Broadwo3 != "0") | (!is.na(Aerial3) & Aerial3 != 0) | (!is.na(OthMthd3) & OthMthd3 != "0") | !is.na(OthMthd3_text) | CovDate3 != "/")))$ID
intersect(id1, id3)
id3
# This will show that intersect(id1, id3) is the same as id3 (that is,
# all the ID's that appear in id3 also appear in id1)









# Cover Crop #4
# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 4 of Q19 for cover crop type, planting
# method, date planted, acres planted, fall manure nutient
# application, spring nutrient application before March 1, spring
# harvesting, or acres harvested in the spring?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/" | !is.na(CovAcr4) | !is.na(FalMnur4) | !is.na(SprNutr4) | !is.na(SprHarv4) | !is.na(SprHarvAcr4))) %>%
  select(ID, SRCID, Cov, all_of(Q19_4)) %>%
  print(n = 50)
# There are 12 such cases

# Save the ID's for this set
id1 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/" | !is.na(CovAcr4) | !is.na(FalMnur4) | !is.na(SprNutr4) | !is.na(SprHarv4) | !is.na(SprHarvAcr4))))$ID

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 4 of Q19 for cover crop type, planting
# method, date planted, or acres planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/" | !is.na(CovAcr4))) %>%
  select(ID, SRCID, Cov, all_of(Q19_4)) %>%
  print(n = 50)
# There are 10 such cases (only 2 cases where the respondent
# ONLY indicated something about their fall or spring nutrient
# application, or harvesting in spring, or acres harvested
# in spring, ID 1414 SRCID 422833)

# Go through the 12x17 table just to check for anamolies
# and verify that it makes sense to change most of these to "yes"
# for growing a cover crop

# Save the ID's for this slightly narrower set
id2 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/" | !is.na(CovAcr4))))$ID
id1
id2
# Use this to check where the 2 discrepancies are (ID 1414, ID 1573)

# Identify the two observations that responded "no" to planting
# a cover crop or did not say whether they planted a cover crop
# and then only checked bubbles for fall/spring nutrient application
# or harvesting in spring, or acres harvested in spring
setdiff(id1, id2)

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop and did not
# indicate any cover crop acres but wrote
# something in SECTION 4 of Q19 for cover crop type, planting
# method, or date planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr4) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/")) %>%
  select(ID, SRCID, Cov, all_of(Q19_4)) %>%
  print(n = 50)
# There are 6 such cases
# This list seems to be where we are most likely to find farms that
# don't belong in the "yes" category for cover crops (based on section
# 4 responses)

# Verify that the set of farms in the filter immediately above is a
# subset of those from the 10 and 12 cases a little further above
id3 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr4) & (!is.na(CovTyp4) | !is.na(OthCov4_text) | (!is.na(Drill4) & Drill4 != "0") | (!is.na(Broadw4) & Broadw4 != 0) | (!is.na(Broadwo4) & Broadwo4 != 0) | (!is.na(Aerial4) & Aerial4 != 0) | (!is.na(OthMthd4) & OthMthd4 != 0) | !is.na(OthMthd4_text) | CovDate4 != "/")))$ID
intersect(id1, id3)
id3
# This will show that intersect(id1, id3) is the same as id3 (that is,
# all the ID's that appear in id3 also appear in id1)










# Cover Crop #5
# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 5 of Q19 for cover crop type, planting
# method, date planted, acres planted, fall manure nutient
# application, spring nutrient application before March 1, spring
# harvesting, or acres harvested in the spring?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp5) | !is.na(OthCov5_text) | (!is.na(Drill5) & Drill5 != "0") | (!is.na(Broadw5) & Broadw5 != 0) | (!is.na(Broadwo5) & Broadwo5 != 0) | (!is.na(Aerial5) & Aerial5 != 0) | (!is.na(OthMthd5) & OthMthd5 != 0) | !is.na(OthMthd5_text) | CovDate5 != "/" | !is.na(CovAcr5) | !is.na(FalMnur5) | !is.na(SprNutr5) | !is.na(SprHarv5) | !is.na(SprHarvAcr5))) %>%
  select(ID, SRCID, Cov, all_of(Q19_5)) %>%
  print(n = 50)
# There are 6 such cases

# Save the ID's for this set
id1 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp5) | !is.na(OthCov5_text) | (!is.na(Drill5) & Drill5 != "0") | (!is.na(Broadw5) & Broadw5 != 0) | (!is.na(Broadwo5) & Broadwo5 != 0) | (!is.na(Aerial5) & Aerial5 != 0) | (!is.na(OthMthd5) & OthMthd5 != 0) | !is.na(OthMthd5_text) | CovDate5 != "/" | !is.na(CovAcr5) | !is.na(FalMnur5) | !is.na(SprNutr5) | !is.na(SprHarv5) | !is.na(SprHarvAcr5))))$ID

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop but wrote
# something in SECTION 5 of Q19 for cover crop type, planting
# method, date planted, or acres planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp5) | !is.na(OthCov5_text) | (!is.na(Drill5) & Drill5 != "0") | (!is.na(Broadw5) & Broadw5 != 0) | (!is.na(Broadwo5) & Broadwo5 != 0) | (!is.na(Aerial5) & Aerial5 != 0) | (!is.na(OthMthd5) & OthMthd5 != 0) | !is.na(OthMthd5_text) | CovDate5 != "/" | !is.na(CovAcr5))) %>%
  select(ID, SRCID, Cov, all_of(Q19_5)) %>%
  print(n = 50)
# There are 3 such cases (only 3 cases where the respondent
# ONLY indicated something about their fall or spring nutrient
# application, or harvesting in spring, or acres harvested
# in spring, ID 1414 SRCID 422833)

# Go through the 6x17 table just to check for anamolies
# and verify that it makes sense to change most of these to "yes"
# for growing a cover crop

# Save the ID's for this slightly narrower set
id2 <- (farm %>% filter((Cov == 1 | is.na(Cov)) & (!is.na(CovTyp5) | !is.na(OthCov5_text) | (!is.na(Drill5) & Drill5 != "0") | (!is.na(Broadw5) & Broadw5 != 0) | (!is.na(Broadwo5) & Broadwo5 != 0) | (!is.na(Aerial5) & Aerial5 != 0) | (!is.na(OthMthd5) & OthMthd5 != 0) | !is.na(OthMthd5_text) | CovDate5 != "/" | !is.na(CovAcr5))))$ID
id1
id2
# Use this to check where the 3 discrepancies are (ID 274, ID 530, ID 1414)

# Identify the three observations that responded "no" to planting
# a cover crop or did not say whether they planted a cover crop
# and then only checked bubbles for fall/spring nutrient application
# or harvesting in spring, or acres harvested in spring
setdiff(id1, id2)

# How many farms responded "no" to planting a cover crop
# or did not say whether they planted a cover crop and did not
# indicate any cover crop acres but wrote
# something in SECTION 5 of Q19 for cover crop type, planting
# method, or date planted?
farm %>%
  filter((Cov == 1 | is.na(Cov)) & is.na(CovAcr5) & (!is.na(CovTyp5) | !is.na(OthCov5_text) | (!is.na(Drill5) & Drill5 != "0") | (!is.na(Broadw5) & Broadw5 != 0) | (!is.na(Broadwo5) & Broadwo5 != 0) | (!is.na(Aerial5) & Aerial5 != 0) | (!is.na(OthMthd5) & OthMthd5 != 0) | !is.na(OthMthd5_text) | CovDate5 != "/")) %>%
  select(ID, SRCID, Cov, all_of(Q19_5)) %>%
  print(n = 50)
# There are 0 such cases









#### Q20 Streams and Buffers ####

# Q20 "Stream"
Q20aGrs <- c("BufferCrp", "Grs10CrpDate", "Grs10CrpGov", "Grs10CrpAcr", "Grs35CrpDate", "Grs35CrpGov", "Grs35CrpAcr")
Q20aTre <- c("BufferCrp", "Tre10CrpDate", "Tre10CrpGov", "Tre10CrpAcr", "Tre35CrpDate", "Tre35CrpGov", "Tre35CrpAcr")
Q20bGrs <- c("BufferPas", "Grs10PasFenc", "Grs10PasDate", "Grs10PasGov", "Grs10PasAcr", "Grs35PasFenc", "Grs35PasDate", "Grs35PasGov", "Grs35PasAcr")
Q20bTre <- c("BufferPas", "Tre10PasFenc", "Tre10PasDate", "Tre10PasGov", "Tre10PasAcr", "Tre35PasFenc", "Tre35PasDate", "Tre35PasGov", "Tre35PasAcr")

# What kind of variables are in Q20?
farm %>% select(all_of(info), Stream, all_of(Q20aGrs))
farm %>% select(all_of(info), Stream, all_of(Q20aTre))
# Stream is numeric

# BufferCrp is numeric

# Grs10CrpDate is a string
# Grs10CrpGov is numeric
# Grs10CrpAcr is numeric
# Grs35CrpDate is a string
# Grs35CrpGov is a numeric
# Grs35CrpAcr is numeric

# Tre10CrpDate is a string
# Tre10CrpGov is numeric
# Tre10CrpAcr is a string
# Tre35CrpDate is a string
# Tre35CrpGov is numeric
# Tre35CrpAcr is numeric

farm %>% select(all_of(info), Stream, all_of(Q20bGrs))
farm %>% select(all_of(info), Stream, all_of(Q20bTre))
# BufferPas is a string

# Grs10PasFenc is numeric
# Grs10PasDate is a string
# Grs10PasGov is numeric
# Grs10PasAcr is a string
# Grs35PasFenc is numeric
# Grs35PasDate is a string
# Grs35PasGov is numeric
# Grs35PasAcr is string

# Tre10PasFenc is numeric
# Tre10PasDate is a string
# Tre10PasGov is numeric
# Tre10PasAcr is numeric
# Tre35PasFenc is numeric
# Tre35PasDate is a string
# Tre35PasGov is numeric
# Tre35PasAcr is numeric

# How many farms had any streams or waterways on the lands that are
# part of your farming operation?
table(farm %>% select(Stream), useNA = "always")
# 1067 had streams or waterways
# 564 did not
# 198 did not say


# How many farms maintain permanent vegetation of an average width
# of at least 10 feet between the stream bank or waterway and any
# of their CROPLAND?
table(farm %>% select(BufferCrp), useNA = "always")
# 779 maintained permament vegetation between stream and cropland
# 134 did not
# 916 did not say

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their CROPLAND
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their CROPLAND but they
# indicated a date established for the vegetation, checked one of
# the bubbles for gov't funds, or indicated an acreage value
# for GRASS BUFFERS?
farm %>%
  filter((BufferCrp == 1 | is.na(BufferCrp)) & (Grs10CrpDate != "/" | !is.na(Grs10CrpGov) | !is.na(Grs10CrpAcr) | Grs35CrpDate != "/" | !is.na(Grs35CrpGov) | !is.na(Grs35CrpAcr))) %>%
  select(all_of(info), Stream, all_of(Q20aGrs)) %>%
  print(n = 150)
# There are 148 such cases

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their CROPLAND
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their CROPLAND but they
# indicated a date established for the vegetation, INDICATED THAT THEY
# RECEIVED GOV'T FUNDS, or indicated an acreage value for GRASS BUFFERS?
farm %>%
  filter((BufferCrp == 1 | is.na(BufferCrp)) & (Grs10CrpDate != "/" | Grs10CrpGov == 2 | !is.na(Grs10CrpAcr) | Grs35CrpDate != "/" | Grs35CrpGov == 2 | !is.na(Grs35CrpAcr))) %>%
  select(all_of(info), Stream, all_of(Q20aGrs)) %>%
  print(n = 120)
# There are 110 such cases
# This set of criteria is a little bit sharper, it does not include
# the farms that ONLY indicated not receiving gov't funds (i.e. it
# filters out those that indicated not receiving gov't funds if that
# was the ONLY part of 20a that they responded to)


# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their CROPLAND
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their CROPLAND but they
# indicated a date established for the vegetation, checked one of
# the bubbles for gov't funds, or indicated an acreage value
# for BUFFERS WITH TREES OR SHRUBS?
farm %>%
  filter((BufferCrp == 1 | is.na(BufferCrp)) & (Tre10CrpDate != "/" | !is.na(Tre10CrpGov) | !is.na(Tre10CrpAcr) | Tre35CrpDate != "/" | !is.na(Tre35CrpGov) | !is.na(Tre35CrpAcr))) %>%
  select(all_of(info), Stream, all_of(Q20aTre)) %>%
  print(n = 80)
# There are 79 such cases

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their CROPLAND
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their CROPLAND but they
# indicated a date established for the vegetation, INDICATED THAT THEY
# RECEIVED GOV'T FUNDS, or indicated an acreage value
# for BUFFERS WITH TREES OR SHRUBS?
farm %>%
  filter((BufferCrp == 1 | is.na(BufferCrp)) & (Tre10CrpDate != "/" | Tre10CrpGov == 2 | !is.na(Tre10CrpAcr) | Tre35CrpDate != "/" | Tre35CrpGov == 2 | !is.na(Tre35CrpAcr))) %>%
  select(all_of(info), Stream, all_of(Q20aTre)) %>%
  print(n = 60)
# There are 57 such cases
# This set of criteria is a little bit sharper, it does not include
# the farms that ONLY indicated not receiving gov't funds (i.e. it
# filters out those that indicated not receiving gov't funds if that
# was the ONLY part of 20a that they responded to)




# Tabulate the variables in Q20a which record the date established,
# the funding source, and the acreage for CROPLAND buffers of various
# widths and vegetation types

# Tabulate date established for cropland buffers
table(farm %>% select(Grs10CrpDate), useNA = "always")
table(farm %>% select(Grs35CrpDate), useNA = "always")
table(farm %>% select(Tre10CrpDate), useNA = "always")
table(farm %>% select(Tre35CrpDate), useNA = "always")

# Tabulate gov't funds for cropland buffers
table(farm %>% select(Grs10CrpGov), useNA = "always")
table(farm %>% select(Grs35CrpGov), useNA = "always")
table(farm %>% select(Tre10CrpGov), useNA = "always")
table(farm %>% select(Tre35CrpGov), useNA = "always")

# Tabulate total buffer acres for cropland
table(farm %>% select(Grs10CrpAcr), useNA = "always")
table(farm %>% select(Grs35CrpAcr), useNA = "always")
table(farm %>% select(Tre10CrpAcr), useNA = "always")
table(farm %>% select(Tre35CrpAcr), useNA = "always")






# How many farms maintain permanent vegetation of an average width
# of at least 10 feet between the stream bank or waterway and any
# of their PASTURE?
table(farm %>% select(BufferPas), useNA = "always")
# 568 maintained permanent vegetation between stream and pasture
# 380 did not
# 881 did not say

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their PASTURE
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their PASTURE but they
# indicated a date established for the vegetation, checked one of
# the bubbles for gov't funds, or indicated an acreage value
# for GRASS BUFFERS?
farm %>%
  filter((BufferPas == 1 | is.na(BufferPas)) & ( !is.na(Grs10PasFenc) | Grs10PasDate != "/" | !is.na(Grs10PasGov) | !is.na(Grs10PasAcr) | !is.na(Grs35PasFenc) | Grs35PasDate != "/" | !is.na(Grs35PasGov) | !is.na(Grs35PasAcr))) %>%
  select(all_of(info), Stream, all_of(Q20b)) %>%
  print(n = 80)
# There are 75 such cases

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their PASTURE
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their PASTURE but they
# indicated a date established for the vegetation, INDICATED RECEIVING
# GOV'T FUNDS, or indicated an acreage value for GRASS BUFFERS?
farm %>%
  filter((BufferPas == 1 | is.na(BufferPas)) & ( !is.na(Grs10PasFenc) | Grs10PasDate != "/" | Grs10PasGov == 2 | !is.na(Grs10PasAcr) | !is.na(Grs35PasFenc) | Grs35PasDate != "/" | Grs35PasGov == 2 | !is.na(Grs35PasAcr))) %>%
  select(all_of(info), Stream, all_of(Q20b)) %>%
  print(n = 70)
# There are 66 such cases
# Maybe eventually narrow this down, filtering out the ones that
# indicated "not used for grazing"


# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their PASTURE
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their PASTURE but they
# indicated a date established for the vegetation, checked one of
# the bubbles for gov't funds, or indicated an acreage value
# for BUFFERS WITH TREES OR SHRUBS?
farm %>%
  filter((BufferPas == 1 | is.na(BufferPas)) & ( !is.na(Tre10PasFenc) | Tre10PasDate != "/" | !is.na(Tre10PasGov) | !is.na(Tre10PasAcr) | !is.na(Tre35PasFenc) | Tre35PasDate != "/" | !is.na(Tre35PasGov) | !is.na(Tre35PasAcr)) ) %>%
  select(all_of(info), Stream, all_of(Q20b)) %>%
  print(n = 40)
# There are 35 such cases

# How many farms responded "no" to maintaining at least 10 feet of
# permanent vegetation between the stream bank and their PASTURE
# or did not say whether they maintained at least 10 feet of permanent
# vegetation between the stream bank and their PASTURE but they
# indicated a date established for the vegetation, INDICATED RECEIVING
# GOV'T FUNDS, or indicated an acreage value for BUFFERS WITH TREES OR
# SHRUBS?
farm %>%
  filter((BufferPas == 1 | is.na(BufferPas)) & ( !is.na(Tre10PasFenc) | Tre10PasDate != "/" | Tre10PasGov == 2 | !is.na(Tre10PasAcr) | !is.na(Tre35PasFenc) | Tre35PasDate != "/" | Tre35PasGov == 2 | !is.na(Tre35PasAcr)) ) %>%
  select(all_of(info), Stream, all_of(Q20b)) %>%
  print(n = 40)
# There are 31 such cases
# Maybe eventually narrow this down, filtering out the ones that
# indicated "not used for grazing"


# Tabulate the variables in Q20b which record whether the
# buffers have fencing, the date established, the funding source,
# and acreage for PASTURE buffers of various widths and vegetation
# types

# Tabulate fencing for pasture fencing
table(farm %>% select(Grs10PasFenc), useNA = "always")
table(farm %>% select(Grs35PasFenc), useNA = "always")
table(farm %>% select(Tre10PasFenc), useNA = "always")
table(farm %>% select(Tre35PasFenc), useNA = "always")

# Tabulate date established for pasture buffers
table(farm %>% select(Grs10PasDate), useNA = "always")
table(farm %>% select(Grs35PasDate), useNA = "always")
table(farm %>% select(Tre10PasDate), useNA = "always")
table(farm %>% select(Tre35PasDate), useNA = "always")

# Tabulate gov't funds for pasture buffers
table(farm %>% select(Grs10PasGov), useNA = "always")
table(farm %>% select(Grs35PasGov), useNA = "always")
table(farm %>% select(Tre10PasGov), useNA = "always")
table(farm %>% select(Tre35PasGov), useNA = "always")

# Tabulate total buffer acres for cropland buffers
table(farm %>% select(Grs10PasAcr), useNA = "always")
table(farm %>% select(Grs35PasAcr), useNA = "always")
table(farm %>% select(Tre10PasAcr), useNA = "always")
table(farm %>% select(Tre35PasAcr), useNA = "always")







#### Q21 Other Conservation Practices ####

Q21 <- c("DairyPre", "DairyPreCows", "TreeUp", "TreeUpAcr", "StreamRst", "StreamRstFt", "WetRst", "WetRstAcr")

# What kind of variables are in Q21?
farm %>% select(all_of(info), all_of(Q21))
# DairyPre is numeric
# DairyPreCows is numeric
# TreeUp is a string
# TreeUpAcr is numeric
# StreamRst is numeric
# StreamRstFt is a string
# WetRst is numeric
# WetRstAcr is a string

# How many farms used dairy precision feeding?
table(farm %>% select(DairyPre), useNA = "always")
# 134 used dairy precision feeding
# 1496 did not
# 199 did not say

# How many farms responded "no" to using dairy precision feeding
# or did not say whether they used dairy precision feeding but
# indicated some number of cows?
farm %>%
  filter((DairyPre == 0 | is.na(DairyPre)) & (!is.na(DairyPreCows) & DairyPreCows > 0)) %>%
  select(all_of(info), all_of(Q21)) %>%
  print(n = 80)
# There are 70 such cases

# It seems like the paper survey automatically read missing
# values for the bubbles in Q21 as a zero and filled bubbles
# as a 1. Thus the only explictly missing values in the Q21
# "bubble questions" are from online surveys

# SIDE NOTE: it looks like ID numbers for online surveys go up
# to at least 269 but there are only 213 observations, this
# must mean that there were roughly 50 online observations that
# were dropped in the data cleaning phase (check on the actual
# number at some point)



# How many farms planted trees on upland agricultural lands (not
# along streams)?
table(farm %>% select(TreeUp), useNA = "always")
# 123 planted trees on upland agricultural lands
# 1524 did not
# 182 did not say

# How many farms responded "no" to planting trees on upland
# agricultural lands or did not say whether they planted trees
# on upland agricultural lands but indicated some positive number
# of acres on which they planted trees?
farm %>%
  filter((TreeUp == 0 | is.na(TreeUp)) & (!is.na(TreeUpAcr) & TreeUpAcr > 0)) %>%
  select(all_of(info), all_of(Q21)) %>%
  print(n = 50)
# There are 38 such cases



# How many farms practiced stream restoration?
table(farm %>% select(StreamRst), useNA = "always")
# 76 planted trees on upland agricultural lands
# 1550 did not
# 203 did not say

# How many farms responded "no" to practicing stream restoration
# or did not say whether they practiced stream restoration but
# indicated restoring streambanks on some positive number of
# linear feet?
farm %>%
  filter((StreamRst == 0 | is.na(StreamRst)) & (!is.na(StreamRstFt) & StreamRstFt != "0.0" & StreamRstFt != "0")) %>%
  select(all_of(info), all_of(Q21)) %>%
  print(n = 20)
# There are 15 such cases



# How many farms practiced wetland restoration?
table(farm %>% select(WetRst), useNA = "always")
# 32 planted trees on upland agricultural lands
# 1592 did not
# 205 did not say

# How many farms responded "no" to practicing wetland restoration
# or did not say whether they practiced wetland restoration but
# indicated restoring some positive number of wetland acres?
farm %>%
  filter((WetRst == 0 | is.na(WetRst)) & (!is.na(WetRstAcr) & WetRstAcr != "0.0" & WetRstAcr != "0")) %>%
  select(all_of(info), all_of(Q21)) %>%
  print(n = 20)
# There are 15 such cases


