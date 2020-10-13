
# Create data set to clean the repaired data, correcting entries that were misread
# from the paper files, dropping observations that are blank or nearly blank, and
# filling in missing "County" variables wherever it's possible to infer county from
# either zip code or a mailing list with SRCID and postal info
farmclean <- farmrepair


#### Make Individual Corrections ####

# Make individual corrections to entries that didn't transfer properly
# from the paper survey into the data set

# The only remaining non-unique SRCID's are 280866, 361086, 380453 so it's ok to
# use SRCID to identify rows in the data as long as you don't combine "which()"
# with these particular SRCID's

# For SRCID 664184,
# Diagnostic: t(farmclean %>% filter(SRCID == "664184")) 
# change OthNutr_text from NA to "LEAF COMPOST"
farmclean[which(farmclean$SRCID == "664184"), "OthNutr_text"] <- "LEAF COMPOST"
# change ConsDate1 from NA to "1/2014"
farmclean[which(farmclean$SRCID == "664184"), "ConsDate1"] <- "1/2014"
# change LowResAcr from 1 to 0.5 (1/2 written in the paper survey)
farmclean[which(farmclean$SRCID == "664184"), "LowResAcr"] <- 0.5
# change CovDate1 from NA to "9/2019"
farmclean[which(farmclean$SRCID == "664184"), "CovDate1"] <- "9/2019"
# change CovTyp2 from NA to "14" (respondent selected the "other" bubble)
farmclean[which(farmclean$SRCID == "664184"), "CovTyp2"] <- 14
# change OthCons from NA to "We do strip farming, The strips are 30'-50' wide
# and have a seven foot permanent grass strip in between"
farmclean[which(farmclean$SRCID == "664184"), "OthCons"] <- "We do strip farming, The strips are 30'-50' wide and have a seven foot permanent grass strip in between"

# For SRCID 58794
# Diagnostic: t(farmclean %>% filter(SRCID == "58794")) 
# Change Tre10CrpAcr from 1 to 0.25 (1/4 written on the paper survey)
farmclean[which(farmclean$SRCID == "58794"), "Tre10CrpAcr"] <- "0.25"

# For SRCID 19026
# Diagnostic: t(farmclean %>% filter(SRCID == "19026")) 
# Change CovAcr1 from 2 to 1.5 (1 1/2 written on the paper survey)
farmclean[which(farmclean$SRCID == "19026"), "CovAcr1"] <- 1.5




#### Remove Observations That Are Blank or Nearly Blank ####

# Check how many online responses had very low "Progress"
# Diagnostic: farmclean %>% count(Progress) %>% print(n = 25)

# Display observations that were are missing "County" or had very little "Progress"
# Diagnostic: farmclean %>% filter(is.na(County) | Progress %in% c("0", "1", "3", "4", "6", "9")) %>% select(ID, SRCID, Progress, status, Name:Acres) %>% print(n = 100)
# There are 93 observations that either have a missing County or "Progress" < 10%

# 2 respondents had "Progress" equal to 0%
# 20 respondents had "Progress" equal to 1%
# 4 respondents had "Progress" equal to 3%
# 2 respondents had "Progeess" equal to 4%

# For the 0% progress group, everything is empty except for metadata: 
# ID, SRCID, StartDate, EndDate, Progress, status, RecordedDate

# ID 262, SRCID 135279, Progress 0%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 262)), useNA = "always")
# Why is the transpose operator t() a necessary part of this command?
# Nothing beyond online survey metadata

# ID 264, SRCID 425711, Progress 0%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 264)), useNA = "always")
# Nothing beyond online survey metadata



# For 18 of the 1% progress group, everything is empty except for metadata:
# ID, SRCID, StartDate, EndDate, Progress, status, RecordedDate

# ID 48, SRCID 519586, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 48)), useNA = "always")
# Nothing beyond online survey metadata

# ID 50, SRCID 552089, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 50)), useNA = "always")
# Nothing beyond online survey metadata

# ID 54, SRCID 449989, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 54)), useNA = "always")
# Nothing beyond online survey metadata

# ID 60, SRCID 900405, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 60)), useNA = "always")
# Nothing beyond online survey metadata

# ID 215, SRCID 744374, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 215)), useNA = "always")
# Nothing beyond online survey metadata

# ID 217, SRCID 22369, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 217)), useNA = "always")
# Nothing beyond online survey metadata

# ID 219, SRCID 670805, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 219)), useNA = "always")
# Nothing beyond online survey metadata

# ID 221, SRCID 871378, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 221)), useNA = "always")
# Nothing beyond online survey metadata

# ID 229, SRCID 366219, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 229)), useNA = "always")
# Nothing beyond online survey metadata

# ID 232, SRCID 626343, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 232)), useNA = "always")
# Nothing beyond online survey metadata

# ID 235, SRCID 29265, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 235)), useNA = "always")
# Nothing beyond online survey metadata

# ID 237, SRCID 687721, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 237)), useNA = "always")
# Nothing beyond online survey metadata

# ID 241, SRCID 554729, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 241)), useNA = "always")
# Nothing beyond online survey metadata

# ID 250, SRCID 272070, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 250)), useNA = "always")
# Nothing beyond online survey metadata

# ID 252, SRCID 177248, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 252)), useNA = "always")
# Nothing beyond online survey metadata

# ID 255, SRCID 577673, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 255)), useNA = "always")
# Nothing beyond online survey metadata

# ID 260, SRCID 123790, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 260)), useNA = "always")
# Nothing beyond online survey metadata

# ID 267, SRCID 613016, Progress 1%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 267)), useNA = "always")
# Nothing beyond online survey metadata


# 2 respondents from the 1% progress group also answered Q2 (Number of acres)
# Keep these in the data set, these might be accurate responses (land but no proper farm)

# ID 245, SRCID 295459, Progress 1%, 313 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 245)), useNA = "always")
# Online survey metadata, Q1 info, Q2 acres, empty beyond Q2 acres

# ID 270, SRCID 231092, Progress 1%, 313 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 270)), useNA = "always")
# Online survey metadata, Q2 acres, empty beyond Q2 acres



# For the 3% progress group, online survey metadata exists but not much else

# ID 62, SRCID 688170, StartDate "3/5/2020 20:56:24", EndDate "3/5/2020 20:57:40", Progress 3%, 313 missing values
# Diagnostic: t(farmclean %>% filter(ID == 62))
# Online survey metadata, Q1 info, Q2 acres, empty beyond Q2 acres

# ID 223, SRCID 449798, Progress 3%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 223)), useNA = "always")
# Nothing beyond online survey metadata

# ID 225, SRCID 50081, StartDate "3/19/2020 8:58:41", EndDate "3/19/2020 13:19:21", Progress 3%, 313 missing values
# Diagnostic: t(farmclean %>% filter(ID == 225))
# Online survey metadata, Q1 info, Q2 acres, empty beyond Q2 acres

# ID 258, SRCID 183862, Progress 3%, 315 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 258)), useNA = "always")
# Online survey metadata, Q1 info



# For the 4% progress group, online survey metadata exists but not much else

# ID 66, SRCID 269610, StartDate "3/6/2020 11:57:35", EndDate "3/6/2020 11:58:24", Progress 4%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 66)), useNA = "always")
# Nothing beyond online survey metadata

# ID 233, SRCID 204738, StartDate "3/21/2020 16:49:17", EndDate "3/21/2020 16:53:08", Progress 4%, 311 missing values
# Diagnostic: t(farmclean %>% filter(ID == 233))
# Online survey metadata, Q1 info, Q2 acres, SoyOwn, no Animals
# Perhaps legitimate to keep this one




# Some online responses say 100% progress but survey is nearly empty aside from
# online survey metadata: ID, SRCID, StartDate, EndDate, Progress, status, RecordedDate

# ID 30, SRCID 373712, StartDate "3/5/2020 7:43:33", EndDate "3/5/2020 7:46:32", Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 30)), useNA = "always")
# Nothing beyond online survey metadata

# ID 94, SRCID "344904", StartDate "3/19/2020 7:35:53", EndDate "3/19/2020 7:41:44", Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 94)), useNA = "always")
# Nothing beyond online survey metadata

# ID 99, SRCID 521036, StartDate "3/19/2020 12:32:46", EndDate "3/19/2020 12:35:09", Progress 100%, 310 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 99)), useNA = "always")
# Online survey metadata, missing Q1 info, no animals, no nutrients, no plans or practices, no streams,
# missing crop info so it's not clear whether this is actually a farm

# ID 100, SRCID 645008, Progress 100%, 319 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 100)), useNA = "always")
# "We are not farmers; Quit sending this stuff to us!!"

# ID 108, SRCID 527541, Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 108)), useNA = "always")
# Nothing beyond online survey metadata

# ID 116, SRCID 91891, Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 116)), useNA = "always")
# Nothing beyond online survey metadata

# ID 172, SRCID 229409, Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 172)), useNA = "always")
# Nothing beyond online survey metadata

# ID 205, SRCID 497864, Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 205)), useNA = "always")
# Nothing beyond online survey metadata

# ID 206, SRCID 806277, Progress 100%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 206)), useNA = "always")
# Nothing beyond online survey metadata




# Consider other online responses with progress < 100% and no county listed

# ID 61, SRCID 506024, StartDate "3/5/2020 19:44:23", EndDate "3/5/2020 19:51:32", Progress 22%, 305 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 61)), useNA = "always")
# Some acres, some hay, some animals, manure application, no plan, no nitrogen practices
# Looks like a legitimate farm

# ID 226, SRCID 352818, Progress 9%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 226)), useNA = "always")
# Nothing beyond online survey metadata

# ID 243, SRCID 807151, Progress 19%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 243)), useNA = "always")
# Nothing beyond online survey metadata

# ID 247, SRCID 97816, Progress 98%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 247)), useNA = "always")
# Nothing beyond online survey metadata

# ID 257, SRCID 265736, Progress 19%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 257)), useNA = "always")
# Nothing beyond online survey metadata

# ID 265, SRCID 677456, Progress 6%, 321 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 265)), useNA = "always")
# Nothing beyond online survey metadata




# Some paper surveys have just SRCID, status, and (possibly scrambled) OthCons

# ID 1155, SRCID 169036, there is a paper survey filled out but no Q1 info
# Diagnostic: table(t(farmclean %>% filter(ID == 1155)), useNA = "always")
# Completely blank except for OthCons (which may be from another survey due to scrambling), 324 missing values

# ID 1292, SRCID 297685, there is a paper survey filled out but no Q1 info
# Diagnostic: table(t(farmclean %>% filter(ID == 1292)), useNA = "always")
# Completely blank except for OthCons (which may be from another survey due to scrambling), 324 missing values




# Collect ID's that are blank except for online survey metadata
justmeta <- c(30, 48, 50, 54, 60, 66, 94, 108, 116, 172, 205, 206, 215,
              217, 219, 221, 223, 226, 229, 232, 235, 237, 241, 243, 247,
              250, 252, 255, 257, 260, 262, 264, 265, 267)
# Collect ID 99 (missing acres, crop info, no animals) and ID 100 ("we are not farmers")
nofarm <- c(99, 100)
# Collect ID's that have Q1 info but nothing beyond that
infothenblank <- c(258)
# Collect ID's that have Q2 acres but nothing beyond that
acresthenblank <- c(62, 225, 245, 270)
# Collect ID's from paper surveys that are blank except for SRCID, status, and (possibly scrambled) OthCons
paperblank <- c(1155, 1292)

# Drop the observations with these ID's from the data set
farmclean <- farmclean %>% filter(!(ID %in% c(justmeta, nofarm, infothenblank, acresthenblank, paperblank)))
# There are now 1830 observations in the data set
# UPDATE: This becomes 1829 observations if one of the Ben Peckman observations is dropped

# Some (online) responses are sparse but keep them in the data set for now
keep <- c(61, 233)



#### Fill in Missing Counties and Standardize County Names ####

# Q1 County

# View the observations that have no county specified
# Diagnostic: farmclean %>% filter(is.na(County)) %>% select(ID, SRCID, status, Name:Acres) %>% print(n = 100)
# There are 48 such observations
nocounty <- (farmclean %>% filter(is.na(County)) %>% select(ID))$ID

# ID 131, SRCID 553586, lists zip code 17019
# Fill in missing county as York per https://www.unitedstateszipcodes.org/17019/
farmclean[which(farmclean$ID == 131), "County"] <- "York"

# ID 228, SRCID 521577, lists City "shrewsbury"
# Fill in missing county as York per https://en.wikipedia.org/wiki/Shrewsbury,_Pennsylvania
farmclean[which(farmclean$ID == 228), "County"] <- "York"

# ID 296, SRCID 12345, lists 298 and 303 KING REN [PEN] RD [Quarryville, PA]
# Fill in missing county as Lancaster per https://en.wikipedia.org/wiki/Quarryville,_Pennsylvania
farmclean[which(farmclean$ID == 296), "County"] <- "Lancaster"

# ID 303, SRCID 13143, lists zip code 17340
# Fill in missing county as Adams per https://www.unitedstateszipcodes.org/17340/
farmclean[which(farmclean$ID == 303), "County"] <- "Adams"

# ID 357, SRCID 18714, lists zip code 17543
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17543/
farmclean[which(farmclean$ID == 357), "County"] <- "Lancaster"

# ID 623, SRCID 46144, lists zip code 17519
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17519/
farmclean[which(farmclean$ID == 623), "County"] <- "Lancaster"

# ID 960, SRCID 85222, lists zip code 17560
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17560/
farmclean[which(farmclean$ID == 960), "County"] <- "Lancaster"

# ID 1091, SRCID 115785, lists zip code 17522
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17522/
farmclean[which(farmclean$ID == 1091), "County"] <- "Lancaster"

# ID 1285, SRCID 284730, lists zip code 17545
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17545/
farmclean[which(farmclean$ID == 1285), "County"] <- "Lancaster"

# ID 1333, SRCID 338928, lists zip code 17557
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17557/
farmclean[which(farmclean$ID == 1333), "County"] <- "Lancaster"

# ID 1474, SRCID 480632, lists zip code 17536
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17536/
farmclean[which(farmclean$ID == 1474), "County"] <- "Lancaster"

# ID 1498, SRCID 501075, lists zip code 17307
# Fill in missing county as Adams per https://www.unitedstateszipcodes.org/17307/
farmclean[which(farmclean$ID == 1498), "County"] <- "Adams"

# ID 1539, SRCID 538454, lists zip code 17557
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17557/
farmclean[which(farmclean$ID == 1539), "County"] <- "Lancaster"

# ID 1586, SRCID 578474, lists zip code 17340
# Fill in missing county as Adams per https://www.unitedstateszipcodes.org/17340/
farmclean[which(farmclean$ID == 1586), "County"] <- "Adams"

# ID 1683, SRCID 663046, lists zip code 17350
# Fill in missing county as Adams per https://www.unitedstateszipcodes.org/17350/
farmclean[which(farmclean$ID == 1683), "County"] <- "Adams"

# ID 1698, SRCID 677670, lists zip code 17563
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17563/
farmclean[which(farmclean$ID == 1698), "County"] <- "Lancaster"

# ID 1757, SRCID 734762, lists zip code 17406
# Fill in missing county as York per https://www.unitedstateszipcodes.org/17406/
farmclean[which(farmclean$ID == 1757), "County"] <- "York"

# ID 1794, SRCID 755431, lists zip code 17509
# Fill in missing county as Lancaster per https://www.unitedstateszipcodes.org/17509/
farmclean[which(farmclean$ID == 1794), "County"] <- "Lancaster"

# ID 1841, SRCID 799990, lists county Adams in the paper survey
# Fill in missing county as Adams per the paper survey
farmclean[which(farmclean$ID == 1841), "County"] <- "Adams"

# ID's corresponding to the observations whose county info was derived from their zip code or other info
infercounty <- c(131, 228, 296, 303, 357, 623, 960, 1091, 1285, 1333, 1474, 1498, 1539, 1586, 1683, 1698, 1757, 1794, 1841)




# Some paper survey respondents left Q1 info blank, i.e. there is no county listed
# But I can find the county from the list of SRCIDs and mailing info (on Box)

# ID 330, SRCID 15481, 216 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 330)), useNA = "always")
# Zip is 17522 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 330), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 330), "Zip"] <- "17522"

# ID 344, SRCID 17091, 247 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 344)), useNA = "always")
# Zip is 17527 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 344), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 344), "Zip"] <- "17527"

# ID 449, SRCID 29129, 233 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 449)), useNA = "always")
# Zip is 17350 and County is Adams per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 449), "County"] <- "Adams"
farmclean[which(farmclean$ID == 449), "Zip"] <- "17350"

# ID 529, SRCID 36554, 218 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 529)), useNA = "always")
# Zip is 17566 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 529), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 529), "Zip"] <- "17566"

# ID 716, SRCID 58243, 208 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 716)), useNA = "always")
# Zip is 17545 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 716), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 716), "Zip"] <- "17545"

# ID 727, SRCID 59288, Acres, Plan, basically nothing else, 261 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 727)), useNA = "always")
# Zip is 17527 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 727), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 727), "Zip"] <- "17527"

# ID 741, SRCID 60496, 215 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 741)), useNA = "always")
# Zip is 17562 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 741), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 741), "Zip"] <- "17562"

# ID 799, SRCID 67306, 217 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 799)), useNA = "always")
# Zip is 17257 and County is Franklin per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 799), "County"] <- "Franklin"
farmclean[which(farmclean$ID == 799), "Zip"] <- "17257"

# ID 873, SRCID 74836, 195 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 873)), useNA = "always")
# Zip is 17602 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 873), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 873), "Zip"] <- "17602"

# ID 892, SRCID 76972, 220 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 892)), useNA = "always")
# Zip is 17302 and County is York per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
# (But SRCID 76972 is listed as "web")
farmclean[which(farmclean$ID == 892), "County"] <- "York"
farmclean[which(farmclean$ID == 892), "Zip"] <- "17302"

# ID 932, SRCID 81693, 237 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 932)), useNA = "always")
# Zip is 17324 and County is Adams per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
# (But SRCID 81693 is listed as "web")
farmclean[which(farmclean$ID == 932), "County"] <- "Adams"
farmclean[which(farmclean$ID == 932), "Zip"] <- "17324"

# ID 936, SRCID 81978, 212 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 936)), useNA = "always")
# Zip is 17566 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 936), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 936), "Zip"] <- "17566"

# ID 996, SRCID 88792, 214 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 996)), useNA = "always")
# Zip is 17543 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 996), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 996), "Zip"] <- "17543"

# ID 1008, SRCID 89898, 210 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1008)), useNA = "always")
# Zip is 17552 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 1008), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 1008), "Zip"] <- "17552"

# ID 1015, SRCID 91496, 228 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1015)), useNA = "always")
# Zip is 17557 and County is Lancaster per "farm data zip and county 5 digit ID.xlsx" from Farmer Mailing List folder on Box
farmclean[which(farmclean$ID == 1015), "County"] <- "Lancaster"
farmclean[which(farmclean$ID == 1015), "Zip"] <- "17557"

# ID's for the missing counties filled in based on 5 digit SRCID and mailing info
mailcounty <- c(330, 344, 449, 529, 716, 727, 741, 799, 873, 892, 932, 936, 996, 1008, 1015)




# Some paper survey respondents left Q1 info blank, i.e. there is no county listed

# I'm able to determine the zip and county for the SRCIDs that are included in the
# mailing list spreadsheet "farm data zip and county 5 digit ID.xlsx" from the Farmer
# Mailing List folder on Box

# But some observations that have a 6 digit SRCID don't appear on the mailing list
# spreadsheet, so I can't infer county that way. These are listed below:

# ID 1147, SRCID 155073, 192 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1147)), useNA = "always")

# ID 1156, SRCID 171781, 200 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1156)), useNA = "always")

# ID 1162, SRCID 176334, 235 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1162)), useNA = "always")

# ID 1186, SRCID 196890, 223 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1186)), useNA = "always")

# ID 1211, SRCID 224005, 233 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1211)), useNA = "always")

# ID 1224, SRCID 234980, 209 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1224)), useNA = "always")

# ID 1314, SRCID 314103, 232 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1314)), useNA = "always")

# ID 1664, SRCID 644481, 237 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1664)), useNA = "always")

# ID 1685, SRCID 663778, 206 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1685)), useNA = "always")

# ID 1778, SRCID 743306, 190 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1778)), useNA = "always")

# ID 1795, SRCID 757560, 238 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1795)), useNA = "always")

# ID 1907, SRCID 858875, 221 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1907)), useNA = "always")

# ID 1910, SRCID 860534, 159 missing values
# Diagnostic: table(t(farmclean %>% filter(ID == 1910)), useNA = "always")

# ID's corresponding to remaining missing counties
missingcounty <- intersect(setdiff(nocounty, infercounty), setdiff(nocounty, mailcounty))
# There are still 14 observations with unknown county

# Look at the original list of observations that have missing counties
# with all but 14 of the entries filled in
# Diagnostic: farmclean %>% filter(ID %in% nocounty) %>% select(ID, SRCID, status, Name:Acres) %>% print(n = 50)




# Make individual corrections to standardize County entries

# Diagnostic: farmclean %>% count(County) %>% print(n = 100)

farmclean[which(farmclean$County == ":ancaster"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "ADAM"), "County"] <- "Adams"
farmclean[which(farmclean$County == "adams"), "County"] <- "Adams"
farmclean[which(farmclean$County == "ADAMS"), "County"] <- "Adams"
farmclean[which(farmclean$County == "ADAMS COUNTY"), "County"] <- "Adams"
farmclean[which(farmclean$County == "ADDAMS"), "County"] <- "Adams"
farmclean[which(farmclean$County == "chester0"), "County"] <- "Chester"
farmclean[which(farmclean$County == "CUMBERLAND"), "County"] <- "Cumberland"
farmclean[which(farmclean$County == "DAUPHIN"), "County"] <- "Dauphin"
farmclean[which(farmclean$County == "FRAKLIN"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANIKLIN"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANKIN"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "franklin"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANKLIN"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANKLIN AND CUMBERLAND"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANKLIN COUNTY"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "Franklin220"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "FRANNKLIN"), "County"] <- "Franklin"
farmclean[which(farmclean$County == "Frederick"), "County"] <- "Frederick, MD"
farmclean[which(farmclean$County == "HUNTINGDON"), "County"] <- "Huntingdon"
farmclean[which(farmclean$County == "I"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "IANCASTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "Junianta"), "County"] <- "Juniata"
farmclean[which(farmclean$County == "JUNIATA"), "County"] <- "Juniata"
farmclean[which(farmclean$County == "LACASTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LACNCASTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LAMCASTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LAN"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANC"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANC CO"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCA"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "lancasrter"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "lancaster"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTER AND CHESTER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTER CO"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTER COUNTY"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "Lancaster18"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTERE"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTERER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTERQ"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCASTERR"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCATER"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "LANCS"), "County"] <- "Lancaster"
farmclean[which(farmclean$County == "NORTHUMBERLAND"), "County"] <- "Northumberland"

# ID 42 Zip is 17327, change county from "PA" to York per https://www.unitedstateszipcodes.org/17327/
farmclean[which(farmclean$ID == 42), "County"] <- "York"

# ID 137 Zip is 17252, change county from "PA" to Franklin per https://www.unitedstateszipcodes.org/17252/ (Ben Peckman)
farmclean[which(farmclean$ID == 137), "County"] <- "Franklin"

# ID 139 Zip is 17252, change county from "PA" to Franklin per https://www.unitedstateszipcodes.org/17252/ (Ben Peckman)
farmclean[which(farmclean$ID == 139), "County"] <- "Franklin"

# ID 161 Zip is 17325, change county from "PA" to Adams per https://www.unitedstateszipcodes.org/17325/
farmclean[which(farmclean$ID == 161), "County"] <- "Adams"

# ID 512 Zip is 17315, change county from "PA" to York per https://www.unitedstateszipcodes.org/17315/
farmclean[which(farmclean$ID == 512), "County"] <- "York"

# ID 1741 Zip is 21722, change county from "PA" to Washington, MD per https://www.unitedstateszipcodes.org/21722/
farmclean[which(farmclean$ID == 1741), "County"] <- "Washington, MD"

# SRCID 807505 respondent lists "FULTON" under municipality/township, which is located
# in southern Lancaster Co. (cursive "Lang" was translated as "PEMA")
farmclean[which(farmclean$County == "PEMA"), "County"] <- "Lancaster"

# ID 44 Zip is 17516, change county from "Pennsylvania" to Lancaster per https://www.unitedstateszipcodes.org/17516/
farmclean[which(farmclean$ID == 44), "County"] <- "Lancaster"

# ID 188 Zip is 17314, change county from "Pennsylvania" to York per https://www.unitedstateszipcodes.org/17314/
farmclean[which(farmclean$ID == 188), "County"] <- "York"

# ID 141 Zip is 17252, change county from "PENNSYLVANIA" to Franklin per https://www.unitedstateszipcodes.org/17252/
farmclean[which(farmclean$ID == 141), "County"] <- "Franklin"

farmclean[which(farmclean$County == "SNYDER"), "County"] <- "Snyder"
farmclean[which(farmclean$County == "UPPER ADAMS"), "County"] <- "Adams"

# ID 25 Zip is 17545, change county from "USA" to Lancaster per https://www.unitedstateszipcodes.org/17545/
farmclean[which(farmclean$ID == 25), "County"] <- "Lancaster"

farmclean[which(farmclean$County == "WASHINGTON"), "County"] <- "Washington"
farmclean[which(farmclean$County == "york"), "County"] <- "York"
farmclean[which(farmclean$County == "YORK"), "County"] <- "York"

# Diagnostic: farmclean %>% count(County) %>% print(n = 100)
