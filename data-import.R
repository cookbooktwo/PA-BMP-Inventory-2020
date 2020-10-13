# M 8/24

#### Setup ####

# Set working directory for the project
setwd("E:/R-projects/PA Farm Survey Project/Summer 2020")

# Load the tidyverse package
library(tidyverse)



#### Import Data ####

# Before importing to R, convert the data set to .csv format
# Convert "Farm Inventory 2020_Merged 8 7 2020.sav" to "~.csv" (outside R)

# Import .csv file as a tibble and call the data set "farm"
farm <- read_csv("Farm Inventory 2020_Merged 8 7 2020.csv")




#### Rename Variables ####

# Rename variables in the data set with more descriptive names

# Rename variables in the Q1 (respondent info)
farm <- farm %>% rename(Name = Qinfo_1, #chr
                        Address = Qinfo_2, #chr
                        City = Qinfo_3, #chr
                        State = Qinfo_4, #chr
                        Zip = Qinfo_5, #chr
                        Town = Qinfo_6, #chr
                        County = Qinfo_7) #chr
# T 8/25
# Rename variables in Q2 through Q4 (farm info)
farm <- farm %>% rename(Acres = Q2, #dbl
                        CgrnOwn = Q3_1_1, #dbl
                        CgrnRnt = Q3_1_2, #dbl
                        CgrnDbl = Q3_1_3, #chr
                        CsilOwn = Q3_2_1, #dbl
                        CsilRnt = Q3_2_2, #dbl
                        CsilDbl = Q3_2_3, #dbl
                        SoyOwn = Q3_3_1, #dbl
                        SoyRnt = Q3_3_2, #dbl
                        SoyDbl = Q3_3_3, #dbl
                        WhtOwn = Q3_4_1, #dbl
                        WhtRnt = Q3_4_2, #dbl
                        WhtDbl = Q3_4_3, #dbl
                        RyeOwn = Q3_5_1, #dbl
                        RyeRnt = Q3_5_2, #dbl
                        RyeDbl = Q3_5_3, #dbl
                        BarlOwn = Q3_6_1, #dbl
                        BarlRnt = Q3_6_2, #dbl
                        BarlDbl = Q3_6_3, #chr
                        AlfOwn = Q3_7_1, #dbl 
                        AlfRnt = Q3_7_2, #dbl
                        AlfDbl = Q3_7_3, #dbl
                        HayOwn = Q3_8_1, #dbl
                        HayRnt = Q3_8_2, #dbl
                        HayDbl = Q3_8_3, #dbl
                        OthcropOwn = Q3_9_1, #dbl
                        OthcropRnt = Q3_9_2, #chr
                        OthcropDbl = Q3_9_3, #dbl
                        Othcrop_text = Q3_9_TEXT, #chr
                        Animals = Q4, #chr
                        Broil = Q4a_1,#dbl
                        Lay = Q4a_2, #dbl
                        Turk = Q4a_3, #chr
                        Duck = Q4a_4, #dbl
                        Npig = Q4a_5, #dbl
                        Fpig = Q4a_6, #chr
                        Sow = Q4a_7, #chr
                        Boar = Q4a_8, #chr
                        Veal = Q4a_9, #dbl
                        Heify = Q4a_10, #dbl
                        Heifo = Q4a_11, #chr
                        Cow = Q4a_12, #dbl
                        Catl = Q4a_13, #dbl
                        Hors = Q4a_14, #dbl
                        Othanm1_text = Q4a_15, #chr
                        Othanm1 = Q4a_15other, #chr
                        Othanm2_text = Q4a_16, #chr
                        Othanm2 = Q4a_16Nother) #dbl

# Rename variables in Q5 through Q7 (nutrient application and management)
farm <- farm %>% rename(Nutr = Q5,
                        Mnur = Q5a_1,
                        Cfert = Q5a_2,
                        Bios = Q5a_3,
                        FPR = Q5a_4,
                        Mush = Q5a_5,
                        OthNutr = Q5a_6,
                        OthNutr_text = Q5a_6_TEXT,
                        Inj = Q6,
                        LowFast = Q6_1_1_1,
                        LowSlow = Q6_1_1_2,
                        HiFast = Q6_1_2_1,
                        HiSlow = Q6_1_2_2,
                        Immed = Q6_2,
                        Plan = Q7,
                        PlanTyp = Q7a,
                        OthPlan_text = Q7a_6_TEXT,
                        PlanMo = Q7b_1,
                        PlanYr = Q7b_2,
                        PlanAcr = Q7c_1,
                        PlanGov = Q7d,
                        PlanNP = Q7e,
                        PlanFolw = Q7f,
                        PlanRec = Q7g)

# Rename variables in Q8 through Q10 (nitrogen application)
farm <- farm %>% rename(NitrLow = Q8_1_1,
                        NitrLowAcr = Q8_1_2_1,
                        NitrSpl8 = Q8_2_1,
                        NitrSpl8Acr = Q8_2_2_1,
                        NitrVar = Q8_3_1,
                        NitrVarAcr = Q8_3_2_1,
                        NitrInj = Q9_1_1,
                        NitrInjAcr = Q9_1_2_1,
                        NitrSet = Q9_2_1,
                        NitrSetAcr = Q9_2_2_1,
                        NitrSpl10 = Q10_1_1,
                        NitrSpl10Acr = Q10_1_2_1,
                        NitrMult = Q10_2_1,
                        NitrMultAcr = Q10_2_2_1)

# Rename variables in Q11 through Q13 (phosphorus application)
farm <- farm %>% rename(PhosLow = Q11_1_1,
                        PhosLowAcr = Q11_1_2_1,
                        PhosRmvl = Q11_2_1,
                        PhosRmvlAcr = Q11_2_2_1,
                        PhosVar = Q11_3_1,
                        PhosVarAcr = Q11_3_2_1,
                        PhosInj = Q12_1_1,
                        PhosInjAcr = Q12_1_2_1,
                        PhosSet = Q12_2_1,
                        PhosSetAcr = Q12_2_2_1,
                        PhosRisk = Q13_1_1,
                        PhosRiskAcr = Q13_1_2_1,
                        PhosIdx = Q13_2_1,
                        PhosIdxAcr = Q13_2_2_1,
                        PhosSpl = Q13_3_1,
                        PhosSplAcr = Q13_3_2_1)
# W 8/26
# Rename variables in Q14 through Q16 (animal conservation practices: manure storage, barnyards, pasture)
farm <- farm %>% rename(Sto = Q14,
                        StoLiv1 = Q14a_1_1,
                        StoTyp1 = Q14a_1_2,
                        StoDate1 = Q14a_1_3_1,
                        StoMo1 = Q14a_1_4_1,
                        StoGov1 = Q14a1_1,
                        StoEngr1 = Q14a1_2,
                        StoCtrl1 = Q14a1_3,
                        Sto2 = QMS2,
                        StoLiv2 = Q14b_1_1,
                        StoTyp2 = Q14b_1_2,
                        StoDate2 = Q14b_1_3_1,
                        StoMo2 = Q14b_1_4_1,
                        StoGov2 = Q14b1_1,
                        StoEngr2 = Q14b1_2,
                        StoCtrl2 = Q14b1_3,
                        Sto3 = QMS3,
                        StoLiv3 = Q14c_1_1,
                        StoTyp3 = Q14c_1_2,
                        StoDate3 = Q14c_1_3_1,
                        StoMo3 = Q14c_1_4_1,
                        StoGov3 = Q14c1_1,
                        StoEngr3 = Q14c1_2,
                        StoCtrl3 = Q14c1_3,
                        Sto4 = QMS4,
                        StoLiv4 = Q14d_1_1,
                        StoTyp4 = Q14d_1_2,
                        StoDate4 = Q14d_1_3_1,
                        StoMo4 = Q14d_1_4_1,
                        StoGov4 = Q14d1_1,
                        StoEngr4 = Q14d1_2,
                        StoCtrl4 = Q14d1_3,
                        Sto5 = QMS5,
                        StoLiv5 = Q14e_1_1,
                        StoTyp5 = Q14e_2_1,
                        StoDate5 = Q14e_3_1_1,
                        StoMo5 = Q14e_4_1_1,
                        StoGov5 = Q14e1_1,
                        StoEngr5 = Q14e1_2,
                        StoCtrl5 = Q14e1_3,
                        Barn = Q15,
                        BarnCtrl = Q15a,
                        BarnDvrt = Q15b_1_1,
                        BarnDvrtDate = Q15b_1_2_1,
                        BarnDvrtGov = Q15b_1_3,
                        BarnConc = Q15b_2_1,
                        BarnConcDate = Q15b_2_2_1,
                        BarnConcGov = Q15b_2_3,
                        BarnCatch = Q15b_3_1,
                        BarnCatchDate = Q15b_3_2_1,
                        BarnCatchGov = Q15b_3_3,
                        Graze = Q16,
                        GrazePlan = Q16a,
                        GrazePlanDate = Q16b,
                        GrazePlanGov = Q16c,
                        GrazePlanImpl = Q16d,
                        GrazePlanAcr = Q16e)

# Rename variables in Q17 through Q19 (crop conservation practices: conservation plans, no till, cover crops)
farm <- farm %>% rename(ConsPlan = Q17,
                        ConsTyp1 = Q17_1_1,
                        ConsDate1 = Q17_1_2_1,
                        ConsRowAcr1 = Q17_1_3_1,
                        ConsHayAcr1 = Q17_1_3_2,
                        ConsPasAcr1 = Q17_1_3_3,
                        ConsGov1 = Q17_1_4,
                        ConsSchd1 = Q17_1_5,
                        ConsTyp2 = Q17_2_1,
                        ConsDate2 = Q17_2_2_1,
                        ConsRowAcr2 = Q17_2_3_1,
                        ConsHayAcr2 = Q17_2_3_2,
                        ConsPasAcr2 = Q17_2_3_3,
                        ConsGov2 = Q17_2_4,
                        ConsSchd2 = Q17_2_5,
                        ConsTyp3 = Q17_3_1,
                        ConsDate3 = Q17_3_2_1,
                        ConsRowAcr3 = Q17_3_3_1,
                        ConsHayAcr3 = Q17_3_3_2,
                        ConsPasAcr3 = Q17_3_3_3,
                        ConsGov3 = Q17_3_4,
                        ConsSchd3 = Q17_3_5,
                        ConsTyp4 = Q17_4_1,
                        ConsDate4 = Q17_4_2_1,
                        ConsRowAcr4 = Q17_4_3_1,
                        ConsHayAcr4 = Q17_4_3_2,
                        ConsPasAcr4 = Q17_4_3_3,
                        ConsGov4 = Q17_4_4,
                        ConsSchd4 = Q17_4_5,
                        ConsTyp5 = Q17_5_1,
                        ConsDate5 = Q17_5_2_1,
                        ConsRowAcr5 = Q17_5_3_1,
                        ConsHayAcr5 = Q17_5_3_2,
                        ConsPasAcr5 = Q17_5_3_3,
                        ConsGov5 = Q17_5_4,
                        ConsSchd5 = Q17_5_5,
                        Notill = Q18,
                        HiResAcr = Q18a_1,
                        MedResAcr = Q18a_2,
                        LowResAcr = Q18a_3,
                        Cov = Q19,
                        CovTyp1 = Q19a1,
                        OthCov1_text = Q19a1_14_TEXT,
                        Drill1 = Q19a2_1,
                        Broadw1 = Q19a2_2,
                        Broadwo1 = Q19a2_3,
                        Aerial1 = Q19a2_4,
                        OthMthd1 = Q19a2_5,
                        OthMthd1_text = Q19a2_5_TEXT,
                        CovDate1 = Q19a3,
                        CovAcr1 = Q19a4,
                        FalMnur1 = Q19a5,
                        SprNutr1 = Q19a6,
                        SprHarv1 = Q19a7,
                        SprHarvAcr1 = Q19a7_2_TEXT,
                        Cov2 = cc2,
                        CovTyp2 = Q19b1,
                        OthCov2_text = Q19b1_14_TEXT,
                        Drill2 = Q19b2_1,
                        Broadw2 = Q19b2_2,
                        Broadwo2 = Q19b2_3,
                        Aerial2 = Q19b2_4,
                        OthMthd2 = Q19b2_5,
                        OthMthd2_text = Q19b2_5_TEXT,
                        CovDate2 = Q19b3,
                        CovAcr2 = Q19b4,
                        FalMnur2 = Q19b5,
                        SprNutr2 = Q19b6,
                        SprHarv2 = Q19b7,
                        SprHarvAcr2 = Q19b7_2_TEXT,
                        Cov3 = cc3,
                        CovTyp3 = Q19c1,
                        OthCov3_text = Q19c1_14_TEXT,
                        Drill3 = Q19c2_1,
                        Broadw3 = Q19c2_2,
                        Broadwo3 = Q19c2_3,
                        Aerial3 = Q19c2_4,
                        OthMthd3 = Q19c2_5,
                        OthMthd3_text = Q19c2_5_TEXT,
                        CovDate3 = Q19c3,
                        CovAcr3 = Q19c4,
                        FalMnur3 = Q19c5,
                        SprNutr3 = Q19c6,
                        SprHarv3 = Q19c7,
                        SprHarvAcr3 = Q19c7_2_TEXT,
                        Cov4 = cc4,
                        CovTyp4 = Q19d1,
                        OthCov4_text = Q19d1_14_TEXT,
                        Drill4 = Q19d2_1,
                        Broadw4 = Q19d2_2,
                        Broadwo4 = Q19d2_3,
                        Aerial4 = Q19d2_4,
                        OthMthd4 = Q19d2_5,
                        OthMthd4_text = Q19d2_5_TEXT,
                        CovDate4 = Q19d3,
                        CovAcr4 = Q19d4,
                        FalMnur4 = Q19d5,
                        SprNutr4 = Q19d6,
                        SprHarv4 = Q19d7,
                        SprHarvAcr4 = Q19d7_2_TEXT,
                        Cov5 = cc5,
                        CovTyp5 = Q19e1,
                        OthCov5_text = Q19e1_14_TEXT,
                        Drill5 = Q19e2_1,
                        Broadw5 = Q19e2_2,
                        Broadwo5 = Q19e2_3,
                        Aerial5 = Q19e2_4,
                        OthMthd5 = Q19e2_5,
                        OthMthd5_text = Q19e2_5_TEXT,
                        CovDate5 = Q19e3,
                        CovAcr5 = Q19e4,
                        FalMnur5 = Q19e5,
                        SprNutr5 = Q19e6,
                        SprHarv5 = Q19e7,
                        SprHarvAcr5 = Q19e7_2_TEXT)

# Rename variables from Q20 (streams and buffers)
farm <- farm %>% rename(Stream = Q20,
                        BufferCrp = Q20a,
                        Grs10CrpDate = Q20ay_1_1_1,
                        Grs10CrpGov = Q20ay_1_2,
                        Grs10CrpAcr = Q20ay_1_3_1,
                        Grs35CrpDate = Q20ay_2_1_1,
                        Grs35CrpGov = Q20ay_2_2,
                        Grs35CrpAcr = Q20ay_2_3_1,
                        Tre10CrpDate = Q20ay_3_1_1,
                        Tre10CrpGov = Q20ay_3_2,
                        Tre10CrpAcr = Q20ay_3_3_1,
                        Tre35CrpDate = Q20ay_4_1_1,
                        Tre35CrpGov = Q20ay_4_2,
                        Tre35CrpAcr = Q20ay_4_3_1,
                        BufferPas = Q20b,
                        Grs10PasFenc = Q20by_1_1,
                        Grs10PasDate = Q20by_1_2_1,
                        Grs10PasGov = Q20by_1_3,
                        Grs10PasAcr = Q20by_1_4_1,
                        Grs35PasFenc = Q20by_2_1,
                        Grs35PasDate = Q20by_2_2_1,
                        Grs35PasGov = Q20by_2_3,
                        Grs35PasAcr = Q20by_2_4_1,
                        Tre10PasFenc = Q20by_3_1,
                        Tre10PasDate = Q20by_3_2_1,
                        Tre10PasGov = Q20by_3_3,
                        Tre10PasAcr = Q20by_3_4_1,
                        Tre35PasFenc = Q20by_4_1,
                        Tre35PasDate = Q20by_4_2_1,
                        Tre35PasGov = Q20by_4_3,
                        Tre35PasAcr = Q20by_4_4_1)

# Rename variables from Q21 and Q22 (other conservation practices)
farm <- farm %>% rename(DairyPre = Q21_1,
                        DairyPreCows = Q21_1_TEXT,
                        TreeUp = Q21_2,
                        TreeUpAcr = Q21_2_TEXT,
                        StreamRst = Q21_3,
                        StreamRstFt = Q21_3_TEXT,
                        WetRst = Q21_4,
                        WetRstAcr = Q21_4_TEXT,
                        OthCons = Q22)




#### Repair the Data Set ####

# Write code to repair these broken observations

# THE FOLLOWING CODE USES PARTICULAR ROW NUMBERS THAT CORRESPOND TO HOW THE
# DATA SET WAS ORIGINALLY LOADED, IT WILL NOT WORK AS INTENDED IF VARIABLES
# ARE ADDED OR DROPPED FROM THE DATA SET, OR IF ROWS ARE REARRANGED

# Create a parameter representing the number of columns in "farm"
K <- dim(farm)[2]

# Create a unique ID number for each row of the data set
farm <- farm %>% rowid_to_column("ID")
# Put the ID variable at the very end so not to mess up the column indexes
farm <- farm %>% select(-ID, ID)

# Create a vector of indexes for the rows that had parsing problems
prob <- unique((problems(farm))$row)

# Create a vector listing any columns that represent "break points" in the data set, where
# values after that column were broken off and placed in the next row  
breakpoint <- c(78, 7, 69, 251, 42)

# Create a vector listing the "actual number of columns" for each of the
# unique row indexes that failed to parse
ncols <- character(96)
i <- 1
while (i <= 96) {
  if (!duplicated(problems(farm)$row)[i]) {
    ncols[i] <- str_sub(problems(farm)$actual[i], 1, 3)
  }
  else {
    ncols[i] <- NA
  }
  i <- i + 1
}
# Drop the duplicate row indexes
ncols <- ncols[!is.na(ncols)]
# It is very important to drop the missing values BEFORE extracting the numbers (next step)

# Extract the number from the string "n columns" in problems(farm)
ncols <- parse_number(ncols)
# This will convert the text string "LEA" (short for "LEAF COMPOST") to NA

# Convert the remaining NA to K (# of columns in the data set) minus ncols (actual number of columns in a particular row) plus 1
for (i in 1:length(ncols)) {if (is.na(ncols[i])) {ncols[i] <- K - ncols[i-1] + 1}}
# This will allow the if statements in the upcoming loops to pick up on the fact that
# the row corresponding to the farm for which only 42 columns were found needs to be repaired

# Create a table listing the unique row indexes that had parsing problems,
# next to a list of the "actual number of columns" for each of the unique row
# indexes that failed to parse as per problems(farm),
# next to a list of SRCID associated with each row,
# next to a list of the variables corresponding to columns 7, 42, 69, 78,
# and 251 of the data set (which are the columns where the breakpoints occurred),
# next to a list of the rows altered in the "2 row" repair loop,
# next to a list of the rows altered in the "3 row" repair loop,
# next to a list of the rows altered in the "4 row" repair loop.
v <- tibble(row = prob,
            ncols = ncols,
            ID = farm$ID[prob],
            SRCID = farm$SRCID[prob],
            Name = farm$Name[prob],
            OthCrop_text = farm$OthCrop_text[prob],
            OthNutr_text = farm$OthNutr_text[prob],
            OthPlan_text = farm$OthPlan_text[prob],
            OthMthd3_text = farm$OthMthd3_text[prob],
            repair2 = numeric(length(prob)),
            repair3 = numeric(length(prob)),
            repair4 = numeric(length(prob)),)
# repair2, repair3, and repair4 will be populated in the three "repair" loops below


# View row number, breakpoint, SRCID, and breakpoint columns all side by side
# Diagnostic: v %>% filter(ncols %in% breakpoint) %>% print(n = 30)

# Create a copy of the farm data set before performing repairs
farmrepair <- farm

# Repair the variables for observations that are spread over 2 rows
for (h in 1:length(breakpoint)) {
  for (i in 1:length(prob)) {
    
    if ((v$ncols[i] == breakpoint[h]) & (v$ncols[i+1] == K-breakpoint[h]+1)) {
      v[i, "repair2"] <- v$row[i]
      farmrepair[v$row[i], breakpoint[h]] <- str_c(farmrepair[v$row[i], breakpoint[h]], " ", farmrepair[v$row[i]+1, "SRCID"])
      
      for (j in 1:(K-breakpoint[h])) {
        if (is.numeric(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.numeric(farmrepair[[v$row[i]+1, j+1]])
        }
        else if (is.character(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.character(farmrepair[[v$row[i]+1, j+1]])
        }
      }
    }
  }
}

# Repair the variables for observations that are spread over 3 rows
for (h in 1:length(breakpoint)) {
  for (i in 1:length(prob)) {
    
    if ((v$ncols[i] == breakpoint[h]) & (v$ncols[i+1] == 1) & (v$ncols[i+2] == K-breakpoint[h]+1)) {
      v[i, "repair3"] <- v$row[i]
      farmrepair[v$row[i], breakpoint[h]] <- str_c(farmrepair[v$row[i], breakpoint[h]], " ", farmrepair[v$row[i]+1, "SRCID"], " ", farmrepair[v$row[i]+2, "SRCID"])
      
      for (j in 1:(K-breakpoint[h])) {
        if (is.numeric(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.numeric(farmrepair[[v$row[i]+2, j+1]])
        }
        else if (is.character(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.character(farmrepair[[v$row[i]+2, j+1]])
        }
      }
    }
  }
}

# Repair the variables for observations that are spread over 4 rows
for (h in 1:length(breakpoint)) {
  for (i in 1:length(prob)) {
    
    if ((v$ncols[i] == breakpoint[h]) & (v$ncols[i+1] == 1) & (v$ncols[i+2] == 1) & (v$ncols[i+3] == K-breakpoint[h]+1)) {
      v[i, "repair4"] <- v$row[i]
      farmrepair[v$row[i], breakpoint[h]] <- str_c(farmrepair[v$row[i], breakpoint[h]], " ", farmrepair[v$row[i]+1, "SRCID"], " ", farmrepair[v$row[i]+2, "SRCID"], " ", farmrepair[v$row[i]+3, "SRCID"])
      
      for (j in 1:(K-breakpoint[h])) {
        if (is.numeric(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.numeric(farmrepair[[v$row[i]+3, j+1]])
        }
        else if (is.character(farmrepair[[v$row[i], j+breakpoint[h]]])) {
          farmrepair[[v$row[i], j+breakpoint[h]]] <- as.character(farmrepair[[v$row[i]+3, j+1]])
        }
      }
    }
  }
}

# The repair loop only gives new assignments to "farmrepair" for rows equal to v$row[i],
# so the columns "repair2", "repair3", and "repair4" record all v$row[i] for which the 
# "if statement" is true for a particular i in 1:length(prob).

# The rows altered in the "2 row" repair loop are listed in "repair2", the rows
# altered in the "3 row" repair loop are listed in "repair 3", and the rows altered
# in the "4 row" repair loop are listed in "repair 4".

# So the rows listed in columns "repair2", "repair3", and "repair4" of table "v" are
# the only rows of the data set that were altered.


# View a summary of the repaired rows, column breakpoints, SRCID, and whether the
# row was repaired in the "2 row", "3 row", or "4 row" repair loop
# Diagnostics:
# v %>% select(row, ncols, ID, SRCID, repair2, repair3, repair4) %>% filter(ncols %in% breakpoint) %>% print(n = 80)
# Create tables to inspect the broken columns side by side, next to the repaired column
# Diagnostics:
# tibble(segment1 = t(farm[which(farm$SRCID == "10595"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "10595")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "10595"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "12993"), 1:K]), segment2 = c(rep(" ", 6), t(farm[which(farm$SRCID == "12993")+1, 1:(K-6)])), repaired = t(farmrepair[which(farmrepair$SRCID == "12993"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "16213"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "16213")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "16213"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "29380"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "29380")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "29380"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "33018"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "33018")+2, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "33018"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "33293"), 1:K]), segment2 = c(rep(" ", 68), t(farm[which(farm$SRCID == "33293")+1, 1:(K-68)])), repaired = t(farmrepair[which(farmrepair$SRCID == "33293"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "36473"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "36473")+3, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "36473"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "37032"), 1:K]), segment2 = c(rep(" ", 250), t(farm[which(farm$SRCID == "37032")+1, 1:(K-250)])), repaired = t(farmrepair[which(farmrepair$SRCID == "37032"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "50337"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "50337")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "50337"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "58794"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "58794")+3, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "58794"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "63717"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "63717")+2, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "63717"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "72438"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "72438")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "72438"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "74628"), 1:K]), segment2 = c(rep(" ", 68), t(farm[which(farm$SRCID == "74628")+1, 1:(K-68)])), repaired = t(farmrepair[which(farmrepair$SRCID == "74628"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$ID == 925), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$ID == 925)+1, 1:(K-77)])), repaired = t(farmrepair[which(farm$ID == 925), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$ID == 927), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$ID == 927)+1, 1:(K-77)])), repaired = t(farmrepair[which(farm$ID == 927), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "80951"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "80951")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "80951"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "87366"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "87366")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "87366"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "139605"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "139605")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "139605"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "238701"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "238701")+3, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "238701"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "503724"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "503724")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "503724"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "564261"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "564261")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "564261"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "584842"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "584842")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "584842"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "664184"), 1:K]), segment2 = c(rep(" ", 41), t(farm[which(farm$SRCID == "664184")+1, 1:(K-41)])), repaired = t(farmrepair[which(farmrepair$SRCID == "664184"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "715388"), 1:K]), segment2 = c(rep(" ", 68), t(farm[which(farm$SRCID == "715388")+1, 1:(K-68)])), repaired = t(farmrepair[which(farmrepair$SRCID == "715388"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "747960"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "747960")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "747960"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "766278"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "766278")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "766278"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "856289"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "856289")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "856289"), 1:K])) %>% print(n = 327)
# tibble(segment1 = t(farm[which(farm$SRCID == "864290"), 1:K]), segment2 = c(rep(" ", 77), t(farm[which(farm$SRCID == "864290")+1, 1:(K-77)])), repaired = t(farmrepair[which(farmrepair$SRCID == "864290"), 1:K])) %>% print(n = 327)
# SRCIDs 80842 are exact duplicates so the only way to pick them out individually
# is to refer to their ID numbers 925 and 927 (which are the row numbers for those
# observations as originally loaded in the data)




#### Remove, Merge, or Keep Duplicates ####


# Find all "legitimate" SRCID that appear multiple times in the data set
# Diagnostic: farmrepair %>% filter((duplicated(SRCID) | duplicated(SRCID, fromLast = TRUE)) & (str_length(SRCID) %in% c(5,6))) %>% select(SRCID, RecordedDate, status, Name, Address) %>% print(n = 40)
# There are 30 such observations that have a matching pair (15 unique SRCIDs and 15 duplicates)

# Some of these are exact duplicates, others have a more complicated story
# (see Word doc for details) 

# Diagnostics:
# Inspect the duplicates
# t(farmrepair %>% filter(SRCID == "67631")) #online and paper submissions are complete but with some discrepencies #MergeTogether
# t(farmrepair %>% filter(SRCID == "796752")) #online and paper submissions are complete but with some discrepencies #MergeTogether
# t(farmrepair %>% filter(SRCID == "597595")) #online submission 3% complete, paper submission full
# t(farmrepair %>% filter(SRCID == "821192")) #online submission 3% complete, paper submission full
# t(farmrepair %>% filter(SRCID == "44933")) #exact duplicate except for OthCons
# t(farmrepair %>% filter(SRCID == "78242")) #two identical paper surveys but small discrepancies in OthCons, Grs10PasFenc, Tre35CrpDate, OthCov2_text, NitrLowAcr, OthNutr_text, Zip, Address
# t(farmrepair %>% filter(SRCID == "80842")) #exact duplicate, two identical paper surveys
# t(farmrepair %>% filter(SRCID == "280866")) #different farm address, 2 distinct paper surveys (w/ the same SRCID)         #KeepBoth
# t(farmrepair %>% filter(SRCID == "361086")) #two distinct paper surveys (w/ the same SRCID), two different farms        #KeepBoth
# t(farmrepair %>% filter(SRCID == "380453")) #two distinct paper surveys (w/ the same SRCID), two different farms        #KeepBoth
# t(farmrepair %>% filter(SRCID == "592357")) #identical paper surveys, though data set has small discrepancies (CgrnOwn)
# t(farmrepair %>% filter(SRCID == "662579")) #identical paper surveys, trivial discrepancy in Address
# t(farmrepair %>% filter(SRCID == "698681")) #two distinct paper surveys but with only trivial differences (Stream)
# t(farmrepair %>% filter(SRCID == "741610")) #identical paper surveys, trivial discrepancy in Address
# t(farmrepair %>% filter(SRCID == "835559")) #identical paper surveys, difference in OthCons (due to scrambling?)

# 3 bins: Keep Both, Merge Together, Drop One

# Keep Both
# SRCID 280866
# SRCID 361086
# SRCID 380453

# Merge Together
# SRCID 67631
# Keep the online response as is unless the online is missing, then transfer the value from the paper survey
for (j in 1:K) {
  if ( is.na(farmrepair[which(farmrepair$SRCID == "67631" & farmrepair$status != 2), j]) & (!is.na(farmrepair[which(farmrepair$SRCID == "67631" & farmrepair$status == 2), j])) ) {
    farmrepair[which(farmrepair$SRCID == "67631" & farmrepair$status != 2), j] <- farmrepair[which(farmrepair$SRCID == "67631" & farmrepair$status == 2), j]
  }
}
# Change the "status" to 3 indicating it is a hybrid of paper submissions pasted onto an online base
farmrepair[which(farmrepair$SRCID == "67631" & farmrepair$status != 2), "status"] <- 3
# Drop the SRCID 67631 paper submission from the farmrepair data set
farmrepair <- farmrepair %>% filter(!(SRCID == "67631" & status == 2))

# SRCID 796752
for (j in 1:K) {
  if ( is.na(farmrepair[which(farmrepair$SRCID == "796752" & farmrepair$status == 2), j]) & (!is.na(farmrepair[which(farmrepair$SRCID == "796752" & farmrepair$status != 2), j])) ) {
    farmrepair[which(farmrepair$SRCID == "796752" & farmrepair$status == 2), j] <- farmrepair[which(farmrepair$SRCID == "796752" & farmrepair$status != 2), j]
  }
}
# Change the "status" to 3 indicating it is a hybrid of online submissions pasted onto a paper base
farmrepair[which(farmrepair$SRCID == "796752" & farmrepair$status == 2), "status"] <- 4
# Drop the SRCID 796752 online submission from the farmrepair data set
farmrepair <- farmrepair %>% filter(!(SRCID == "796752" & status == 1))

# SRCID 78242
for (j in 1:K) {
  if ( is.na(farmrepair[which(farmrepair$SRCID == "78242" & farmrepair$OthNutr_text == "AEROMASTER HUMUS"), j]) & (!is.na(farmrepair[which(farmrepair$SRCID == "78242" & is.na(farmrepair$OthNutr_text)), j])) ) {
    farmrepair[which(farmrepair$SRCID == "78242" & farmrepair$OthNutr_text == "AEROMASTER HUMUS"), j] <- farmrepair[which(farmrepair$SRCID == "78242" & is.na(farmrepair$OthNutr_text)), j]
  }
}
# Change the "status" to 5 indicating it is a hybrid of two paper submissions
farmrepair[which(farmrepair$SRCID == "78242" & farmrepair$OthNutr_text == "AEROMASTER HUMUS"), "status"] <- 5
# Drop the SRCID 78242 that doesn't say "AEROMASTER HUMUS" from the farmrepair data set
farmrepair <- farmrepair %>% filter(!(SRCID == "78242" & OthNutr_text != "AEROMASTER HUMUS"))

# SRCID 44933 (preserve the one with OthCons text)
farmrepair <- farmrepair %>% filter(!(SRCID == "44933" & is.na(OthCons)))

# SRCID 592357
for (j in 1:K) {
  if ( is.na(farmrepair[which(farmrepair$SRCID == "592357" & !is.na(farmrepair$CgrnOwn)), j]) & (!is.na(farmrepair[which(farmrepair$SRCID == "592357" & is.na(farmrepair$CgrnOwn)), j])) ) {
    farmrepair[which(farmrepair$SRCID == "592357" & !is.na(farmrepair$CgrnOwn)), j] <- farmrepair[which(farmrepair$SRCID == "592357" & is.na(farmrepair$CgrnOwn)), j]
  }
}
# Change the "status" to 5 indicating it is a hybrid of two paper submissions
farmrepair[which(farmrepair$SRCID == "592357" & !is.na(farmrepair$CgrnOwn)), "status"] <- 5
# Change the value of "RyeOwn" to zero to reflect what's in the paper survey (paper survey has a line striking through the boxes)
farmrepair[which(farmrepair$SRCID == "592357" & !is.na(farmrepair$CgrnOwn)), "RyeOwn"] <- 0
# Change cover crop planted from NA to both rye and barley
farmrepair[which(farmrepair$SRCID == "592357" & !is.na(farmrepair$CgrnOwn)), "CovTyp1"] <- "1, 3"
# Drop from the data set the SRCID 592357 that has a missing value for CgrnOwn
farmrepair <- farmrepair %>% filter(!(SRCID == "592357" & is.na(CgrnOwn)))

# SRCID 662579
# Drop from the data set the SRCID 662579 that has a missing value for OthCons
farmrepair <- farmrepair %>% filter(!(SRCID == "662579" & is.na(OthCons)))

# SRCID 698681
for (j in 1:K) {
  if ( is.na(farmrepair[which(farmrepair$SRCID == "698681" & farmrepair$Inj == 1), j]) & (!is.na(farmrepair[which(farmrepair$SRCID == "698681" & is.na(farmrepair$Inj)), j])) ) {
    farmrepair[which(farmrepair$SRCID == "698681" & farmrepair$Inj == 1), j] <- farmrepair[which(farmrepair$SRCID == "698681" & is.na(farmrepair$Inj)), j]
  }
}
# Change the "status" to 5 indicating it is a hybrid of two paper submissions
farmrepair[which(farmrepair$SRCID == "698681" & farmrepair$Inj == 1), "status"] <- 5
# Drop from the data set the SRCID 698681 that has a missing value for Inj
farmrepair <- farmrepair %>% filter(!(SRCID == "698681" & is.na(Inj)))

# SRCID 741610
# Drop from the data set the SRCID 741610 that has a missing value for OthCons
farmrepair <- farmrepair %>% filter(!(SRCID == "741610" & is.na(OthCons)))

# SRCID 835559 (preserve the one with OthCons text)
farmrepair <- farmrepair %>% filter(!(SRCID == "835559" & is.na(OthCons)))

# Drop One
# SRCID 597595 (drop online submission, keep paper submission)
farmrepair <- farmrepair %>% filter(!(SRCID == "597595" & status != 2))
# Keep SRCID 597595 if it was a paper submission, otherwise drop

# SRCID 821192 (drop online submission, keep paper submission)
farmrepair <- farmrepair %>% filter(!(SRCID == "821192" & status != 2))
# Keep SRCID 821192 if it was a paper submission, otherwise drop

# SRCID 80842
farmrepair <- farmrepair %>% filter(!(SRCID == "80842" & is.na(OthCons)))
# Keep SRCID 80842 if OthCons has text, but drop the other




# Handle the Ben Peckman situation in one of the ways below (ask Matt which he prefers)

# Drop Ben Peckman's 4789 Lincoln Way West responses
# farmrepair <- farmrepair %>% filter(!(ID %in% c(139, 141)))

# Drop Ben Peckman's second 4789 Lincoln Way West response
# farmrepair <- farmrepair %>% filter(!(ID == 141))

# Drop Ben Peckman's 5425 Lincoln Way West response
# farmrepair <- farmrepair %>% filter(!(ID == 137))

# Drop Ben Peckman's 5425 Lincoln Way West response and his second 4789 Lincoln Way West response
farmrepair <- farmrepair %>% filter(!(ID %in% c(137, 141)))

# Drop Ben Peckman's 5425 Lincoln Way West response and his first 4789 Lincoln Way West response
#farmrepair <- farmrepair %>% filter(!(ID %in% c(137, 139)))




#### Remove Row Fragments ####

# 12 of the first 270 rows of the data set (i.e. the online responses)
# have just 1 line of open-ended comments placed in the SRCID column
# and all other columns past SRCID are blank

# These should be dropped or the text should be collected separately
# Create a vector of row numbers for these 12 "observations"
online1colrows <- prob[1:12]



# Display the text from these observations and store in a table
# Diagnostic: farmrepair[which(farmrepair$ID %in% online1colrows), ] %>% select(SRCID)


# Collect all the text from these "one column rows" associated with
# online responses (which make up the first 270 rows of the data set, 271 and beyond are paper)
misc_OthCons <- farmrepair[which(farmrepair$ID %in% online1colrows), ] %>% select(SRCID)

# Remove these 12 rows from the data set, along with the partial segments that
# were used to reconstruct the now-repaired rows



# View the SRCID column for the rows to be dropped
# Diagnostic: farmrepair %>% filter(ID %in% v$ID[which(v$repair2 == 0 & v$repair3 == 0 & v$repair4 == 0)]) %>% select(SRCID) %>% print(n = 50)
# There are 48 such rows

# Select the ID (original row numbers) in "v" that were not reassigned new values in any
# of the three "repair loops" above and drop these rows from the data set
farmrepair <- farmrepair %>% filter(!(ID %in% v$ID[which(v$repair2 == 0 & v$repair3 == 0 & v$repair4 == 0)]))
# There are 1873 rows left. These correspond to the 1870 unique SRCIDs plus the 3
# duplicate SRCIDs that were actually separate farms with distinct survey responses

# UPDATE: This becomes 1872 rows if one of the Ben Peckman observations is dropped
