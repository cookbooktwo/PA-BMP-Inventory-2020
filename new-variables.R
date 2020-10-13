
# Generate new variables to aggregate farm characteristics and
# create categories for sorting and filtering nutrient management
# practice adoption



#### Create Bins Based on Total Acres Reported ####

# Q2 Acres

# Create new categorical variable: farm size bins based on Table 8 of the NASS Ag Census"
farmclean <- farmclean %>% mutate(Bin8 = case_when((is.na(Acres)) ~ "Did not specify",
                                                   (Acres == 0) ~ "zero acres",
                                                   (Acres > 0 & Acres <= 10) ~ "0-10 acres",
                                                   (Acres > 10 & Acres <= 20) ~ "10-20 acres",
                                                   (Acres > 20 & Acres <= 30) ~ "20-30 acres",
                                                   (Acres > 30 & Acres <= 50) ~ "30-50 acres",
                                                   (Acres > 50 & Acres <= 100) ~ "50-100 acres",
                                                   (Acres > 100 & Acres <= 200) ~ "100-200 acres",
                                                   (Acres > 200 & Acres <= 500) ~ "200-500 acres",
                                                   (Acres > 500 & Acres <= 1000) ~ "500-1000 acres",
                                                   (Acres > 1000 & Acres <= 2000) ~ "1000-2000 acres", 
                                                   (Acres > 2000) ~ "> 2000 acres"))

# Create new categorical variable: farm size bins based on Table 9 of the NASS Ag Census"
farmclean <- farmclean %>% mutate(Bin9 = case_when((is.na(Acres)) ~ "Did not specify", 
                                                   (Acres == 0) ~ "zero acres", 
                                                   (Acres > 0 & Acres <= 10) ~ "0-10 acres", 
                                                   (Acres > 10 & Acres <= 50) ~ "10-50 acres", 
                                                   (Acres > 50 & Acres <= 70) ~ "50-70 acres", 
                                                   (Acres > 70 & Acres <= 100) ~ "70-100 acres", 
                                                   (Acres > 100 & Acres <= 140) ~ "100-140 acres", 
                                                   (Acres > 140 & Acres <= 180) ~ "140-180 acres", 
                                                   (Acres > 180 & Acres <= 220) ~ "180-220 acres", 
                                                   (Acres > 220 & Acres <= 260) ~ "220-260 acres", 
                                                   (Acres > 260 & Acres <= 500) ~ "260-500 acres", 
                                                   (Acres > 500 & Acres <= 1000) ~ "500-1000 acres", 
                                                   (Acres > 1000 & Acres <= 2000) ~ "1000-2000 acres", 
                                                   (Acres > 2000 & Acres <= 5000) ~ "2000-5000 acres", 
                                                   (Acres > 5000) ~ "> 5000 acres"))

# Come back to this and change these from strings to factors




#### Aggregate Crop Acreages and Categorize Farms Based on Crop Types ####

# Q3 Crop Acreages

# Convert CgrnDbl, BarlDbl, and OthcropRnt from character to numeric
farmclean <- farmclean %>% mutate(CgrnDbl = as.numeric(CgrnDbl),
                                  BarlDbl = as.numeric(BarlDbl),
                                  OthcropRnt = as.numeric(OthcropRnt))


# Replace NA with zero, else new variables will "become infected" with NA
farmclean <- farmclean %>% mutate(CgrnOwn = case_when(is.na(CgrnOwn) ~ 0, !is.na(CgrnOwn) ~ CgrnOwn),
                                  CgrnRnt = case_when(is.na(CgrnRnt) ~ 0, !is.na(CgrnRnt) ~ CgrnRnt),
                                  CgrnDbl = case_when(is.na(CgrnDbl) ~ 0, !is.na(CgrnDbl) ~ CgrnDbl),
                                  CsilOwn = case_when(is.na(CsilOwn) ~ 0, !is.na(CsilOwn) ~ CsilOwn),
                                  CsilRnt = case_when(is.na(CsilRnt) ~ 0, !is.na(CsilRnt) ~ CsilRnt),
                                  CsilDbl = case_when(is.na(CsilDbl) ~ 0, !is.na(CsilDbl) ~ CsilDbl),
                                  SoyOwn = case_when(is.na(SoyOwn) ~ 0, !is.na(SoyOwn) ~ SoyOwn),
                                  SoyRnt = case_when(is.na(SoyRnt) ~ 0, !is.na(SoyRnt) ~ SoyRnt),
                                  SoyDbl = case_when(is.na(SoyDbl) ~ 0, !is.na(SoyDbl) ~ SoyDbl),
                                  WhtOwn = case_when(is.na(WhtOwn) ~ 0, !is.na(WhtOwn) ~ WhtOwn),
                                  WhtRnt = case_when(is.na(WhtRnt) ~ 0, !is.na(WhtRnt) ~ WhtRnt),
                                  WhtDbl = case_when(is.na(WhtDbl) ~ 0, !is.na(WhtDbl) ~ WhtDbl),
                                  RyeOwn = case_when(is.na(RyeOwn) ~ 0, !is.na(RyeOwn) ~ RyeOwn),
                                  RyeRnt = case_when(is.na(RyeRnt) ~ 0, !is.na(RyeRnt) ~ RyeRnt),
                                  RyeDbl = case_when(is.na(RyeDbl) ~ 0, !is.na(RyeDbl) ~ RyeDbl),
                                  BarlOwn = case_when(is.na(BarlOwn) ~ 0, !is.na(BarlOwn) ~ BarlOwn),
                                  BarlRnt = case_when(is.na(BarlRnt) ~ 0, !is.na(BarlRnt) ~ BarlRnt),
                                  BarlDbl = case_when(is.na(BarlDbl) ~ 0, !is.na(BarlDbl) ~ BarlDbl),
                                  AlfOwn = case_when(is.na(AlfOwn) ~ 0, !is.na(AlfOwn) ~ AlfOwn),
                                  AlfRnt = case_when(is.na(AlfRnt) ~ 0, !is.na(AlfRnt) ~ AlfRnt),
                                  AlfDbl = case_when(is.na(AlfDbl) ~ 0, !is.na(AlfDbl) ~ AlfDbl),
                                  HayOwn = case_when(is.na(HayOwn) ~ 0, !is.na(HayOwn) ~ HayOwn),
                                  HayRnt = case_when(is.na(HayRnt) ~ 0, !is.na(HayRnt) ~ HayRnt),
                                  HayDbl = case_when(is.na(HayDbl) ~ 0, !is.na(HayDbl) ~ HayDbl),
                                  OthcropOwn = case_when(is.na(OthcropOwn) ~ 0, !is.na(OthcropOwn) ~ OthcropOwn),
                                  OthcropRnt = case_when(is.na(OthcropRnt) ~ 0, !is.na(OthcropRnt) ~ OthcropRnt),
                                  OthcropDbl = case_when(is.na(OthcropDbl) ~ 0, !is.na(OthcropDbl) ~ OthcropDbl))

# Create new variables summarizing acreage grown for different crops,
# total crop acreage, percentage of total acreage grown for different
# crops, and classifying the farm as "Corn", "Soy", "Small Grain",
# "Hay", "Other", "Mixed", or "No Crops"
farmclean <- farmclean %>% mutate(CgrnAcr = CgrnOwn + CgrnRnt + CgrnDbl,
                                  CsilAcr = CsilOwn + CsilRnt + CsilDbl,
                                  SoyAcr = SoyOwn + SoyRnt + SoyDbl,
                                  WhtAcr = WhtOwn + WhtRnt + WhtDbl,
                                  RyeAcr = RyeOwn + RyeRnt + RyeDbl,
                                  BarlAcr = BarlOwn + BarlRnt + BarlDbl,
                                  AlfAcr = AlfOwn + AlfRnt + AlfDbl,
                                  HayAcr = HayOwn + HayRnt + HayDbl,
                                  OthcropAcr = OthcropOwn + OthcropRnt + OthcropDbl,
                                  CropAcr = CgrnAcr + CsilAcr + SoyAcr + WhtAcr + RyeAcr + BarlAcr + AlfAcr + HayAcr + OthcropAcr,
                                  CornAcr = CgrnAcr + CsilAcr,
                                  SmgrnAcr = WhtAcr + RyeAcr + BarlAcr,
                                  HalfAcr = AlfAcr + HayAcr,
                                  CornPct = CornAcr/CropAcr,
                                  SoyPct = SoyAcr/CropAcr,
                                  SmgrnPct = SmgrnAcr/CropAcr,
                                  HalfPct = HalfAcr/CropAcr,
                                  OthcropPct = OthcropAcr/CropAcr,
                                  CropTyp = case_when(CornPct >= 2/3 ~ "Corn",
                                                      SoyPct >= 2/3 ~ "Soy",
                                                      SmgrnPct >= 2/3 ~ "Small Grain",
                                                      HalfPct >= 2/3 ~ "Hay",
                                                      OthcropPct >= 2/3 ~ "Other",
                                                      (CornPct < 2/3 & SoyPct < 2/3 & SmgrnPct < 2/3 & HalfPct < 2/3 & OthcropPct < 2/3) ~ "Mixed",
                                                      CropAcr == 0 ~ "None"))




#### Summarize Animal Holdings and Categorize Farms Based on Animal Types ####

# Q4 Animals

# Convert Turk, Fpigs, Sow, Boar, Heifo from character to numeric
farmclean <- farmclean %>% mutate(Turk = as.numeric(Turk),
                                  Fpig = as.numeric(Fpig),
                                  Sow = as.numeric(Sow),
                                  Boar = as.numeric(Boar),
                                  Heifo = as.numeric(Heifo))

# Replace NA with zero, else new variables will "become infected" with NA
farmclean <- farmclean %>% mutate(Broil = case_when(is.na(Broil) ~ 0, !is.na(Broil) ~ Broil),
                                  Lay = case_when(is.na(Lay) ~ 0, !is.na(Lay) ~ Lay),
                                  Turk = case_when(is.na(Turk) ~ 0, !is.na(Turk) ~ Turk),
                                  Duck = case_when(is.na(Duck) ~ 0, !is.na(Duck) ~ Duck),
                                  Npig = case_when(is.na(Npig) ~ 0, !is.na(Npig) ~ Npig),
                                  Fpig = case_when(is.na(Fpig) ~ 0, !is.na(Fpig) ~ Fpig),
                                  Sow = case_when(is.na(Sow) ~ 0, !is.na(Sow) ~ Sow),
                                  Boar = case_when(is.na(Boar) ~ 0, !is.na(Boar) ~ Boar),
                                  Veal = case_when(is.na(Veal) ~ 0, !is.na(Veal) ~ Veal),
                                  Heify = case_when(is.na(Heify) ~ 0, !is.na(Heify) ~ Heify),
                                  Heifo = case_when(is.na(Heifo) ~ 0, !is.na(Heifo) ~ Heifo),
                                  Cow = case_when(is.na(Cow) ~ 0, !is.na(Cow) ~ Cow),
                                  Catl = case_when(is.na(Catl) ~ 0, !is.na(Catl) ~ Catl),
                                  Hors = case_when(is.na(Hors) ~ 0, !is.na(Hors) ~ Hors))

# Create new variables summarizing total animal equivalent units (AEU)
# each farm has in different animal types, total AEU for each farm, percentage
# of total AEU that different animal types represent for each farm, and
# classifying the farm as "Poultry", "Swine", "Dairy", "Beef", "Equine"
# according to those percentages
farmclean <- farmclean %>% mutate(BroilAEU = Broil*3/1000,
                                  LayAEU = Lay*3/1000,
                                  TurkAEU = Turk*14/1000,
                                  DuckAEU = Duck*3.5/1000,
                                  NpigAEU = Npig*30/1000,
                                  FpigAEU = Fpig*155/1000,
                                  SowAEU = Sow*400/1000,
                                  BoarAEU = Boar*450/1000,
                                  VealAEU = Veal*270/1000,
                                  HeifyAEU = Heify*300/1000,
                                  HeifoAEU = Heifo*750/1000,
                                  CowAEU = Cow*1100/1000,
                                  CatlAEU = Catl*1300/1000,
                                  HorsAEU = Hors*1200/1000,
                                  AEU = BroilAEU + LayAEU + TurkAEU + DuckAEU + NpigAEU + FpigAEU + SowAEU + BoarAEU + VealAEU + HeifyAEU + HeifoAEU + CowAEU + CatlAEU + HorsAEU,
                                  PoulAEU = BroilAEU + LayAEU + TurkAEU + DuckAEU,
                                  SwinAEU = NpigAEU + FpigAEU + SowAEU + BoarAEU,
                                  DairAEU = HeifyAEU + HeifoAEU + CowAEU,
                                  BeefAEU = VealAEU + CatlAEU,
                                  PoulPct = PoulAEU/AEU,
                                  SwinPct = SwinAEU/AEU,
                                  DairPct = DairAEU/AEU,
                                  BeefPct = BeefAEU/AEU,
                                  HorsPct = HorsAEU/AEU,
                                  AnmTyp = case_when(PoulPct >= 2/3 ~ "Poultry",
                                                     SwinPct >= 2/3 ~ "Swine",
                                                     DairPct >= 2/3 ~ "Dairy",
                                                     BeefPct >= 2/3 ~ "Beef",
                                                     HorsPct >= 2/3 ~ "Equine",
                                                     (PoulPct < 2/3 & SwinPct < 2/3 & DairPct < 2/3 & BeefPct < 2/3 & HorsPct < 2/3) ~ "Mixed",
                                                     AEU == 0 ~ "None"))
