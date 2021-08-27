

#### Data load and cleaning

# info load
experiment.information <- read.csv("experiment.information.csv")

# data correction
experiment.information$trtmnt.type[experiment.information$trtmnt.type == "unt"] <- "Gy"
experiment.information$trtmnt.type[experiment.information$trtmnt.type == "mock"] <- "Ola"

# loading quantative data
all.reads <- read_csv("all.crispresso.clean.csv")

# cleaning
all.reads <- all.reads[,-1]
replace.list <- as.list(rep(0, ncol(all.reads)))
names(replace.list) <- names(all.reads)


# replace NA's with zeros
all.reads <- replace_na(data = all.reads, replace = replace.list)

# WT will only be in all.reads
# p4 and p5 will only be in the all.reads, all.hap, and all.dip
cell.type <- experiment.information$Cell.Type
treatment.type <- experiment.information$trtmnt.type

# percent.boolean takes care of the NA's                                                                     # drops WT's             # drops p4's and p5's
percent.boolean <- experiment.information$data.type == "%" & !is.na(experiment.information$data.type)        & cell.type != "WT"      & treatment.type != "p4" & treatment.type != "p5"



#### Boolean info, hardcoded

all.boolean <- percent.boolean
all.hap <- cell.type == "H" & percent.boolean
all.dip <- cell.type == "D" & percent.boolean

hap.nira <- cell.type == "H" & percent.boolean & treatment.type == "Nira"
hap.ola <- cell.type == "H" & percent.boolean & treatment.type == "Ola"
hap.gy <- cell.type == "H" & percent.boolean & treatment.type == "Gy"

dip.nira <- cell.type == "D" & percent.boolean & treatment.type == "Nira"
dip.ola <- cell.type == "D" & percent.boolean & treatment.type == "Ola"
dip.gy <- cell.type == "D" & percent.boolean & treatment.type == "Gy"

boolean.list <- list(all.boolean = all.boolean, all.hap = all.hap, all.dip = all.dip, hap.nira = hap.nira, hap.ola = hap.ola, hap.gy = hap.gy, dip.nira = dip.nira, dip.ola = dip.ola, dip.gy = dip.gy)

boolean.name.list = list(all.boolean = "all.boolean", all.hap = "all.hap", all.dip = "all.dip", hap.nira = "hap.nira", hap.ola = "hap.ola", hap.gy = "hap.gy", dip.nira = "dip.nira", dip.ola = "dip.ola", dip.gy = "dip.gy")


require(crayon)
cat(crayon::green("data.load.clean.R file successfully sourced\n"))




