# Objective:
#   Create a set of blacklist. Restrict edges coming to clinical variables

# PC:
#setwd("/Users/stebliankin/Desktop/AntibioticsProject/AntibioticsPTR/PTR-analysis/causality/data/castalia") # This is input folder, input and output file will be in this directory

# Castalia:
#setwd("/disk/castalia/lclhome/vsteb002/AntibioticsProject/causality/data/castalia") # This is input folder, input and output file will be in this directory


#data_file_name <- "PTR_species_filtered_metadata_major_NANis1.csv" #input file

# Compute Correlations for Causal Graph
dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")
source("RIO.R")



# Input:
input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];

parameters <<- readParameters(inputfile)
	data_file_name <<- paste(pfix, parameters["data_file_name", 2], sep="/")#"PTR_species_filtered_metadata_major_NANis1.csv" #input file
        clinical_file <<- paste(pfix, parameters["clinical", 2], sep="/")
}


run <- function() {
df2 <<- read.csv(data_file_name)

clinical_var_list <- readLines(clinical_file)
#clinical_var_list <- c("Day_of_Life","PostMenst_Age	Gestational_Age	Birthweight	Gentamicin	Cefazolin	Ampicillin	Trimethoprim-Sulfamathoxazole	Meropenem	Vancomycin	Ticarcillin-Clavulanate	Clindamycin	Cefotaxime	Total_abx	r_Gentamicin	r_Meropenem	r_Ticarcillin-Clavulanate	r_Vancomycin	r_Ampicillin	r_Cefotaxime	r_TOTAL	Human_Milk	Maternal_Milk	Donor_Milk	Formula	Fortification	Vitamin_A	Caffeine	Iron	Furosemide_Lasix	m_ampicillin	m_ceftriaxone	m_azithromycin	m_amoxicillin	m_cefazolin	m_erythromycin	m_gentamicin	m_penicillin	m_vancomycin	m_clindamycin	m_cefotaxime	dur_membrane_rupture	CRIB II Score	Total Antibiotic Days")
cols <- colnames(df2)

# Restrict edges from all columns to Clinical Variables
restricted_vec <- c()
for(from in cols){
  for(to in cols){
    if(to %in% clinical_var_list){
      restricted_vec <- rbind(restricted_vec, c(from, to))
    }
  }
}

# Restrict edges between averagePTR and PTR of any taxa
#for(col in cols){
#  print(col)
#  if(grepl(".PTR", col) | grepl(".abundance", col)){
#    restricted_vec <- rbind(restricted_vec, c(col, "AveragePTR"))
#    restricted_vec <- rbind(restricted_vec, c("AveragePTR", col))
#  }
#}

# Restrict edges between PTR-PTR
for (col1 in cols){
  if (grepl("PTR",col1)){
    taxa1 <- gsub(".PTR","", col1)
    for (col2 in cols){
      # PTR-PTR
      if (grepl("PTR",col2)){
        restricted_vec <- rbind(restricted_vec, c(col1, col2))
      # PTR - abundace
      }else if(grepl("abundance",col2)){
        taxa2 <- gsub(".abundance","",col2)
        if (taxa1!=taxa2){
          restricted_vec <- rbind(restricted_vec, c(col1, col2))
        }
      }else if(grepl("ARO",col2)){
        restricted_vec <- rbind(restricted_vec, c(col1, col2))
      }
      
    }
  }
}


for (col1 in cols){
  if (grepl("abundance",col1)){
    for (col2 in cols){
      if (grepl("PTR",col2)){
        restricted_vec <- rbind(restricted_vec, c(col1, col2))
      }
      
    }
  }
}

# Restrinct gene-gene
for (col1 in cols){
  if (grepl("ARO.",col1)){
    for (col2 in cols){
      if (grepl("ARO.",col2)){
        restricted_vec <- rbind(restricted_vec, c(col1, col2))
      }
      
    }
  }
}
df2 <<- data.frame(restricted_vec)
colnames(df2)<<- c("from", "to")
}

output <- function(outputfile) {
write.csv(df2, outputfile, row.names = FALSE)
}

