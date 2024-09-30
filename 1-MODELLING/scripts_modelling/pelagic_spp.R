# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------

# remove certain pelagic species due to
# 1 - very sparse distribution map
# and/or
# 2 - they will not benefit from spatial protection apart from at aggregation spots
# these were checked using fishbase as well as geremy's database
pelagic_species = c(
  #SHARKS
  'ALOPIAS VULPINUS',
  'ALOPIAS PELAGICUS',
  'ALOPIAS SUPERCILIOSUS',
  # can be found inshore but model not good due to scattered/insufficient data
  "CARCHARHINUS FALCIFORMIS",
  "CARCHARHINUS LONGIMANUS",
  "CARCHARODON CARCHARIAS",
  "CETORHINUS MAXIMUS",
  "ISURUS OXYRINCHUS",
  "ISURUS PAUCUS",
  "LAMNA NASUS",
  "PRIONACE GLAUCA",
  "PSEUDOCARCHARIAS KAMOHARAI",
  "RHINCODON TYPUS",
  "SPHYRNA MOKARRAN",
  #RAYS
  "PTEROPLATYTRYGON VIOLACEA",
  "MOBULA THURSTONI",
  "MOBULA MOBULAR",
  "MOBULA EREGOODOO"
)

problem_species = c(
  # SHARKS
  # need to use IUCN as insufficient data
  "ACROTERIOBATUS OCELLATUS",
  "CALLORHINCHUS CAPENSIS",
  #"CARCHARHINUS LEUCAS", too important so included for now
  "GALEUS POLLI",
  "CENTROPHORUS UYATO",
  "CENTROPHORUS GRANULOSUS",
  "DALATIAS LICHA",
  "DEANIA PROFUNDORUM",
  "ETMOPTERUS BIGELOWI",
  "HEPTRANCHIAS PERLO",
  "HEXANCHUS GRISEUS", 
  "HOLOHALAELURUS PUNCTATUS",
  "SPHYRNA LEWINI",
  "SPHYRNA MOKARRAN",
  "SQUALUS ACUTIPINNIS",
  "SQUATINA AFRICANA",
  "ZAMEUS SQUAMULOSUS",
  # RAYS
  # Dipturus spp have taxonomic confusion (Ebert et al.)
  "DIPTURUS DOUTREI",
  "DIPTURUS SPRINGERI"
)

problem_species_all = c(problem_species,pelagic_species)
problem_species_all = unique(problem_species_all)
rm(problem_species,pelagic_species)
