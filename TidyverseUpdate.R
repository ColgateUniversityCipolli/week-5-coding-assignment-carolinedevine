
################################################################################
# Lab 3
# Caroline Devine
################################################################################


################################################################################
# 2: Lab Coding Task: Compile Data from Essentia
################################################################################

library("jsonlite")
help("jsonlite")

library("tidyverse")
help("tidyverse")
# Step 1 

# File Name + Extraction 
current.filename <- ("The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json")
curr.file.parts <- str_split(current.filename, "-", simplify = TRUE)
curr.artist.file <- curr.file.parts[1]
curr.album.file <- curr.file.parts[2]
curr.track.file <- curr.file.parts[3] |>
  str_remove(".json$")

# Load JSON file
json.storing <- fromJSON(file.path("EssentiaOutput", current.filename))

# Extract metadata
overall.loudness <- json.storing$lowlevel$loudness_ebu128$integrated
spectral.energy <- json.storing$lowlevel$spectral_energy
dissonance <- json.storing$lowlevel$dissonance
pitch.salience <- json.storing$lowlevel$pitch_salience
bpm <- json.storing$rhythm$bpm
beats.loudness <- json.storing$rhythm$beats_loudness
danceability <- json.storing$rhythm$danceability
tuning.frequency <- json.storing$tonal$tuning_frequency


######## Step 2 #########
help("map_dfr") # cite purrr package for map_dfr --> documentation says purrr:map functions can be thought of as a replacement to for loops

data.extraction <- function(json.file.names){
  df <- map_dfr(json.file.names, function(file){
    file.parts <- str_split(file, "-", simplify = TRUE)
    artist <- file.parts[1]
    album <- file.parts[2]
    track <- file.parts[3]|>
      str_remove(".json$")
    
    json.storage <- fromJSON(file.path("EssentiaOutput", current.filename))
    
    tibble(
      artist = artist,
      album = album,
      track = track,
      overall_loudness = json.storage$lowlevel$loudness_ebu128$integrated,
      spectral_energy = json.storage$lowlevel$spectral_energy,
      dissonance = json.storage$lowlevel$dissonance,
      pitch_salience = json.storage$lowlevel$pitch_salience,
      bpm = json.storage$rhythm$bpm,
      beats_loudness = json.storage$rhythm$beats_loudness,
      danceability = json.storage$rhythm$danceability,
      tuning_frequency = json.storage$tonal$tuning_frequency
    )
    
  })
  return(df)
}

# List of .json files
json.file.names <- list.files("EssentiaOutput", pattern = "\\.json$")
df <- data.extraction(json.file.names)
view(df)

# Create Empty Data Frame with 10 given columns
# df <- data.frame(artist = rep(x = NA, times=length(json.file.names)), 
#                  album = rep(x = NA, times=length(json.file.names)), 
#                  track = rep(x = NA, times=length(json.file.names)), 
#                  overall_loudness = rep(x = NA, times=length(json.file.names)), 
#                  spectral_energy = rep(x = NA, times=length(json.file.names)), 
#                  dissonance = rep(x = NA, times=length(json.file.names)), 
#                  pitch_salience = rep(x = NA, times=length(json.file.names)), 
#                  bpm = rep(x = NA, times=length(json.file.names)), 
#                  beats_loudness = rep(x = NA, times=length(json.file.names)),
#                  danceability = rep(x = NA, times=length(json.file.names)),
#                  tuning_frequency = rep(x = NA, times=length(json.file.names)))



################################################################################
# Step 3: Essentia Model Cleanup 
################################################################################

# Load csv file
essentiamodeldf <- read_csv(file.path("EssentiaOutput","EssentiaModelOutput.csv"))
View(essentiamodeldf)
dim(essentiamodeldf)

cleaned.essentia.model <- essentiamodeldf |>
  mutate(
    valence = rowMeans(essentiamodeldf[,c("deam_valence", "emo_valence", "muse_valence")], na.rm = T),
    arousal = rowMeans(essentiamodeldf[,c("deam_arousal", "emo_arousal", "muse_arousal")], na.rm = T),
    aggressive = rowMeans(essentiamodeldf[,c("eff_aggressive", "nn_aggressive")], na.rm = T),
    happy = rowMeans(essentiamodeldf[,c("eff_happy", "nn_happy")], na.rm = T),
    party = rowMeans(essentiamodeldf[,c("eff_party", "nn_party")], na.rm = T),
    relax = rowMeans(essentiamodeldf[,c("eff_relax", "nn_relax")], na.rm = T),
    sad = rowMeans(essentiamodeldf[,c("eff_sad", "nn_sad")], na.rm = T),
    acoustic = rowMeans(essentiamodeldf[,c("eff_acoustic", "nn_acoustic")], na.rm = T),
    electronic = rowMeans(essentiamodeldf[,c("eff_electronic", "nn_electronic")], na.rm = T),
    instrumental = rowMeans(essentiamodeldf[,c("eff_instrumental", "nn_instrumental")], na.rm = T)
  ) |>
  rename(timbreBright = eff_timbre_bright) |>
  select("artist",
         "album",
         "track",
         "valence",
         "arousal",
         "aggressive",
         "happy",
         "party",
         "relax",
         "sad",
         "acoustic",
         "electronic",
         "instrumental",
         "timbreBright")

View(cleaned.essentia.model)

################################################################################
# Step 4: LIWC Merge
################################################################################

# Load csv file
folder2 = "LIWCOutput"
LIWCOutputdf <- read.csv(paste(folder2,"LIWCOutput.csv", sep="/"))

help("merge")

merge_1 <- merge(df, cleaned.essentia.model, 
                 by = intersect(names(df), names(cleaned.essentia.model)), 
                 all = TRUE)

final.merge <- merge(merge_1, LIWCOutputdf, 
                     by = intersect(names(df), names(cleaned.essentia.model)), 
                     all = TRUE)

colnames(final.merge)[colnames(final.merge) == "function."] <- "funct"

View(final.merge)
dim(final.merge)

################################################################################
# Step 5: Two new .csv Files
################################################################################

allentown.index <- which(final.merge$track == "Allentown")
trainingdata <- final.merge[-c(120), ]
write.csv(trainingdata,"trainingdata.csv")

testingdata <- final.merge[c(120), ]
write.csv(testingdata, "testingdata.csv")
View(final.merge)

################################################################################
# Part 3: Challenge
################################################################################

help("boxplot")
valence.plot <- boxplot(valence ~ artist, data = final.merge, na.action = NULL, 
                        xlab = "Artist", ylab = "Valence")
arousal.plot <- boxplot(arousal ~ artist, data = final.merge, na.action = NULL, 
                        xlab = "Artist", ylab = "Arousal")
bpm.plot <- boxplot(bpm ~ artist, data = final.merge, na.action = NULL, 
                    xlab = "Artist", ylab = "BPM")

## This is my attempt to create box plots for certain characteristics of 
## each of the artists. I am unsure exactly how these maybe helpful in 
## understanding the overall question of which band had a bigger impact 
## on AllenTown as these plots look at the valence, arousal, and bpm and how 
## they vary per artist; they do not look directly at AllenTown.

allentown <- final.merge[final.merge$track == "Allentown"]
View(allentown)

## I plan on using the training and testing data to figure this out. 
