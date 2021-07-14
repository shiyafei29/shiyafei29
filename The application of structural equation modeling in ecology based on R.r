####### The application of structural equation modeling in ecology based on R #######

# Original author of the data: Jarrett Byrnes, Jarrett.Byrnesvims@umb.edu 

# Load required libraries
library(ape) 
install.packages("TH.data")
library(caper) 
library(nlme) 
library(lavaan) 
library(piecewiseSEM) 


# Read in data
kelp <- read.csv("kelp.csv")
str(kelp)
# Convert -Inf to NA
kelp$max_Max.OV[which(kelp$max_Max.OV == -Inf)] = NA

# Log-transformation for the response variables
kelp$kelp < log(kelp$kelp + 1)
kelp$prev.kelp <-  log(kelp$prev.kelp + 1)
kelp[, 18:23] <- log(kelp[, 18:23] + 1)

# Remove rows where response or predictors are NA
vars <- c("SITE", "TRANSECT", "YEAR", "max_Max.OV", "prev.kelp", "habitat", "spring_canopy_150", "kelp", 
         "algae_richness", "sessile_invert_richness", "mobile_richness", "richness", "consumer_richness", "linkdensity")
kelp <- kelp[, vars]
kelp <- na.omit(kelp)

# Create interaction term
kelp$wave_kelp_int <- kelp$max_Max.OV * kelp$prev.kelp



#################### Fit the model by lavaan ##########################

#fit the model
sem.model1 <- sem(
  model = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int 

  kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150

  richness ~ kelp   + prev.kelp  + spring_canopy_150

  linkdensity ~ richness  + kelp  + prev.kelp  + spring_canopy_150
', 
  data = kelp)
# Summary output with standardized coefficients and Modification Indices
summary(sem.model1, standardized = T, rsq = T, modindices=TRUE)

#Adjust the model structure based on the modification indice and re-fit the model
sem.model2 <- sem(
  model = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int 

  kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150

  richness ~ kelp   + prev.kelp  + spring_canopy_150 + wave_kelp_int

  linkdensity ~ richness  + kelp  + prev.kelp  + spring_canopy_150', 
  data = kelp)
summary(sem.model2, standardized = T, rsq = T, modindices=TRUE)

#Compare the two model
anova(sem.model1, sem.model2)


#################### Fit the model by piecewiseSEM ##########################

# Fit piecewise model with random effect
sem.mod3 <- psem( 
  lme(spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int, 
      random = ~ 1 | SITE, data = kelp),
  lme(kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150,
      random = ~ 1 | SITE, data = kelp),
  lme(richness ~ kelp + prev.kelp + spring_canopy_150, 
      random = ~ 1 | SITE, data = kelp),
  lme(linkdensity ~ richness + kelp + prev.kelp  + spring_canopy_150, 
      random = ~ 1 | SITE, data = kelp), 
  data = kelp)
#Print the model result
summary(sem.mod3)


