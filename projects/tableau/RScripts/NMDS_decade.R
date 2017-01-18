#-----------------------------------------------------
#---------Data Import and Transformation--------------
#-----------------------------------------------------

# Import data in longform
raw <-read.csv("/Sara/SaraGitHub/skammlade.github.io/projects/tableau/datafiles/QuickCropDataAvgValueByDecade.csv", 
               stringsAsFactors = FALSE,
               strip.white = TRUE,
               na.strings = c("NA",""))

head(raw)
tail(raw)

colnames(raw)<-c("CountryName","ItemName", "ElementName", "Decade","AvgValueForDecade")

# save Country as a factor in new column 'fCountry', as some of the statistical methods require factors
raw<-within(raw, 
            fCountry<-as.factor(CountryName))

# Calculate total value of (kcal, grams, etc.) across all crops for each country in each year
countryDecadeSums<-aggregate(raw$AvgValueForDecade, 
                             by=list(raw$CountryName, 
                                     raw$Decade, 
                                     raw$ElementName), 
                             FUN=sum,
                             na.rm=TRUE)

head(countryDecadeSums)

colnames(countryDecadeSums)<-c("CountryName","Decade","ElementName","CountryDecadeSumValue")
crops<-merge(raw,countryDecadeSums)

head(crops)

#Grand total of calories in each year, all countries combined
globalSum<-aggregate(crops$AvgValueForDecade, 
                           by=list(crops$ElementName, 
                                   crops$Decade), 
                           FUN=sum,na.rm=TRUE)

colnames(globalSum)<-c("ElementName","Decade","GlobalSum")
crops<-merge(crops,
             globalSum,
             by=c("Decade","ElementName"))

head(crops)
tail(crops)

#Calculate proportion total for each crop
crops$Proportion<-crops$AvgValueForDecade/crops$CountryDecadeSumValue

#Add presence-absence - NOTE THAT THIS ASSUMES NA's ARE 0's!
crops$pres.abs<-0
crops$pres.abs[crops$AvgValueForDecade>0]<-1

#-------------------------------------------------------
#--------- Change in species Composition----------------
#-------------------------------------------------------

#create data subsets by ElementName
cropsCalories<-subset(crops, ElementName=="Calories")
cropsFat<-subset(crops, ElementName=="Fat")
cropsProtein<-subset(crops, ElementName=="Protein")
cropsFoodWeight<-subset(crops, ElementName=="Food Weight")

head(cropsCalories)
  
#convert missing values to zeros
cropsCalories$AvgValueForDecade[is.na(cropsCalories$AvgValueForDecade)]<-0
cropsFat$AvgValueForDecade[is.na(cropsFat$AvgValueForDecade)]<-0
cropsProtein$AvgValueForDecade[is.na(cropsProtein$AvgValueForDecade)]<-0
cropsFoodWeight$AvgValueForDecade[is.na(cropsFoodWeight$AvgValueForDecade)]<-0

#pivot datatable
pivotCropsCalories<-dcast(cropsCalories, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFat<-dcast(cropsFat, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsProtein<-dcast(cropsProtein, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFoodWeight<-dcast(cropsFoodWeight, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")

#remove country and decade columns
dataNMDSDecadesCropsCalories<-pivotCropsCalories[,3:56]
dataNMDSDecadesCropsFat<-pivotCropsFat[,3:56]
dataNMDSDecadesCropsProtein<-pivotCropsProtein[,3:56]
dataNMDSDecadesCropsFoodWeight<-pivotCropsFoodWeight[,3:56]

#open vegan library
#https://cran.r-project.org/web/packages/vegan/vegan.pdf
library(vegan)

#Nonmetric multidimensional scaling with stable solution from random starts, axis scaling, and species scores
MDSCalories<-metaMDS(dataNMDSDecadesCropsCalories,
                        distance="bray",
                        k=2, 
                        trymax = 20, 
                        stratmax=0.9999999999 )

MDSFat<-metaMDS(dataNMDSDecadesCropsFat,
                     distance="bray",
                     k=2, 
                     trymax = 20, 
                     stratmax=0.9999999999 )

MDSProtein<-metaMDS(dataNMDSDecadesCropsProtein,
                distance="bray",
                k=2, 
                trymax = 20, 
                stratmax=0.9999999999 )

MDSFoodWeight<-metaMDS(dataNMDSDecadesCropsFoodWeight,
                    distance="bray",
                    k=2, 
                    trymax = 20, 
                    stratmax=0.9999999999 )

#stressplot to visualize fit of ordination
stressplot(MDSCalories)
stressplot(MDSFat)
stressplot(MDSProtein)
stressplot(MDSFoodWeight)

#Stress test to show decrease in ordination stress with an increas in the number of dimensions
#https://cran.r-project.org/web/packages/goeveg/goeveg.pdf
library(goeveg)
dimcheckMDS(dataNMDSDecadesCropsCalories, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsFat, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsProtein, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsFoodWeight, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)

#create variable 'NMDSGroups' with Country and Decade information
NMDSGroups<-pivotCropsCalories[,1:2]

#return NMDS coordinates for all Measurements including Country and Decade information
NMDSCoordinatesCalories<-data.frame(MDS1 = MDSCalories$points[,1], 
                                     MDS2 = MDSCalories$points[,2],
                                     group=NMDSGroups$Decade,
                                     Country=NMDSGroups$Country)
NMDSCoordinatesFat<-data.frame(MDS1 = MDSFat$points[,1], 
                                    MDS2 = MDSFat$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$Country)
NMDSCoordinatesProtein<-data.frame(MDS1 = MDSProtein$points[,1], 
                                    MDS2 = MDSProtein$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$Country)
NMDSCoordinatesFoodWeight<-data.frame(MDS1 = MDSFoodWeight$points[,1], 
                                    MDS2 = MDSFoodWeight$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$Country)

#add column 'ElementName' to dataframes
NMDSCoordinatesCalories$ElementName <- rep("Calories", nrow(NMDSCoordinatesCalories))
NMDSCoordinatesFat$ElementName <- rep("Fat", nrow(NMDSCoordinatesFat))
NMDSCoordinatesProtein$ElementName <- rep("Protein", nrow(NMDSCoordinatesProtein))
NMDSCoordinatesFoodWeight$ElementName <- rep("Food Weight", nrow(NMDSCoordinatesFoodWeight))

#merge all measurement coordinates dataframes vertically into one dataframe
NMDSCoordinatesDecadesAllMeasurements <- rbind(NMDSCoordinatesCalories,
                                NMDSCoordinatesFat,
                                NMDSCoordinatesProtein,
                                NMDSCoordinatesFoodWeight)

#export dataframe to csv file
write.csv(NMDSCoordinatesDecadesAllMeasurements, "NMDSCoordinatesDecadesAllMeasurements.csv")


#----------------------------------------------------------
#---------Additional scripts to obtain ellipses------------
#----------------------------------------------------------

#Get species and site scores
speciesScoreCalories<-scores(MDSCalories,display="species",shrink=shrink)
speciesScoreFat<-scores(MDSFat,display="species",shrink=shrink)
speciesScoreProtein<-scores(MDSProtein,display="species",shrink=shrink)
speciesScoreFoodWeight<-scores(MDSFoodWeight,display="species",shrink=shrink)

countryScoreCalories<-scores(MDSCalories,display="sites")
countryScoreFat<-scores(MDSFat,display="sites")
countryScoreProtein<-scores(MDSProtein,display="sites")
countryScoreFoodWeight<-scores(MDSFoodWeight,display="sites")


#Work out ranges
ylimyCalories<-range(speciesScoreCalories[,2],countryScoreCalories[,2])
xlimxCalories<-range(speciesScoreCalories[,1],countryScoreCalories[,1])

ylimyFat<-range(speciesScoreFat[,2],countryScoreFat[,2])
xlimxFat<-range(speciesScoreFat[,1],countryScoreFat[,1])

ylimyProtein<-range(speciesScoreProtein[,2],countryScoreProtein[,2])
xlimxProtein<-range(speciesScoreProtein[,1],countryScoreProtein[,1])

ylimyFoodWeight<-range(speciesScoreFoodWeight[,2],countryScoreFoodWeight[,2])
xlimxFoodWeight<-range(speciesScoreFoodWeight[,1],countryScoreFoodWeight[,1])


