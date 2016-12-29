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


pivotCropsCalories<-dcast(cropsCalories, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFat<-dcast(cropsFat, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsProtein<-dcast(cropsProtein, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFoodWeight<-dcast(cropsFoodWeight, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")

dataNMDSDecadesCropsCalories<-pivotCropsCalories[,3:56]
dataNMDSDecadesCropsFat<-pivotCropsFat[,3:56]
dataNMDSDecadesCropsProtein<-pivotCropsProtein[,3:56]
dataNMDSDecadesCropsFoodWeight<-pivotCropsFoodWeight[,3:56]


library(vegan)

MDSCalories<-metaMDS(dataNMDSDecadesCropsCalories,
                        distance="bray",
                        k=2, 
                        trymax = 20, 
                        stratmax=0.9999999999 )
stressplot(MDSCalories)

MDSFat<-metaMDS(dataNMDSDecadesCropsFat,
                     distance="bray",
                     k=2, 
                     trymax = 20, 
                     stratmax=0.9999999999 )
stressplot(MDSFat)

MDSProtein<-metaMDS(dataNMDSDecadesCropsProtein,
                distance="bray",
                k=2, 
                trymax = 20, 
                stratmax=0.9999999999 )
stressplot(MDSProtein)


MDSFoodWeight<-metaMDS(dataNMDSDecadesCropsFoodWeight,
                    distance="bray",
                    k=2, 
                    trymax = 20, 
                    stratmax=0.9999999999 )
stressplot(MDSFoodWeight)


#Stress test to show decrease in ordination stress with an increas in the number of dimensions
#https://cran.r-project.org/web/packages/goeveg/goeveg.pdf
library(goeveg)
dimcheckMDS(dataNMDSDecadesCropsCalories, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsFat, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsProtein, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
dimcheckMDS(dataNMDSDecadesCropsFoodWeight, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)

#Get the species and site scores
speciesScoreCalories<-scores(MDSCalories,display="species",shrink=shrink)
speciesScoreFat<-scores(MDSFat,display="species",shrink=shrink)
speciesScoreProtein<-scores(MDSProtein,display="species",shrink=shrink)
speciesScoreFoodWeight<-scores(MDSFoodWeight,display="species",shrink=shrink)

countryScoreCalories<-scores(MDSCalories,display="sites")
countryScoreFat<-scores(MDSFat,display="sites")
countryScoreProtein<-scores(MDSProtein,display="sites")
countryScoreFoodWeight<-scores(MDSFoodWeight,display="sites")


#Work out the ranges
ylimyCalories<-range(speciesScoreCalories[,2],countryScoreCalories[,2])
xlimxCalories<-range(speciesScoreCalories[,1],countryScoreCalories[,1])

ylimyFat<-range(speciesScoreFat[,2],countryScoreFat[,2])
xlimxFat<-range(speciesScoreFat[,1],countryScoreFat[,1])

ylimyProtein<-range(speciesScoreProtein[,2],countryScoreProtein[,2])
xlimxProtein<-range(speciesScoreProtein[,1],countryScoreProtein[,1])

ylimyFoodWeight<-range(speciesScoreFoodWeight[,2],countryScoreFoodWeight[,2])
xlimxFoodWeight<-range(speciesScoreFoodWeight[,1],countryScoreFoodWeight[,1])


