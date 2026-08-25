install.packages("wallace")
library(wallace)
run_wallace()
install.packages("ggbreak")
library(ggbreak)
#GWC Gall Analysis


library(ggplot2)
library(dplyr)

#Load in data ##############################################
#Load the compiled dataset 
dat <- read.csv("~/Documents/Grad/Research/Research Analysis/Data Files/GWC July Sampling LDE, EMH 8.30.2024.csv")

#checking that dat reads in properly
colnames(dat)
#[1] "Accession_."             "Mapping_Order"           "Full_Name"               "Genus"                  
#[5] "Species"                 "Origin"                  "Location"                "Latitude"               
#[9] "Longitude"               "Height_.ft."             "Sun_Exposure_..."        "Distance_from_Road_.ft."
#[13] "Leaf_Accessibility"      "Gall_Presence"           "total_galls"             "Gall_Species_1"         
#[17] "X._galls_1"              "Gall_Species_2"          "X._galls_2"              "Gall_Species_3"         
#[21] "X._galls_3"              "Gall_Species_4"          "X._galls_4"              "Gall_Species_5"         
#[25] "X._galls_5"              "Notes"                   "Lichen_Presence"         "Blue.FlavoCap_Log."     
#[29] "GreenFlavoCap_Log."      "GrayFlavoCap_Log."       "MintGreenLeafy_Log."     "BrightGreenPowder_Log." 
#[33] "LightGreenPowder_Log."   "GrayGreenPowder_Log."    "GreyPowder_Log."         "MintGreenPowder_Log."   
#[37] "NeonGreen_Log."          "Physcia_Log."            "WhitePhysciaPowder_Log." "BlueGreenSpeckled_Log." 
#[41] "GrayBlueCrust_Log."      "Degrees_Measured" 

#Subset quercus at GWC
species_list <- c("Quercus alba", "Quercus robur", "Quercus palustris", "Quercus acutissima")
GWC_quercus <- subset(dat, Full_Name %in% species_list)

unique(GWC_quercus$Full_Name)
nrow(GWC_quercus)
#load the LeafByte data
#Original format, with leaf#s and averages
LeafByte  <-read.csv("~/Documents/Grad/Research/Research Analysis/Data Files/GWC July Sampling LDE, EMH 7.30.24 LeafByte.csv")
#"Desktop/GWC trees, Summer 2024/Liam's GWC tree data R/GWC July Sampling LDE, EMH 7.30.24 LeafByte.csv")
#EMH reorganized into single columns
LeafByte2 <-read.csv("~/Documents/Grad/Research/Research Analysis/Data Files/GWC July Sampling LDE, EMH 8.1.24 LeafByte.csv")

colnames(LeafByte)
# [1] "Accession_."             "Mapping_Order"           "Species"                
#[4] "Latitude"                "Longitude"               "Height_.ft."            
#[7] "Sun_Exposure_..."        "Distance_from_Road_.ft." "Leaf_Accessibility"     
#[10] "Gall_Presence"           "Herbivory_Avg"           "Leaf_1"                 
#[13] "Leaf_2"                  "Leaf_3"                  "Leaf_4"                 
#[16] "Leaf_5"                  "Leaf_6"                  "Leaf_7"                 
#[19] "Leaf_8"                  "Leaf_9"                  "Leaf_10" 

colnames(LeafByte2)
#[1] "Accession_No"          "Mapping_Order"         "Full_name"            
#[4] "Genus"                 "Species"               "Origin"               
#[7] "Leaf_no"               "Latitude"              "Longitude"            
#[10] "Height_ft"             "Sun_Exposure_percent"  "Distance_from_Road_ft"
#[13] "Leaf_Accessibility"    "Gall_Presence"         "Leaf_Herbivory"

#Load the iNaturalist Brooklyn data
#~/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files
iNat <-read.csv("/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/iNaturalist Brooklyn Data.csv")

colnames(iNat)
#[1] "Taxa"                 "Arthropod_full_name"  "Arthropod_Genus"      "Arthropod_Species"   
#[5] "Host_Plant_full_name" "Host_Plant_Genus"     "Host_Plant_Species"   "Host_Plant_Origin"   
#[9] "Count"                "Notes"   

#Condensed iNaturalist Brooklyn data (without fuzziness, aka host plants to genus or N/A)
#~/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files
iNat_condensed <-read.csv("/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/Condensed iNaturalist Brooklyn Galling.csv")

colnames(iNat_condensed)
#[1] "Taxa"       "Arthropod"  "Host.Plant" "Origin"     "Count" 

#Load the NYBG Herbaria Galling and Herbivory data
#NYBG Galling Data
#/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/NYBG Galling Data.csv
NYBG_galls <-read.csv("/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/NYBG Galling Data.csv")

colnames(NYBG_galls)
#[1] "occurrenceID"                  "institutionCode"               "basisOfRecord"                 "catalogNumber"              
#[5] "recordedBy"                    "recordNumber"                  "year"                          "month"                      
#[9] "day"                           "continent"                     "country"                       "stateProvince"            
#[13] "county"                        "locality"                      "decimalLatitude"               "decimalLongitude"          
#[17] "georeferenceProtocol"          "coordinateUncertaintyInMeters" "kingdom"                       "phylum"                    
#[21] "class"                         "order"                         "family"                        "genus"                     
#[25] "specificEpithet"               "infraspecificEpithet"          "scientificName"                "identifiedBy"              
#[29] "yearIdentified"                "dcterms.references"            "associatedMedia"               "Origin"                       
#[33] "Gall.Presence..Y.N."           "Notes"                         "Total.Galls"                   "Gall.Species.1"                
#[37] "Number.1"                      "Gall.Species.2"                "Number.2"                      "Gall.Species.3"
#[41] "Number.3"                     

#NYBG Herbivory Data
#/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/NYBG Herbivory Data.csv
NYBG_herb <-read.csv("/Users/liamengel/Documents/Grad/Research/Research Analysis/Data Files/NYBG Herbivory Data.csv")

colnames(NYBG_herb)
#[1] "occurrenceID"                  "institutionCode"               "basisOfRecord"                 "catalogNumber"              
#[5] "recordedBy"                    "recordNumber"                  "year"                          "month"                      
#[9] "day"                           "continent"                     "country"                       "stateProvince"              
#[13] "county"                        "locality"                      "decimalLatitude"               "decimalLongitude"           
#[17] "georeferenceProtocol"          "coordinateUncertaintyInMeters" "kingdom"                       "phylum"                     
#[21] "class"                         "order"                         "family"                        "genus"                      
#[25] "specificEpithet"               "infraspecificEpithet"          "scientificName"                "identifiedBy"               
#[29] "yearIdentified"                "dcterms.references"            "associatedMedia"               "Notes"                      
#[33] "Herbivory.Avg"                 "Leaf.1"                        "Leaf.2"                        "Leaf.3"                     
#[37] "Leaf.4"                        "Leaf.5"                        "Leaf.6"                        "Leaf.7"                    
#[41] "Leaf.8"                        "Leaf.9"                        "Leaf.10"


##############################################
################### Gall data
##############################################
#Rename species in the GWC dat file
library(dplyr)

dat <- dat %>%
  mutate(Full_Name = recode(Full_Name,
                          "Quercus_alba" = "Quercus alba",
                          "Quercus_robur" = "Quercus robur",
                          "Acer_rubrum" = "Acer rubrum",
                          "Acer_platanoides" = "Acer platanoides",
                          "Tilia_americana" = "Tilia americana",
                          "Tilia_cordata" = "Tilia cordata",
                          "Quercus_acutissima" = "Quercus acutissima",
                          "Quercus_palustris" = "Quercus palustris"))

#separate the genera into individual datafiles####
quercus1 <- dat[1:20,]
pinus1   <- dat[21:40,]
acer1    <- dat[41:60,]
tilia1   <- dat[61:80,] #removed the extra tree
picea1   <- dat[81:100,]
quercus2 <- dat[101:120,]
prunus1  <- dat[121:140,]
quercus3 <- dat[141:169,] #BBG samples
gwc_data <-dat[1:140,]

#stats playing for GWC gall data ##############################################
#quick stats on total galls by species within each of the genera. tests for significance of non-parametric (non-normal) distribution of number of galls compared to 0...null hypothesis is that there is no difference in distribution compared to mean of 0###

wilcox.test(total_galls ~ Origin, data=quercus1)
#W = 85, p-value = 0.002201
wilcox.test(total_galls ~ Species, data=quercus2)
#W = 100, p-value = 6.386e-05
wilcox.test(total_galls ~ Origin, data=pinus1)
#W = 50, p-value = NA
wilcox.test(total_galls ~ Origin, data=acer1) #55 when testing Origin
#W = 45, p-value = 0.3681
wilcox.test(total_galls ~ Origin, data=tilia1)
#W = 87.5, p-value = 0.004622
wilcox.test(total_galls ~ Origin, data=picea1)
#W = 50, p-value = NA
wilcox.test(total_galls ~ Origin, data=prunus1)
#W = 50, p-value = NA

#For all GWC
wilcox.test(total_galls ~ Origin, data=gwc_data)
#W = 3222.5, p-value = 1.528e-05

#Extra stats tests for GWC#############
#see pg 157; https://rcompanion.org/documents/RCompanionBioStatistics.pdf
library(nlme)
#ANOVA is used to test for diff btw means of mult. categories (3+), then must do a post-hoc test (Tukey) to test which of the categories is causing the difference
#model specifies that there could be a random effect caused by the random-sampling of the accession number. 
model=lme(data=quercus1, 
          total_galls ~ Species, random=~1|Accession_.,
          method="REML")  #Leaf size ~ Species + (1|Tree/Species)
anova.lme(model, type="sequential", adjustSigma=F)

GWC_oak_ANOVA <- aov(total_galls ~ Full_Name, data = GWC_quercus)
summary(GWC_oak_ANOVA)
#Df Sum Sq Mean Sq F value Pr(>F)
#Full_Name    3  13204    4401   2.599 0.0625 .
#Residuals   50  84682    1694

plot(GWC_oak_ANOVA)
#dataset is not normal, so need to use Kruskal-Wallis test instead

#non-parametric test
kruskal.test(total_galls ~ Full_Name, data = GWC_quercus)
#data:  total_galls by Full_Name
#Kruskal-Wallis chi-squared = 41.498, df = 3, p-value = 5.127e-09

#pairwise comparison for non-parametric test, using p adjust "fdr" 
pairwise.wilcox.test(quercus_allGWC$total_galls, quercus_allGWC$Full_Name,
                     p.adjust.method = "bonferroni")

#                     Quercus_alba   Quercus_palustris   Quercus_robur
#Quercus_palustris     0.22543        -                   -            
#Quercus_robur         0.00092        0.00016             -            
#Quercus_acutissima    0.00092        0.00016             -      

wilcox.test(GWC_robur$total_galls, GWC_acutissima$total_galls)

##############################################
###################Plots of GWC gall data ##
#create a series of plots showing galling within each genus, color-code native/non-native trees in plot ####
#cowplot allows you to put multiple plots side by side
library(cowplot)
#RColorBrewer gives nice color options
library(RColorBrewer)
#car is an example data set to use to play with options
library(car)

#All plots here have log-transformed gall data, +1 (b/c many zeros)
querc1pl<- 
  ggplot(quercus1, aes(y=log(total_galls+1), x=Full_Name, fill=Origin)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) )

querc2pl<- 
  ggplot(quercus2, aes(y=log(total_galls+1), x=factor(Full_Name, levels=unique(Full_Name)), fill=Origin)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) )

#subset to get all the quercus in GWC
which(dat$Genus=="Quercus") #1-20, 101-120, 141-169 From 141 onwards is BBG
quercus_allGWC <- dat[c(1:20, 101:120),]

#Reorder the trees to plot non-ABC order
quercus_allGWC$Full_Name <- factor(quercus_allGWC$Full_Name, 
                                   levels=c("Quercus alba", "Quercus palustris", 
                                             "Quercus robur", "Quercus acutissima"))

quercAllGWC <- 
  ggplot(quercus_allGWC, aes(y=log(total_galls+1), x=Full_Name, fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) ) +
  annotate("text", x = 2.5, y = 5.5, label = "*", size = 10) +
  coord_cartesian(ylim = c(0, 5.75))

quercAllGWC

acer1pl<- 
  ggplot(acer1, aes(y=log(total_galls+1), x=factor(Full_Name, levels=unique(Full_Name)), fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0.02, alpha = 0.4, size = 1.5) +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "", fill = NULL) +
  theme(legend.position="none", #legend.position="bottom", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 11),
        axis.text = element_text(size = 16) ) +
  coord_cartesian(ylim = c(0, 2))

acer1pl

tilia1pl<- 
  ggplot(tilia1, aes(y=log(total_galls+1), x=Full_Name, fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 11),
        axis.text = element_text(size = 16) ) +
  annotate("text", x = 1.5, y = 3.5, label = "*", size = 10) +
  coord_cartesian(ylim = c(0, 3.6))

tilia1pl

prunus1pl<- #no galls!
pinus1pl<- #no galls! 
picea1pl<- #no galls!  
##### plotting gall data ##########
#Plot to have all quercus data on top, and tilia/acer on the bottom row
bottom_row <- plot_grid(tilia1pl, acer1pl, labels = c('B', 'C'), label_size = 12)

plot_grid(quercAllGWC, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)


#Running statistics ####
##Tukey Model
model <- aov(total_galls ~ Genus + Species, data=dat)
summary(model)
TukeyHSD(model)

#$Genus
#                   diff        lwr      upr     p adj
#Picea-Acer    -2.500000e-01 -20.872392 20.37239 0.9999997
#Pinus-Acer    -2.500000e-01 -21.122370 20.62237 0.9999997
#Quercus-Acer   1.470000e+01  -6.172370 35.57237 0.2939513
#Tilia-Acer     1.108333e+01  -9.539059 31.70573 0.5680291
#Pinus-Picea   -1.243450e-14 -20.622392 20.62239 1.0000000
#Quercus-Picea  1.495000e+01  -5.672392 35.57239 0.2660624 (closest to SSD)
#Tilia-Picea    1.133333e+01  -9.036014 31.70268 0.5341967
#Quercus-Pinus  1.495000e+01  -5.922370 35.82237 0.2775757 (closest to SSD)
#Tilia-Pinus    1.133333e+01  -9.289059 31.95573 0.5463665
#Tilia-Quercus -3.616667e+00 -24.239059 17.00573 0.9882948

#$Species
#                             diff       lwr       upr     p adj
#alba-abies               1.495000e+01 -19.44804 49.348035 0.9213141 
#americana-abies          8.466667e+00 -25.93137 42.864702 0.9984574
#cordata-abies           -7.696970e+00 -41.30414 25.910201 0.9991262
#glauca-abies            -4.352074e-14 -33.60717 33.607170 1.0000000 (Picea in-genus not SSD)
#platanoides-abies       -2.500000e-01 -34.64804 34.148035 1.0000000
#robur-abies             -1.495000e+01 -49.34804 19.448035 0.9213141
#rubrum-abies             2.500000e-01 -34.14804 34.648035 1.0000000
#strobus-abies           -3.463896e-14 -34.39804 34.398035 1.0000000
#wallichiana-abies       -2.664535e-14 -34.39804 34.398035 1.0000000
#americana-alba          -6.483333e+00 -40.88137 27.914702 0.9998209
#cordata-alba            -2.264697e+01 -56.25414 10.960201 0.4742950
#glauca-alba             -1.495000e+01 -48.55717 18.657170 0.9102609
#platanoides-alba        -1.520000e+01 -49.59804 19.198035 0.9135569
#robur-alba              -2.990000e+01 -64.29804  4.498035 0.1456918 (*Quercus in-genus marginal SSD)
#rubrum-alba             -1.470000e+01 -49.09804 19.698035 0.9285979
#strobus-alba            -1.495000e+01 -49.34804 19.448035 0.9213141
#wallichiana-alba        -1.495000e+01 -49.34804 19.448035 0.9213141
#cordata-americana       -1.616364e+01 -49.77081 17.443534 0.8636207 (Tilia in-genus not SSD)
#glauca-americana        -8.466667e+00 -42.07384 25.140504 0.9981511
#platanoides-americana   -8.716667e+00 -43.11470 25.681369 0.9980655
#robur-americana         -2.341667e+01 -57.81470 10.981369 0.4593437
#rubrum-americana        -8.216667e+00 -42.61470 26.181369 0.9987806
#strobus-americana       -8.466667e+00 -42.86470 25.931369 0.9984574
#wallichiana-americana   -8.466667e+00 -42.86470 25.931369 0.9984574
#glauca-cordata           7.696970e+00 -25.10027 40.494210 0.9989395
#platanoides-cordata      7.446970e+00 -26.16020 41.054140 0.9993290
#robur-cordata           -7.253030e+00 -40.86020 26.354140 0.9994574
#rubrum-cordata           7.946970e+00 -25.66020 41.554140 0.9988741
#strobus-cordata          7.696970e+00 -25.91020 41.304140 0.9991262
#wallichiana-cordata      7.696970e+00 -25.91020 41.304140 0.9991262
#platanoides-glauca      -2.500000e-01 -33.85717 33.357170 1.0000000
#robur-glauca            -1.495000e+01 -48.55717 18.657170 0.9102609
#rubrum-glauca            2.500000e-01 -33.35717 33.857170 1.0000000
#strobus-glauca           8.881784e-15 -33.60717 33.607170 1.0000000
#wallichiana-glauca       1.687539e-14 -33.60717 33.607170 1.0000000
#robur-platanoides       -1.470000e+01 -49.09804 19.698035 0.9285979
#rubrum-platanoides       5.000000e-01 -33.89804 34.898035 1.0000000 (Acer in-genus not SSD)
#strobus-platanoides      2.500000e-01 -34.14804 34.648035 1.0000000
#wallichiana-platanoides  2.500000e-01 -34.14804 34.648035 1.0000000
#rubrum-robur             1.520000e+01 -19.19804 49.598035 0.9135569
#strobus-robur            1.495000e+01 -19.44804 49.348035 0.9213141
#wallichiana-robur        1.495000e+01 -19.44804 49.348035 0.9213141
#strobus-rubrum          -2.500000e-01 -34.64804 34.148035 1.0000000
#wallichiana-rubrum      -2.500000e-01 -34.64804 34.148035 1.0000000
#wallichiana-strobus      7.993606e-15 -34.39804 34.398035 1.0000000 (Pinus in-genus not SSD)

# Did Shapiro test for environmental variables, all are non-normal
shapiro.test(dat$Height_.ft.[dat$Origin == "Native"])
 #W = 0.92294, p-value = 8.101e-05
shapiro.test(dat$Height_.ft.[dat$Origin == "Non-native"])
 #W = 0.92727, p-value = 0.0001468
shapiro.test(dat$Sun_Exposure_...[dat$Origin == "Native"])
#W = 0.86239, p-value = 2.25e-07
shapiro.test(dat$Sun_Exposure_...[dat$Origin == "Non-native"])
#W = 0.8198, p-value = 9.873e-09
shapiro.test(dat$Distance_from_Road_.ft.[dat$Origin == "Native"])
#W = 0.73966, p-value = 5.658e-11
shapiro.test(dat$Distance_from_Road_.ft.[dat$Origin == "Non-native"])
#W = 0.77631, p-value = 5.672e-10
shapiro.test(dat$Leaf_Accessibility_Score[dat$Origin == "Native"])
#W = 0.7588, p-value = 1.689e-10
shapiro.test(dat$Leaf_Accessibility_Score[dat$Origin == "Non-native"])
#W = 0.74032, p-value = 6.949e-11


library("car")


qqnorm(dat$Height_.ft.)
qqline(dat$Height_.ft.)
#looks normal
qqnorm(dat$Sun_Exposure_...)
qqline(dat$Sun_Exposure_...)

qqnorm(dat$Distance_from_Road_.ft.)
qqline(dat$Distance_from_Road_.ft.)

 #height
t.test(Height_.ft. ~ Origin, data = dat)
 #t = 1.6841, df = 161.69, p-value = 0.0941

 #sun exposure
t.test(Sun_Exposure_... ~ Origin, data = dat)
 #t = -0.7154, df = 166.02, p-value = 0.4754

 #distance from road
t.test(Distance_from_Road_.ft. ~ Origin, data = dat)
 #t = -0.53933, df = 166.71, p-value = 0.5904

 #leaf accessibility, need to make numeric to run t-test
dat$Leaf_Accessibility_Score <- as.numeric(factor(dat$Leaf_Accessibility, 
                                                  levels = c("low", "medium", "high")))
 #this makes low=1, med=2, and high=3
t.test(Leaf_Accessibility_Score ~ Origin, data = dat)
 #t = -2.6116, df = 166.82, p-value = 0.009833

aggregate(Leaf_Accessibility_Score ~ Origin, data = dat, FUN = mean)
 #Non-natives has a statistically significant high leaf accessibility
 #      Origin Leaf_Accessibility_Score
 #1     Native                 1.917647
 #2 Non-native                 2.261905

##GWC subset only
#Subset dat to be just GWC
subset_GWC <- subset(dat, Location == "GWC")

# Checked for normality, data is normal for both groups but kind of close
shapiro.test(subset_GWC$Height_.ft.[dat$Origin == "Native"])
 #W = 0.96015, p-value = 0.02551
shapiro.test(subset_GWC$Height_.ft.[dat$Origin == "Non-native"])
 #W = 0.93422, p-value = 0.001178

# Therefore, use t-test for each variable
#height
t.test(Height_.ft. ~ Origin, data = subset_GWC)
#t = 2.0308, df = 136.69, p-value = 0.04422
#significant difference by height between native and non-native
aggregate(Height_.ft. ~ Origin, data = subset_GWC, FUN = mean)
#Origin Height_.ft.
#1     Native    51.34286
#2 Non-native    44.58571

#sun exposure
t.test(Sun_Exposure_... ~ Origin, data = subset_GWC)
#t = -0.78262, df = 137.56, p-value = 0.4352

#distance from road
t.test(Distance_from_Road_.ft. ~ Origin, data = subset_GWC)
#t = -0.49073, df = 137.98, p-value = 0.6244

#leaf accessibility, made numeric to run t-test
t.test(Leaf_Accessibility_Score ~ Origin, data = subset_GWC)
#t = -2.9305, df = 136.12, p-value = 0.00397
#significant difference by leaf accessibility between native and non-native
aggregate(Leaf_Accessibility_Score ~ Origin, data = subset_GWC, FUN = mean)
#Origin Leaf_Accessibility_Score
#1     Native                 1.971429
#2 Non-native                 2.385714


##Leaf Chemistry subset only
Leafchem <-read.csv("/Users/liamengel/Documents/Grad/Research/Coding Work/Data Files/LeafChem.csv")

#column names
colnames(Leafchem)

#[1] "Accession.."             "Species"                 "Origin"                 
#[4] "Date.Sampled"            "Latitude"                "Longitude"              
#[7] "Height..ft."             "Sun.Exposure...."        "Distance.from.Road..ft."
#[10] "Leaf.Accessibility"      "Gall.Presence"  

# Checked for normality
shapiro.test(Leafchem$Height..ft.[Leafchem$Origin == "Native"])
#W = 0.92912, p-value = 0.04652
shapiro.test(Leafchem$Height..ft.[Leafchem$Origin == "Non-native"])
#W = 0.93422, p-value = 0.001178

# If using t-test for each variable
#height
t.test(Height..ft. ~ Origin, data = Leafchem)
#t = 0.14512, df = 55.508, p-value = 0.8851
#sample estimates:
#  mean in group Native mean in group Non-native 
# 48.80000                 48.06667 

t.test(Height..ft. ~ Origin, data = Leafchem, paired=T)
#data:  Height..ft. by Origin
#t = 0.13716, df = 29, p-value = 0.8919
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -10.20168  11.66834
#sample estimates:
#  mean difference 
#0.7333333 

# create ANOVA with Leafchem ~ Origin looking at environmental variables. 
Leafchem$Origin <- as.numeric(factor(Leafchem$Origin, 
                                     levels = c("Native", "Non-native")))

anova_leafchem <- aov(Origin ~ Height..ft. + Sun.Exposure.... + Distance.from.Road..ft. + Leaf_Accessibility_Score, data = Leafchem)

summary(anova_leafchem)
#Df Sum Sq Mean Sq F value Pr(>F)
#Height..ft.               1  0.005  0.0054   0.022  0.883
#Sun.Exposure....          1  0.572  0.5716   2.281  0.137
#Distance.from.Road..ft.   1  0.624  0.6238   2.489  0.120
#Leaf_Accessibility_Score  1  0.013  0.0128   0.051  0.822
#Residuals                55 13.786  0.2507               
#80 observations deleted due to missingness

par(mfrow = c(2,2)) #makes next plot 2 x 2
plot(anova_leafchem)
par(mfrow = c(1,1)) #returns following plots to 1 x 1
#are not exactly following lines, the residuals look to be not quite normal


##Would I need to run a different type of test instead, like MANOVA?
#run a MANOVA (Multivariate Analysis of Variance) to test the effect of Origin (native vs. non-native) on multiple dependent variables (Height..ft., Sun_Exposure_..., Distance_from_Road_.ft., Leaf_Accessibility_Score)

#leaf accessibility, need to make numeric to run t-test
Leafchem$Leaf_Accessibility_Score <- as.numeric(factor(Leafchem$Leaf.Accessibility, 
                                                  levels = c("low", "medium", "high")))
# Run the MANOVA model
manova_leafchem <- manova(cbind(Height..ft., Sun.Exposure...., Distance.from.Road..ft., Leaf_Accessibility_Score) ~ Origin, data = Leafchem)

# Summarize the MANOVA results
summary(manova_leafchem)

plot(manova_leafchem)

#sun exposure
t.test(Sun_Exposure_... ~ Origin, data = subset_GWC)
#t = -0.78262, df = 137.56, p-value = 0.4352

#distance from road
t.test(Distance_from_Road_.ft. ~ Origin, data = subset_GWC)
#t = -0.49073, df = 137.98, p-value = 0.6244

#leaf accessibility, made numeric to run t-test
t.test(Leaf_Accessibility_Score ~ Origin, data = subset_GWC)
#t = -2.9305, df = 136.12, p-value = 0.00397
#significant difference by leaf accessibility between native and non-native
aggregate(Leaf_Accessibility_Score ~ Origin, data = subset_GWC, FUN = mean)
#Origin Leaf_Accessibility_Score
#1     Native                 1.971429
#2 Non-native                 2.385714



##############################################
# BBG Gall data alone ##############################

#Load the compiled dataset
bbgdat <- read.csv("~/Documents/Grad/Research/Research Analysis/Data Files/BBG 2024 Sampling.csv")

#check that data reads in properly
colnames(bbgdat)

#[1] "Accession_."             "Mapping_Order"           "Full_Name"              
#[4] "Genus"                   "Species"                 "Origin"                 
#[7] "Location"                "Latitude"                "Longitude"              
#[10] "Height_.ft."             "Sun_Exposure_..."        "Distance_from_Road_.ft."
#[13] "Leaf_Accessibility"      "Gall_Presence"           "total_galls"            
#[16] "Gall_Species_1"          "X._galls_1"              "Gall_Species_2"         
#[19] "X._galls_2"              "Gall_Species_3"          "X._galls_3"             
#[22] "Gall_Species_4"          "X._galls_4"              "Gall_Species_5"         
#[25] "X._galls_5"              "Notes"                   "X"                      
#[28] "X.1"                     "X.2"                     "X.3"                    
#[31] "X.4"

#separate data by origin
bbgnative <-bbgdat[1:15,]
bbgnonnative <- bbgdat[16:29,]

#remove excess rows
bbgdat <- bbgdat[1:29, ]

#stats playing for BBG gall data ##############################################
#quick stats on total galls by origin (native or non-native). tests for significance of non-parametric (non-normal) distribution of number of galls compared to 0...null hypothesis is that there is no difference in distribution compared to mean of 0###
wilcox.test(total_galls ~ Origin, data=bbgdat)
#W = 189, p-value = 4.552e-05
#alternative hypothesis: true location shift is not equal to 0

#see pg 157; https://rcompanion.org/documents/RCompanionBioStatistics.pdf
library(nlme)
#ANOVA is used to test for diff btw means of mult. categories (3+), then must do a post-hoc test (Tukey) to test which of the categories is causing the difference
#model specifies that there could be a random effect caused by the random-sampling of the accession number. 

#possible ANOVAs - by species, GWC vs BBG, etc -- could be just for the natives


#for all natives: aov(total_galls ~ Species * Location (GWC vs BBG) * Tree height * Distance to Road * Sun Exposure * Leaf Accessibility, data= XX)

allgalls_AOV = aov(total_galls ~ Species * Location * Height_.ft. * Distance_from_Road_.ft. * Sun_Exposure_... * Leaf_Accessibility, data=dat )
summary(allgalls_AOV)


# data frame of oak galls from BBG
oak_galls = dat[dat$Genus == 'Quercus' & dat$Location == 'BBG' & dat$Origin == 'Native',]
oak_bbg_galls_AOV = aov(total_galls ~ Species * Height_.ft. * Distance_from_Road_.ft. * Sun_Exposure_... * Leaf_Accessibility, data=oak_galls)
summary(oak_bbg_galls_AOV)

plot(oak_bbg_galls_AOV)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Species                              3  22131    7377  365.58 0.0384 *
#Height_.ft.                          1   5569    5569  275.97 0.0383 *
#Distance_from_Road_.ft.              1   1822    1822   90.31 0.0667 .
#Sun_Exposure_...                     1    711     711   35.21 0.1063  
#Leaf_Accessibility                   2   1085     542   26.88 0.1351  
#Species:Height_.ft.                  3   1965     655   32.47 0.1281  
#Height_.ft.:Distance_from_Road_.ft.  1    290     290   14.37 0.1642  
#Height_.ft.:Sun_Exposure_...         1   1994    1994   98.82 0.0638 .
#Residuals                            1     20      20                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

cor.test(oak_galls$Sun_Exposure_..., oak_galls$Height_.ft.)

#Pearson's product-moment correlation:
#data:  oak_galls$Sun_Exposure_... and oak_galls$Height_.ft.
#t = -0.50371, df = 13, p-value = 0.6229
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6075603  0.4024263
#sample estimates:
#      cor 
#-0.138359 

#Helpful Note for ANOVA: start with + (treat each of these things independently, one does not drive the other)
#using * (probably what we want, treats them all as overlapping dependent variables, with all the possible overlap options) -> species * origin, for example, will prob be significant. along with their independent variables.

model=lme(data=bbgnative, 
          total_galls ~ Species, random=~1|Accession_.,
          method="REML")  #Leaf size ~ Species + (1|Tree/Species)
anova.lme(model, type="sequential", adjustSigma=F)

#numDF denDF  F-value p-value
#(Intercept)     1    11 11.68242  0.0057
#Species         3    11  6.03042  0.0111

#If I did that correctly, that means there are significant differences between the mean number of total galls  for native species
#what would the significance of the intercept mean?

library(multcompView)
library(lsmeans)
leastsquare=lsmeans(model, pairwise ~ Species, adjust="tukey")
cld(leastsquare, alpha=0.05, Letters=letters, adjust="tukey")
#could not find function cld



########make plot of BBG gall data#######

#cowplot allows you to put multiple plots side by side
library(cowplot)
#RColorBrewer gives nice color options
library(RColorBrewer)
#car is an example data set to use to play with options
library(car)

#All plots here have log-transformed gall data, +1 (b/c many zeros)
#plot of galls for all BBG species
bbgpl<- 
  ggplot(bbgdat, aes(y=log(total_galls+1), x=Full_Name, fill=Origin)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(labels = c("Quercus_acutissima" = "Q. acutissima", "Quercus_alba" = "Q. alba", "Quercus_castaneifolia" = "Q. castaneifolia", "Quercus_cerris" = "Q. cerris", "Quercus_coccinea" = "Q. coccinea", "Quercus_glandulifera var. brevipedunculata"= "Q. glandulifera", "Quercus_libani" = "Q. libani", "Quercus_myrsinifolia" = "Q. myrsinifolia","Quercus_palustris" = "Q. palustris", "Quercus_robur" = "Q. robur", "Quercus_rubra" = "Q. rubra", "Quercus_serrata" = "Q. serrata")) +  # Change labels
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic", angle = 45, hjust = 1),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) )

bbgpl

#plot of galls for native BBG species
bbgnativeplot<-
  ggplot(bbgnative, aes(y=log(total_galls+1), x=Full_Name, fill=Origin)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(labels = c("Quercus_alba" = "Q. alba", "Quercus_coccinea" = "Q. coccinea", "Quercus_palustris" = "Q. palustris", "Quercus_rubra" = "Q. rubra")) +  # Change labels
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) )
  
bbgnativeplot

#plot of galls on BBG natives vs. non-natives
bbgcomparisonplot <-
  ggplot(bbgdat, aes(y=log(total_galls+1), x=Origin, fill=Origin)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree +1)", fill = NULL) +
  scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(legend.position="none", 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16),
        axis.ticks = element_blank()) +
  annotate("text", x = 2.5, y = 5.5, label = "*", size = 10)

bbgcomparisonplot <-
  ggplot(bbgdat, aes(y=log(total_galls+1), x=Origin, fill=Origin)) + 
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 11),
        axis.text = element_text(size = 16)) +
  annotate("text", x = 1.5, y = 5.5, label = "*", size = 10)
bbgcomparisonplot

#######################################
##### NYBG Herbarium Data
#######################################
##NYBG Galling Data#####
##Data Clean-Up

# Replace "" with NA
NYBG_galls[NYBG_galls == ""] <- NA
# Then drop rows where all columns are NA
NYBG_galls <- NYBG_galls[!apply(is.na(NYBG_galls), 1, all), ]

##Test for significance of galling by origin
# Create table for galling by origin
NYBG_galls_table <- table(NYBG_galls$Origin, NYBG_galls$Gall.Presence..Y.N.)
NYBG_galls_table
#             N   Y
#Native      239  22
#Non-native  130   1

#Fisher's Exact Test -- testing if significantly more galling on natives than non-natives
fisher.test(NYBG_galls_table)
#p-value = 0.001125

##Test for number of galls by Species
str(NYBG_galls)
species_sums <- aggregate(Total.Galls ~ scientificName, data = NYBG_galls, sum, na.rm = TRUE)
species_sums
#Acer rubrum L.          46
#Quercus alba L.          20
#Quercus bicolor Willd.           2
#Quercus palustris Münchh.          10
#Quercus rubra L.          9
#Quercus velutina Lam.          25
#Tilia americana L.           7
#Tilia cordata Mill.           3

#Test for percentage of each species that was galled
total_species_counts <- NYBG_galls %>%
  count(scientificName, name = "Total_Trees")

galled_species_counts <- NYBG_galls %>%
  filter(Gall.Presence..Y.N. == "Y") %>%
  count(scientificName, name = "Galled_Trees")

NYBG_galled_trees_table <- left_join(total_species_counts, galled_species_counts, by = "scientificName")

NYBG_galled_trees_table

#plot of galls on NYBG natives vs. non-natives
nybgcomparisonplot<-
  ggplot(NYBG_galls, aes(y=log(Total.Galls+1), x=Origin, fill=Origin)) + 
  geom_boxplot() +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "Log(Total galled leaves per tree+1)", fill = NULL) +
  theme(legend.position="none", 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 10.5),
        axis.text = element_text(size = 16) )

nybgcomparisonplot


##NYBG Galling Timescale Plot#####
library(MASS)
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

# Clean data
NYBG_galls_clean <- NYBG_galls %>%
  filter(!is.na(year), !is.na(Total.Galls)) %>%
  mutate(year = as.numeric(as.character(year)))

# Fit Negative Binomial Model 
nb_model <- glm.nb(Total.Galls ~ year * Origin, data = NYBG_galls_clean)

# Create prediction grid
newdata <- expand.grid(
  year = seq(min(NYBG_galls_clean$year, na.rm = TRUE),
             max(NYBG_galls_clean$year, na.rm = TRUE),
             length.out = 200),
  Origin = unique(NYBG_galls_clean$Origin)
)

# Predict fitted values and CI
pred <- predict(nb_model, newdata, type = "link", se.fit = TRUE)
newdata <- newdata %>%
  mutate(
    fit   = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit)
  )

# Split data by Origin
newdata_native <- newdata %>% filter(Origin == "Native")

# Base plot with points, Native fitted line, and CI ribbon
base_plot <- ggplot(NYBG_galls_clean, aes(x = year, y = Total.Galls, color = Origin)) +
  geom_point(alpha = 0.5) +
  # CI ribbon only for Native
  geom_ribbon(data = newdata_native,
              aes(x = year, ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.4, inherit.aes = FALSE) +
  # Native regression line with matching legend color
  geom_line(data = newdata_native,
            aes(x = year, y = fit, color = Origin),
            size = 1) +
  scale_y_continuous(trans = "log1p") +
  theme_bw() +
  labs(x = "Year", y = "Total Galls (log scale)", color = "Origin")

# Lower range zoomed plot
p1 <- base_plot +
  coord_cartesian(ylim = c(0, 7)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Upper range zoomed plot 
p2 <- base_plot +
  coord_cartesian(ylim = c(10, 50)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# Combine plots with patchwork
combined <- (p2 / p1) + plot_layout(heights = c(1, 3), guides = "collect") &
  theme(legend.position = "bottom")

grid.newpage()
combined


#############################
library(MASS)
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

# --- 0. Clean data ---
NYBG_galls_clean <- NYBG_galls %>%
  filter(!is.na(year), !is.na(Total.Galls)) %>%
  mutate(year = as.numeric(as.character(year)))

# --- 1. Fit Negative Binomial Model ---
nb_model <- glm.nb(Total.Galls ~ year * Origin, data = NYBG_galls_clean)

# --- 2. Create prediction grid ---
newdata <- expand.grid(
  year = seq(min(NYBG_galls_clean$year, na.rm = TRUE),
             max(NYBG_galls_clean$year, na.rm = TRUE),
             length.out = 200),
  Origin = unique(NYBG_galls_clean$Origin)
)

# --- 3. Predict fitted values and CI ---
pred <- predict(nb_model, newdata, type = "link", se.fit = TRUE)
newdata <- newdata %>%
  mutate(
    fit   = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit)
  )

# --- Split data by Origin ---
newdata_native <- newdata %>% filter(Origin == "Native")

# --- 4. Base plot with points, Native fitted line, and CI ribbon ---
base_plot <- ggplot(NYBG_galls_clean, aes(x = year, y = Total.Galls, color = Origin)) +
  geom_point(alpha = 0.5) +
  # CI ribbon only for Native
  geom_ribbon(data = newdata_native,
              aes(x = year, ymin = lower, ymax = upper),
              fill = "gray70", alpha = 0.4, inherit.aes = FALSE) +
  # Native regression line with matching legend color
  geom_line(data = newdata_native,
            aes(x = year, y = fit, color = Origin),
            size = 1) +
  theme_bw() +
  labs(x = "Year", y = "Total Galls", color = "Origin")

# --- 5. Lower range zoomed plot ---
p1 <- base_plot +
  coord_cartesian(ylim = c(0, 7)) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# --- 6. Upper range zoomed plot ---
p2 <- base_plot +
  coord_cartesian(ylim = c(10, 50)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# --- 7. Combine plots with patchwork ---
combined <- (p2 / p1) + plot_layout(heights = c(1, 3), guides = "collect") &
  theme(legend.position = "bottom")

# --- 8. Draw final plot ---
grid.newpage()
combined

#Statistics for NYBG Galling Timescale Plot#####

summary(NYBG_galls$Total.Galls)
hist(NYBG_galls$Total.Galls)
#data is heavily overdispersed, since most data is 0's and a few have lots of galls. Therefore, use negative binomial test which assumes the variance is greater than the mean (unlike the similar Poisson model) 

#Test if zero-inflated model fits better than negative binomial
library(pscl)

zinb_model <- zeroinfl(Total.Galls ~ Origin | 1, 
                       data = NYBG_galls_clean, 
                       dist = "negbin")

summary(zinb_model)
AIC(nb_model, zinb_model)
vuong(nb_model, zinb_model)
#zero-inflated model does NOT fit significantly better, so not worth the added complexity, stick with Negative Binomial model

##OVERALL INTERPRETATION FOR USING THIS STAT: 
#A negative binomial GLM fits best because gall counts are highly variable and overdispersed, with many zeros but no evidence for a separate “always zero” group. The NB model captures that variability without overfitting.

#Test for significance of Origin over time
#Check native trees
native_data <- subset(NYBG_galls, Origin == "Native" & !is.na(year))
#use negative binomial model again
nb_time <- glm.nb(Total.Galls ~ year, data = native_data)
summary(nb_time)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -59.789352  13.958640  -4.283 1.84e-05 ***
#  year          0.029982   0.007111   4.216 2.48e-05 ***

##Stats with interactions from plot added in
library(MASS)
library(dplyr)

## --- 1. Fit negative binomial model with interaction ---
nb_model <- glm.nb(Total.Galls ~ year * Origin, data = NYBG_galls_clean)

summary(nb_model)

#NYBG Galling Histograms#####

#histogram of NYBG galling data with summary data
hist(NYBG_galls$year, breaks = 20); summary(NYBG_galls$year)

#histogram of natives only
hist(NYBG_galls$year[NYBG_galls$Origin=="Native"], breaks = 20); summary(NYBG_galls$year[NYBG_galls$Origin=="Native"])

#histogram of non-natives only
hist(NYBG_galls$year[NYBG_galls$Origin=="Non-native"], breaks = 20); summary(NYBG_galls$year[NYBG_galls$Origin=="Non-native"])

##histogram of NYBG herbivory data with summary data
hist(NYBG_herb$year, breaks = 20); summary(NYBG_herb$year)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1883    1918    1976    1965    2009    2016      13 

#plot of NYBG herbivory averages over time
plot(NYBG_herb$year, NYBG_herb$Herbivory.Avg)

plot(NYBG_herb$year[NYBG_herb$Origin=="Native"], NYBG_herb$Herbivory.Avg[NYBG_herb$Origin=="Native"])

plot(NYBG_herb$year[NYBG_herb$Origin=="Non-native"], NYBG_herb$Herbivory.Avg[NYBG_herb$Origin=="Non-native"])

plot(NYBG_herb$year[NYBG_herb$scientificName=="Prunus serotina"], NYBG_herb$Herbivory.Avg[NYBG_herb$scientificName=="Prunus serotina"])

plot(NYBG_herb$year[NYBG_herb$scientificName=="Prunus serrulata"], NYBG_herb$Herbivory.Avg[NYBG_herb$scientificName=="Prunus serrulata"])

#NYBG Herbivory Timescale Plot and Stats#########
for (i in 1:length(unique(NYBG_herb$scientificName))) {
  print(i)
  plot(NYBG_herb$year[NYBG_herb$scientificName == unique(NYBG_herb$scientificName)[i]], NYBG_herb$Herbivory.Avg[NYBG_herb$scientificName == unique(NYBG_herb$scientificName)[i]], main = unique(NYBG_herb$scientificName)[i], xlim=c(1880, 2020), ylim=c(0,14))
}
  #Prunus serrulata is cut off at the top (high herbivory outliers)
  
  #combine records (except Prunus) with native vs non-native
  library(ggplot2)
  library(dplyr)
  

##VERSION EXCLUDING PRUNUS -- used in paper
  # Exclude Prunus species
  herb_clean <- NYBG_herb %>%
    filter(!grepl("^Prunus", scientificName))
  
  # Subset by Origin
  natives <- herb_clean %>% filter(Origin == "Native")
  nonnatives <- herb_clean %>% filter(Origin == "Non-native")
  
  # Plot native vs non-native
  ggplot(herb_clean, aes(x = year, y = Herbivory.Avg, color = Origin)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(
      title = "Herbivory Over Time by Origin (Excluding Prunus)",
      x = "Year",
      y = "Herbivory Average",
      color = "Origin"
    )
  
  #zoomed in plot
  
  herbtimescale <- ggplot(herb_clean, aes(x = year, y = Herbivory.Avg, color = Origin)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Average Herbivory",
      color = "Origin"
    ) +
    coord_cartesian(ylim = c(-2, 12)) +
    scale_x_continuous(breaks = c(1900, 1950, 2000))

  herbtimescale
  
  # Combine original 'combined' plot (stacked gall plots) with herb_plot
  
  # Define layout grid manually
  layout <- "
  A#
  BC"
  
  # A = p2 (top-left)
  # B = p1 (bottom-left)
  # C = herb_plot (bottom-right)
  
  # Combine using custom layout and collect legends
  final_plot <- p2 + p1 + herbtimescale +
    plot_layout(design = layout, guides = "collect")
  
  # Draw the final plot
  grid.newpage()
  final_plot
  
######Other timescale herb plot and normality testing with diff exclusions#####
  ##VERSION WITH ALL DATA (INCL PLOT)
  # Subset by Origin
  natives <- NYBG_herb %>% filter(Origin == "Native")
  nonnatives <- NYBG_herb %>% filter(Origin == "Non-native")
  
  # Plot native vs non-native
  ggplot(NYBG_herb, aes(x = year, y = Herbivory.Avg, color = Origin)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_bw() +
    labs(
      title = "Herbivory Over Time by Origin (Excluding Prunus)",
      x = "Year",
      y = "Herbivory Average",
      color = "Origin"
    )
  

  ##TESTING NORMALITY WITH DIFFERENT EXCLUSIONS
  
  ##All data present
  herb_model_all <- lm(Herbivory.Avg ~ year * Origin, data = NYBG_herb, na.action = "na.omit")
  summary(herb_model_all)
  
  qqnorm(residuals(herb_model_all))
  qqline(residuals(herb_model_all), col = "red")
  #very skewed on right tail
  plot(fitted(herb_model_all), residuals(herb_model_all))
  abline(h = 0, col = "red")
  #big skew!
  
  ##Prunus excluded
  qqnorm(residuals(herb_model))
  qqline(residuals(herb_model), col = "red")
  #not very skewed
  shapiro.test(residuals(herb_model))
  #p=0.02
  plot(fitted(herb_model), residuals(herb_model))
  abline(h = 0, col = "red")
  ##bit of a funnel shape, possible skew
  
  ##Oak excluded
  herb_oakremoved <- NYBG_herb %>%
    filter(!grepl("^Quercus", scientificName))
  
  herb_model_querc <- lm(Herbivory.Avg ~ year * Origin, data = herb_oakremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_querc))
  qqline(residuals(herb_model_querc), col = "red")
  #very skewed on right tail
  plot(fitted(herb_model_querc), residuals(herb_model_querc))
  abline(h = 0, col = "red")
  #big skew!
  
  ##Oak + Prunus excluded
  herb_oakpruremoved <- NYBG_herb %>%
    filter(!grepl("^Quercus", scientificName),
           !grepl("^Prunus", scientificName))
  
  herb_model_quercpru <- lm(Herbivory.Avg ~ year * Origin, data = herb_oakpruremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_quercpru))
  qqline(residuals(herb_model_quercpru), col = "red")
  #
  plot(fitted(herb_model_quercpru), residuals(herb_model_quercpru))
  abline(h = 0, col = "red")
  #
  
  ##Maple excluded
  herb_mapleremoved <- NYBG_herb %>%
    filter(!grepl("^Acer", scientificName))
  
  herb_model_maple <- lm(Herbivory.Avg ~ year * Origin, data = herb_mapleremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_maple))
  qqline(residuals(herb_model_maple), col = "red")
  #very skewed on right tail
  plot(fitted(herb_model_maple), residuals(herb_model_maple))
  abline(h = 0, col = "red")
  #big skew!
  
  ##Maple + Prunus excluded
  herb_maplepruremoved <- NYBG_herb %>%
    filter(!grepl("^Acer", scientificName),
           !grepl("^Prunus", scientificName))
  
  herb_model_maplepru <- lm(Herbivory.Avg ~ year * Origin, data = herb_maplepruremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_maplepru))
  qqline(residuals(herb_model_maplepru), col = "red")
  #
  plot(fitted(herb_model_maplepru), residuals(herb_model_maplepru))
  abline(h = 0, col = "red")
  #
  
  ##Linden excluded
  herb_lindenremoved <- NYBG_herb %>%
    filter(!grepl("^Tilia", scientificName))
  
  herb_model_linden <- lm(Herbivory.Avg ~ year * Origin, data = herb_lindenremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_linden))
  qqline(residuals(herb_model_linden), col = "red")
  #very skewed on right tail
  plot(fitted(herb_model_linden), residuals(herb_model_linden))
  abline(h = 0, col = "red")
  #big skew!
  
  ##Linden + Prunus excluded
  herb_lindenpruremoved <- NYBG_herb %>%
    filter(!grepl("^Tilia", scientificName),
           !grepl("^Prunus", scientificName))
  
  herb_model_lindenpru <- lm(Herbivory.Avg ~ year * Origin, data = herb_lindenpruremoved, na.action = "na.omit")
  
  qqnorm(residuals(herb_model_lindenpru))
  qqline(residuals(herb_model_lindenpru), col = "red")
  #
  plot(fitted(herb_model_lindenpru), residuals(herb_model_lindenpru))
  abline(h = 0, col = "red")
 
##TESTING SIGNIFICANCE WITH PRUNUS EXCLUDED USING LINEAR MODEL ####
  #to test significance, use linear model
  
  #can use linear model because Shapiro-Wilk normality test and Q-Q plot
  # Linear model: test for time trend and interaction with Origin
  herb_model <- lm(Herbivory.Avg ~ year * Origin, data = herb_clean, na.action = "na.omit")
  summary(herb_model)
  #significant correlation between data and year, and between native and non-native

  #Coefficients:
   # Estimate Std. Error t value Pr(>|t|)  
  #(Intercept)            50.27367   22.70606   2.214   0.0320 *
   # year                   -0.02363    0.01159  -2.039   0.0475 *
  #  OriginNon-native      -81.39336   38.94997  -2.090   0.0425 *
  #  year:OriginNon-native   0.04018    0.01973   2.037   0.0477 *
  
  #to run lm as ANCOVA
  anova(herb_model)
  #Response: Herbivory.Avg
  #Df Sum Sq Mean Sq F value  Pr(>F)  
  #year         1  24.37  24.366  3.0886 0.08580 .
  #Origin       1  48.56  48.564  6.1559 0.01699 *
  #  year:Origin  1  32.72  32.722  4.1478 0.04773 *
  #  Residuals   44 347.12   7.889
  
  
  library(emmeans)
  
  # Estimate slopes (simple regressions) by Origin
  slopes <- emtrends(herb_model, ~ Origin, var = "year")
  slopes
  summary(slopes)
  
  #>   summary(slopes)
  #Origin     year.trend     SE df lower.CL  upper.CL
  #Native        -0.0236 0.0116 44  -0.0470 -0.000276
  #Non-native     0.0165 0.0160 44  -0.0156  0.048714
#Confidence level used: 0.95
  
  ##TEST FOR SIGNIFICANCE OF THE SLOPES
  
  # Model for natives
  native_model <- lm(Herbivory.Avg ~ year, data = natives)
  summary(native_model)
  
  #Multiple R-squared:  0.122,	Adjusted R-squared:  0.0821 
  #F-statistic: 3.057 on 1 and 22 DF,  p-value: 0.09432
  
  
  # Model for non-natives
  nonnative_model <- lm(Herbivory.Avg ~ year, data = nonnatives)
  summary(nonnative_model)
   ##below models the same thing, just wanted to check it a different way
  lm_nonnative_herb <- lm(Herbivory.Avg ~ year, data = subset(herb_clean, Origin == "Non-native"))
  summary(lm_nonnative_herb)
  
  #Multiple R-squared:  0.07088,	Adjusted R-squared:  0.02865 
  #F-statistic: 1.678 on 1 and 22 DF,  p-value: 0.2086
  
  


##NYBG Herbivory Stats#####

#First separate by genus
NYBGquercus <- NYBG_herb[21:40,]
NYBGtilia <- NYBG_herb[41:60,]
NYBGacer <- NYBG_herb[61:80,]
NYBGprunus <- NYBG_herb[1:20,]

##Wilcoxon tests for avg herbivory of each tree by genus
wilcox.test(Herbivory.Avg ~ scientificName, data= NYBGquercus)
#data:  Herbivory.Avg by scientificName
#W = 66.5, p-value = 0.2261
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(Herbivory.Avg ~ scientificName, data= NYBGtilia)
#data:  Herbivory.Avg by scientificName
#W = 78.5, p-value = 0.03416
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(Herbivory.Avg ~ scientificName, data= NYBGacer)
#data:  Herbivory.Avg by scientificName
#W = 23, p-value = 0.04499
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(Herbivory.Avg ~ scientificName, data= NYBGprunus)
#data:  Herbivory.Avg by scientificName
#W = 23, p-value = 0.04499
#alternative hypothesis: true location shift is not equal to 0


##NYBG Herbivory Histograms#####
###Need to get histograms to bin in the same way

#checking histogram of herbivory rates on native and non-native trees
par(mfrow=c(1,2))

# Histogram for Quercus alba
hist(NYBGquercus$Herbivory.Avg[NYBGquercus$scientificName == 'Quercus alba'],
     main = "Quercus alba",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "forestgreen")

# Histogram for Quercus cerris
hist(NYBGquercus$Herbivory.Avg[NYBGquercus$scientificName == 'Quercus cerris'],
     main = "Quercus cerris",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "firebrick")

##Note: Native alba eaten more heavily than non-native cerris

# Histogram for Tilia americana
hist(NYBGtilia$Herbivory.Avg[NYBGtilia$scientificName == 'Tilia americana'],
     main = "Tilia americana",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "forestgreen")

# Histogram for Tilia cordata
hist(NYBGtilia$Herbivory.Avg[NYBGtilia$scientificName == 'Tilia cordata'],
     main = "Tilia cordata",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "firebrick")

##Note: Native americana eaten more heavily than non-native cordata, but cordata graph looks messed up

# Histogram for Acer rubrum
hist(NYBGacer$Herbivory.Avg[NYBGacer$scientificName == 'Acer rubrum'],
     main = "Acer rubrum",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "forestgreen")

# Histogram for Acer platanoides
hist(NYBGacer$Herbivory.Avg[NYBGacer$scientificName == 'Acer platanoides'],
     main = "Acer platanoides",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 25),
     col = "firebrick")

##Note: Much higher herbivory on native rubrum than non-native platanoides

# Histogram for Prunus serotina
hist(NYBGprunus$Herbivory.Avg[NYBGprunus$scientificName == 'Prunus serotina'],
     main = "Prunus serotina",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 50),
     col = "forestgreen")

# Histogram for Prunus serrulata
hist(NYBGprunus$Herbivory.Avg[NYBGprunus$scientificName == 'Prunus serrulata'],
     main = "Prunus serrulata",
     xlab = "Herbivory Avg",
     ylim = c(0, 10),
     xlim = c(0, 50),
     col = "firebrick")

# More herbivory on non-native serrulata, those trees are a lot older, also having issue with binning


#NYBG Herbivory plotting ########################
library(cowplot)
library(RColorBrewer)
library(ggplot2)

#Plots have log-transformed herbivory + 1 (to avoid zeros messing up plots)
#removed y-axis title for quercus and prunus for visualization
quercusHerb<- 
  ggplot(NYBGquercus, 
         aes(y=log(Herbivory.Avg+1), 
             x=scientificName, fill=specificEpithet)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = " ", fill = NULL) +
  coord_cartesian(ylim = c(0, 3))

tiliaHerb<- 
  ggplot(NYBGtilia, 
         aes(y=log(Herbivory.Avg+1), 
             x=scientificName, fill=specificEpithet)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = "log(% leaf consumed+1)", fill = NULL) +
  annotate("text", x = 1.5, y = 2.9, label = "*", size = 10) +
  coord_cartesian(ylim = c(0, 3))

# Manually assign reversed colors (example colors from "Paired" palette)
custom_colors <- c("platanoides" = "#1F78B4", "rubrum" = "#A6CEE3")  # Reversed from original
acerHerb<- ###more complicatd 'x' line to switch rubrum and platanoides from alphabetical order
  ggplot(NYBGacer, 
         aes(y=log(Herbivory.Avg+1), 
             x=factor(scientificName, levels = rev(unique(scientificName))),  fill=specificEpithet)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_manual(values = custom_colors) +
  labs(x = " ", y = "log(% leaf consumed+1)", fill = NULL) +
  annotate("text", x = 1.5, y = 2.9, label = "*", size = 10) +
  coord_cartesian(ylim = c(0, 3))

prunusHerb<- 
  ggplot(NYBGprunus, 
         aes(y=log(Herbivory.Avg+1), 
             x=scientificName, fill=specificEpithet)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(
    labels = function(x) ifelse(x == "Prunus serrulata", "Prunus serrulata", x)
  ) +
  labs(x = " ", y = " ", fill = NULL) +
  annotate("text", x = 1.5, y = 3.9, label = "*", size = 10) +
  coord_cartesian(ylim = c(0, 4))

plot(prunusHerb)
plot_grid(acerHerb, quercusHerb, tiliaHerb, prunusHerb, 
          labels = c('E', 'F', 'G', 'H'), 
          ncol=2)

##############################################
#iNat Galling Data
#############################################

# Create matrix of counts
iNat <- matrix(
  c(
    549, 16,   # Cynipini
    175, 0,    # Eriophyidae
    146, 8     # Cecidomyiidae
  ),
  nrow = 3,
  byrow = TRUE
)

# Add row and column names
rownames(iNat) <- c("Cynipini", "Eriophyidae", "Cecidomyiidae")
colnames(iNat) <- c("Native", "Non_native")

# View the table
iNat

# Chi-squared test of independence
chi_iNat <- chisq.test(iNat)
chi_iNat

#Chi-squared result:
#X-squared = 8.589, df = 2, p-value = 0.01364

# Fisher's exact test (more accurate with low counts)
fisher_iNat <- fisher.test(iNat)
fisher_iNat

#Result:
#p-value = 0.004986

#DO THE SAME FOR CONDENSED iNAT DATA
# Create matrix of counts
iNat_condensed <- matrix(
  c(
    503, 16,   # Cynipini
    99, 9,    # Eriophyidae
    87, 8     # Cecidomyiidae
  ),
  nrow = 3,
  byrow = TRUE
)

# Add row and column names
rownames(iNat_condensed) <- c("Cynipini", "Eriophyidae", "Cecidomyiidae")
colnames(iNat_condensed) <- c("Native", "Non_native")

# View the table
iNat_condensed

#              Native Non_native
#Cynipini         503         16
#Eriophyidae       99          9
#Cecidomyiidae     87         8

# Chi-squared test of independence
chi_iNat_condensed <- chisq.test(iNat_condensed)
chi_iNat_condensed

#Chi-squared result:
#X-squared = 9.3685, df = 2, p-value = 0.009239

# Fisher's exact test (more accurate with low counts)
fisher_iNat_condensed <- fisher.test(iNat_condensed)
fisher_iNat_condensed

#Result:
#p-value = 0.007994

##############################################
#Lichen Data ###################
#Lichen data 
# Plot of Blue.FlavoCap lichens on trees (one w/ heavy # of observations) 
#summarize the # of observations in each category of the lichen observations
dat2 <- as.data.frame(dat %>% count(Blue.FlavoCap_Log., Full_Name, Genus)) 
colnames(dat2) #"Blue.FlavoCap_Log." "Full_Name"          "n"  
ggplot(dat2, aes(y=n, x=Full_Name, fill=Genus)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1)) +
  labs(x = "Tree name", y = "n trees with Log% observations of Blue.FlavoCap", fill = NULL) +
  facet_grid(rows=vars(Blue.FlavoCap_Log.)) + #make separate facets for each category of lichen cover
  ylim(0,10)

#Plot of BrightGreenPowder lichens on trees (one w/ heavy # of observations) 
#summarize the # of observations in each category of the lichen observations
dat3 <- as.data.frame(dat %>% count(BrightGreenPowder_Log., Full_Name, Genus)) 
colnames(dat3) #"BrightGreenPowder_Log." "Full_Name"          "n"  
ggplot(dat3, aes(y=n, x=Full_Name, fill=Genus)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1)) +
  labs(x = "Tree name", y = "n trees with Log% observations of BrightGreenPowder", fill = NULL) +
  facet_grid(rows = vars(BrightGreenPowder_Log.)) + #make separate facets for each category of lichen cover
  ylim(0,10) 

# Plot of NeonGreen lichens on trees (one w/ heavy # of observations) 
#summarize the # of observations in each category of the lichen observations
dat4 <- as.data.frame(dat %>% count(NeonGreen_Log., Full_Name, Genus)) 
colnames(dat4) #"NeonGreen_Log." "Full_Name"          "n"  
ggplot(dat4, aes(y=n, x=Full_Name, fill=Genus)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1)) +
  labs(x = "Tree name", y = "n trees with Log% observations of NeonGreen", fill = NULL) +
  facet_grid(rows = vars(NeonGreen_Log.)) + #make separate facets for each category of lichen cover
  ylim(0,10)



##############################################
# Plot LeafByte/Herbivory data (only for acer, quercus, tilia)
##############################################
# LeafByte2, new format ######################## 
#Make error bars ###
#help from: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization 
#data : a data frame
#varname : the name of a column containing the variable to be summarized
#groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE), sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Run function with my data
df2 <- data_summary(LeafByte2, varname="Leaf_Herbivory", 
                    groupnames=c("Genus", "Full_name"))
# Convert herbivory to a factor variable
df2$herb=as.factor(df2$herb)
head(df2)


#Now, make the bar plot ####
#NOTE: arrange so native, non-native plant order along X; change bar color to reflect native/non-native?
#Try as box-whisker
ggplot(df2, aes(y=Leaf_Herbivory, x=Full_name, fill=Genus)) + 
  #make the bar plots
  geom_bar(position="dodge", stat="identity") +
  #add standard error bars
  geom_errorbar(aes(ymin=Leaf_Herbivory-sd/sqrt(20), ymax=Leaf_Herbivory+sd/sqrt(20)), width=.2, position=position_dodge(.9))+
  theme_bw()+
  labs(x = "Tree name", y = "Mean Herbivory (% leaf consumed)", fill = NULL) 


#Running some prelim statistics ####
model <- aov(Leaf_Herbivory ~ Genus + Species, data=LeafByte2)
summary(model)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Genus         3    892  297.35   5.912 0.000543 ***
#  Species       4    392   98.09   1.950 0.100234    
#Residuals   812  40838   50.29                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#9 observations deleted due to missingness

TukeyHSD(model)

#$Genus
#diff        lwr       upr     p adj
#Prunus-Acer     2.9700000  1.1443146 4.7956854 0.0001828
#Quercus-Acer    1.7250000 -0.1006854 3.5506854 0.0718987
#Tilia-Acer      1.4457576 -0.3379524 3.2294676 0.1583095
#Quercus-Prunus -1.2450000 -3.0706854 0.5806854 0.2957817
#Tilia-Prunus   -1.5242424 -3.3079524 0.2594676 0.1241007
#Tilia-Quercus  -0.2792424 -2.0629524 1.5044676 0.9778532

#$Species
#diff       lwr       upr     p adj
#americana-alba        -1.46621212 -4.443886 1.5114617 0.8095053
#cordata-alba          -1.12378788 -4.101462 1.8538860 0.9461300
#platanoides-alba      -1.55500000 -4.602746 1.4927464 0.7795398
#robur-alba            -2.59000000 -5.637746 0.4577464 0.1637888
#rubrum-alba           -1.03500000 -4.082746 2.0127464 0.9695222
#serotina-alba         -0.86500000 -3.912746 2.1827464 0.9891654
#serrulata-alba        -1.72500000 -4.772746 1.3227464 0.6742135
#cordata-americana      0.34242424 -2.563488 3.2483364 0.9999642
#platanoides-americana -0.08878788 -3.066462 2.8888860 1.0000000
#robur-americana       -1.12378788 -4.101462 1.8538860 0.9461300
#rubrum-americana       0.43121212 -2.546462 3.4088860 0.9998557
#serotina-americana     0.60121212 -2.376462 3.5788860 0.9987043
#serrulata-americana   -0.25878788 -3.236462 2.7188860 0.9999956
#platanoides-cordata   -0.43121212 -3.408886 2.5464617 0.9998557
#robur-cordata         -1.46621212 -4.443886 1.5114617 0.8095053
#rubrum-cordata         0.08878788 -2.888886 3.0664617 1.0000000
#serotina-cordata       0.25878788 -2.718886 3.2364617 0.9999956
#serrulata-cordata     -0.60121212 -3.578886 2.3764617 0.9987043
#robur-platanoides     -1.03500000 -4.082746 2.0127464 0.9695222
#rubrum-platanoides     0.52000000 -2.527746 3.5677464 0.9995692
#serotina-platanoides   0.69000000 -2.357746 3.7377464 0.9973055
#serrulata-platanoides -0.17000000 -3.217746 2.8777464 0.9999998
#rubrum-robur           1.55500000 -1.492746 4.6027464 0.7795398
#serotina-robur         1.72500000 -1.322746 4.7727464 0.6742135
#serrulata-robur        0.86500000 -2.182746 3.9127464 0.9891654
#serotina-rubrum        0.17000000 -2.877746 3.2177464 0.9999998
#serrulata-rubrum      -0.69000000 -3.737746 2.3577464 0.9973055
#serrulata-serotina    -0.86000000 -3.907746 2.1877464 0.9895338

#Run stats for herbivory ########################  

##Remove the additional T. cordata and the duplicated T. cordata
###Make sure of the rows
LeafByte2[c(231:240, 411:420), c("Accession_No", "Full_name")]
###Slice out
LeafByte2 <- LeafByte2 %>%
  slice(-c(231:240, 411:420))

##Now there are the correct 800 total observations

##Check for normality of data

# --- 1. Shapiro-Wilk normality test, split by Genus and Origin ---
normality_check <- GWC_tree_avg %>%
  group_by(Genus, Origin) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(Herbivory.Avg)$p.value,
    .groups = "drop"
  )
print(normality_check)

# --- 2. Visual check: QQ plots for each Genus x Origin group ---
ggplot(GWC_tree_avg, aes(sample = Herbivory.Avg)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(Genus ~ Origin, scales = "free") +
  theme_bw() +
  labs(title = "QQ plots by Genus and Origin")

# --- 3. Visual check: histograms/density by group ---
ggplot(GWC_tree_avg, aes(x = log(Herbivory.Avg+1), fill = Origin)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 8) +
  facet_wrap(~ Genus, scales = "free") +
  theme_bw() +
  labs(title = "Herbivory distributions by genus and origin")

##Mostly non-normal so use Wilcox

#Then separate by genus
quercus <- LeafByte2[1:200,]
tilia <- LeafByte2[201:400,]
acer <- LeafByte2[401:600,]
prunus <- LeafByte2[601:800,]

##Create average herb for each tree rather than individual datapoint per leaf
GWC_tree_avg <- LeafByte2 %>%
  group_by(Accession_No, Full_name, Genus, Species, Origin) %>%
  summarise(Herbivory.Avg = mean(Leaf_Herbivory, na.rm = TRUE),
            n_leaves = n(),
            .groups = "drop")

##Create that avg herb for each genus
quercus_avg <- quercus %>%
  group_by(Accession_No, Full_name, Genus, Species, Origin) %>%
  summarise(Herbivory.Avg = mean(Leaf_Herbivory, na.rm = TRUE),
            n_leaves = n(),
            .groups = "drop")

tilia_avg <- tilia %>%
  group_by(Accession_No, Full_name, Genus, Species, Origin) %>%
  summarise(Herbivory.Avg = mean(Leaf_Herbivory, na.rm = TRUE),
            n_leaves = n(),
            .groups = "drop")

acer_avg <- acer %>%
  group_by(Accession_No, Full_name, Genus, Species, Origin) %>%
  summarise(Herbivory.Avg = mean(Leaf_Herbivory, na.rm = TRUE),
            n_leaves = n(),
            .groups = "drop")

prunus_avg <- prunus %>%
  group_by(Accession_No, Full_name, Genus, Species, Origin) %>%
  summarise(Herbivory.Avg = mean(Leaf_Herbivory, na.rm = TRUE),
            n_leaves = n(),
            .groups = "drop")

#Run Wilcoxon Tests for each genus (ran both for leaves and trees, trees is more appropriate and used in paper to avoid pseudoreplication)

##Quercus##
wilcox.test(Leaf_Herbivory ~ Species, data= quercus)
#W = 6432, p-value = 0.0004068
wilcox.test(Herbivory.Avg ~ Species, data= quercus_avg)
#W = 78, p-value = 0.0372 ##SIGNIFICANT

##Tilia##
wilcox.test(Leaf_Herbivory ~ Species, data=tilia)
#W = 5262, p-value = 0.08269
wilcox.test(Herbivory.Avg ~ Species, data= tilia_avg)
#W = 51.5, p-value = 0.9396 ##NOT SIGNIFICANT

##Acer##
wilcox.test(Leaf_Herbivory ~ Species, data=acer)
#W = 3458.5, p-value = 7.017e-05
wilcox.test(Herbivory.Avg ~ Species, data=acer_avg)
#W = 31.5, p-value = 0.1733 ##NOT SIGNIFICANT

##Prunus##
wilcox.test(Leaf_Herbivory ~ Species, data=prunus)
#W = 4974.5, p-value = 0.9509
wilcox.test(Herbivory.Avg ~ Species, data=prunus_avg)
#W = 61, p-value = 0.4267 ##NOT SIGNIFICANT

##histograms of leaf herbivory####
#checking histogram of herbivory rates on native and non-native trees
par(mfrow=c(1,2))
hist(quercus$Leaf_Herbivory[quercus$Species == 'alba'], ylim=c(0,100), xlim=c(0,35))
hist(quercus$Leaf_Herbivory[quercus$Species == 'robur'], ylim=c(0,100), xlim=c(0,35))
#Many zeros, but more regularly eaten than other genera; never eaten at a very high %
# Native alba eaten more heavily than non-native robur

hist(acer$Leaf_Herbivory[acer$Species == 'rubrum'], ylim=c(0,100), xlim=c(0,60))
hist(acer$Leaf_Herbivory[acer$Species == 'platanoides'], ylim=c(0,100), xlim=c(0,60))
#Many zeros
# Non-native platanoides has some heavy occasional herbivory vs native rubrum

hist(tilia$Leaf_Herbivory[tilia$Species == 'americana'], ylim=c(0,100), xlim=c(0,60))
hist(tilia$Leaf_Herbivory[tilia$Species == 'cordata'], ylim=c(0,100), xlim=c(0,60))
#Many zeros
# About equal herbivory on native americana and non-native cordata

hist(prunus$Leaf_Herbivory[prunus$Species == 'serotina'], ylim=c(0,100), xlim=c(0,60))
hist(prunus$Leaf_Herbivory[prunus$Species == 'serrulata'], ylim=c(0,100), xlim=c(0,60))
#Many zeros
# About equal herbivory, native serotina has wider spread
par(mfrow=c(1,1))

#create a series of plots showing herbivory within each genus, color-code native/non-native trees in plot ########################
library(cowplot)
library(RColorBrewer)
library(ggplot2)

#Plots have log-transformed herbivory + 1 (to avoid zeros messing up plots)
#removed y-axis title for quercus and prunus for visualization

# Manually assign reversed colors (example colors from "Paired" palette)
custom_colors <- c("Non-native" = "#1F78B4", "Native" = "#A6CEE3")  # Reversed from original
  
ace<- #need to reverse order of plotted trees so rubrum is 1st
  ggplot(acer_avg, aes(y=log(Herbivory.Avg+1), x=factor(Full_name, levels = c("Acer rubrum", "Acer platanoides")), fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_manual(values = custom_colors) +
  labs(x = " ", y = "log(% leaf consumed+1)", fill = NULL) +
  coord_cartesian(ylim = c(0, 3))

ace

#change prunus serrulata 'kanzan' to just prunus serrulata
prunus_avg$Full_name <- gsub("['\"].*['\"]", "", prunus_avg$Full_name)
prunus_avg$Full_name <- trimws(prunus_avg$Full_name)
pru<-
  ggplot(prunus_avg, aes(y=log(Herbivory.Avg+1), x=factor(Full_name, levels = rev(unique(Full_name))), fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  theme(legend.position = "none", #legend.position = "bottom"
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16) ) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = " ", fill = NULL) +
  coord_cartesian(ylim = c(0, 3))

pru

querc<- 
  ggplot(quercus_avg, aes(y=log(Herbivory.Avg+1), x=Full_name, fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw()+
  scale_fill_brewer(palette = "Paired") +
  labs(x = " ", y = " ", fill = NULL) +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16)) +
  coord_cartesian(ylim = c(0, 3)) +
  annotate("text", x = 1.5, y = 2.9, label = "*", size = 10)

querc

til<-
  ggplot(tilia_avg, aes(y=log(Herbivory.Avg+1), x=Full_name, fill=Origin)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5) +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position="none", 
        axis.text.x = element_text(face = "italic"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16)) +
  labs(x = " ", y = "log(% leaf consumed+1)", fill = NULL) +
  coord_cartesian(ylim = c(0, 3))

til

plot_grid(ace, querc, til, pru, 
          labels = c('A', 'B', 'C', 'D'), 
          ncol=2)#, rel_heights=c(1,1,1.2,1.2))
#This puts the significant trees on the top row; non-SSD on the bottom



#####ADDITIONAL TIMESCALE PLOTS
#####Relative Percent Herbivory Plot####
library(dplyr)
library(ggplot2)
library(scales)

# Bin year into decades and calculate percent native/non-native per decade
herb_decade <- herb_clean %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade, Origin) %>%
  summarise(n = n(), .groups = "drop")

herb_decade_totals <- herb_clean %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(total_n = n(), .groups = "drop")

# Match the color scheme from the scatterplot
origin_colors <- c("Native" = "#F8766D", "Non-native" = "#00BFC4")

herb_decade <- herb_clean %>%
  filter(!is.na(year)) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade, Origin) %>%
  summarise(n = n(), .groups = "drop")

herb_decade_totals <- herb_clean %>%
  filter(!is.na(year)) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(total_n = n(), .groups = "drop")

ggplot(herb_decade, aes(x = factor(decade), y = n, fill = Origin)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    data = herb_decade_totals,
    aes(x = factor(decade), y = 1.05, label = paste0("n=", total_n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_fill_manual(values = origin_colors) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(0, 1.1)) +
  theme_bw() +
  labs(
    title = "Relative Percent of Native vs. Non-Native Specimens Over Time",
    x = "Decade",
    y = "Percent of Specimens",
    fill = "Origin"
  )

####Native herbivory baseline plot#####
#native = baseline at 0, line + points show non-native trend relative to native
library(dplyr)
library(ggplot2)
library(tidyr)

# Bin year into 40-year windows
herb_40yr <- herb_clean %>%
  filter(!is.na(year)) %>%
  mutate(period = floor(year / 40) * 40,
         period_mid = period + 20) %>%  # midpoint of each 40-year block
  group_by(period, period_mid, Origin) %>%
  summarise(mean_herbivory = mean(Herbivory.Avg, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# Pivot so Native and Non-native are side by side, then compute the difference
herb_diff <- herb_40yr %>%
  select(period, period_mid, Origin, mean_herbivory) %>%
  pivot_wider(names_from = Origin, values_from = mean_herbivory) %>%
  mutate(diff = `Non-native` - Native)

# Bring back sample sizes for labeling (total n per period)
herb_diff_n <- herb_40yr %>%
  group_by(period, period_mid) %>%
  summarise(total_n = sum(n), .groups = "drop")

herb_diff <- herb_diff %>%
  left_join(herb_diff_n, by = c("period", "period_mid"))

# Plot
ggplot(herb_diff, aes(x = period_mid, y = diff)) +
  geom_hline(yintercept = 0, color = "#F8766D", linewidth = 1) +
  geom_line(color = "#00BFC4", linewidth = 1) +
  geom_point(color = "#00BFC4", size = 3) +
  geom_text(aes(label = paste0("n=", total_n), y = diff + 0.5), size = 3) +
  theme_bw() +
  labs(
    x = "Year (40-Year Block Midpoint)",
    y = "Herbivory Difference (Non-native − Native)"
  ) + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5))





########################################################################



#OLD/IGNORE Make error bars ####
#help from: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization 
#data : a data frame
#varname : the name of a column containing the variable to be summariezed
#groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE), sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Run function with my data
df1 <- data_summary(dat, varname="total_galls", 
                    groupnames=c("Genus", "Full_Name"))
# Convert herbivory to a factor variable
df1$tot_galls=as.factor(df1$total_galls)
head(df1)

#OLD/IGNORE Plot of total galls per tree ####
ggplot(df1, aes(y=(total_galls+0.001), x=Full_Name, fill=Genus)) + 
  #Added a nominal value to total_galls so they could be visualized on the graph
  #make the bar plots
  geom_bar(position="dodge", stat="identity") +
  #add standard deviation bars
  geom_errorbar(aes(ymin=total_galls-sd, ymax=total_galls+sd), width=.2, position=position_dodge(.9))+
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=.5)) +
  labs(x = "Tree name", y = "Mean galls observed", fill = NULL)

#IGNORE/DO NOT USE LeafByte, original format #######################
ggplot(LeafByte, aes(y=Herbivory_Avg, x=Full_Name, fill=Genus)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1)) +
  labs(x = "Tree name", y = "Mean Herbivory (XX Units)", fill = NULL) 
