#load emuR library
library(emuR)
#creating emuDB
emuR::convert_TextGridCollection(dir="C:\\rec\\KH",dbName="KH",targetDir="C:/rec")
#loading emuDB to KH
KH <- load_emuDB("C:\\rec\\KH_emuDB")

list_levelDefinitions(KH)
list_linkDefinitions(KH)
#linking the hierarchy levels
add_linkDefinition(KH,type = "ONE_TO_MANY",superlevelName = "ORT",sublevelName = "KAN")
add_linkDefinition(KH,type = "ONE_TO_MANY",superlevelName = "KAN",sublevelName = "MAU")
autobuild_linkFromTimes(KH,"ORT","KAN")
autobuild_linkFromTimes(KH,"KAN","MAU")
#to view in using webApp
serve(KH)
#creating Group of shortvowels
add_labelGroup(KH,"shortvowels",c("a","e","i","o","u"))
q1_KH <- emuR::query(KH,query="[MAU==shortvowels]")
# To track the data
q1_KH.trackdata <- get_trackdata(KH,seglist = q1_KH,ssffTrackName = "F0")
serve(KH)




                                 
#list_ssffTrackDefinitions(KH)
plot(q1_KH.trackdata)
plot(q1_KH.trackdata[3],type="l")
# to view the summary
summary(q1_KH)
summary(q1_KH.trackdata)
head(q1_KH)
head(q1_KH.trackdata)
names(q1_KH.trackdata)
table(q1_KH$bundle)

q1_KH.trackdata[1]
temp=q1_KH.trackdata[1]

