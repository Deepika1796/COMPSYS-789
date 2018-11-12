kh_all<-read.csv("C:\\rec\\KH\\kapahaka_combined.csv")
#aggregating by mean
agg <- aggregate(list(kh_all$h1,kh_all$h2,kh_all$energy), by=list(kh_all$Speakers,  kh_all$p_types), mean)
colnames(agg) <- c("speaker","performance_types","H1","H2","energy_ratio")
#aggregating by standard deviation
agg_sd <- aggregate(list(kh_all$h1,kh_all$h2,kh_all$energy), by=list(kh_all$Speakers,  kh_all$p_types),sd)
colnames(agg_sd) <- c("speaker","performance_types","H1_sd","H2_sd","energy_ratio_sd")
#aggregating for vowel count
agg1 <- aggregate(list(kh_all$vowels), by=list(id11 = kh_all$Speakers, id2= kh_all$p_types, kh_all$vowels), length)
colnames(agg1) <- c("speaker","performance_types","vowel","count")
#rename this


#barplot for count
ggplot(data=agg1, aes(x=vowel, y=count, fill=(performance_types))) +
         geom_bar(stat="identity", position="dodge") + ggtitle("count of each vowels vs performance types")
#barplot vowels vs performance types for female
f1<- agg1[ which(agg1$speaker=='f1'),]
ggplot(data=f1, aes(x=vowel, y=count, fill=(performance_types))) +
  geom_bar(stat="identity", position="dodge") + ggtitle(" vowels vs performance types for f1")
#barplot vowels vs different performance types for male2
m2 <- agg1[ which(agg1$speaker=='m2'),]
ggplot(data=m2, aes(x=vowel, y=count, fill=(performance_types))) +
  geom_bar(stat="identity", position="dodge") + ggtitle(" vowels vs performance types for m2")
#barplot vowels vs different performance types for male3
m3 <- agg1[ which(agg1$speaker=='m3'),]
ggplot(data=m3, aes(x=vowel, y=count, fill=(performance_types))) +
  geom_bar(stat="identity", position="dodge") + ggtitle(" vowels vs performance types for m3")
#barplot vowels vs different performance types for male4
m4 <- agg1[ which(agg1$speaker=='m4'),]
ggplot(data=m4, aes(x=vowel, y=count, fill=(performance_types))) +
  geom_bar(stat="identity", position="dodge") + ggtitle(" vowels vs performance types for m4")

library(ggplot2)
#barplot for mean h1 for all speakers
ggplot(data=agg, aes(y=H1, x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" Mean H1 vs performance types for all speakers")
ggplot(data=agg, aes(y=abs(agg$H1), x=agg$performance_types, fill=agg$speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" Absolute Mean H2 vs performance types for all speakers")

#barplot for mean h2 for all speakers
ggplot(data=agg, aes(y=agg$H2, x=agg$performance_types, fill=agg$speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" Mean H2 vs performance types for all speakers")
ggplot(data=agg, aes(y=abs(H2), x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" Absolute Mean H2 vs performance types for all speakers")
#barplot for mean energy ratio for all speakers
ggplot(data=agg, aes(y=energy_ratio, x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" Mean energy_ratio vs performance types for all speakers")

#barplot for sd h1 for all speakers
ggplot(data=agg_sd, aes(y=H1_sd, x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle("standard deviation for H1 vs performance types for all speakers")

#barplot for sd h2 for all speakers
ggplot(data=agg_sd, aes(y=H2_sd, x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" standard deviation H2 vs performance types for all speakers")
#barplot for sd energy_ratio for all speakers
ggplot(data=agg_sd, aes(y=energy_ratio_sd, x=performance_types, fill=speaker)) +
  geom_bar(stat="identity", position="dodge")+ ggtitle(" SD for energy ratio vs performance types for all speakers")


#boxplot  for h1,h2,power for all speakers

#boxplot for H1 for the performance types of all speakers
ggplot(data=kh_all, aes(y=h1, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H1 for different performance types - all speakers")
ggplot(data=kh_all, aes(y=h2, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H2 for different performance types - all speakers")
p<- kh_all[ which(kh_all$energy<"0.05"),]
ggplot(data=p, aes(y=energy, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("HFE Ratio for different performance types - all speakers")


#subset data
f1_all<- kh_all[ which(kh_all$Speakers=='f1'),]
ggplot(data=f1_all, aes(y=h1, x=p_types)) +
  geom_boxplot()+ ggtitle("H1 for different performance types - f1")

#boxplot for H1 for the performance types of m2 speakers
m2_all<- kh_all[ which(kh_all$Speakers=='m2'),]
ggplot(data=m2_all, aes(y=h1, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H1 for different performance types - m2 speakers")
# boxplot for H1 for the performance types of m3 speakers
m3_all<- kh_all[ which(kh_all$Speakers=='m3'),]
ggplot(data=m3_all, aes(y=h1, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H1 for different performance types - m3 speakers")
# boxplot for H1 for the performance types of m4 speakers
m4_all<- kh_all[ which(kh_all$Speakers=='m4'),]
ggplot(data=m4_all, aes(y=h1, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H1 for different performance types - m4 speakers")
# boxplot for H2 for the performance types of m3 speakers
f11_all<- kh_all[ which(kh_all$Speakers=='f1'),]
ggplot(data=f11_all, aes(y=h2, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H2 for different performance types - f1 speakers")
# boxplot for H2 for the performance types of m3 speakers
m22_all<- kh_all[ which(kh_all$Speakers=='m2'),]
ggplot(data=m22_all, aes(y=h2, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H2 for different performance types - m2 speakers")
# boxplot for H2 for the performance types of m3 speakers
m33_all<- kh_all[ which(kh_all$Speakers=='m3'),]
ggplot(data=m33_all, aes(y=h2, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H2 for different performance types - m3 speakers")
# boxplot for H2 for the performance types of m3 speakers
m44_all<- kh_all[ which(kh_all$Speakers=='m4'),]
ggplot(data=m44_all, aes(y=h2, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("H2 for different performance types - m4 speakers")
# boxplot for energy ratio for the performance types of m1 speakers
f111_all<- kh_all[ which(kh_all$Speakers=='f1'),]
ggplot(data=f111_all, aes(y=energy, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("Energy Ratio for different performance types - f1 speakers")

# boxplot for energy ratio for the performance types of m2 speakers
m222_all<- kh_all[ which(kh_all$Speakers=='m2'),]
ggplot(data=m222_all, aes(y=energy, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("Energy Ratio for different performance types - m2 speakers")

# boxplot for energy ratio for the performance types of m3 speakers
m333_all<- kh_all[ which(kh_all$Speakers=='m3'),]
ggplot(data=f111_all, aes(y=energy, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("Energy Ratio for different performance types - m3 speakers")
# boxplot for energy ratio for the performance types of m3 speakers
m444_all<- kh_all[ which(kh_all$Speakers=='m4'),]
ggplot(data=m444_all, aes(y=energy, x=Speakers, fill=p_types)) +
  geom_boxplot()+ ggtitle("Energy Ratio for different performance types - m4 speakers")









#subset data based on speaker
#plots for that as well

