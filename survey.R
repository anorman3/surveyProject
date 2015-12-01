## R project, rodent data

# install and load library
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# download the file
download.file("http://files.figshare.com/2236372.csv","data/portal_data_joined.csv")

# import the file
combined <- read.csv('data/portal_data_joined.csv')

# filter and column selection for box plot(weight)
merriami <- combined %>%
  filter(species == "merriami") %>%
  select(species, weight) %>%
  filter(!is.na(weight))


ordii <- combined %>%
  filter(species == "ordii") %>%
  select(species, weight) %>%
  filter(!is.na(weight))

# combine data
combo <- rbind(spectabilis, ordii, merriami)

# to do a t-test
t.test(weight ~ species, data = combo)

# to create a boxplot
pdf("figures/plot1.pdf")
ggplot(data = combo, aes(x=species, y=weight, fill = species)) + xlab("Species") + ylab("Weight(g)") + geom_boxplot() + scale_fill_discrete(name = "p-value < 2.2e-16")
dev.off()

# to make scatter plot, HFL vs weight
speciesMerriami <- combined %>%
  filter(species == "merriami") %>%
  select(hindfoot_length, weight) %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight))


speciesOrdii <- combined %>%
  filter(species == "ordii") %>%
  select(hindfoot_length, weight) %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight))

# merriami vs HFL vs weight
pdf("figures/plot2.pdf")
ggplot(speciesMerriami, aes(x=weight, y=hindfoot_length)) + geom_point(alpha = 1/10, colour = "red", size = (1)) + geom_smooth(method=lm) + xlab("Weight(g)") +ylab("Hind Foot Length(mm)")
dev.off()
# ordii vs HFL vs weight
pdf("figures/plot3.pdf")
ggplot(speciesOrdii, aes(x=weight, y=hindfoot_length)) + geom_point(alpha = 1/10, colour = "red", size = (1)) + geom_smooth(method=lm) + xlab("Weight(g)") +ylab("Hind Foot Length(mm)")
dev.off()

# to make histogram of frequency vs HFL
speciesDO <- combined %>%
  filter(species_id == "DO") %>%
  select(hindfoot_length) %>%
  filter(!is.na(hindfoot_length))

pdf("figures/plot4.pdf")
qplot(hindfoot_length, data = speciesDO, xlab = "Hind Foot Length(mm)", ylab = "Frequency", binwidth = 1, geom = "histogram")
dev.off()

