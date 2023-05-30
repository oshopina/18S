##Beta-diversity for 18S data
##All the code is for relative abundance

##Load libraries
library(vegan)
library(dplyr)

##Load data
env.18S <- read.table('Data/env_18S.csv',header=TRUE,sep=',',row.names=1)
otu.18S.tmp <- read.table('Data/otu_18S_4350.csv',header=TRUE,sep=',',row.names=1)

##Normilize data
otu.18S <- as.data.frame(t(otu.18S.tmp/4350))
rowSums(otu.18S)

##Change the row orders to the same in both tables
otu.18S <- otu.18S[match(rownames(env.18S), rownames(otu.18S)), ]

##Set colours for pH group
env.18S <- env.18S %>%
  mutate(Colour = case_when(
    endsWith(pH.group, "g4") ~ "#b51945",
    endsWith(pH.group, "g4.5") ~ "#d13a4c",
    endsWith(pH.group, "g5") ~ "#f89151",
    endsWith(pH.group, "g5.5") ~ "#fdcf7d",
    endsWith(pH.group, "g6") ~ "#aedea1",
    endsWith(pH.group, "g6.5") ~ "#68c2a3",
    endsWith(pH.group, "g7") ~ "#388fb8"
  ))

##Count DCA
DCA.18S.RA <- decorana(otu.18S)

## Extract the site and species scores from DECORANA results
site_scores <- scores(DCA.18S.RA, display = 'sites')
species_scores <- scores(DCA.18S.RA, display = 'species')

## Convert the scores to data frames
df_sites <- as.data.frame(site_scores)
df_species <- as.data.frame(species_scores)

##Choose the most abundant OTUs for loadings
bigs_otu <- otu.18S %>% t() %>%
  as.data.frame() %>%
  mutate(total_abundance = colSums(otu.18S)) %>%
  slice_max(n = 20, order_by = total_abundance)

df_species = df_species[rownames(df_species) %in% rownames(bigs_otu),]

## Plot the ordination with sites as points and species as error bars
ggplot() +
  geom_point(
    data = df_sites,
    aes(x = DCA1, y = DCA2),
    col = env.18S$Colour,
    size = 3
  ) +
  geom_segment(
    data = df_species,
    aes(
      x = 0,
      y = 0,
      xend = (DCA1),
      yend = (DCA2)
    ),
    color = "black",
    arrow = arrow(length = unit(1 / 2, "picas"))
  ) +
  labs(x = "DCA Axis 1", y = "DCA Axis 2") +
  annotate(
    "text",
    x = (df_species$DCA1),
    y = (df_species$DCA2),
    label = rownames(df_species)
  )

##PERMANOVA
adonis2(otu.18S ~ env.18S$pH)
