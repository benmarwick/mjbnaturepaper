# Re-do the table of Supplementary Table 15

# because we used the back depths, and should have used the front depths
# did we get the phase numbers backwards in the chi-sq for raw materials?
# or make some other mistake there? counts of artefacts seem very high

B6_raw_materials <- readr::read_csv("data/stone_artefact_data/B6_raw_material_table.csv")

## plot_raw_materials_technology <-
##  raw_materials_technology_plots(B6_raw_materials, spit_depths_B6_output)

# that fn does this:

B6_raw_materials_technology_depths <-
  B6_raw_materials %>%                        # from CSV file, ok
  left_join(spit_depths_B6_output,            # ok
            by = c("Spit" = "spit")) %>%      # of B6 spits & depths
  # replacing each NA with interpolated values, ok
  mutate(depth_below_surface = zoo::na.approx(depth_below_surface)) %>%
  # compute the: thickess of each spit, and the mid-point of the
  # depth of each spit, yes, ok
  mutate(depth_diff = c(0, diff(.$depth_below_surface)),
         x_centre = c(depth_below_surface[1]/2,
                      zoo::rollmean(depth_below_surface, 2)),
         Quartzite = Qtztite,
         Quartz = Qtz)

# check, what does rollmean do?
1:5
zoo::rollmean(1:5, 2)
# gives the halfway point between each value

# At this point we have a data frame, one row is one spit, one col
# is one raw material. We also have spit depths.

# Now we make a long table, one row is one spit-raw material pair, ok
# 'value' is count of artefact of that raw material in that spit
# `Artefacts per Litre` is a volumetric expression of artefact abundance.

B6_raw_materials_plot_data <-
  B6_raw_materials_technology_depths %>%
  gather(`Raw material`,
         value,
         -Spit,
         -depth_below_surface,
         -`Volume Excavated`,
         -depth_diff,
         -x_centre) %>%
  filter(`Raw material` %in% c("Quartzite",
                               "Quartz",
                               "`Crystal Qtz`",
                               "Silcrete",
                               "`Rare Quartzite (Brown and Dark Grey)`",
                               "`Buff and Red Mudstone`",
                               "`Fine Qtzite`",
                               "Chert",
                               "Volcanic",
                               "Mica",
                               "Glass",
                               "`Gerowie Tuff`" ))  %>%
  mutate(`Artefacts per Litre` = as.numeric(value)/`Volume Excavated`,
         `Depth below surface (m)` = zoo::na.approx(round(depth_below_surface, 2)))

# what spit is in what depth?
B6_raw_materials_plot_data %>%
  select(Spit, `Depth below surface (m)`)

## chi-sq for raw material by phase
## chi_sq_raw_material_by_phase_tbl <- chi_sq_raw_material_by_phase(plot_raw_materials_technology)

# that fn does this:

# drop some columns
raw_materials_technology_chi <-
  B6_raw_materials_plot_data %>%
  dplyr::select(Spit, depth_below_surface, `Raw material`, value) %>%
  arrange(desc(depth_below_surface))

# explore the output here...
raw_materials_technology_chi %>%
  mutate(artefact_count = as.numeric(value)) %>%
  arrange(desc(artefact_count))
# yes, ok

raw_materials_technology_chi %>%
  mutate(artefact_count = as.numeric(value)) %>%
  group_by(`Raw material`) %>%
  summarise(sum(artefact_count))

# compare to CSV

sum(B6_raw_materials$Qtz) # 10,566

raw_materials <-  c(
  "Qtz",
  "Crystal Qtz"   ,
  "Silcrete"  ,
  "Rare Quartzite (Brown and Dark Grey)",
  "Buff and Red Mudstone" ,
  "Fine Qtzite"            ,
  "Chert"   ,
  "Volcanic"  ,
  "Mica"   ,
  "Glass"     ,
  "Gerowie Tuff"
)

B6_raw_materials %>%
  select(raw_materials) %>%
  colSums()

# it's ok, the raw material totals are still the same as what we put in.

# group spits into phases

# round the depths, convert to cm
raw_materials_technology_chi$depth_below_surface_round <-
  round(raw_materials_technology_chi$depth_below_surface, 2) * 100

# get start and end of phases
the_phases <- phases()
start <- the_phases$upper * 100
end <- c(350, (the_phases$lower * 100)[-1]) # extend depth to base of artefact

# now we have

# # A tibble: 7 x 3
# phase upper lower
# <dbl> <dbl> <dbl>
#   1     1  2.30  2.70
#   2     2  1.90  2.30
#   3     3  1.50  1.90
#   4     4  1.20  1.50
#   5     5  0.85  1.20
#   6     6  0.50  0.85
#   7     7  0.00  0.50

# comes from CC email 24/11/2016
# "Ben, the phase depths are wrong for EDF6! [later became EDF7, the geoarch fig] Remember this is C2, where everything is substantially higher! So, we should probably go with these dpeths for each phase:"

# 1. 2.7-2.3
# 2. 2.3-2.0
# 3.1.4-2.0
# 4.1.2-1.4
# 5.0.8-1.2
# 6. 0.5-0.8
# 7. 0-0.5

# Compute which layer each spit belongs in using the
# IRanges package

# source("https://bioconductor.org/biocLite.R")
# biocLite("GenomicRanges")
require(IRanges)
depth_values <-
  IRanges(na.omit(raw_materials_technology_chi$depth_below_surface_round),
          width = 1,
          names = na.omit(raw_materials_technology_chi$depth_below_surface_round))
ranges_for_phases <-
  IRanges(start = start,
          end = end,
          names = the_phases$phase)
olaps <- findOverlaps(depth_values, ranges_for_phases)
phases_from_depths <- subjectHits(olaps)

# attach phases to long data frame
raw_materials_technology_chi$phases_from_depths <- phases_from_depths

# check expectations...
raw_materials_technology_chi %>%
  group_by(phases_from_depths, `Raw material`) %>%
  summarise(totals = sum(as.numeric(value)))

# looks a bit odd, 1319 Quartz pieces in phase 1?

# what spits are in phase 1? It going up to 2.3 m
# perhaps the problem here is that the phase depths are for
# back (C2), but the artefact data is for the front (B6)

raw_materials_technology_chi %>%
  group_by(phases_from_depths) %>%
  summarise(highest_spit = min(Spit),
            lowest_spit = max(Spit))


# these are copied from the published Bayesian OSL figure
phases_front <- tibble::frame_data(
  ~phase, ~upper, ~lower,
  1,     2.6,     4.6,
  2,     2.15,    2.59,
  3,     1.55,    2.149,
  4,     0.95,    1.549,
  5,     0.70,    0.949,
  6,     -0.5,       0.69)

# recompute depth-phases using this new set of depths
# get start and end of phases
the_phases <- phases_front
start <- the_phases$upper * 100
end <- the_phases$lower * 100 # extend depth to base of artefact


depth_values <-
  IRanges(na.omit(raw_materials_technology_chi$depth_below_surface_round),
          width = 1,
          names = na.omit(raw_materials_technology_chi$depth_below_surface_round))
ranges_for_phases <-
  IRanges(start = start,
          end = end,
          names = phases_front$phase)
olaps <- findOverlaps(depth_values, ranges_for_phases)
phases_from_depths <- subjectHits(olaps)

# attach phases to long data frame
raw_materials_technology_chi$phases_from_front_depths <- phases_from_depths

# check to see what difference these front phase depths makes
raw_materials_technology_chi %>%
  group_by(phases_from_front_depths, `Raw material`) %>%
  summarise(total_count = sum(as.numeric(value)))

# # A tibble: 42 x 3
# # Groups:   phases_from_front_depths [?]
# phases_from_front_depths `Raw material` total_count
# <int>          <chr>       <dbl>
# 1                        1          Chert           1
# 2                        1          Glass           0
# 3                        1           Mica           0
# 4                        1         Quartz          28
# 5                        1      Quartzite          36
# 6                        1       Silcrete           3
# 7                        1       Volcanic           0

# looks much better. So the problem was that my phase 1 actually included a bunch of phase 2 artefacts, because I'm using values for the back of the site, for the geoarch figure which has artefacts from C2. That's what I prepared the phases() fn for. But this is the front of the site, B6.




# focus on  Qtztite, Qtz, Silcrete, Chert
chi_sq_raw_material_by_phase_output_front <-
  raw_materials_technology_chi %>%
  dplyr::filter(!`Raw material` %in% c('Glass', 'Mica', 'Volcanic')) %>%
  dplyr::select(phases_from_front_depths, `Raw material`, value) %>%
  group_by(`Raw material`, phases_from_front_depths ) %>%
  dplyr::summarise(artefact_count = sum(as.numeric(value))) %>%
  tidyr::spread(key = `Raw material`, value = artefact_count, fill = 0)


# check totals
colSums(chi_sq_raw_material_by_phase_output_front)

# compare to
B6_raw_materials %>%
  select(raw_materials) %>%
  colSums()

# yes, amounts of raw materials are still ok

# chi-sq for raw material by phase
chi_sq_raw_material_by_phase_output_front %>%
  dplyr::select(-phases_from_front_depths) %>%
  chisq.test()

# # A tibble: 6 x 4
# Chert Quartz Quartzite Silcrete
# * <dbl>  <dbl>     <dbl>    <dbl>
#   1     1     28        36        3
#   2    69   1950       674       87
#   3    51   2551       178       11
#   4    59   3349       278       86
#   5    23   1009        13        2
#   6    10   1679        93        3
# > # chi-sq for raw material by phase
#   > chisq.test(chi_sq_raw_material_by_phase_output_front)
#
# Pearson's Chi-squared test
#
# data:  chi_sq_raw_material_by_phase_output_front
# X-squared = 1118.5, df = 15, p-value < 2.2e-16
#
# Warning message:
# In chisq.test(chi_sq_raw_material_by_phase_output_front) :
# Chi-squared approximation may be incorrect

# what are the phases of each spit?

raw_materials_technology_chi %>%
  group_by(phases_from_front_depths) %>%
  summarise(highest_spit = min(Spit),
            lowest_spit = max(Spit))
#-------------------------------------------------------------------------------

# check again the locations of SW-B and SW-C OSL samples
surf <- 100.693213

library(dplyr)
library(ggplot2)
library(plotly)


# from csv file
osl_1989 <-
  read.table(header = TRUE, text = "
             X	Y		osl_sample age depth1989
             -0.222184031	98.78729662		KTL_97 24ka 190
             -0.134978511	98.79767823		KTL_97 24ka 190
             -0.224260353	98.60665662		KTL_97 24ka 190
             -0.134978511	98.6149619		KTL_97 24ka 190
             -0.230489318	98.31804788		KTL_158 52ka 242
             -0.141207477	98.31804788		KTL_158 52ka 242
             -0.224260353	98.18308695		KTL_158 52ka 242
             -0.139131155	98.18516327		KTL_158 52ka 242
             0.114180115	98.19554488		KTL_162 55ka 254
             0.089264252	98.16647638		KTL_162 55ka 254
             0.116256436	98.14571316		KTL_162 55ka 254
             0.137019655	98.17062902		KTL_162 55ka 254
             -0.230489318	97.78235683		KTL_141 61ka 295
             -0.141207477	97.78443315		KTL_141 61ka 295
             -0.230489318	97.59756418		KTL_141 61ka 295
             -0.141207477	97.59548786		KTL_141 61ka 295
             -0.237524611	96.8747364		KTL_116 86ka 390
             -0.153120659	96.88739699		KTL_116 86ka 390
             -0.233304413	96.6890477		KTL_116 86ka 390
             -0.148900462	96.6890477		KTL_116 86ka 390
             0.289648999	98.0018874		TL ?ka 0
             0.260243752	97.96783922		TL ?ka 0
             0.294291933	97.93998161		TL ?ka 0
             0.317506602	97.97557744		TL ?ka 0
             0.664867717	98.45361138		KTL_164 44ka 230
             0.603906483	98.40671813		KTL_164 44ka 230
             0.664867717	98.37858217		KTL_164 44ka 230
             0.735207603	98.41609678		KTL_164 44ka 230
             ")

# not on section drawings: KTL165 15ka 155
# ZJ says 'originally at 149-155 cm depth'
# this is all we can do:
surf - 149/100
surf - 155/100

osl_1989_points <-
  osl_1989 %>%
  mutate(elevation_from_jhe_table = surf - depth1989/100) %>%
  group_by(osl_sample, age, elevation_from_jhe_table) %>%
  summarise(elevation_max = max(Y),
            elevation_min = min(Y),
            depth_below_surf_min = surf - elevation_max,
            depth_below_surf_max = surf - elevation_min,
            meanX = mean(X),
            maxY = max(Y),
            Elevation = mean(c(elevation_max, elevation_min)))
write.csv(osl_1989_points, "E:/My Documents/My UW/Research/1206 M2 excavation/Section photos/Depth of 1989 OSL samples/osl_1989_points.csv")

# get the osl samples from the NE and SW (back of site)
stray_osl_points <-
  cleaned_rotated_points_in_main_excavation_area %>%
  filter(str_detect(Description,
                    "NE_SECT_OSL|OSL_NE_1A|OSL_SW_NA|GAMMA")) %>%
  select(Description,
         Ynew,
         Xnew_flipped,
         Elevation)

# where do they plot?

p <-
  ggplot() +
  geom_point(data = stone_artefacts_only,
             aes(Xnew_flipped,
                 Elevation),
             size = 0.2) +
  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "green") +
  geom_text_repel(data = c14_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(round(Bchron_Median/1000,2),
                                     " (", gsub("_$", "", square_spit), ")" )),
                  size = 3) +
  geom_point(data = osl_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "red") +
  geom_text_repel(data = osl_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(osl_age, " (", Sample, ")" )),
                  size = 3) +
  geom_linerange(data = osl_1989_points,
                 aes(x = meanX,
                     ymin = elevation_min,
                     ymax = elevation_max),
                 colour = "blue",
                 size = 3) +
  geom_text_repel(data = osl_1989_points,
                  aes(meanX,
                      elevation_min,
                      label = paste0(age,
                                     " (", osl_sample,
                                     ")")),
                  size = 3,
                  colour = "blue",
                  nudge_x = -0.5) +
  coord_equal() +
  theme_minimal()

ggplotly(p)


# can we get a 3d plot of lithics and ages?
# combine the various tables...

three_d_plot_data <-
  stone_artefacts_only %>%
  mutate(ID = Description,
         colour = "stone artefact") %>%
  select(ID,
         Xnew_flipped,
         Ynew,
         Elevation,
         colour) %>%
  bind_rows(c14_ages %>%
              mutate(ID = Sample.ID,
                     colour = "c14") %>%
              select(ID,
                     Xnew_flipped,
                     Ynew,
                     Elevation,
                     colour) ) %>%
  bind_rows(osl_ages %>%
              mutate(ID = Sample,
                     colour = "osl") %>%
              select(ID,
                     Xnew_flipped,
                     Ynew,
                     Elevation,
                     colour)) %>%
  bind_rows(osl_1989_points %>%
              mutate(ID = osl_sample,
                     Xnew_flipped = meanX,
                     Ynew = 0,
                     colour = "osl") %>%
              select(ID,
                     Xnew_flipped,
                     Ynew,
                     Elevation,
                     colour) ) %>%
  bind_rows(stray_osl_points %>%
              mutate(ID = Description,
                     colour = "osl") %>%
              select(ID,
                     Xnew_flipped,
                     Ynew,
                     Elevation,
                     colour) ) %>%
  select(-osl_sample,
         -age) %>%
  mutate(size = ifelse(colour == "c14", 3,
                       ifelse(colour == "osl", 3,
                              0.5)))

# This will make a 3d plot with artefacts, osl & c14 sample locations
p <-
three_d_plot_data %>%
  plot_ly(x = ~Xnew_flipped,
          y = ~-Ynew,
          z = ~Elevation,
          text = ~ID,
          size = ~size,
          marker = list(symbol = 'circle',
                        sizemode = 'diameter'),
          sizes = c(1, 5),
          color = ~colour) %>%
  add_markers()

# save the page with the plot
htmlwidgets::saveWidget(p, "mjb_lithics_osl_and_c14_sample_locations.html")





#-------------------------------------------------------------------------------
require(Ckmeans.1d.dp)
x <- c(rnorm(50, mean=-1, sd=0.3), rnorm(50, mean=1, sd=1), rnorm(50, mean=2, sd=0.4))
# Divide x into k clusters, k automatically selected (default: 1~9)
result <- Ckmeans.1d.dp(x)
plot(result)

k <- max(result$cluster)
plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Optimal univariate clustering with k estimated",
     sub=paste("Number of clusters is estimated to be", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("topleft", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")


x <- stone_artefacts_only$depth_below_ground_surface

ahist(x, k=max(result$cluster), col="gray",
      lwd=2, lwd.stick=6, col.stick="chocolate")

stone_artefacts_only$cluster <- result$cluster


# only plot one point per artefact (some artefacts have multiple total station points)
library(viridis)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find, cluster) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))



# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 0.25

p <- ggplot() +
  geom_point(data = stone_artefacts_only_one,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 colour = as.character(cluster)),
             size = size) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("") +
  ylab("Depth below \nground surface (m)") +
  scale_colour_brewer(palette = "Set1",
                      "Cluster") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_equal()

row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
nums = paste0("B", 6:1)
row_mids <-  row_c[-length(row_c)] + diff(row_c)/2

library(grid)
for(i in 1:length(row_mids)){
  p = p + annotation_custom(grob = textGrob(nums[i], gp=gpar(fontsize=10)),
                            xmin =  row_mids[i],
                            xmax =  row_mids[i],
                            ymin = -8.5,
                            ymax = 2)
}

# Code to override clipping
grid.newpage()
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
# output to RStudio plot pane, then save as SVG


# compare a null hypothesis
make_null_distribution <- function() {
  x_unif <- runif(n = length(x),
                  min = min(x),
                  max = max(x))

  x_unif_result <-
    Ckmeans.1d.dp(x_unif, k = c(1, 50), estimate.k = "BIC")
  # plot(x_unif_result)

  x_unif_k <- max(x_unif_result$cluster)

  return(x_unif_k)
}

n = 10000
generated_null_distributions <- replicate(make_null_distribution(), n)
hist(generated_null_distributions)

# get a p-value: proportion of simulations with the same or less clusters than observed
p_sim <- sum(generated_null_distributions <= k) / length(generated_null_distributions)


# plot observed with null
library(glue)
obs_null <-
  ggplot(data_frame(g = generated_null_distributions),
         aes(g)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept =  k,
             colour = "red",
             size = 1.5) +
  xlab(glue("Number of clusters in {n} random distributions")) +
  annotate("text",
           x = k-1.5,
           y = 1500,
           label = "Observed k") +
  annotate("text",
           x = 17,
           y = 1200,
           label = glue("Probability of the observed or\nfewer number of clusters\n is {p_sim}"),
           hjust = 0)

library(cowplot)

plot_grid(obs_null,
          gt,
          ncol = 1,
          align = "hv",
          axis = "lr")

# what does it look like?
stone_artefacts_only$simulated_depths <- x_unif
stone_artefacts_only$simulated_cluster <- x_unif_result$cluster

ggplot() +
  geom_point(data = stone_artefacts_only,
             aes(Xnew_flipped,
                 simulated_depths,
                 colour = as.factor(simulated_cluster))
  ) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("")



# test of k range, yes we can get more than 9
t1 <- flatten_dbl(map(seq(1,100,5), ~rnorm(n= 10, mean = .x, sd = 0.0001)))
hist(t1, breaks = 100)
(t2 <- Ckmeans.1d.dp(t1, k = c(1,50), estimate.k = "BIC"))
max(t2$cluster)

#----------------------------------------------------------------------------
# mag sus plot over hearth

ms <- readxl::read_excel("D:/My Documents/My UW/Research/1206 M2 excavation/MS data/notebook/hearth_feature_SF53.xlsx")

library(tidyverse)
ms %>%
  rowwise() %>%
  mutate(mean_ms = mean(c(meas1, meas2, meas3), na.rm = TRUE)) %>%
  ggplot(aes(distance,
             mean_ms)) +
  geom_point() +
  geom_line() +
  ylab("Magnetic Susceptibility\nSI units") +
  theme_bw() +
  annotate("rect",
           xmin = 83,
           xmax = 90,
           ymin = -Inf,
           ymax = 300,
           fill = "red",
           alpha=0.5 ) +
  annotate("text",
           x = 73,
           y = 250,
           label = "Hearth\nlocation",
           hjust = 0)

