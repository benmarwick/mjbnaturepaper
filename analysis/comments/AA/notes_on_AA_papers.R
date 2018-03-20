

# C14 fails ---------------------------

c14_fails <- readxl::read_excel(here::here('analysis/data/ages/C14 Fails.xlsx'), skip = 1)

library(tidyverse)
c14_fails_binned  <-
c14_fails %>%
  mutate(bin = as.character(cut(c14_fails$`Depth m`,
                   breaks=seq(from = 0,
                              to = ceiling(max(c14_fails$`Depth m`)),
                              by = 0.5)))) %>%
  mutate(bin = str_replace_all(bin, "\\(|\\]", "")) %>%
  mutate(bin = str_replace(bin, "\\,", "-")) %>%
  mutate(bin = ifelse(bin == "2-2.5", "1.5-2", bin)) %>%
  mutate(bin = ifelse(bin == "1.5-2", ">1.5", bin)) %>%
  mutate(bin = factor(bin, levels = c("0-0.5", "0.5-1", "1-1.5", ">1.5"))) %>%
  group_by(bin, `Failure (F)`) %>%
  tally() %>%
  mutate(perc = n / sum(n) * 100) %>%
  filter(`Failure (F)` == "F")


  ggplot(c14_fails_binned,
         aes(bin,
             perc)) +
    geom_col() +
    geom_text(aes(y = perc - 2.5,
                  label = str_glue('{round(perc, 0)} %')),
              colour = "white",
              size = 5) +
    xlab("Depth below surface (m)") +
    ylab("Percentage of charcoal samples that failed pretreatment") +
    theme_minimal(base_size = 14)

  ggsave(here::here('analysis/figures/percentage_of_charcoal_samples_that_failed_pretreatment.png'),
         h = 7, w = 7)



# Create the response variable:
library(survival)
S <- Surv(
  time = rep(0, nrow(c14_fails)),
  time2 = c14_fails$`Depth m`,
  event = c14_fails$`Failure (F)`)




#- Supp table 15 chi-square -------------------------------------------------------------
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


#-  3d plot of lithics and dating sample locations --------------------------------

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
# write.csv(osl_1989_points, "E:/My Documents/My UW/Research/1206 M2 excavation/Section photos/Depth of 1989 OSL samples/osl_1989_points.csv")

# get the osl samples from the NE and SW (back of site)
stray_osl_points <-
  cleaned_rotated_points_in_main_excavation_area %>%
  filter(str_detect(Description,
                    "NE_SECT_OSL|OSL_NE_1A|OSL_SW_NA|GAMMA")) %>%
  select(Description,
         Ynew,
         Xnew_flipped,
         Elevation)

end_level_corners <-
  cleaned_rotated_points_in_main_excavation_area %>%
  filter(str_detect(Description,
                    "^EL_")) %>%
  select(Description,
         Ynew,
         Xnew_flipped,
         Elevation)

# where do they plot?
library(ggrepel)

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


# 3d interactive plot of lithics and ages -------------------


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
  bind_rows(end_level_corners %>%
              mutate(ID = Description,
                     colour = "end level") %>%
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
htmlwidgets::saveWidget(p, "mjb_lithics_osl_and_c14_sample_locations-and_end_levels.html")

# plan view of OSL ages on site grid ---------------

row_c = c(2.35, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
col_c = c(-1.5, -0.5, 0.5, 1.5, 2.5)
col_labels <-
  data_frame(names = 6:1,
             row_mids =  row_c[-length(row_c)] + diff(row_c)/2)
row_labels <-
  data_frame(names = LETTERS[2:5],
             col_mids =  col_c[-length(col_c)] + diff(col_c)/2)
row_c_df <- enframe(row_c)
col_c_df <- enframe(col_c)

# one giant data frame
library(ggrepel)
p_plan <-
  ggplot() +

  geom_point(data = three_d_plot_data %>% filter(colour == 'end level'),
             aes(Xnew_flipped,
                 Ynew),
             colour = "green",
             alpha = 0.6,
             size = 0.2) +

  geom_point(data = three_d_plot_data %>% filter(colour == 'stone artefact'),
             aes(Xnew_flipped,
                 Ynew),
             colour = "orange",
             alpha = 0.6,
             size = 0.2) +

  geom_point(data = three_d_plot_data %>% filter(colour == 'osl',
                                                 !str_detect(ID, "KTL|TL")),
             aes(Xnew_flipped,
                 Ynew),
             colour = "blue",
             alpha = 1,
             size = 1.5) +

  geom_text_repel(data = three_d_plot_data %>% filter(colour == 'osl',
                                                      !str_detect(ID, "KTL|TL")),
                  aes(Xnew_flipped,
                      Ynew,
                      label = ID)) +

  geom_segment(data = row_c_df,
               aes(x = value,
                   y = rep(first(col_c_df$value), nrow(row_c_df)),
                   xend = value,
                   yend = rep(last(col_c_df$value), nrow(row_c_df))),
               colour = "black") +

  geom_segment(data = col_c_df,
               aes(y = value,
                   x = rep(last(row_c_df$value), nrow(col_c_df)),
                   yend = value,
                   xend = rep(first(row_c_df$value), nrow(col_c_df))),
               colour = "black") +

  geom_text(data = col_labels,
            aes(x = row_mids,
                y = rep(-2.5, nrow(col_labels)),
                label = names),
                fontface = "bold",
                size = 5) +

  geom_text(data = row_labels,
            aes(y = col_mids,
                x = rep(-3.8, nrow(row_labels)),
                label = names),
                fontface = "bold",
                size = 5) +

  theme_minimal() +

  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        # remove the  grid lines
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank() ,
        panel.grid.minor.y = element_blank()) +

  coord_equal()

ggsave(here::here('analysis/figures/mjb_plan_view_osl_lithics_end_levels.png'),
       h = 10,
       w = 10)

# separate data frames
p_plan <-
  ggplot() +
  geom_point(data = stone_artefacts_only,
             aes(Xnew_flipped,
                 Ynew),
             size = 0.2,
             alpha = 0.4,
             colour = "orange") +

  geom_point(data = end_level_corners,
             aes(Xnew_flipped,
                 Ynew),
             size = 0.2,
             alpha = 0.1,
             colour = "blue") +

  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Ynew ),
             colour = "green") +

  geom_point(data = osl_ages,
             aes(Xnew_flipped,
                 Ynew ),
             colour = "red") +

  geom_text_repel(data = osl_ages,
                  aes(Xnew_flipped,
                      Ynew,
                      label = paste0(osl_age, " (", Sample, ")" )),
                  size = 3) +

  geom_segment(data = row_c_df,
               aes(x = value,
                   y = rep(first(col_c_df$value), nrow(row_c_df)),
                   xend = value,
                   yend = rep(last(col_c_df$value), nrow(row_c_df))),
               colour = "black") +

  geom_segment(data = col_c_df,
               aes(y = value,
                   x = rep(last(row_c_df$value), nrow(col_c_df)),
                   yend = value,
                   xend = rep(first(row_c_df$value), nrow(col_c_df))),
               colour = "black") +

  geom_text(data = col_labels,
            aes(x = row_mids,
                y = rep(-2, nrow(col_labels)),
                label = names)) +

  geom_text(data = row_labels,
            aes(y = col_mids,
                x = rep(-3.8, nrow(row_labels)),
                label = names)) +

  theme_minimal() +

  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        # remove the  grid lines
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank() ,
        panel.grid.minor.y = element_blank()) +

  coord_equal()




#- density plot with ages, with C and B only------------------------------------
# density plot with C and B only

# <!-- run code from prepare_data chunk in supplementary information-->

# only plot one point per artefact (some artefacts have multiple total station points)
library(viridis)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))

# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 0.25

library(readxl)
artefacts_per_litre <- read_excel(str_glue('{here::here()}/analysis/data/stone_artefact_data/Artefacts per litre discard BC4-6 by depth.xlsx'))

C2_3 <- read_excel(str_glue('{here::here()}/analysis/data/stone_artefact_data/Artefacts per litre discard BC4-6 by depth.xlsx'), sheet = 2)

names(C2_3) <- C2_3[1, ]
C2_3 <- C2_3[-1, ]
C2_3 <- map_df(C2_3, as.numeric)

C4 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 1:4]
names(C4) <- C4[1, ]
C4 <- C4[-1, ]
C4 <- map_df(C4, as.numeric)

B4 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 6:11]
names(B4) <- B4[1, ]
B4 <- B4[-1, ]
B4 <- map_df(B4, as.numeric)

B5 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 13:16]
names(B5) <- B5[1, ]
B5 <- B5[-1, ]
B5 <- map_df(B5, as.numeric)

B6 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 18:22]
names(B6) <- B6[1, ]
B6 <- B6[-1, ]
B6 <- map_df(B6, as.numeric)

C5 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 24:28]
names(C5) <- C5[1, ]
C5 <- C5[-1, ]
C5 <- map_df(C5, as.numeric)

C6 <- artefacts_per_litre[1:nrow(artefacts_per_litre) , 30:34]
names(C6) <- C6[1, ]
C6 <- C6[-1, ]
C6 <- map_df(C6, as.numeric)

# for E2 let's use plotted finds
E2_artefacts <- stone_artefacts_only %>%
  filter(find == "L") %>%
  filter(grepl("E2", .$Description)) %>%
  mutate(Spit = as.numeric(gsub("//d", "", spit))) %>%
  group_by(Spit) %>%
  tally()

E2_depths <- # 42 rows
  cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl("E2", .$Description)) %>%
  filter(grepl("EL", .$Description)) %>%
  group_by(spit) %>%
  summarise(m_d = mean(depth_below_ground_surface, na.rm = TRUE)) %>%
  mutate(Spit = as.numeric(gsub("[A-B]", "", spit))) %>%
  arrange(Spit) %>%
  distinct(Spit, .keep_all = TRUE)

E2_depths$E2_depths_diffs <- abs(c(0, diff(E2_depths$m_d)))
E2_depths$vol <- E2_depths$E2_depths_diffs * 100 * 100 / 1000

E2 <-
  E2_artefacts %>%
  left_join(E2_depths) %>%
  mutate(artefacts_per_cubic_m = n / vol) %>%
  select(-spit, -n, -vol, -E2_depths_diffs) %>%
  rename(`Artefacts/Litre` =  artefacts_per_cubic_m) %>%
  rename(depth = m_d)

# or let's use whatever Chris sent, not quite sure how he got that
E2 <- read_excel(str_glue('{here::here()}/analysis/data/stone_artefact_data/E2 7mm and plotted lithics per litre.xlsx'))


# combine these into one df
the_list <- list(
  "B4" = B4,
  "B5" = B5,
  "B6" = B6,
  "C2/3" = C2_3,
  "C4" = C4,
  "C5" = C5,
  "C6" = C6,
  "E2" = E2
)
artefacts_per_litre_long <-
  bind_rows(the_list,
            .id = "id")

# Depth in two cols...
artefacts_per_litre_long$depth <-
  with(artefacts_per_litre_long, ifelse(is.na(depth),
                                        Depth, depth))

# make facets in specific order
order_we_want <- c("E2",   "B4",  "B5",   "B6" ,
                   "C2/3", "C4",  "C5",   "C6")
artefacts_per_litre_long$id_f <-
  factor(artefacts_per_litre_long$id,
         levels= order_we_want)

# draw vertical lines to show phases
# for back we use  1.90  2.30
# for others we use 2.15-2.6
artefacts_per_litre_long <-
  artefacts_per_litre_long %>%
  mutate(phase_two_upper =
           if_else(id_f %in% c("C4", "C5","C6", "B4", "B5", "B6"),
                   2.15, 1.9)) %>%
  mutate(phase_two_lower =
           if_else(id_f %in% c("C4", "C5","C6", "B4", "B5", "B6"),
                   2.6, 2.3))


ggplot(artefacts_per_litre_long,
       aes(depth,
           `Artefacts/Litre`)) +
  geom_line() +
  facet_wrap(~id_f, scales = "free_y", nrow = 2
  ) +
  geom_vline(aes(xintercept = phase_two_upper),
             colour = "red") +
  geom_vline(aes(xintercept = phase_two_lower),
             colour = "red") +
  geom_text(aes(x = phase_two_upper,
                y = -0.5,
                label = phase_two_upper),
            size = 2) +
  geom_text(aes(x = phase_two_lower,
                y = -0.5,
                label = phase_two_lower),
            size = 2) +
  ylim(0, NA) +
  theme_minimal() +
  xlab("Depth below surface (m)") +
  ggtitle("MJB artefact density by depth in select squares")


# get OSL ages from published SI, to be sure we have the right ones
si_p_31 <- tabulizer::extract_tables(str_glue('{here::here()}/analysis/data/ages/Clarkson_Jacobs_Marwick_2017_SI.pdf'), pages = 31, method = "data.frame")[[2]][-c(1:5), ]
si_p_32 <- tabulizer::extract_tables(str_glue('{here::here()}/analysis/data/ages/Clarkson_Jacobs_Marwick_2017_SI.pdf'), pages = 32, method = "data.frame")[[2]][-c(1:3), ]
si_p_34 <- tabulizer::extract_tables(str_glue('{here::here()}/analysis/data/ages/Clarkson_Jacobs_Marwick_2017_SI.pdf'), pages = 34,method = "data.frame")[[2]][-c(1:5), ]

# Want two cols: Sample, osl_age
si_p_31_a <-
  si_p_31 %>%
  mutate(Sample = gsub("\\s.*", "", X038.nature22968.RESEARCH)) %>%
  mutate(depth = as.numeric(stringr::str_match(X038.nature22968.RESEARCH,
                                               "^\\w*\\s(\\d.\\d*)")[,2])) %>%
  mutate(osl_age = as.numeric(gsub("\\s.*", "", SUPPLEMENTARY.INFORMATION))) %>%
  select(-X038.nature22968.RESEARCH, -SUPPLEMENTARY.INFORMATION) %>%
  mutate(Sample = if_else(Sample == "38.3", "SW11A", Sample ))

si_p_31_a$depth[which(is.na(si_p_31_a$depth))] <- 2.28

# ensure that we use 51.7 for SW11A
si_p_31_a$osl_age[which(si_p_31_a$Sample == "SW11A")] <- 51.7

si_p_32_a <-
  si_p_32 %>%
  mutate(Sample = gsub("\\s.*", "", X038.nature22968)) %>%
  mutate(osl_age = as.numeric(gsub("\\s.*", "", X.6))) %>%
  mutate(depth = as.numeric(stringr::str_match(X038.nature22968,
                                               "^\\w*\\s(\\d.\\d*)")[,2])) %>%
  select(-X038.nature22968,
         -  RESEARCH.SUPPLEMENTARY.INFORMATION,
         -starts_with("X")) %>%
  mutate(Sample = ifelse(Sample == "", NA, Sample ))

# patch up the ages that have both CAM and MAM
si_p_32_a$Sample[c(15,17,18,20,24,26,28,30)] <-
  c("SW3B", "SW3B",
    "SW3A", "SW3A",
    "NW3", "NW3",
    "NW2", "NW2")

si_p_32_a$depth[which(is.na(si_p_32_a$depth))] <-
  c(0.85, 0.85,
    0.85, 0.85,
    0.54, 0.54,
    0.33, 0.33)

si_p_32_a <-
  si_p_32_a %>%
  filter(!is.na(osl_age))

si_p_34_a <-
  si_p_34 %>%
  mutate(Sample = gsub("\\s.*", "", X038.nature22968.RESEARCH)) %>%
  mutate(osl_age = as.numeric(gsub("\\s.*", "", SUPPLEMENTARY.INFORMATION))) %>%
  mutate(depth = -as.numeric(stringr::str_match(X038.nature22968.RESEARCH,
                                                "^\\w*\\s(-\\d.\\d*)")[,2])) %>%
  select(-X038.nature22968.RESEARCH, -SUPPLEMENTARY.INFORMATION)

si_osl_ages <-
  rbind(si_p_31_a,
        si_p_32_a,
        si_p_34_a) %>%
  arrange(osl_age)

# get depth data from the earlier version of osl_ages
osl_ages2 <-
  osl_ages %>%
  select(Sample, total_station_depth_below_surf) %>%
  left_join(si_osl_ages)

# We need to combine OSL and C14, with a col for square, a col for depth, a col for sample ID, and a col for approx age

c14_ages_rugplot_data <-
  c14_ages %>%
  select(square,
         depth_below_ground_surface,
         Bchron_Median,
         Lab.ID) %>%
  dplyr::rename(grid_square = square,
                age = Bchron_Median,
                depth = depth_below_ground_surface,
                id = Lab.ID) %>%
  mutate(age = round(age / 1000, 1))

# For the OSL samples, what square was each sample taken from? We don
# have this in our data so far. We can look EDF 8 from the Nature article.

# NE, NW, SWA, SWB, SWC,
# E2, C4, B4,  B5,  B5,

osl_ages_rugplot_data <-
  si_osl_ages %>%
  mutate(grid_square = case_when(
    grepl("SW8C", Sample) ~ "B6",
    grepl("NE1B", Sample) ~ "C6",
    grepl("NE", Sample) ~ "E2",
    grepl("NW8B", Sample) ~ "B5",
    grepl("NW9B", Sample) ~ "B5",
    grepl("NW", Sample) ~ "C5",
    grepl("SW.{1,2}A", Sample) ~ "B4",
    grepl("SW.{1,2}B", Sample) ~ "B5",
    grepl("SW.{1,2}C", Sample) ~ "B5",
    grepl("KTL158", Sample) ~ "B4", # auger
    grepl("KTL162", Sample) ~ "B4", # DEF30
    grepl("KTL164", Sample) ~ "B5", # DEF30
    grepl("KTL165", Sample) ~ "B5", # DEF30
    is.na(Sample) ~ "Unknown"
  )) %>%
  select(grid_square,
         #total_station_depth_below_surf,
         depth,
         osl_age,
         Sample) %>%
  dplyr::rename( # depth = total_station_depth_below_surf,
    age = osl_age,
    id = Sample) %>%
  mutate( # depth = abs(depth),
    age = as.numeric(age)) %>%
  filter(grid_square != 'Unknown')



# combine the two sets of dates
date_for_plots <-
  bind_rows(c14_ages_rugplot_data,
            osl_ages_rugplot_data)

date_for_plots <-
  date_for_plots %>%
  dplyr::rename(id = grid_square,
                sample_id = id) %>%
  filter(id %in% artefacts_per_litre_long$id )

# find max y-axis value for each sq to plot age points
date_for_plots_max_y <-
  artefacts_per_litre_long %>%
  group_by(id) %>%
  summarise(max_artefacts_litre = max(`Artefacts/Litre`, na.rm = TRUE))

# add max height for red lines
vlines <-
  artefacts_per_litre_long %>%
  select(id, phase_two_upper, phase_two_lower) %>%
  gather(value, xintercept, -id) %>%
  select(-value) %>%
  distinct(.keep_all = TRUE)

vlines <-
  vlines %>%
  left_join(date_for_plots_max_y)

date_for_plots <-
  date_for_plots %>%
  left_join(date_for_plots_max_y) %>%
  filter(age > 50) %>%
  filter(age < 90) %>%
  mutate( label = paste0(sample_id, ": ", age, " ka"))

# plot with ages
vlines$id_f <- factor(vlines$id, levels = order_we_want)
date_for_plots$id_f <- factor(date_for_plots$id, levels = order_we_want)

p <-
  ggplot(artefacts_per_litre_long,
         aes(depth,
             `Artefacts/Litre`)) +
  geom_line() +
  geom_segment(data = vlines,
               aes(x = xintercept,
                   xend = xintercept,
                   y = 0,
                   yend = max_artefacts_litre + 2),
               colour = "red") +
  geom_text(data = vlines,
            aes(x = xintercept,
                y = - max_artefacts_litre / 30,
                label = xintercept),
            size = 2) +
  geom_point(data = date_for_plots,
             aes(depth,
                 max_artefacts_litre + 2)) +
  geom_text(data = date_for_plots,
            aes(depth,
                max_artefacts_litre + 2,
                label = label),
            size = 2,
            angle = 45,
            hjust = -0.25,
            vjust = 0) +
  facet_wrap(~id_f,
             scales = "free_y",
             nrow = 2
  ) +
  theme_minimal() +
  xlab("Depth below surface (m)") # +
# ggtitle("MJB artefact density by depth in select squares")


library(grid)
svg(str_glue('{here::here()}/analysis/figures/Artefact density by depth in select squares.svg'),
    width = 10, height = 7)
gt = ggplot_gtable(ggplot_build(p))
gt$layout$clip = "off"
grid.draw(gt)
dev.off()


#- section plot with B and C only-----------------------------------------------------

stone_artefacts_only_one_B_C <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   Ynew = mean(Ynew),
                   depth_below_ground_surface = mean(depth_below_ground_surface)) %>%
  filter(find == "L") %>%
  filter(str_detect(Description, "_B|_C")) %>%
  mutate(square = if_else(str_detect(Description, "_B"), "B",
                          if_else(str_detect(Description, "_C"), "C",
                          "NA"))) %>%
  ungroup()

# determined by plotting row C end levels
row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
square_labels <-
  data_frame(names = map_chr(6:1, ~str_glue('B{.x}/C{.x}')),
             row_mids =  row_c[-length(row_c)] + diff(row_c)/2)

ggplot(stone_artefacts_only_one_B_C,
       aes(Xnew_flipped,
           depth_below_ground_surface)) +
  geom_point(aes(colour = square),
             size = 0.5) +
  coord_equal() +
  scale_y_reverse() +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1,
                                           color="grey80" ) ) +
  geom_vline(xintercept = row_c,
             colour = "grey80") +
  geom_text(data = square_labels,
            aes(row_mids,
                y = -0.4,
                label = names)) +
  xlab("") +
  ylab("Depth below ground surface (m)") +
  ggtitle("Section view of stone artefacts from squares B and C")

ggsave(str_glue('{here::here()}/analysis/figures/section_view_of_stone_artefacts_from_squares_b_and_c.png'))



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

# A planview of the OSL samples -------------------------------------------



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

