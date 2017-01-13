
#  ------------------------------------------------------------------------
# read in data
# phases of grind stones from Ebbe
library(tidyverse)
library(readxl)
gs_phases <- read_excel("E:/My Documents/My UW/Research/1206 M2 excavation/data/1506 M2 excavation/GSPhases_BM1.xlsx")
names(gs_phases) <- make.names(names(gs_phases))
# I made some edits to this sheet to make the GS numbers more consistent, and move details added by EH to the codes into a 'notes' column

# a little cleaning, there is no A spit for these
gs_phases$Spit.Square <- ifelse(gs_phases$Spit.Square == "D2/16A",
                                 "D2/16",
                         ifelse(gs_phases$Spit.Square == "D2/26A",
                                        "D2/26",
                                gs_phases$Spit.Square))

# separate the unplotted artefacts
gs_phases$Artefact_no <- ifelse(!grepl("UP", gs_phases$Artefact.no.),
                                gs_phases$Artefact.no., NA)


#  ------------------------------------------------------------------------

# review what we want to get: These are the ones we want
# got good position data for these
GS_2012 <- c("GS1", # not GS01
             "GS2",
             "GS3",
             "GS4",
             "GS5",
             "GS6",
             "GS9",
             "GS10",
             "GS14",
             "GS15",
             "GS16",
             "GS17",
             "GS18",
             "GS20",
             "GS27",
             "GS28",
             "GS29",
             "GS37",
             "GS39",
             "GS40",
             "GS41",
             "GS42",
             "GS46",
             "GS47",
             "GS49",
             "GS50")

# got good position data for these
# these are the 2015 ones
GS_2015 <- c("GS53",
         "GS56",
         "GS73",
         "GS74",
         "GS75",
         "GS79",
         "C6_53_GS53")

GS_problematic <- c("GS8",
                    "R68",
                    "R5",
                    "UPGS24",
                    "UPGS25",
                    "GS30",
                    "GS31",
                    "GS33",
                    "GS35",
                    "GS36",
                    "GS32",
                    "GS43",
                    "GS7",
                    "GS13",
                    "GS19",
                    "GS21",
                    "GS22",
                    "GS23",
                    "GS24",
                    "GS26",
                    "UPGS37",
                    "UPGS28",
                    "UPGS29",
                    "GS38",
                    "GS44",
                    "GS45",
                    "GS48")

length(GS_2012) +
length(GS_2015) +
length(GS_problematic) # 60

# --------------------------------------------------------------------------------


# get phase data
phases <- mjbnaturepaper::phases()
# make the bottom of phase 1 a bit lower to catch on artefact here
phases$lower[1] <- 2.75

# --------------------------------------------------------------------------------

# prepare a short description without PF because that's missing for a few

cleaned_rotated_points_in_main_excavation_area$Description_short <-
  gsub("^[[:print:]]{1,2}_", "", cleaned_rotated_points_in_main_excavation_area$Description)

cleaned_rotated_points_in_main_excavation_area$Description_sq_sp <-
  gsub("^[[:print:]]{1,2}_|_[A-Z0-9]+$", "", cleaned_rotated_points_in_main_excavation_area$Description)

cleaned_rotated_points_in_main_excavation_area$Description_artefact_ID <-
  gsub("^[[:print:]]{1,2}_[[:print:]]{1,3}_[[:print:]]{1,3}_", "", cleaned_rotated_points_in_main_excavation_area$Description)

cleaned_rotated_points_in_main_excavation_area$code <- NULL

# --------------------------------------------------------------------------------

# join depths from total station data to Ebbe's data, use Ebbe's data to make a
# Desc  to match with in the ts data, eg "PF_D2_5_GS1"
# This is a join by both artefact ID and square-spit

gs_phases_with_Description <-
  gs_phases %>%
  filter(!is.na(Artefact_no)) %>%  # exclude unplotted pieces here
  separate(Spit.Square,
           c("square", "spit"),
           "/",
           remove = FALSE) %>%
  # make a short description for joining
  mutate(Description_short = paste0(square,
                              "_",
                              spit,
                              "_",
                              sub("\\s", "", Artefact_no))) %>%

  left_join(cleaned_rotated_points_in_main_excavation_area) %>%
  group_by(Artefact_no, Artefact.no., Spit.Square, Description) %>%
  # compute means for multiple points for one GS
  summarise_if(is.numeric, mean, na.rm = TRUE)  %>%
  filter(!is.nan(depth_below_ground_surface)) # 70 rows

# --------------------------------------------------------------------------------

# get the depths of the unplotted grindstones by finding the depths of their square and spit.

gs_phases_with_Description_unplotted <-
gs_phases %>%
  separate(Spit.Square,
           c("square", "spit"),
           "/",
           remove = FALSE) %>%
  filter(is.na(Artefact_no)) %>%
  # make a short description for joining
  mutate(Description_sq_sp = paste0(square,
                                  "_",
                                  spit)) %>%
  left_join(cleaned_rotated_points_in_main_excavation_area) %>%
  group_by(Artefact.no., Spit.Square) %>%
  # compute means for multiple points for one GS
  summarise_if(is.numeric, mean, na.rm = TRUE)  %>%
  filter(!is.nan(depth_below_ground_surface)) # 42 rows

################ combine plotted with non-plotted #################
gs_phases_with_depths <-
rbind(gs_phases_with_Description,
      gs_phases_with_Description_unplotted) %>%
  arrange(Artefact.no.) # 112 rows

# what have we got here?
sum(!is.nan(gs_phases_with_depths$depth_below_ground_surface)) # 112 / 146 artefacts with depths
sum(gsub(" ", "", gs_phases_with_depths$Artefact.no.)[!is.nan(gs_phases_with_depths$depth_below_ground_surface)] %in% GS_2012) # 26 / 26, all 2012 GS have depths!
sum(gsub(" ", "", gs_phases_with_depths$Artefact.no.)[!is.nan(gs_phases_with_depths$depth_below_ground_surface)] %in% GS_2015) # 5 / 7,   almost all 2015 GS have depths
sum(gsub(" ", "", gs_phases_with_depths$Artefact.no.)[!is.nan(gs_phases_with_depths$depth_below_ground_surface)] %in% GS_problematic) # 16 / 27,   still more to do

# what are we missing?

missing <-
  gs_phases$Artefact.no.[!gs_phases$Artefact.no. %in% gs_phases_with_depths$Artefact.no.]

length(missing) # 33

missing <- gsub(" ", "", missing)

# --------------------------------------------------------------------------------

# For these missing ones,
# the problem seems to be that the square/spit in Ebbe's data
# doesn't match the square/spit accociated with the artefact ID in the
# cleaned_rotated_points_in_main_excavation_area. Let's do the unplotted GS first, then
# come back to those mismatching ones...

gs_phases_by_artefact_ID_only <-
gs_phases %>%
  # make an artefact ID for joining
  mutate(Description_artefact_ID = gsub(" ", "", Artefact.no.)) %>%
  filter(Description_artefact_ID %in% missing) %>%  # only use the missing rows
  left_join(cleaned_rotated_points_in_main_excavation_area) %>%
  group_by(Artefact.no., Spit.Square) %>%
  # compute means for multiple points for one GS
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  filter(!is.nan(depth_below_ground_surface)) # gets 22 artefacts

################ combine with missing #################

gs_phases_with_depths_1 <-
  rbind(gs_phases_with_depths,
        gs_phases_by_artefact_ID_only) %>%
  arrange(Artefact.no.)

nrow(gs_phases_with_depths_1) # 134 / 149 have depths now

# what are we missing?

missing <- gs_phases$Artefact.no.[!gs_phases$Artefact.no. %in% gs_phases_with_depths_1$Artefact.no.]

length(missing) # 11


# --------------------------------------------------------------------------------

# try to catch these last few

# missing <- gsub(" ", "", missing)

# [1] "GS 36"    "UP GS 1"  "UP GS 38" "GS 70"    "GS 74"
# [6] "GS 85"    "GS 86"    "GS 87"    "L9060"    "R299"
# [11] "R305"

cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl(paste0(gsub(" ", "", missing), collapse = "|"), Description))
#  nothing

# let's look at the square-spit for these missing ones and see if there are any odd typos

gs_phases_by_square_spit_only <-
gs_phases %>%
  filter(Artefact.no. %in% missing) %>%
  separate(Spit.Square,
           c("square", "spit"),
           "/",
           remove = FALSE) %>%
  # make a short description for joining
  mutate(Description_sq_sp = paste0(square,
                                    "_",
                                    spit)) %>%
  left_join(cleaned_rotated_points_in_main_excavation_area) %>%
  group_by(Artefact.no., Spit.Square) %>%
  # compute means for multiple points for one GS
  summarise_if(is.numeric, mean, na.rm = TRUE)  %>%
  filter(!is.nan(depth_below_ground_surface)) # 8 rows


################ combine with missing #################

gs_phases_with_depths_2 <-
  rbind(gs_phases_with_depths_1,
        gs_phases_by_square_spit_only) %>%
  arrange(Artefact.no.) # 142 / 149

# what are we missing now?

missing <- gs_phases$Artefact.no.[!gs_phases$Artefact.no. %in% gs_phases_with_depths_2$Artefact.no.]

length(missing) # 3

# [1] "UP GS 1"  "UP GS 38" "GS 85"
#      no data    no data   no data

#  ------------------------------------------------------------------------

# get phases and ages for the artefacts

# compute phases
library(fuzzyjoin)
ebbes_artefacts_with_phases <-
  fuzzy_left_join(gs_phases_with_depths_2,
                  phases,
                  by = c("depth_below_ground_surface" = "upper",
                         "depth_below_ground_surface" = "lower"),
                  match_fun = list(`>=`, `<=`)) %>%
  distinct(.keep_all = TRUE)

# get ages from the oxcal plot in the Nature paper

mjb_phase_ages <-
  frame_data(~phase, ~start_up, ~start_lo, ~end_up,  ~end_lo,
                1,     87410,    72960,     76600,    65440,
                2,     68690,    61260,     55090,    50380,
                3,     53980,    49160,     30110,    26000,
                4,     28920,    24560,     14210,    12210,
                5,     10530,    8850,      9020,     7080,
                "6-7",     8180,     6090,      3140,     0)

mjb_phase_ages$age_range <-
  with(mjb_phase_ages,
       paste0(prettyNum(start_up, big.mark = ","),
       "-",
       prettyNum(end_lo, big.mark = ",")))

# deal with the phase 6-7 dating
ebbes_artefacts_with_phases$phase  <-
  with(ebbes_artefacts_with_phases,
  ifelse(phase == 6 | phase == 7,
         "6-7", phase))

ebbes_artefacts_with_phases_and_ages <-
  left_join(ebbes_artefacts_with_phases,
                mjb_phase_ages)

ebbes_artefacts_with_phases_and_ages$phase_and_age <-
  with(ebbes_artefacts_with_phases_and_ages,
       paste0(phase, " (", age_range, " BP", ")"))

nrow(ebbes_artefacts_with_phases_and_ages) # 142

# what are we missing...
gs_phases$Artefact.no.[!gs_phases$Artefact.no. %in% ebbes_artefacts_with_phases_and_ages$Artefact.no.]
# [1] "UP GS 1"  "UP GS 38" "GS 85"

#  ------------------------------------------------------------------------

# plot just these GS with all the flakes, etc.
# only plot one point per artefact (some artefacts have multiple total station points)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))



# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 3

library(viridis)
library(ggplot2)
library(ggrepel)
p <- ggplot() +
  geom_point(data = stone_artefacts_only_one,
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size-2.5) +
  # here are our grindstones
  geom_point(data = ebbes_artefacts_with_phases_and_ages,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 colour = phase_and_age ),
             size = size-0.5)  +
  # here are the labels for the grindstones
  geom_text_repel(data = ebbes_artefacts_with_phases_and_ages,
                  aes(Xnew_flipped,
                      depth_below_ground_surface,
                      label = Artefact.no.),
                  size = 1.5,
                  segment.size = 0.25,
                  segment.alpha = 0.4) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("") +
  ylab("Depth below \nground surface (m)") +
  scale_color_viridis(discrete=TRUE,
                      "phase") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_equal()

row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
nums = paste0("B", 7:2)
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

# save copy
 png("figures/grinding_stones_SW_section.png",
     height = 1200,
     width = 1200*1.92,
    res = 300)
# antialias = "cleartype")
grid.draw(gt)
dev.off()

#  ------------------------------------------------------------------------

# save copy of data
write.csv(ebbes_artefacts_with_phases_and_ages,
          "E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/ebbes_artefacts_with_phases_and_ages.csv")




