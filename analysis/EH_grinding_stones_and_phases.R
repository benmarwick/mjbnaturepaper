
#  ------------------------------------------------------------------------
# read in data
# phases of grind stones from Ebbe
library(tidyverse)
library(readxl)
gs_phases <- read_excel("data/stone_artefact_data/GSPhases_BM1.xlsx")
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
phases$row <- 2 # back rows

# get front phase depths
front_phases <- mjbnaturepaper::front_phases()
front_phases$row <- 4 # front rows

all_phases <- rbind(phases, front_phases)
all_phases_two <- all_phases %>% filter(phase == 2)

# CC says The depths for Phase 2 for the B-E2 row is 1.9-2.3m and for squares BE 4-6 its 2.15-2.6m. It’s never really been worked out for the 1 and 3 rows. Let’s say half way between 2 and 4, so 2-2.45 say?

# CC says: A histogram of plotted artefacts for Row 1 and compared to Row 2 would probably do it. We can draw lines around the dense lower band that way

plotted_lithics_row_1_and_2 <-
stone_artefacts_only %>%
  separate(square,
           into = c("col", "row"), sep = 1) %>%
  filter(str_detect(row, "1|2|3|4|5|6")) %>%
  filter(row != "23") %>%
  filter(row != "C3") %>%
  filter(row != "WC4")

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

custom_breaks <- seq(0, 3, 0.1)
ggplot(plotted_lithics_row_1_and_2,
       aes(round(depth_below_ground_surface,3))) +
  geom_histogram(bins = 100) +
  geom_vline(data = all_phases_two,
             aes(xintercept = upper), colour = "red", size = 3) +
  geom_vline(data = all_phases_two,
             aes(xintercept = lower), colour = "green", size = 3) +
  facet_wrap(~ row, ncol = 1) +
  theme_minimal() +
  xlab("Depth below ground surface (m)") +
  scale_x_continuous(breaks = custom_breaks,
                     labels = every_nth(custom_breaks, 5, inverse = TRUE)) +
  ggtitle("Plotted lithics by excavation row")


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
  mutate(Artefact_no = if_else(Artefact_no == "R68", "NA", Artefact_no)) %>%
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
# cleaned_rotated_points_in_main_excavation_area. Let's do the unplotted GS first, then come back to those mismatching ones...

gs_phases_by_artefact_ID_only <-
gs_phases %>%
  # make an artefact ID for joining
  mutate(Description_artefact_ID = gsub(" ", "", Artefact.no.)) %>%
  filter(Description_artefact_ID %in% missing) %>%  # only use the missing rows
  left_join(cleaned_rotated_points_in_main_excavation_area) %>%
  group_by(Artefact.no., Spit.Square) %>%
  # compute means for multiple points for one GS
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  filter(!is.nan(depth_below_ground_surface)) %>% # gets 22 artefacts
  mutate()

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
  filter(Artefact.no. %in% c("R68", missing)) %>%
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
# 28 June 2017, checking a few things with EH

# how many in each phase
ebbes_artefacts_with_phases_and_ages %>%
  group_by(phase) %>%
  tally

# drop GS 51 though GS 99, keep 53, 56, 73, 74, 75, 79, ie GS_2015


# keep these
keep_gs <- paste0(substr(GS_2015, 1, 2),  " ", substr(GS_2015, 3, 5))
keep_gs <- keep_gs[-length(keep_gs)]

# drop these
drop_gs0 <- paste0("GS ", 51:99)
drop_gs1 <- paste0("GS ", c(17, 42))
drop_gs2 <- c(drop_gs0, drop_gs1)

# exclude the ones we want to keep from the ones we want to drop
drop_gs <- drop_gs2[-match(keep_gs,drop_gs2)]

ebbes_artefacts_with_phases_and_ages_analysed <-    # 126 rows
ebbes_artefacts_with_phases_and_ages %>%            # 142 rows
  ungroup %>%
  filter(!Artefact_no %in% drop_gs) %>%
  filter(!Artefact.no. %in% drop_gs) %>%
  mutate(N = as.numeric(gsub("[A-Z]", "", Artefact_no))) %>%
  mutate(Artefact.no. = gsub("UP GS", "UPGS", Artefact.no.))

# how many in each phase
ebbes_artefacts_with_phases_and_ages_analysed %>%
  group_by(phase) %>%
  tally

# check the phases from EH's PNAS suppl tables spreadsheet
supp_tbl <- read_excel("data/stone_artefact_data/EH_grinding_stone_details.xlsx",
                       sheet = "Sheet1")

supp_tbl_joined <-
supp_tbl %>%
  mutate(`Grinding stone number` = gsub("\\*", "", `Grinding stone number` )) %>%
  full_join(ebbes_artefacts_with_phases_and_ages_analysed,
            by = c("Grinding stone number" = "Artefact.no."))  %>%
  filter(!is.na(Phase) & Phase != "-") %>%
  filter(!(`Grinding stone number` == "R68" &
           depth_below_ground_surface  > 2 &
           `Spit/Square` == "E1/18"))

=# write_csv(supp_tbl_joined, "supp_tbl_joined.csv")

supp_tbl_joined %>%
  select(`Grinding stone number`, Phase, phase)

# make summary table counts per phase
supp_tbl_joined %>%
  group_by(phase_and_age) %>%
  tally

# points for GS from 2015 not analysed
gs_2015_not_analysed <-
ebbes_artefacts_with_phases_and_ages %>%
  filter(Artefact.no. %in%  drop_gs0[-match(keep_gs, drop_gs0)])



#------------------------------------------------------------

# plot just these GS with all the flakes, etc.
# only plot one point per artefact (some artefacts have multiple total station points)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))

supp_tbl_joined


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
  # grey points for
  geom_point(data = gs_2015_not_analysed,
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey60",
             size = size-0.5) +
  # here are our grindstones
  geom_point(data = supp_tbl_joined,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 colour = phase_and_age ),
             size = size-0.5)  +
  # here are the labels for the grindstones
  geom_text_repel(data = supp_tbl_joined,
                  aes(Xnew_flipped,
                      depth_below_ground_surface,
                      label = `Grinding stone number`),
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
  guides(colour = guide_legend(override.aes = list(size = 5),
                               reverse = TRUE)) +
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

#--------------------------------------------------------------------------
# make the width of the bars in the plot for count of each phase to indicate time period for each phase
# Oxygen istope wiggle over the top?



#  ------------------------------------------------------------------------

# save copy of data
# write.csv(ebbes_artefacts_with_phases_and_ages,
#           "E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/ebbes_artefacts_with_phases_and_ages.csv")

# -------------------------------------------------------------------------
# have a go at a plot for table 2: Phase distribution of grinding stones by function. This is a table that EH has hand-made

library(tidyverse)
library(readxl)
gs_table_2 <- read_excel("data/stone_artefact_data/GSPhases_BM1.xlsx", sheet = "Sheet3")

names(gs_table_2) <- gs_table_2[1, ]
names(gs_table_2)[1] <- "phase"
gs_table_2 <- gs_table_2[-c(1,nrow(gs_table_2)), -ncol(gs_table_2) ]

# delet text in brackets
gs_table_2 <-
map_df(gs_table_2, ~gsub("\\s*\\([^\\)]+\\)","", as.character(.x)))

# chr to cols
gs_table_2[, 2:ncol(gs_table_2)] <-
  map(gs_table_2[, 2:ncol(gs_table_2)], ~as.numeric(.x))

# we don't want to plot metal unknown, so drop those cols

gs_table_2 <-
gs_table_2 %>%
  select(-metal, -unknown)


gs_table_2a_long <- gather(gs_table_2,
                           variable,
                           value,
                           -phase)

gs_table_2a_long_cleaner <-
gs_table_2a_long %>%
  mutate(phase = tolower(phase))

# make cumulative sum so we can stack geom_rect nicely
# xstart, xend
gs_table_2a_long_cleaner <-
gs_table_2a_long_cleaner %>%
  filter(value != 0) %>%
  group_by(phase) %>%
  mutate(xstart = c(0, cumsum(value)[-length(value)])) %>%
  mutate(xend = xstart + value) %>%
  arrange(phase)


gs_table_2a_long_cleaner_wth_perc <-
  gs_table_2a_long_cleaner # %>%
#   group_by(variable) %>%
#   mutate(perc = round(value / sum(value) * 100, 1)) %>%
#   filter(phase != "unknown") %>%
#   filter(!variable %in% c("animal", "unknown", "metal"))

# facetted plot of percentages for each phase
# ggplot(gs_table_2a_long_cleaner_wth_perc,
#        aes(variable,
#            perc)) +
#   geom_col() +
#   facet_wrap( ~ phase) +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
#   ggtitle("Percent of grinding stones in each phase with \na specific type of usewear/residue")

# stacked bar plot with counts
# library(viridis)
# ggplot(gs_table_2a_long_cleaner_wth_perc,
#        aes(phase,
#            value,
#            fill = variable)) +
#   geom_col() +
#   scale_fill_viridis(discrete = TRUE) +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle = 90,
#                                  vjust = 0.3)) +
#   ggtitle("Count of grinding stones in each phase with\nidentifiable usewear/residue")

# -------------------------------------------------------------------------
# make it into a time series
gs_table_2a_long_cleaner_wth_perc_with_ages <-
gs_table_2a_long_cleaner_wth_perc %>%
  separate(phase, c("word", "phase_number"), sep = " ") %>%
  mutate(phase_number = gsub("/", "-", phase_number),
         material = variable) %>%
  left_join(mjb_phase_ages, by = c('phase_number' = 'phase')) %>%
  group_by(phase_number) %>%
  mutate(phase_mid_point = end_lo + (start_lo - end_lo)/2)

# labels
plot_x_axis_breaks <-
seq(0, max(gs_table_2a_long_cleaner_wth_perc_with_ages$start_lo, na.rm=TRUE), 1e4)
plot_x_axis_breaks <- c(plot_x_axis_breaks, max(plot_x_axis_breaks))
plot_x_axis_limits <- c(0, max(plot_x_axis_breaks) )
base_size <- 8
phase_labels <- data_frame(phase_mid_point = rev(unique(na.omit(gs_table_2a_long_cleaner_wth_perc_with_ages$phase_mid_point))),
                           phase_labels = rev(unique(na.omit(gs_table_2a_long_cleaner_wth_perc_with_ages$phase_number))))

library(ggplot2)
library(viridis)

ggplot(gs_table_2a_long_cleaner_wth_perc_with_ages) +
  geom_rect(aes(xmin = end_lo,
                xmax = start_lo,
                ymin = xstart,
                ymax = xend,
                fill = material)) +
  scale_fill_viridis(discrete = TRUE,
                      "variable") +
  scale_x_continuous(breaks = plot_x_axis_breaks,
                     labels = plot_x_axis_breaks / 1000) +
  theme_bw(base_size)

# add MIS, d18O, sea levels
library(gsloid)
library(glue)

# subset the MIS data for the last 80 ka years
mis_last_90ka <- LR04_MISboundaries[LR04_MISboundaries$LR04_Age_ka_start <= 90, ]

aspect_ratio_gs <- 3/4 # 3/4 is best
aspect_ratio <- 1/6    # 1/4 looks best

gs_plot <-
ggplot() +
  annotate("rect",
           xmin = mis_last_90ka$LR04_Age_ka_end * 1000,
           xmax = mis_last_90ka$LR04_Age_ka_start * 1000,
           ymin = -Inf,
           ymax = Inf,
           alpha = .2,
           fill = c(rep(c("grey70", "white"),
                      2), c("grey70", "white"))) +
  geom_rect(data = gs_table_2a_long_cleaner_wth_perc_with_ages,
            aes(xmin = end_lo,
                xmax = start_lo,
                ymin = xstart,
                ymax = xend,
                fill = material)) +
  annotate("text",
           x = phase_labels$phase_mid_point,
           y = 2,
           label = phase_labels$phase_labels) +
  scale_fill_viridis(discrete = TRUE,
                     "") +
  guides(fill=guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = plot_x_axis_breaks,
                     labels = plot_x_axis_breaks / 1000,
                     expand = c(0, 0)) +
  coord_cartesian(xlim = plot_x_axis_limits) +
  scale_y_continuous(name = "Number of\ngrinding stones") +
  theme_bw(base_size) +
  # Put upper-right corner of legend box in upper-right corner of graph
  theme(legend.justification = c(1, 1),
        legend.position = c(0.99, 0.99),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(2, "mm"),
        axis.text.x=element_blank(),
        plot.margin=unit(c(0,1,-0.5,1), "cm"),
        aspect.ratio = aspect_ratio_gs,
        axis.ticks = element_blank(),
        axis.title.x = element_blank())

d18O_plot <-
  ggplot() +
  geom_line(data = lisiecki2005,   # add d18O line
            aes(Time,
                d18O)) +
  annotate("rect",
           xmin = mis_last_90ka$LR04_Age_ka_end,
           xmax = mis_last_90ka$LR04_Age_ka_start,
           ymin = -Inf,
           ymax = Inf,
           alpha = .2,
           fill = rep(c("grey70", "white"),
                      nrow(mis_last_90ka)/2)) +
  annotate("text",
           label = glue("MIS {mis_last_90ka$label_MIS}"),
           x =     mis_last_90ka$LR04_Age_ka_mid,
           y = rep(4, nrow(mis_last_90ka)),
           size = 3) +
  scale_x_continuous(breaks = plot_x_axis_breaks / 1000,
                     labels = plot_x_axis_breaks / 1000,
                     name = "",
                     expand = c(0, 0)) +
  coord_cartesian(xlim = plot_x_axis_limits / 1000) +
  scale_y_reverse(name = bquote(Benthic~delta^18*O),
                  position = "right") + # put y-axis at right to avoid label crowding
  theme_bw(base_size) +
  theme(axis.text.x = element_blank(),
        plot.margin = unit(c(0,1,-0.5,1), "cm"),
        aspect.ratio = aspect_ratio,
        axis.ticks = element_blank())

sea_level_plot <-
  ggplot() +
  geom_line(data = spratt2016,
            aes(age_calkaBP,
                SeaLev_shortPC1)) +
  annotate("rect",
           xmin = mis_last_90ka$LR04_Age_ka_end,
           xmax = mis_last_90ka$LR04_Age_ka_start,
           ymin = -Inf,
           ymax = Inf,
           alpha = .2,
           fill = rep(c("grey70", "white"),
                      nrow(mis_last_90ka)/2)) +
  scale_x_continuous(breaks = plot_x_axis_breaks / 1000,
                     labels = plot_x_axis_breaks / 1000,
                     name = "Thousands of years before the present",
                     expand = c(0, 0)) +
  coord_cartesian(xlim = plot_x_axis_limits / 1000) +
  scale_y_continuous(name =  "Sea Level, meters\nabove present day") +
  theme_bw(base_size) +
  theme(plot.margin = unit(c(0,1,0,1), "cm"),
        aspect.ratio = aspect_ratio)


# library(gridExtra)
#
# # ok, but not quite aligned properly
# grid.arrange(gs_plot,
#              d18O_plot,
#              sea_level_plot,
#              ncol = 1)

# This is how we can line them up individually
## convert plots to gtable objects
library(gtable)
library(grid) # low-level grid functions are required
# function to make the panel plot
draw_panel_plot <- function(){
g1 <- ggplotGrob(gs_plot)
g2 <- ggplotGrob(d18O_plot)
g3 <- ggplotGrob(sea_level_plot)
g <- rbind(g1, g2, g3, size="first") # stack the two plots
g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
grid.newpage()
grid.draw(g)
}

# draw it:
draw_panel_plot()

# save it:
pnas_col_width <- 87 # mm
png("C:/Users/bmarwick/Desktop/fig2.png",
    width = pnas_col_width * 1.5,
    height = pnas_col_width * 1.5 * 1.7,
    units = "mm", res = 300)
draw_panel_plot()
dev.off()



# -------------------------------------------------------------------------

# overall, how many GS in each phase?

cleaned_rotated_points_in_main_excavation_area_GS <-
  cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl("GS", cleaned_rotated_points_in_main_excavation_area$Description)) %>%
  group_by(Description) %>%
  slice(1)

# compute phases
library(fuzzyjoin)
cleaned_rotated_points_in_main_excavation_area_GS_phases <-
  fuzzy_left_join(cleaned_rotated_points_in_main_excavation_area_GS,
                  phases,
                  by = c("depth_below_ground_surface" = "upper",
                         "depth_below_ground_surface" = "lower"),
                  match_fun = list(`>=`, `<=`)) %>%
  distinct(.keep_all = TRUE)

# deal with the phase 6-7 dating
cleaned_rotated_points_in_main_excavation_area_GS_phases$phase  <-
  with(cleaned_rotated_points_in_main_excavation_area_GS_phases,
       ifelse(phase == 6 | phase == 7,
              "6-7", phase))

# count GS per phase
cleaned_rotated_points_in_main_excavation_area_GS_phases_tally <-
cleaned_rotated_points_in_main_excavation_area_GS_phases %>%
  left_join(mjb_phase_ages) %>%
  group_by(phase, age_range, start_up, end_lo) %>%
  tally() %>%
  mutate(gs_per_1000_years = n / (start_up - end_lo) * 1000)


#------------------------------------------------------------------------------
## can you give me a summarised list all the final artefact counts comprising both the 2012 and 2015 assemblages? I want to know the % of grinding stones compared with other lithic tools (I think it is around 4%)?

library(dplyr)
stone_artefacts_only %>%
  filter(find != "LINE") %>%
  group_by(find) %>%
  tally() %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

stone_artefacts_only %>%
  filter(find != "LINE") %>%
  #group_by(find, year) %>%
  count(year, find) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(year)


## Phases for 2015 plotted GS that you didn't analyse, GS 51-87

# ebbes_artefacts_with_phases %>%
#   filter(Artefact.no. %in% drop_gs0) %>%
#   write_csv("GS_51_onwards.csv")


# R68 is in E1/18 but I've got phase 1! Should be phase 6-7
gs_phases %>%
  filter(grepl("R68", Artefact.no.))

#------------------------------------------------------------------------------
# I know that the grinding stones account for about 2% of the total stone
# artefact count over the entire assemblage, but can you provide me a break
# down of this % per phase? I think perhaps the % of grinding stones gets
# higher in Phase 4 coinciding with the LGM? Can you confirm this?

# get phases for every artefact
stone_artefacts_only_phases <-
  stone_artefacts_only %>%
  filter(find != "LINE") %>%
  fuzzy_left_join(phases,
                  by = c("depth_below_ground_surface" = "upper",
                         "depth_below_ground_surface" = "lower"),
                  match_fun = list(`>=`, `<=`)) %>%
  distinct(.keep_all = TRUE)

# how many artefacts?
# nrow(stone_artefacts_only_phases)

# how many have a phase number?
# sum(stone_artefacts_only_phases$phase != "", na.rm = TRUE)
# we're missing a few, never mind...

# deal with the phase 6-7 dating
stone_artefacts_only_phases$phase  <-
  with(stone_artefacts_only_phases,
       ifelse(phase == 6 | phase == 7,
              "6-7", phase))

# a break down of GS % per phase
stone_artefacts_only_phases_tally <-
stone_artefacts_only_phases %>%
  group_by(phase, find) %>%
  tally() %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  filter(!is.na(phase))

write.csv(stone_artefacts_only_phases_tally,
          "D:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/stone_artefacts_finds_by_phases_tally.csv",
          row.names = FALSE)

plot_stone_artefacts_only_phases_tally <-
ggplot(stone_artefacts_only_phases_tally,
       aes(phase,
           n)) +
  geom_col(aes(fill = find)) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Artefact type") +
  theme_bw() +
  ylab("Number of artefacts by type")

plot_gs_percentage_by_phase <-
stone_artefacts_only_phases_tally %>%
  filter(find == "GS") %>%
ggplot(aes(phase,
           percentage)) +
  geom_col(fill = viridis(6)[4]) +
  scale_fill_discrete(guide = FALSE) +
  theme_bw() +
  ggtitle("Grinding stones as percentage of \nall artefacts in each phase")

library(cowplot)

ggdraw() +
  draw_plot(plot_stone_artefacts_only_phases_tally,
            0, 0, 1, 1) +
  draw_plot(plot_gs_percentage_by_phase,
            0.1, 0.7, 0.4, 0.25)

ggsave("D:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/plot_gs_percentage_by_phase.png", h = 7, w = 7)

#------------------------------------------------------------------------------
# I need to know the exact depths for:  GS 36 & GS 37

# we can get GS37 no problem
ebbes_artefacts_with_phases_and_ages %>%
  filter(Artefact_no %in% c('GS 36', 'GS 37')) %>%
  select(Artefact_no, depth_below_ground_surface)

# but GS36 is problematic
stone_artefacts_only %>%
  filter(str_detect(Description, 'GS36')) %>%
  select(Description, depth_below_ground_surface)

gs_phases_with_depths_2 %>%
  filter(Artefact.no. == 'GS 36') %>%
  select(Artefact.no., Spit.Square, depth_below_ground_surface)
