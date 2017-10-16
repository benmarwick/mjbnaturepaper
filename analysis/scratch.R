


stone_artefacts_in_sqs_all <- stone_artefacts_in_sqs
ggplot(stone_artefacts_in_sqs,
       aes(Xnew_flipped,
           Elevation)) +
         geom_point()

ggplot() +
  geom_point(data = stone_artefacts_in_sqs_all,
             aes(Xnew_flipped,
                 Elevation),
             colour = "green") +

  geom_point(data = stone_artefacts_in_sqs,
             aes(mean_Xnew_flipped,
                 mean_Elevation),
             colour = "red")

refit_data_long_coords_start <-
  refit_data_long_coords[duplicated(refit_data_long_coords$set), ]

refit_data_long_coords_end <-
  refit_data_long_coords[!refit_data_long_coords$descr %in% refit_data_long_coords_start$descr, ]

# 34
dim(refit_data_long_coords_start) # [1] 17 11
dim(refit_data_long_coords_end) #   [1] 22 11
check <- ( rbind(refit_data_long_coords_start, refit_data_long_coords_end))

check %>%
  arrange(descr)


ggplot(refit_data_long_coords,
       aes(Xnew_flipped,
           Elevation,
           group = set)) +
  geom_segment(aes(x = Xnew_flipped,
                   y = Elevation,
                   xend = Xnew_flipped,
                   yend = Elevation,
                   group = set))

ggplot(stone_artefacts_only,
       aes(Xnew_flipped,
           Elevation)) +
  geom_point() +
  geom_text_repel(aes(label = Description)) +

  # tess murphey particle size SW section
  # Lindsey Hess particle size SW - report, code and data
  # Makiah Salinas particle size NE section - report, code and data
  # Mikaela Svob Mag sus

  plotting_data_ <- plotting_data
plotting_data_$depth <-  depth
plotting_data_long <- tidyr::gather(plotting_data_,
                                    variable,
                                    value,
                                    -depth)

ggplot(plotting_data_long,
       aes(x = value,
           y = depth)) +
  scale_y_reverse() +
  geom_line() +
  facet_wrap(~variable,
             nrow = 1,
             scales = "free") +
  theme_minimal()



plot_list <- vector("list", length = ncol(plotting_data))
for(i in 1:ncol(plotting_data)){
  n <- i
  # extract one col
  plotting_data_gg <- data.frame(cbind(plotting_data[,n],
                                       depth))
  # omit NA
  plotting_data_gg <- plotting_data_gg[!is.na(plotting_data_gg[,1]),]
  # plot
  plot_list[[i]] <-
    ggplot(plotting_data_gg,
           aes(as.numeric(depth), as.numeric(V1))) +
    geom_line() +
    coord_flip() +
    scale_y_continuous(expand=c(0,0)) +
    # labels = c(round(min(plotting_data_gg$V1),2),
    #            round(max(plotting_data_gg$V1)),2),
    # breaks = c(round(min(plotting_data_gg$V1),2),
    #            round(max(plotting_data_gg$V1)),2)) +
    scale_x_reverse(breaks = c(3:0),
                    labels = c(3:0),
                    limits = c(3.2,0),
                    expand=c(0,0)) +

    theme_minimal() %+replace%  theme(plot.margin=margin(10, 10, 10, 10),
                                      axis.ticks = element_blank(),
                                      axis.text.y = element_blank(),
                                      axis.title.y = element_blank())

  # remove y-axis ticks and labels for all but first plot
  if(i > 1){
    plot_list[[i]] <-
      plot_list[[i]]
  } else {
    plot_list[[i]] <-
      plot_list[[i]] +
      xlab("Depth below surface (m)")
  }

}


library(gridExtra)
grid.newpage()
do.call(grid.arrange, c(plot_list, nrow=1))

cleaned_rotated_points_in_main_excavation_area[grepl("E3_5A",  cleaned_rotated_points_in_main_excavation_area$Description), ]

sf <- cleaned_rotated_points_in_main_excavation_area[grepl("E3_5A_SF",  cleaned_rotated_points_in_main_excavation_area$Description), ]

unique(sf$Description)


SF19 39 cm
SF20 44 cm
SF21 11 cm

uhijjkkkkkjjjijjjijyhjghhhughghgungbngbngknhjkbnhinkinuynguynngynungnyungu123456


E3/5A/SF20

the_points[is.na(the_points$find), ]
the_points[the_points$find == "",]

unique(stone_artefacts_only$find)

stone_artefacts_only[is.na(stone_artefacts_only$find),]

stone_artefacts_only[stone_artefacts_only$find == "",] # <- NA

[1] "L"     "HM"    "HMB" 1  "GS"    "LV" 1   "LFEMP" 1"LFEMD"1 "LTAL" 1 "LCAL"
[10] "AF"    NA      "AXE"   "LHUMP" 2"LHUMD" 2"ART"   ""      "LITHC" 1

# look into artefacts with no 'find'
xx <- cleaned_rotated_points_in_main_excavation_area
stone_artefacts_only_no_find <- rbind(xx[is.na(xx$find),],
                                      xx[xx$find == "",])
# get last bit of desc
last_bit_of_desc <- sapply(stone_artefacts_only_no_find$code, function(i) i[length(i)])
#
stone_artefacts_only[is.na(stone_artefacts_only$find),]$find <-
  gsub("[0-9]", "", last_bit_of_desc)


#  ------------------------------------------------------------------------

# where are the squares?
EL <- cleaned_rotated_points_in_main_excavation_area[grepl("EL", cleaned_rotated_points_in_main_excavation_area$Description), ]
# only B
EL_ <- EL[grepl("C", EL$Description), ]
# not centers
EL_ <- EL_[!grepl("_C$", EL_$Description), ]
EL_$code <- NULL
grep("C1", EL_$Description, value = TRUE)

# draw some vertical lines on for the squares, iterate their locations
library(ggplot2)
ggplot(stone_artefacts_only,
       aes(Xnew_flipped,
           depth_below_ground_surface,
           colour = find)) +
  geom_point(size = 0.5) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  scale_x_continuous(breaks = NULL) +
  xlab("Southwest Section") +
  ylab("Depth below ground surface (m)") +
  scale_color_discrete("Artefact\ntype") +
  geom_vline(xintercept = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6),
             colour = "grey80") +
  geom_point(data = EL_,
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "green")

# look at the plan view for lines also
ggplot(stone_artefacts_only,
       aes(Xnew_flipped,
           Ynew,
           colour = find)) +
  geom_point(size = 0.5) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  scale_x_continuous(breaks = NULL) +
  xlab("Southwest Section") +
  scale_color_discrete("Artefact\ntype") +
  geom_vline(xintercept = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6),
            colour = "grey80") +
  geom_hline(yintercept = c(2.5, 1.5, 0.5, -0.5, -1.5),
             colour = "grey80") +
  geom_point(data = EL,
             aes(Xnew_flipped,
                 Ynew),
             colour = "green")

# these are the vertical lines
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2

library(viridis)
library(grid)
p <- ggplot(stone_artefacts_only,
       aes(Xnew_flipped,
           depth_below_ground_surface,
           colour = find)) +
  geom_point(size = 0.5) +
  scale_y_reverse(limits = c(3,0),
                  breaks = seq(0, 3, 0.1)) +
  theme_minimal() +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("") +
  ylab("Depth below \nground surface (m)") +
  scale_color_viridis(discrete=TRUE,
                      "Artefact\ntype") +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(size = 5)))

row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
nums = paste0("B", 7:2)
row_mids <-  row_c[-length(row_c)] + diff(row_c)/2


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


# http://stackoverflow.com/a/16442029/1036500
gb <- ggplot_build(p)
# first check if theme sets an aspect ratio
ar <- gb$plot$coordinates$ratio

# second possibility: aspect ratio is set by the coordinates, which results in
# the use of 'null' units for the gtable layout. let's find out
g <- ggplot_gtable(gb)
nullw <- sapply(g$widths, attr, "unit")
nullh <- sapply(g$heights, attr, "unit")

# ugly hack to extract the aspect ratio from these weird units
if(any(nullw == "null"))
  ar <- unlist(g$widths[nullw == "null"]) / unlist(g$heights[nullh == "null"])



#  ------------------------------------------------------------------------


stone_artefacts_only_B_C <- stone_artefacts_only[grep("B|C", stone_artefacts_only$square), ]

# put age and error in separate cols
osl_ages$osl_age <-
  as.numeric(gsub(" .*$", "", osl_ages$`Age (ka)`))
osl_ages$osl_error <-
  as.numeric(gsub(".*± ", "", osl_ages$`Age (ka)`))

# The Bacon function
# for Bacon age models
bacon_df <- data.frame(labID = c(as.character(c14_ages$Description),
                               as.character(osl_ages$Description)),
                       age = c(c14_ages$Mean.14C.Age..BP.,
                                osl_ages$osl_age * 1000),
                       error = c(c14_ages$X1s.14C.Age..BP.,
                                 osl_ages$osl_error * 1000),
                       depth = c(c14_ages$depth_below_ground_surface * 100,
                                 -osl_ages$total_station_depth_below_surf * 100),
                       cc = c(rep(3, nrow(c14_ages)),
                              rep(0, nrow(osl_ages))))

# subset Sw and front samples
front <- "B3|B4|B5|C3|C4|C5|D3|D4|D5|E3|E4|E5|NW|SW"
back <- "B1|B2|C1|C2|D1|D2|E1|E2"
bacon_df <- bacon_df[grepl(back, bacon_df$labID), ]

write.csv(bacon_df, row.names = FALSE, "C:/Users/bmarwick/Downloads/WinBacon_2.2/winBacon_2.2/Cores/MJB/MJB.csv")

# delete everything except the CSV in this folder before running model
setwd("C:/Users/bmarwick/Downloads/WinBacon_2.2/winBacon_2.2/")
source("Bacon.R")
Bacon(core = "MJB", thick = 5, acc.mean = 200)
dev.off()
par(mfrow = c(2,1))
hist(Bacon.Age.d(210))

# check that CC's list to remove was removed
grep("8001", stone_artefacts_only$Description, value = TRUE)
# yes

B3_B4_B5_C3_C4_C5_D3_D4_D5_E3_E4_E5_NW_SW

# The Bchronology function fits the age-depth model outlined by Haslett and Parnell (2008).
library(Bchron)
bacon_df_sorted <- bacon_df %>% arrange(depth)
Out = Bchronology(ages=bacon_df_sorted$age,
                  ageSds=bacon_df_sorted$error,
                  calCurves=c(rep('intcal13', sum(bacon_df_sorted$cc == 3)),
                              rep('normal', sum(bacon_df_sorted$cc == 0))),
                  positions=bacon_df_sorted$depth,
                  positionThicknesses=5,
                  ids=bacon_df_sorted$labID)

plot(Out)

predictAges = predict(Out,
                      newPositions = c(210,270),
                      newPositionThicknesses=c(5,5))
par(mfrow = c(2, 1))
hist(predictAges[,1], main = "Bchron age at 210 cm")
hist(predictAges[,2], main = "Bchron age at 270 cm")


#  ------------------------------------------------------------------------

# reproduce ZJ's plot of tweaking OSL dates
library(ggplot2)
ggplot() +
  geom_point(data = c14_ages,
             aes(x = Bchron_Median,
                 y = depth_below_ground_surface),
             shape = 15,
             colour = "red",
             size = 3) +
  geom_point(data = osl_ages,
             aes(x = osl_age * 1000,
                 y = -total_station_depth_below_surf),
             shape = 18,
             colour = "blue",
             size = 3)  +
  scale_y_reverse(limits = c(2, 0)) +
  xlim(0,50000) +
  theme_minimal()

  ggplot(bacon_df,
      aes(x = age,
          y = depth,
          shape = as.character(cc),
          colour = as.character(cc),
          label = labID),
      size = 3) +
  geom_point()  +
  #geom_text_repel(size = 3) +
  labs(color = "Dating \nmethod") +
  scale_color_manual(labels = c("OSL", "C14"), values = c("blue", "red")) +
  scale_shape(guide=FALSE) +
  scale_y_reverse(limits = c(200, 0)) +
  xlim(0,50000) +
  theme_minimal()


# check what dates are hearth dates
hearths <- read.csv("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/MKII_both_years_hearth_features.CSV")

find_hull <- function(hearths) hearths[chull(hearths$Easting, hearths$Elevation), ]
hearth_hulls <-  plyr::ddply(hearths, "Description", find_hull)
hearth_hulls_labels <-
  hearth_hulls %>%
  group_by(Description) %>%
  summarise(Easting = mean(Easting),
            Elevation = mean(Elevation))

surf <- 100.693213
hearths$depth_below_surf <- hearths$Elevation - surf
hearth_hulls$depth_below_surf <-  hearth_hulls$Elevation - surf
hearth_hulls_labels$depth_below_surf <-  hearth_hulls_labels$Elevation - surf

library(ggrepel)
ggplot() +
  geom_point(data = hearths,
             aes(Easting,
                 depth_below_surf ),
             colour = "green") +
  geom_point(data = c14_ages[c14_ages$Lab.ID %in% c("OZQ460", "OZT587"), ],
             aes(Easting,
                 depth_below_surface),
             colour = "black",
             size = 5) +
  geom_polygon(data = hearth_hulls,
               aes(Easting,
                   depth_below_surf,
                   colour = Description,
                   fill = Description),
               alpha = 0.5) +
  geom_text_repel(data = hearth_hulls_labels,
                  aes(Easting,
                      depth_below_surf,
                      label = Description),
                  size = 5,
                  colour = "blue") +
  geom_point(data = c14_ages,
             aes(Easting,
                 depth_below_surface ),
             colour = "red") +
  geom_text_repel(data = c14_ages,
                  aes(Easting,
                      depth_below_surface,
                      label = Lab.ID),
                  size = 3) +
  theme_minimal()
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/MKII_both_years_hearths_and_c14_samples.png", width = 30, height = 20)


### RF askes about ages of hatchtet heads
library(docxtractr)
hatchets <- read_docx("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/AXE paper tables.docx")
hatchets_tbl <- docx_extract_all_tbls(hatchets)
hatchets_tbl_1 <- hatchets_tbl[[1]]
hatchets_tbl_1$`MJB excavation unit` <-
  gsub(" ", "_", hatchets_tbl_1$`MJB excavation unit`)

# Let's plot the dates, then plot polygons for the axes
hatchets_tbl_1_spits <-
  cleaned_rotated_points_in_main_excavation_area[grep(paste0(hatchets_tbl_1$`MJB excavation unit`,
                                                             collapse = "|"),
                                                             cleaned_rotated_points_in_main_excavation_area$Description), ]
# EH says that EGH are AXEs
hatchets_tbl_1_axes <-
  stone_artefacts_only[grep("AXE",  stone_artefacts_only$Description), ]
# make hulls for axes
find_axe_hull <- function(hatchets_tbl_1_axes) hatchets_tbl_1_axes[chull(hatchets_tbl_1_axes$Xnew_flipped,
                                                                     hatchets_tbl_1_axes$Elevation), ]
axe_hulls <-  plyr::ddply(hatchets_tbl_1_axes, "Description", find_axe_hull)
axe_hulls_labels <-
  axe_hulls %>%
  group_by(Description) %>%
  summarise(Xnew_flipped = mean(Xnew_flipped),
            Elevation = mean(Elevation))
library(ggrepel)
ggplot() +
  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "green") +
  geom_text_repel(data = c14_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(round(Bchron_Median/1000,2), " (", gsub("_$", "", square_spit), ")" )),
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
  geom_polygon(data = axe_hulls,
               aes(Xnew_flipped,
                   Elevation,
                   colour = Description,
                   fill = Description),
               alpha = 0.5) +
  geom_text_repel(data = axe_hulls_labels,
                  aes(Xnew_flipped,
                      Elevation,
                      label = Description),
                  size = 3,
                  colour = "blue") +
  ggtitle("Selected edge-ground hatchets heads with OSL and C14 ages on MJB SW section") +
  theme_minimal()
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/MKII_both_years_axes_and_ages.png", width = 30, height = 20)

# grinding stones for EH
# --E2/28A-- (not plotted - found in sieve)
# C3/44
# D1/37 (GS39)
#
# From 2015 pit
# B6/54 (GS79)
# B5/52 (GS73)

eh_gs <- c("GS39", "GS79", "GS73", "GS_39", "GS_79", "GS_73", "C3_44")

eh_gs_total_station_points <-
  cleaned_rotated_points_in_main_excavation_area[grep(paste0(eh_gs,
                                                             collapse = "|"),
                                                      cleaned_rotated_points_in_main_excavation_area$Description), ]

# deal with the spit polygon having so many unique descriptions
eh_gs_total_station_points$Description <-
  with(eh_gs_total_station_points,
                        ifelse(grepl("C3_44",
                                     Description),
                               "C3_44",
                               Description))

# make hulls for gs
find_gs_hull <- function(eh_gs_total_station_points) eh_gs_total_station_points[chull(eh_gs_total_station_points$Xnew_flipped,
                                                                                      eh_gs_total_station_points$Elevation), ]
gs_hulls <-  plyr::ddply(eh_gs_total_station_points, "Description", find_gs_hull)
gs_hulls_labels <-
  gs_hulls %>%
  group_by(Description) %>%
  summarise(Xnew_flipped = mean(Xnew_flipped),
            Elevation = mean(Elevation))


library(ggrepel)
ggplot() +
  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "green") +
  geom_text_repel(data = c14_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(round(Bchron_Median/1000,2), " (", gsub("_$", "", square_spit), ")" )),
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
  geom_polygon(data = gs_hulls,
               aes(Xnew_flipped,
                   Elevation,
                   colour = Description,
                   fill = Description),
               alpha = 0.5) +
  geom_text_repel(data = gs_hulls_labels,
                  aes(Xnew_flipped,
                      Elevation,
                      label = Description),
                  size = 3,
                  colour = "blue") +
  ggtitle("Selected grindstones with OSL and C14 ages on MJB SW section") +
  theme_minimal()
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/MKII_both_years_gs_and_ages.png", width = 30, height = 20)

# consider age from bacon age model
# get depth below in cm
surf <- 100.693213   # NE_SEC_TAPE_1
axe_depths_cm_below <- 100 * (surf - axe_hulls_labels$Elevation)
gs_depths_cm_below <- 100 * (surf - gs_hulls_labels$Elevation)
axe_gs_df <- data.frame(depths_below_surf_cm = c(axe_depths_cm_below,
                                                 gs_depths_cm_below),
                        Description = c(axe_hulls_labels$Description,
                                        gs_hulls_labels$Description))

# with depth correction to account for slope
# if in row 4-5-6, don't adjust, if in row 1-2-3, then -50 cm
back <- c("B1|C1|D1|E1|B2|C2|D2|E2|B3|C3|D3|E3")
axe_gs_df$depths_below_surf_cm_adj <-
  with(axe_gs_df,
       ifelse(grepl(back, Description),
                    depths_below_surf_cm - 50,
                    depths_below_surf_cm))
# run the Bacon model...

bacon_artefact_ages <- vector("list", length = nrow(axe_gs_df))
for(i in 1:length(bacon_artefact_ages)){
  bacon_artefact_ages[[i]] <-
    data.frame(Description = axe_gs_df$Description[i],
      bacon_age = Bacon.Age.d(axe_gs_df$depths_below_surf_cm_adj[i]))
}


bacon_artefact_ages_df <- bind_rows(bacon_artefact_ages)

ggplot(bacon_artefact_ages_df,
       aes(Description,
           bacon_age)) +
  geom_violin() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/Bacon_ages_gs_and_axes_50cm_correction.png", width = 15, height = 10)

# without 50 cm adjustment
bacon_artefact_ages_no_adj <- vector("list", length = nrow(axe_gs_df))
for(i in 1:length(bacon_artefact_ages)){
  bacon_artefact_ages_no_adj[[i]] <-
    data.frame(Description = axe_gs_df$Description[i],
               bacon_age = Bacon.Age.d(axe_gs_df$depths_below_surf_cm[i]))
}


bacon_artefact_ages_no_adj_df <- bind_rows(bacon_artefact_ages_no_adj)

ggplot(bacon_artefact_ages_no_adj_df,
       aes(Description,
           bacon_age)) +
  geom_violin() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/Bacon_ages_gs_and_axes_no_depth_correction.png", width = 15, height = 10)

# on same plot, corrected and uncorrected
bacon_artefact_ages_no_adj_df$adj <-  "0"
bacon_artefact_ages_df$adj <-  "-50"

bacon_artefact_ages_comb <- rbind(bacon_artefact_ages_no_adj_df, bacon_artefact_ages_df)
ggplot(bacon_artefact_ages_comb,
       aes(Description,
           bacon_age,
           colour = adj)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/Bacon_ages_gs_and_axes.png",
       width = 15, height = 10)

# ZJ's secret SW S dates
sw_col <-  read.table(header = TRUE, text = c("
total_station_depth_below_surf	'Age (ka)'	sample_ID
80	15	SW
117	24	SW
159	45	SW
165	49	SW
207	63	SW
213	60	SW
219	65	SW
231	86	SW
275	110	SW
325	130	SW"))
# get coords for these col samples
sw_coords <- read.csv("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/data/MKII_both_years_rotated_OSL_with_column.csv")
sw_coords$total_station_depth_below_surf <- round(-sw_coords$depth_below_surface * 100, 0)
sw_coords$upper <- round((surf - sw_coords$upper) * 100, 0 )
sw_coords$lower <- round((surf - sw_coords$lower) * 100, 0 )
sw_coords <- sw_coords[!is.na(sw_coords$upper),]

# match by ranges of depths
source("https://bioconductor.org/biocLite.R")
biocLite("GenomicRanges")
library(IRanges)
isnps <- with(sw_col, IRanges(total_station_depth_below_surf, width=1, names=sw_col))
igenes <- with(sw_coords, IRanges(upper, lower, names=sw_coords))
olaps <- findOverlaps(isnps, igenes)
queryHits(olaps)
subjectHits(olaps)
sw_col_join <- cbind(sw_col[queryHits(olaps),], sw_coords[subjectHits(olaps),])
names(sw_col_join) <-  c(
 "total_station_depth_below_surf" ,"Age..ka."                     ,
 "sample_ID"                      ,"X.1"                          ,
 "Description"                    ,"Easting"                      ,
 "Northing"                       ,"Elevation"                    ,
 "year"                           ,"X"                            ,
 "Y"                              ,"Xnew"                         ,
 "Ynew"                           ,"Xnew_flipped"                 ,
 "type"                           ,"square"                       ,
 "spit"                           ,"cnr"                          ,
 "find"                           ,"findn"                        ,
 "row"                            ,"Sample"                       ,
 "upper"                          ,"lower"                        ,
 "sample_ID1"                      ,"Description_short"            ,
 "OSL_ID"                         ,"depth_below_surface"          ,
 "total_station_depth_below_surf1")
sw_col_join <- sw_col_join %>%
                group_by(total_station_depth_below_surf) %>%
                summarise(Xnew_flipped = mean(Xnew_flipped),
                          Elevation = mean(Elevation),
                          Age..ka. = mean(Age..ka.))


ggplot() +
  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "green") +
  geom_text_repel(data = c14_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(round(Bchron_Median/1000,2), " (", gsub("_$", "", square_spit), ")" )),
                  size = 3) +
  geom_point(data = sw_col_join,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "purple") +
  geom_text_repel(data = sw_col_join,
                  aes(Xnew_flipped,
                      Elevation,
                      label = Age..ka.),
                  size = 3) +
  geom_point(data = osl_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "red") +
  geom_text_repel(data = osl_ages,
                  aes(Xnew_flipped,
                      Elevation,
                      label = paste0(osl_age, " (", Sample, ")" )),
                  size = 3)  +
  geom_polygon(data = gs_hulls,
               aes(Xnew_flipped,
                   Elevation,
                   colour = Description,
                   fill = Description),
               alpha = 0.5) +
  geom_text_repel(data = gs_hulls_labels,
                  aes(Xnew_flipped,
                      Elevation,
                      label = Description),
                  size = 3,
                  colour = "blue") +
    geom_polygon(data = axe_hulls,
                 aes(Xnew_flipped,
                     Elevation,
                     colour = Description,
                     fill = Description),
                 alpha = 0.5) +
    geom_text_repel(data = axe_hulls_labels,
                    aes(Xnew_flipped,
                        Elevation,
                        label = Description),
                    size = 3,
                    colour = "blue")  +
  ggtitle("Selected grindstones and axes with OSL and C14 ages on MJB SW section") +
  theme_minimal()
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/MKII_both_years_axes_gs_and_ages.png", width = 30, height = 20)

# what about a bacon model with just the back dates?
# using 'back' subset
bacon_df$labID <- as.character(bacon_df$labID )
bacon_df <- rbind(bacon_df,
                  data.frame(labID = sw_col_join$total_station_depth_below_surf,
                             age = sw_col_join$Age..ka. * 1000,
                             error = 5,
                             depth = 100 * (surf - sw_col_join$Elevation),
                             cc = 0))
# now run the model...
# check the ages we get for the model of the back dates:

bacon_artefact_ages <- vector("list", length = nrow(axe_gs_df))
for(i in 1:length(bacon_artefact_ages)){
  bacon_artefact_ages[[i]] <-
    data.frame(Description = axe_gs_df$Description[i],
               bacon_age = Bacon.Age.d(axe_gs_df$depths_below_surf_cm_adj[i]))
}


bacon_artefact_ages_df <- bind_rows(bacon_artefact_ages)

ggplot(bacon_artefact_ages_df,
       aes(Description,
           bacon_age)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0,50000)
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/Bacon_ages_gs_and_axes_back_dates_only.png", width = 15, height = 10)

stone_artefacts_only[grepl("8233", stone_artefacts_only$Description),]

ggplot(stone_artefacts_only_one[grepl("B4|B5|B6", stone_artefacts_only_one$Description) &
                                  stone_artefacts_only_one$depth_below_ground_surface > 2.5,],
       aes(Xnew_flipped,
           depth_below_ground_surface)) +
  scale_y_reverse() +
  geom_point() +
      geom_text_repel(aes(label = round(depth_below_ground_surface,2)))

# get points for the 1989 OSL ages

surf <- 100.693213
# look at MJB_15_section_drawgin.qgs section drawing in QGIS
# surf there, at 1989 column is 100.803

library(dplyr)
library(ggplot2)
library(plotly)

cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl("SW_SECT_N", Description)) %>%
  select(-code) %>%
  write.csv(., "SW_SECT_N_points.csv")

matches <- c("NAIL", "EL")
cleaned_rotated_points_in_main_excavation_area %>%
 # filter(Reduce(`&`, lapply(matches, grepl, Description))) %>%
  mutate(nail = grepl("SECT", Description)) %>%
  mutate(el = grepl("EL", Description)) %>%
  plot_ly(.,
          x = Xnew_flipped,
          y = Elevation,
          z = Ynew,
          text = Description,
          type = "scatter3d",
          color = nail,
          mode = "markers")

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
            meanX = mean(X))
write.csv(osl_1989_points, "E:/My Documents/My UW/Research/1206 M2 excavation/Section photos/Depth of 1989 OSL samples/osl_1989_points.csv")

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
                      label = paste0(round(Bchron_Median/1000,2), " (", gsub("_$", "", square_spit), ")" )),
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
                  nudge_x = -0.5)



p <-
  ggplot() +
  geom_point(data = c14_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "green") +

  geom_point(data = osl_ages,
             aes(Xnew_flipped,
                 Elevation ),
             colour = "red") +

  geom_point(data = osl_1989_points,
                 aes(x = meanX,
                     #ymin = minY,
                     y = maxY),
                 colour = "blue")

ggplotly(p)

# total count of artefacts without haematite
library(dplyr)
stone_artefacts_only %>% filter(find == "L") %>% nrow


# total count of in situ cultural
in_situ_cultural <-
cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl("PF_", Description)) %>%
  filter(!grepl("OSL|C14X|BADSHOT|NA", Description))
# what do we have exactly?
unique(in_situ_cultural$find)
# how many unique objects/features recorded?
length(unique(in_situ_cultural$Description))
#

# how many from the lowest dense pulse?
library(plotly)
plot_ly(stone_artefacts_only_B_C,
        type = "scatter",
        x = Xnew_flipped,
        y = -depth_below_ground_surface,
        mode = "markers")

# alook at ZJ's phases and our artefacts...
wanted <- "B3|B4|B5|C3|C4|C5|D3|D4|D5|E3|E4|E5"
wanted_Lithics <- stone_artefacts_only[grepl(wanted, stone_artefacts_only$Description), ]
wanted_Lithics$depth_below_surface <- - (wanted_Lithics$Elevation - surf)

#  CC says
#  C4-B6, I think they are best described as
# 2.1-2.6m for the lower pulse, 1-1.7 for the middle pulse, and 0.4-0.8
# for the upper pulse, trying to account for the slight dip.

zj_artefact_pulses <-    c(0.05, 0.7,  1,    1.5,  2.15, 2.6) # blue
bm_artefact_pulses  <-   c(0.35, 0.70, 0.95, 1.55, 2.3,  2.7) # red
cc_artefact_pulses <- c(0, 0.40, 0.80, 1.0,  1.7,  2.1,  2.6) # green
best <-            c(0.05, 0.35, 0.70, 0.95, 1.55, 2.15, 2.6)

mini_scale <- seq(0, 3, 0.05)

zj_phases <- c(0, 0.65, 1.0, 1.4, 2.1, 2.6, 2.9)
zj_phases_ages <- c("4,030 - 0",       # phase 6 end
                    "7,690 - 5,990",   # phase 6 start
                    "8,880 - 7,020",   # phase 5 end
                    "13,980 - 9,190",  # phase 5 start
                    "22,560 - 11,300", # phase 4 end
                    "26,670 - 21,960", # phase 4 start
                    "29,280 - 24,290", # phase 3 end
                    "54,110 - 48,090", # phase 3 start
                    "55,930 - 50,240", # phase 2 end
                    "69,170 - 60,790", # phase 2 start
                    "??? - ???",       # phase 1 end
                    ">87,000 - 72,880" # phase 1 start
                  )

zj_phases_ages_locations <- sort(c((zj_phases + 0.05), (zj_phases - 0.05)))
zj_phases_ages_locations <-  zj_phases_ages_locations[-c(1, length(zj_phases_ages_locations))]

ggplot(wanted_Lithics,
  aes(depth_below_surface)) +
  geom_histogram(binwidth = 0.05) +
  theme_minimal(base_size = 14) +
  geom_vline(xintercept=zj_artefact_pulses,
             colour = "blue",
             linetype="dotted",
             size = 1) +
  geom_vline(xintercept=bm_artefact_pulses,
             colour = "red",
             linetype="dotted",
             size = 1) +
  annotate("text",
           x = mini_scale,
           y = rep(-3, length(mini_scale)),
           label = mini_scale,
           size = 2.5)  +
  geom_vline(xintercept=cc_artefact_pulses,
             colour = "green",
             linetype="dotted",
             size = 1) +
  coord_flip() +
  scale_x_reverse(limits = c(3,0)) +
  xlab("Depth below surface (m)") +
  ylab("Number of artefacts") +
  ggtitle("Plotted lithics from B/C/D-3/4/5")

### artefact size sorting
nonartefacts <- readxl::read_excel("E:\\My Documents\\My UW\\Research\\1206 M2 excavation\\1506 M2 excavation\\data\\nonartefacts.xlsx")
size_sorting_plotted_B6 <- readxl::read_excel("E:\\My Documents\\My UW\\Research\\1206 M2 excavation\\1506 M2 excavation\\data\\Size Sorting plotted from B6.xlsx")

# boxplot
library(ggbeeswarm)
ggplot(size_sorting_plotted_B6,
       aes(Spit, Mass,
           group = Spit)) +
 # geom_boxplot() +
  geom_quasirandom(alpha = 0.3,
                   size = 0.9) +
  scale_y_log10() +
  theme_minimal()

# with smoother
ggplot(size_sorting_plotted_B6,
       aes(Spit, Mass,
           group = Spit)) +
  geom_boxplot(colour = "grey20") +
  geom_quasirandom(alpha = 0.4,
                   size = 0.5) +
  geom_smooth(aes(group=1)) +
  scale_y_log10() +
  theme_minimal()

# stat tests
summary(aov(Spit ~ Mass, data = size_sorting_plotted_B6))
kruskal.test(Spit ~ Mass, data = size_sorting_plotted_B6)

## Raw materials.
library(tidyverse)
B6_raw_materials <- readr::read_csv("data/stone_artefact_data/B6_raw_material_table.csv")
C4_raw_materials <- readr::read_csv("data/stone_artefact_data/C4_raw_material_table.csv")

library(tidyverse)
library(viridis)
B6_raw_materials_technology_depths <-
B6_raw_materials %>%
  left_join(spit_depths_B6_output,
            by = c("Spit" = "spit")) %>%
  mutate(depth_below_surface = zoo::na.approx(depth_below_surface)) %>%
  mutate(depth_diff = c(0, diff(.$depth_below_surface)),
         x_centre = c(depth_below_surface[1]/2, zoo::rollmean(depth_below_surface, 2)),
         Quartzite = Qtztite,
         Quartz = Qtz)

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

B6_raw_materials_plot <-
  ggplot(B6_raw_materials_plot_data,
         aes(x_centre,
             `Artefacts per Litre`,
             fill = `Raw material`)) +
  geom_bar(stat = "identity",
           position = "stack",
           aes(width = depth_diff)) +
  scale_x_reverse(name = "Depth below surface (m)") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  theme_minimal()

B6_technology_plot_data <-
  B6_raw_materials_technology_depths %>%
  gather(`Technology`,
         value,
         -Spit,
         -depth_below_surface,
         -`Volume Excavated`,
         -depth_diff,
         -x_centre) %>%
  filter(`Technology` %in% c("Thinning Flakes",
                             "Retouched" ,
                             "Points",
                             "Cores",
                             "Bipolar",
                             "`Convergent Flakes`",
                             "`Axe Flakes`",
                             "`Grindstones and Fragments`"))  %>%
  mutate(`Artefacts per Litre` = as.numeric(value)/`Volume Excavated`,
         `Depth below surface (m)` = zoo::na.approx(round(depth_below_surface, 2)))

B6_technology_plot <-
  ggplot(B6_technology_plot_data,
         aes(x_centre,
             `Artefacts per Litre`)) +
  geom_bar(stat = "identity",
           aes(width = depth_diff,
               fill = `Technology`)) +
  scale_x_reverse(name = "") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  theme_minimal()

library(gridExtra)
grid.arrange(B6_raw_materials_plot,
             B6_technology_plot,
             ncol = 2)


# refit vertical distance

hist(with(refit_output$refit_data_coords_wide, Elevation.x  -  Elevation.y))
summary(abs(with(refit_output$refit_data_coords_wide, Elevation.x  -  Elevation.y)))


# refit plot

rf <-
ggplot() +

  geom_segment(data = refit_data_coords_wide,
               aes(x = Xnew_flipped.x,
                   y = depth_below_surface.x,
                   xend = Xnew_flipped.y,
                   yend = depth_below_surface.y),
               size = 1,
               colour =  viridis(10)[3]) +

  geom_point(data = refit_data_long_coords,
             aes(Xnew_flipped,
                 depth_below_surface),
             colour = viridis(10)[7]) +

  geom_text_repel(data = refit_data_long_coords,
                  aes(Xnew_flipped,
                      depth_below_surface,
                      label = descr)) +

  scale_y_reverse(limits = c(3, 0),
                  breaks = rev(seq(0, 3, 0.5))) +
  ylab("Depth below surface (m)") +
  xlab("") +
  coord_equal() +
  theme_minimal()



# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2

nums = paste0("B", 7:2)
row_mids <-  row_c[-length(row_c)] + diff(row_c)/2


for(i in 1:length(row_mids)){
  rf = rf + annotation_custom(grob = textGrob(nums[i], gp=gpar(fontsize=10)),
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

# mass by depth, major boundaries
library(ggbeeswarm)
  ggplot(size_sorting_plotted_B6,
         aes(depth_below_surface, Mass,
             group = depth_below_surface)) +
  # geom_boxplot(colour = "grey20") +
  geom_quasirandom(alpha = 0.1,
                   size = 1) +
  geom_smooth(aes(group=1)) +
  scale_y_log10() +
  theme_minimal() +
  ylab("Artefact mass (g)") +
  xlab("Depth below surface (m)") +
  scale_x_reverse(limits = c(3, 0),
                  breaks = rev(seq(0,3,0.5))) +
  geom_vline(xintercept = c(0.5, 1.25,
                            1.8, 2.6),
             colour = "red") +
  coord_flip()



## Simulation function to create data, analyze it using
## kruskal.test, and return the p-value
## change rexp to change the simulation distribution

simfun <- function(means, k=length(means), n=rep(50,k)) {
  mydata <- lapply( seq_len(k), function(i) {
    rexp(n[i], 1) - 1 + means[i]
  })
  kruskal.test(mydata)$p.value
}

# simulate under the null to check proper sizing
B <- 10000
out1 <- replicate(B, simfun(rep(3,4)))
hist(out1)
mean( out1 <= 0.05 )
binom.test( sum(out1 <= 0.05), B, p=0.05)

### Now simulate for power

B <- 10000
out2 <- replicate(B, simfun( c(3,3,3.2,3.3)))
hist(out2)
mean( out2 <= 0.05 )
binom.test( sum(out2 <= 0.05), B, p=0.05 )

# stat test of difference in size with depth
size_sorting_plotted_B6 <- readxl::read_excel("E:\\My Documents\\My UW\\Research\\1206 M2 excavation\\1506 M2 excavation\\data\\Size Sorting plotted from B6.xlsx")

# get depths
size_sorting_plotted_B6 <-
  left_join(size_sorting_plotted_B6,
            spit_depths_B6_output,
            by = c("Spit" = "spit") )

# looks like just one or two spits are sig, probably ones with very few artefacts
# If we exclude spits with <n artefacts
the_n <- 0
only_spits_with_more_than_n_artefacts <-
  size_sorting_plotted_B6 %>%
  group_by(Spit) %>%
  tally() %>%
  filter(n > the_n) %>%
  left_join(size_sorting_plotted_B6)


# test for heteroscedasticity with Levene's test:
library(car)
l_test <- leveneTest(Mass ~ as.factor(depth_below_surface),
                     only_spits_with_more_than_n_artefacts)

# check to see what the variances of the groups are
# linear models are fairly robust to heterogeneity of variance
# so long as the maximum variance is no more than 4× greater
# than the minimum variance. If it's high, we cannot use ANOVA
library(tidyverse)
variance_ratio <-
  only_spits_with_more_than_n_artefacts %>%
  group_by(depth_below_surface) %>%
  summarise(vars = var(Mass, na.rm = TRUE)) %>%
  summarise(ratios = range(.$vars, na.rm = TRUE)[2] / range(.$vars, na.rm = TRUE)[1])

# resampling test in place of classic ANOVA
library(coin)
oneway_test_result <-
  oneway_test(Mass ~ as.factor(depth_below_surface),
              data = only_spits_with_more_than_n_artefacts,
              distribution=approximate(B=1e5))

# Games-Howell Post-Hoc Test, does not assume equal variances and sample sizes.
library(userfriendlyscience)
post_hoc_test <-
  with(only_spits_with_more_than_n_artefacts,
       posthocTGH(Mass, as.factor(depth_below_surface)))

# filter output to see what comparisons are significant
sig_comparisons <-
  post_hoc_test$output$games.howell %>%
  as_data_frame() %>%
  mutate(comparison = unlist(dimnames(post_hoc_test$output$games.howell)[1])) %>%
  filter(p < 0.05) %>%
  separate(comparison,
           into = c("spit_a",
                    "spit_b"),
           sep = ":",) %>%
  mutate(spit_a = round(as.numeric(spit_a), 3),
         spit_b = round(as.numeric(spit_b), 3))

# Bayesian linear regression for depth and mass
library(rstanarm)
stan_lm_output <-
stan_lm(Mass ~ depth_below_surface,
        data = only_spits_with_more_than_n_artefacts,
        prior = NULL,
        chains = 1,
        cores = 2,
        seed = 1)

mean(as.data.frame(stan_lm_output)$depth_below_surface < 0) # Pr(beta_{Density} < 0)

# slope
slope <- as.data.frame(stan_lm_output)$depth_below_surface
hist(slope)
mean(slope)
quantile(slope, probs=c(.025,.975))

# r-squared
r_squared <- as.data.frame(stan_lm_output)$R2
hist(r_squared)
mean(r_squared)
quantile(r_squared, probs=c(.025,.975))


launch_shinystan(stan_lm_output)

draws <- as.data.frame(stan_lm_output)
colnames(draws)[1:2] <- c("a", "b")

pp_check(stan_lm_output, check = "distributions", overlay = FALSE, nreps = 5)

only_spits_with_more_than_n_artefacts %>%
  filter(Mass < 100) %>%
ggplot(aes(x = depth_below_surface,
           y = Mass)) +
  geom_point(size = 1) +
  geom_abline(data = draws,
              aes(intercept = a,
                  slope = b),
              color = "skyblue",
              size = 0.2,
              alpha = 0.05) +
  geom_abline(intercept = coef(stan_lm_output)[1],
              slope = coef(stan_lm_output)[2],
              color = "skyblue4",
              alpha = 0.4,
              size = 1)



plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(stan_lm_output, "hist", fill = "skyblue") + plot_title

pp_check(stan_lm_output, check = "resid", overlay = TRUE)
plot(stan_lm_output, "ess")

rstan::stan_trace(stan_lm_output)

# split to look at upper band of artefacts and lower band of artefacts
upper_band <- size_sorting_plotted_B6 %>%
  filter(depth_below_surface > 0.5 &
         depth_below_surface < 1.25) %>%
  stan_lm(Mass ~ depth_below_surface,
          data = .,
          prior = NULL,
          chains = 1,
          cores = 2,
          seed = 1)

lower_band <- size_sorting_plotted_B6 %>%
  filter(depth_below_surface > 1.8 &
           depth_below_surface < 2.6) %>%
  stan_lm(Mass ~ depth_below_surface,
          data = .,
          prior = NULL,
          chains = 1,
          cores = 2,
          seed = 1)

devtools::install_github("gavinsimpson/analogue")
library("analogue")
set.seed(1)
df <- setNames(data.frame(matrix(rnorm(200 * 3), ncol = 3)),
               c("d13C", "d15N", "d18O"))
df <- transform(df, Age = 1:200)
exprs <- expression(delta^{13}*C,  # label for 1st variable
                    "salted \npeanuts",  # label for 2nd variable
                    delta^{18}*O)  # label for 3rd variable
Stratiplot(Age ~ .,
           data = df,
           labelValues = exprs,
           varTypes = "absolute",
           type = "h")

### Phase depths from ZJ SOM and bayesian model figure #####

# •	band 1 is the archaeologically sterile sand at the base of the deposit (4.6–2.6 m depth);
# •	band 2 is the lowest dense artefact layer (2.6–2.15 m depth);
# •	band 3 represents a ~65 cm-thick layer of lower lithic abundance (2.1–1.55 m depth);
# •	band 4 is the middle dense artefact layer (1.55–0.95 m depth);
# •	band 5 represents a ~30 cm-thick layer of lowest lithic abundance (0.95–0.70 m depth);
# •	band 6 is the uppermost dense artefact layer (0.70–0.35 m depth) and the only phase with a similarly high lithic abundance as the lowest dense artefact layer; and
# •	band 7 represents the uppermost 35 cm of deposit, which consists mostly of shell midden

phases <- frame_data(
  ~phase, ~upper, ~lower,
     1,     2.6,    4.6,
     2,     2.15,   2.6,
     3,     1.55,   2.1,
     4,     0.95,   1.55,
     5,     0.7,    0.95,
     6,     0.35,   0.7,
     7,     0.0,    0.35)




# We'll use 'coniss', a stratigraphically constrained cluster
# analysis by method of incremental sum of squares

diss <- dist(na.omit(plotting_data))
clust <- rioja::chclust(diss, method = "coniss")
# broken stick model to suggest significant zones, 3?
bstick(clust) # look for a sharp elbow, that's the ideal number of clusters
plot(clust, hang = -1)

#----------------------------------------------------

# explore artefact mass and depths


# Resampling ANOVA
size_sorting_stat_test_output <-
  size_sorting_stat_test(spit_depths_B6_output, the_n = 0)

# Bayesian linear regression for depth and mass
only_spits_with_more_than_n_artefacts <-
  size_sorting_stat_test_output$only_spits_with_more_than_n_artefacts

library(rstanarm)
stan_lm_output <-
  stan_lm(Mass ~ depth_below_surface,
          data = only_spits_with_more_than_n_artefacts,
          prior = NULL,
          chains = 1,
          cores = 2,
          seed = 1)

# slope
slope <- as.data.frame(stan_lm_output)$depth_below_surface
mean_slope <- mean(slope)
ninetyfive_credible_slope <- quantile(slope, c(0.025, 0.975))

# r-squared
r_squared <- as.data.frame(stan_lm_output)$R2
mean_r_squared <- mean(r_squared)
ninetyfive_credible_r_squared <-  quantile(r_squared, c(0.025, 0.975))

size_sorting_plotted_B6 <- readxl::read_excel("data\\stone_artefact_data\\size_sorting_plotted_from_B6.xlsx")

# get depths
size_sorting_plotted_B6 <-
  left_join(size_sorting_plotted_B6,
            spit_depths_B6_output,
            by = c("Spit" = "spit") )

# split to look at upper band of artefacts and lower band of artefacts
upper_band <- size_sorting_plotted_B6 %>%
  filter(depth_below_surface > 0.5 &
           depth_below_surface < 1.25) %>%
  stan_lm(Mass ~ depth_below_surface,
          data = .,
          prior = NULL,
          chains = 1,
          cores = 2,
          seed = 1)

lower_band <- size_sorting_plotted_B6 %>%
  filter(depth_below_surface > 1.8 &
           depth_below_surface < 2.6) %>%
  stan_lm(Mass ~ depth_below_surface,
          data = .,
          prior = NULL,
          chains = 1,
          cores = 2,
          seed = 1)


# note fining up of sediments when artefacts peak

# --------------------------------------------------------------------
# chi-sq on artefact raw matererial and phases


# chi-sq for raw material by depth
raw_materials_technology_chi <-
plot_raw_materials_technology$B6_raw_materials_plot_data %>%
  dplyr::select(depth_below_surface, `Raw material`, value) %>%
  arrange(desc(depth_below_surface))


# group spits into phases

raw_materials_technology_chi$depth_below_surface_round <-
  round(raw_materials_technology_chi$depth_below_surface, 2) * 100

# get start and end of phases
start <- the_phases$upper * 100
end <- c(350, (the_phases$lower * 100)[-1]) # extend depth to base of artefact

# Compute which layer each spit belongs in using the
# IRanges package

# source("https://bioconductor.org/biocLite.R")
# biocLite("GenomicRanges")
library(IRanges)
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

raw_materials_technology_chi$phases_from_depths <- phases_from_depths

# focus on  Qtztite, Qtz, Silcrete, Chert
raw_materials_technology_chi %>%
  filter(!`Raw material` %in% c('Glass', 'Mica', 'Volcanic')) %>%
  dplyr::select(phases_from_depths, `Raw material`, value) %>%
  group_by(`Raw material`, phases_from_depths ) %>%
  dplyr::summarise(artefact_count = sum(as.numeric(value))) %>%
  spread(key = `Raw material`, value = artefact_count, fill = 0) %>%
  dplyr::select(-phases_from_depths) %>%  # sum to get total n
  chisq.test()



# -------------------------------------------------------------

#  a plot of C14 samples and depths for rows 4-6.
c14_rows_4_5_5 <-
cleaned_rotated_points_in_main_excavation_area %>%
  filter(grepl("C14", Description),
         grepl("4|5|6", square))

MJB_C14_in_rows_4_6 <-
ggplot() +
  geom_point(data = stone_artefacts_only,
             aes(Xnew_flipped,
                 -depth_below_ground_surface,
                 text = Description),
             colour = "grey80") +
  geom_point(data = c14_rows_4_5_5,
             aes(Xnew_flipped,
                 -depth_below_ground_surface,
                 text = Description),
             colour = "red") +

  theme_bw() +
  xlab("Rows 4-6") +
  coord_fixed()

library(plotly)
ggplotly(MJB_C14_in_rows_4_6)


#---------------------------------------------------------------
##
# get OSL ages from the published paper!
library(tabulizer)
library(tidyverse)
f <- "D:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/paper/Clarkson_Jacobs_Marwick_2017_SI.pdf"
tab <- extract_tables(f, pages = 32)
df <- data.frame(tab[[2]])
df <- df[-c(1:4),]
df <- df %>%
  separate(X1, c("Sample", "depth"), sep = " ") %>%
  mutate(X9 = as.character(X9)) %>%
  separate(X9, into = c("osl_age", "other1", "osl_error", "other2"), sep = " ")

write_csv(df, "D:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/paper/mjbnaturepaper/analysis/data/ages/OSL_Data_tables_all_samples_table_from_SI.csv")

# No, that's not very easy to work with. Let's take the table from the most recent word doc for the SI and make that a data frame

library(readxl)
osl_ages_from_SI <-  read_excel("analysis/data/ages/OSL_Data_tables_all_samples_table_from_SI.xlsx")


