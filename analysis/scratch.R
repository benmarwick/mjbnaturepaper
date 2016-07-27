
# [] Make rare items plot above abundant ones
# [] Age-depth curve
# [] circular stats



library(ggplot2)

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

row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2

library(viridis)
library(grid)
p <- ggplot(stone_artefacts_only,
       aes(Xnew_flipped,
           depth_below_ground_surface,
           colour = find)) +
  geom_point(size = 0.5) +
  scale_y_reverse(limits = c(3,0)) +
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
                       error = c(c14_ages$X1..14C.Age..BP.,
                                 osl_ages$osl_error * 1000),
                       depth = c(c14_ages$depth_below_ground_surface * 100,
                                 -osl_ages$total_station_depth_below_surf * 100),
                       cc = c(rep(3, nrow(c14_ages)),
                              rep(0, nrow(osl_ages))))

# subset Sw and front samples
bacon_df <- bacon_df[grepl("B3|B4|B5|C3|C4|C5|D3|D4|D5|E3|E4|E5|NW|SW", bacon_df$labID), ]

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


ggplot() +
  geom_point(data = hearths,
             aes(Easting,
                 Elevation ),
             colour = "green") +
  geom_point(data = c14_ages[c14_ages$Lab.ID %in% c("OZQ460", "OZT587"), ],
             aes(Easting,
                 Elevation),
             colour = "black",
             size = 5) +
  geom_polygon(data = hearth_hulls,
               aes(Easting,
                   Elevation,
                   colour = Description,
                   fill = Description),
               alpha = 0.5) +
  geom_text_repel(data = hearth_hulls_labels,
                  aes(Easting,
                      Elevation,
                      label = Description),
                  size = 5,
                  colour = "blue") +
  geom_point(data = c14_ages,
             aes(Easting,
                 Elevation ),
             colour = "red") +
  geom_text_repel(data = c14_ages,
                  aes(Easting,
                      Elevation,
                      label = Lab.ID),
                  size = 3) +
  theme_minimal()
ggsave("E:/My Documents/My UW/Research/1206 M2 excavation/1506 M2 excavation/section photos/MKII_both_years_hearths_and_c14_samples.png", width = 30, height = 20)



data(iris)
df<-iris
find_hull <- function(df) df[chull(df$Sepal.Length, df$Sepal.Width), ]
hulls <- plyr::ddply(df, "Species", find_hull)
plot <- ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width, colour=Species, fill = Species)) +
  geom_point() +
  geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "Sepal.Length", y = "Sepal.Width")
plot






