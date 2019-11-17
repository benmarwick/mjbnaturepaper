
library(here)
dC13_data <- readxl::read_excel(here("analysis/data/geoarchaeology_data/Page_160404_CombinedResults final full.xlsx"))

# clean_and_tidydC13_data

# data are in rows 4 to 78
dC13_data_rows <- c(4:78)
# sample IDs, etc
sample_ID <- dC13_data[ dC13_data_rows, ][[2]]
sample_mass <- as.numeric(dC13_data[ dC13_data_rows, ][[9]])
d15N <- as.numeric(dC13_data[ dC13_data_rows, ][[16]])
d13C_corrected <- as.numeric(dC13_data[ dC13_data_rows, ][[27]])
# col 6 is depth, remember that ground surface was 1 m on the tape
depth <- as.numeric(dC13_data[ dC13_data_rows, ][[6]]) / 100 - 1
# assign run numbers using date
analysis_date <- as.numeric(dC13_data[ dC13_data_rows, ][[1]])

# put into dataframe
d13C_depth <- data.frame(sample_ID = sample_ID,
                         sample_mass = sample_mass,
                         d15N = d15N,
                         depth = depth ,
                         d13C_corrected = d13C_corrected,
                         analysis_date = analysis_date
)

# there are some replicates, let's identify them and get
# max and min values for each sample
d13C_depth_means <- d13C_depth %>%
  group_by(factor(depth)) %>%
  summarize(mean_d13C_corrected = mean(d13C_corrected, na.rm = TRUE),
            max_d13C_corrected = max(d13C_corrected),
            min_d13C_corrected = min(d13C_corrected),
            diff_d13C_corrected = max_d13C_corrected - min_d13C_corrected) %>%
  mutate(depth = (as.character(`factor(depth)`))) %>%
  select(-`factor(depth)`)


# get depths

# NE_SECT_TAPE 1 is the ground surface, then the other points are one meter apart
NE_SEC_TAPE <-
  cleaned_rotated_points_in_main_excavation_area[grepl("NE_SEC_TAPE",
                                                       cleaned_rotated_points_in_main_excavation_area$Description), ]

# put tape and total station depths together
tape_depths <- data.frame(tape = 0:3,
                          total_station = NE_SEC_TAPE$Elevation)

# need to interpolate to 5 cm to match with mag_sus_B2$Depth.Below.Surface..cm.

# mag sus from KL
mag_sus_B2 <- read.csv(here("analysis/data/geoarchaeology_data/MJB_Lowe2016.csv"), stringsAsFactors = FALSE, fileEncoding="latin1")
mag_sus_B2 <- mag_sus_B2[c(2:54), ]
mag_sus_B2$Depth.Below.Surface..m <- as.numeric(mag_sus_B2$X)
# remove the C3 stuff
mag_sus_B2$X <- NULL
mag_sus_B2$SQB2.1 <- NULL
mag_sus_B2$SQC3 <- NULL
mag_sus_B2$SQC3.1 <- NULL

xout <- seq(min(mag_sus_B2$Depth.Below.Surface..m),
            3.5,
            by = 0.01)

# approx returns a list, convert to dataframe
# we also need to extrapolate a little
tape_depths_interp <- data.frame(do.call(cbind,
                                         Hmisc::approxExtrap(tape_depths$tape,
                                                             tape_depths$total_station,
                                                             n = length(xout),
                                                             xout = xout)))
names(tape_depths_interp) <- names(tape_depths)
tape_depths_interp$tape <- as.character(tape_depths_interp$tape )

# get the total station depths for the tape depths for dC13

d13C_depth_means_total_station <-
  dplyr::left_join(d13C_depth_means,
                   tape_depths_interp,
                   by = c('depth' = 'tape'))

# get phases from B2, emailed from CC
phases_and_depths <- readxl::read_excel(here("analysis/data/Phases and depths for all spits.xlsx"))

phases_and_depths_b2 <-
phases_and_depths %>%
  filter(Square == "B2")

b2_extrapolate <-
bind_rows(
Hmisc::approxExtrap(phases_and_depths_b2$Depth,
                    phases_and_depths_b2$Phase,
                    n = length(xout),
                    xout = xout)) %>%
  mutate(phase = as.integer(y))

#-----------------------------------------------
# want:
# - C isotopes
# - the lithic raw material and
# - lithic technology types

# compute polynomial regression
wanted_both_methods_model <- lm(age ~
                                  poly(depth_below_surface, 2),
                                data = na.omit(depth_age_tbl))

# interpolate ages using age model fit of both c14 and OSL
# we can use this to get age of depths
ages_of_xout <- tibble(depth = xout,
   age = unname(predict(wanted_both_methods_model,
                                 data.frame(depth_below_surface = xout))))

ggplot(depth_age_tbl,
       aes(age, depth_below_surface)) +
  geom_point() +
  geom_line(data = ages_of_xout,
             aes(age, depth),
            colour = "green")

# join ages onto isotope depths
d13C_depth_means_total_station_phases <-
d13C_depth_means_total_station %>%
  left_join(b2_extrapolate %>%
              mutate(depth = as.character(x))) %>%
  left_join(ages_of_xout %>%
              mutate(depth = as.character(depth))) %>%
  mutate(age = age * 1000)

# apply phase-wise corrections
volker_offsets <-
  readxl::read_excel(here("analysis/data/geoarchaeology_data/Voelker-2016a Offsets.xlsx")) %>%
  separate(`Age range`,
           into = c("start", "end"),
           sep = "-") %>%
  mutate_at(vars(start, end),
            parse_number)

hare_offsets <-
  readxl::read_excel(here("analysis/data/geoarchaeology_data/mmc1.xlsx"), skip = 39)

hare_offsets_correction <-
  hare_offsets %>%
  mutate(correction = hare_offsets$`Voelker-2016a (angiosperms)`[1] - `Voelker-2016a (angiosperms)`)

# join on age at one decimal place
d13C_depth_means_total_station_phases_chr <-
d13C_depth_means_total_station_phases %>%
  mutate(age_chr = as.character(round(age /1000, 1))) %>%
  left_join(hare_offsets_correction %>%
              select(`AGE (kyr)`, correction) %>%
              mutate(age_chr = as.character(round(`AGE (kyr)`, 1)))) %>%
  mutate(correction = zoo::na.approx(correction, rule = 2)) %>%
  mutate(archy_corrected_d13 = mean_d13C_corrected - correction) %>%
  mutate(phase = ifelse(age < 747, 7, phase))

iso_limits <- c(-31, -25)

phases_midpoints <-
structure(list(start = c(65000, 51600, 40000, 26700, 9700, 602,
                         150), end = c(52700, 40000, 28100, 13200, 8000, 599, -69)), class = c("tbl_df",
                                                                                               "tbl", "data.frame"), row.names = c(NA, -7L))

ggplot(d13C_depth_means_total_station_phases_chr,
       aes(age,
           archy_corrected_d13)) +
  geom_line() +
  theme_minimal() +
  scale_x_reverse(labels = scales::comma,
                  limits = c(70000, -1000),
                  breaks = seq(70000, 0, -10000)) +
  labs(y = expression(delta^13*C~V*`-`*PDB~('%')),
       x = "Years BP") +

  annotate("rect",
           xmax = phases_midpoints$start[1],
           xmin = phases_midpoints$end[1],
           ymax = -Inf,
           ymin = Inf,
           # fill = viridis::viridis(7)[1],
           alpha = 0.1) +

  annotate("rect",
           xmax = phases_midpoints$start[2],
           xmin = phases_midpoints$end[2],
           ymax = -Inf,
           ymin = Inf,
           # fill = viridis::viridis(7)[2],
           alpha = 0.1) +

  annotate("rect",
           xmax = phases_midpoints$start[3] - 500,
           xmin = phases_midpoints$end[3],
           ymax = -Inf,
           ymin = Inf,
           # fill = viridis::viridis(7)[3],
           alpha = 0.1) +

  annotate("rect",
           xmax = phases_midpoints$start[4],
           xmin = phases_midpoints$end[4],
           ymax = -Inf,
           ymin = Inf,
           # fill = viridis::viridis(7)[4],
           alpha = 0.1) +

  annotate("rect",
           xmax = phases_midpoints$start[5],
           xmin = phases_midpoints$end[5],
           ymax = -Inf,
           ymin = Inf,
           # fill = viridis::viridis(7)[5],
           alpha = 0.1) +

  annotate("rect",
           xmax = phases_midpoints$start[6],
           xmin = phases_midpoints$end[7],
           ymax = -Inf,
           ymin = Inf,
           #fill = viridis::viridis(7)[6],
           alpha = 0.1)



# boxplot
library(ggbeeswarm)
ggplot(d13C_depth_means_total_station_phases_chr,
       aes(as.factor(phase),
           archy_corrected_d13 )) +
  geom_boxplot() +
  geom_quasirandom() +
  theme_minimal()


# Lithic raw materials by age

# from functions.R

B6_raw_materials_technology_depths <-
  B6_raw_materials %>%
  left_join(spit_depths_B6_output,
            by = c("Spit" = "spit")) %>%
  mutate(depth_below_surface = zoo::na.approx(depth_below_surface)) %>%
  mutate(depth_diff = c(0, diff(.$depth_below_surface)),
         x_centre = c(depth_below_surface[1]/2, zoo::rollmean(depth_below_surface, 2)),
         Quartzite = Qtztite,
         Quartz = Qtz)

# add the ages on to the depths

B6_raw_materials_technology_depths_ages <-
B6_raw_materials_technology_depths %>%
  mutate(depth = as.character(round(depth_below_surface, 2))) %>%
  left_join( ages_of_xout %>% mutate(depth = as.character(depth) )) %>%
  mutate(age_diff = c(0, diff(.$age)),
         age_centre = c(age[1]/2, zoo::rollmean(age, 2)),
         Quartzite = Qtztite,
         Quartz = Qtz)


B6_raw_materials_plot_data <-
  B6_raw_materials_technology_depths_ages %>%
  gather(`Raw material`,
         value,
         -Spit,
         -age,
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
  mutate(`Artefacts per Litre` = as.numeric(value)/`Volume Excavated`)

library(viridis)

B6_raw_materials_plot <-
  ggplot(B6_raw_materials_plot_data,
         aes(age,
             `Artefacts per Litre`,
             fill = `Raw material`)) +
  geom_bar(stat = "identity",
           position = "stack",
           aes(width = depth_diff)) +
  scale_x_reverse(name = "Depth below surface (m)") +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 1,
                             title.position = "bottom")) +
  theme(aspect.ratio = 1/4,
        legend.position="bottom",
        legend.box = "horizontal")



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
  theme_minimal() +
  guides(fill = guide_legend(nrow = 4, title.position = "bottom")) +
  theme(aspect.ratio = 3,
        legend.position="bottom",
        legend.box = "horizontal")

grid::grid.draw(cbind(ggplot2::ggplotGrob(B6_raw_materials_plot),
                      ggplot2::ggplotGrob(B6_technology_plot),
                      size = "first"))





























