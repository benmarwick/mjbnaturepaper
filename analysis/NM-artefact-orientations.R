library(tidyverse)
library(mjbnaturepaper) # devtools::install_github("benmarwick/mjbnaturepaper")
library(useful)
library(fuzzyjoin)
library(plotly)

# paper with maps etc. http://faculty.washington.edu/bmarwick/PDFs/Clarkson_Jacobs_Marwick_2017.pdf

cleaned_rotated_points_in_main_excavation_area <-
  readRDS(here::here("analysis/data/cleaned_rotated_points_in_main_excavation_area.rds"))

# end levels to check geometry
el <-
cleaned_rotated_points_in_main_excavation_area %>%
  dplyr::filter(type == "EL") %>%
  filter(cnr != "C") %>%
  mutate( X1 = Xnew_flipped,
          Y1 = Ynew,
          Z1 = depth_below_ground_surface)

# filter to keep only lithics with exactly 2 total station points,
# suitable for orientation, etc. analysis
lithics_with_two_points <-
cleaned_rotated_points_in_main_excavation_area %>%
  add_count(Description) %>%
  filter(n == 2) %>%
  filter(find == "L") %>%
  arrange(Description) %>%
  tibble()

# we also recorded 4 points on some artefacts, let's deal with those
# find the two points that define the long axis of the artefact
lithics_with_four_points <-
  cleaned_rotated_points_in_main_excavation_area %>%
  add_count(Description) %>%
  filter(n == 4) %>%
  filter(find == "L") %>%
  arrange(Description) %>%
  tibble()

# take a look at all possible combinations of the four points
# to find the max distance and see if they are useful here

which_pair_has_max_dist <- function(x){

dists <- (c(

dist(rbind(unname(unlist(x[1,c(1:3)])),
           unname(unlist(x[2,c(1:3)])))),

dist(rbind(unname(unlist(x[1,c(1:3)])),
           unname(unlist(x[3,c(1:3)])))),

dist(rbind(unname(unlist(x[1,c(1:3)])),
           unname(unlist(x[4,c(1:3)])))),

dist(rbind(unname(unlist(x[2,c(1:3)])),
           unname(unlist(x[4,c(1:3)])))),

dist(rbind(unname(unlist(x[3,c(1:3)])),
           unname(unlist(x[4,c(1:3)]))))
))

which_max <- which.max(dists)
max_dist <- max(dists)

return(list(dists = dists,
            which_max = which_max,
            max_dist = max_dist))
}

which_pair_has_max_dist_df <-
lithics_with_four_points %>%
  select( Description, Easting, Northing, Elevation ) %>%
  group_by(Description) %>%
  nest() %>%
  mutate(max_dists = map(data, ~which_pair_has_max_dist(.x)$max_dist)) %>%
  unnest(max_dists)
# they are all crazy large, probably not the artefacts we are looking for

# get into format x1, y1, z1, x2, y2, z2 for McPherron's workflow
# our values are  Ynew, Xnew_flipped, depth_below_ground_surface
lithics_with_two_points_mcpherron <-
lithics_with_two_points %>%
  group_by(Description) %>%
  mutate(id = row_number()) %>%
  filter(depth_below_ground_surface > 0) %>%
  select(Description, id, Ynew, Xnew_flipped, depth_below_ground_surface) %>%
  pivot_longer(cols = c(Ynew, Xnew_flipped, depth_below_ground_surface),
               names_to = "coord",
               values_to = "value") %>%
  mutate(coord = case_when(
    coord == "Xnew_flipped" ~ "X",
    coord == "Ynew" ~ "Y",
    coord == "depth_below_ground_surface" ~ "Z"
   )) %>%
  unite(col="coord_set",
        coord,
        id,
        sep = "") %>%
  pivot_wider(names_from = coord_set,
              values_from = value) %>%
  mutate(useful::cart2pol(X1-X2, Y1-Y2,  degrees = T)) %>%
  # suspicious number of artefacts all the same...
  # let's try to remove them
  filter(!near(theta, 100.821741, tol = 0.0005)) %>%
  filter(!near(theta, 280.821741, tol = 0.0005)) %>%
  # remove things that are improbably long for a stone artefact
  filter(r < 0.15)

# identify phases, we have phases() for rows 1-2-3 and front_phases() for rows 4-5-6
back_phases_tbl <- phases()
front_phases_tbl <- front_phases()

lithics_with_two_points_mcpherron_position <-
lithics_with_two_points_mcpherron %>%
  mutate(exc_row = str_remove(Description, ".{1,2}_")) %>%
  mutate(exc_row = str_remove(exc_row, "_.*")) %>%
  mutate(exc_row = parse_number(exc_row)) %>%
  mutate(exc_loc = ifelse(exc_row %in% 1:3, "back", "front"))

# non-equi join (join by range) to add phase information to depths

lithics_with_two_points_mcpherron_position_front_phase <-
lithics_with_two_points_mcpherron_position %>%
  filter(exc_loc == 'front') %>%
  mutate(Z_mean = mean(Z1, Z2)) %>%
  fuzzy_left_join(
    front_phases_tbl,
                  by = c("Z_mean" = "upper",
                         "Z_mean" = "lower"),
                  match_fun = list(`>=`, `<=`)) %>%
  distinct()

lithics_with_two_points_mcpherron_position_back_phase <-
  lithics_with_two_points_mcpherron_position %>%
  filter(exc_loc == 'back') %>%
  mutate(Z_mean = mean(Z1, Z2)) %>%
  fuzzy_left_join(
    back_phases_tbl,
    by = c("Z_mean" = "upper",
           "Z_mean" = "lower"),
    match_fun = list(`>=`, `<=`)) %>%
  distinct()


# combine again into one table
lithics_with_two_points_mcpherron_position_phases <-
bind_rows(lithics_with_two_points_mcpherron_position_front_phase,
          lithics_with_two_points_mcpherron_position_back_phase) %>%
  filter(!is.na(phase))

# how many in each phase?
lithics_with_two_points_mcpherron_position_phases %>%
  group_by(phase) %>%
  tally()

# for detailed analysis, consider phase 2 vs one group of phases 3-7

# take a look

# distances from one end of the artefact to another
ggplot(lithics_with_two_points_mcpherron_position_phases,
       aes(r)) +
  geom_histogram()

ggplot(lithics_with_two_points_mcpherron_position_phases,
       aes(theta)) +
  geom_histogram() +
  coord_polar(start = 0)

# plan view
p1 <-
ggplot() +
  geom_segment(data = lithics_with_two_points_mcpherron_position_phases,
               aes(x = X1,
                   y = Y1,
                   xend = X2,
                   yend = Y2),
               colour = "red") +
  theme_minimal() +
  coord_equal()

p1

p1 +
  geom_point(data = el,
             aes(x = X1,
                 y = Y1),
             alpha = 0.05)

# section view
p2 <-
  ggplot() +
  geom_segment(data = lithics_with_two_points_mcpherron_position_phases,
               aes(x = Y1,
                   y = Z1,
                   xend = Y2,
                   yend = Z2,
                   colour = as.factor(phase))) +
  scale_y_reverse() +
  theme_minimal() +
  coord_equal()

p2

p2 +
  geom_point(data = el,
             aes(x = Y1,
                 y = Z1),
             alpha = 0.05)

plotly::ggplotly(p2)


