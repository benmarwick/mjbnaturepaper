#' mjbnaturepaper: A package containing code and data for analysis of the 2012 and 2015 MJB excavations
#'
#' The foo package provides ...
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name mjbnaturepaper
NULL

#' read_2015_total_station_data
#'
#' @param total_station_csv_directory_2015 A directory containing one or more CSV files
#'
#' @return a data frame
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @export
#'
read_2015_total_station_data <- function(total_station_csv_directory_2015){

  # read in CSV for 2015 data: "data/MKII15_04JUL15.CSV"

  files_2015 <- dir(total_station_csv_directory_2015,
               recursive=TRUE,
               full.names=TRUE,
               pattern="\\.(csv|CSV)$")
  # read in the CSV files and add the filename of each file as a column to
  # each dataset so we can trace back dodgy data
  # create a function to read tehe CSV and get filenames
  read.tables <- function(file.names, ...) {
    plyr::ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)),.progress = 'text')
  }
  # execute function
  ts_data <- read.tables(files_2015, stringsAsFactors = FALSE)

  # simplify to join with 2012 data
  ts_data_2015 <-  ts_data[,c('Description', 'Easting', 'Northing', 'Elevation')]
  ts_data_2015$year <- "2015"

  # remove duplicates
  # ts_data_2015 <- ts_data_2015[!duplicated(ts_data_2015),]


  return(ts_data_2015)
}

#' read_2012_total_station_data
#'
#' @param total_station_csv_directory_2012 A directory containing one or more CSV files
#'
#' @return a data frame
#'
#' @importFrom plyr ldply
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @export
#'
read_2012_total_station_data <- function(total_station_csv_directory_2012){

  # "data/MII 2012 Total_Station_Data"
  files_2012 <- dir(total_station_csv_directory_2012,
               recursive=TRUE,
               full.names=TRUE,
               pattern="\\.(csv|CSV)$")
  # read in the CSV files and add the filename of each file as a column to
  # each dataset so we can trace back dodgy data
  # create a function to read tehe CSV and get filenames
  read.tables <- function(file.names, ...) {
    plyr::ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)),.progress = 'text')
  }
  # execute function
  points.all <- read.tables(files_2012, stringsAsFactors = FALSE)

  # put all the upper case
  points.all$description <- toupper(points.all$description)
  points.all$Description <- toupper(points.all$Description)

  # how many NAs in the Desc?
  # nrow(points.all)
  # length(points.all$description[points.all$description == "NA"])
  # length(points.all$Description[points.all$Description == "NA"])

  # if 'Description' is empty, move the contents of the 'description' into it

  points.all$Description <-
    ifelse(is.na(points.all$Description),
           points.all$description,
           points.all$Description)


  # simplify x-y-z columns for days when different headers were used by the total station operator
  points.all$Xx <-
    ifelse(grepl('[0-9]+', points.all$x),
           points.all$x,
       ifelse(grepl('[0-9]+', points.all$Easting),
              points.all$Easting,
          ifelse(grepl('[0-9]+', points.all$Eastig),
                 points.all$Eastig,
                 points.all$X)))


points.all$Yy <-
  ifelse(grepl('[0-9]+', points.all$y),
         points.all$y,
      ifelse(grepl('[0-9]+', points.all$Northing),
             points.all$Northing, points.all$Y))


points.all$Zz <-
  ifelse(grepl('[0-9]+', points.all$z),
         points.all$z,
    ifelse(grepl('[0-9]+', points.all$Z),
           points.all$Z, points.all$Elevation))


  # remove duplicates by looking at the location and removing points
  # that have the exact same x-y-z coord
  points.all$coords <-  paste0(points.all$Xx, points.all$Yy, points.all$Zz)

  # identify and remove duplicate rows in the data frame
  points.all <- points.all[!duplicated(points.all$coords), ]

  # On some days it seems like the total station swapped columns
  # for northing and easting in the CSV files, so we check to make
  # sure Northing is always greater than Easting. They were originally
  # set at 5000, 5000 (better would have been 5000, 2000, so
  # it's obvious which is which). Y must always be bigger than X
  # for correct measurements

  points.all$xgreaterthany <- ifelse(points.all$Xx > points.all$Yy, "X>Y", "ok")

  points.all$Xall <- ifelse(points.all$xgreaterthany == "X>Y", points.all$Yy, points.all$Xx)
  points.all$Yall <- ifelse(points.all$xgreaterthany == "X>Y", points.all$Xx, points.all$Yy)


  surf <- 100.693213   # NE_SEC_TAPE_1
  points.all$depth.below.surface <-  surf - points.all$Zz


  # now that we've got the right angle, rotate all the artefact data...
  # limit to dig area
  EL <- points.all[grepl("EL_", points.all$Description),]


  points.all <- points.all[points.all$Xall < max(na.omit(EL$Xall)) &
                      points.all$Xall > min(na.omit(EL$Xall)) &
                      points.all$Yall < max(na.omit(EL$Yall)) &
                      points.all$Yall > min(na.omit(EL$Yall))
                    , ]

  # make some common column names
  points.all$Easting <- points.all$Xall
  points.all$Northing <- points.all$Yall
  points.all$Elevation <- points.all$Zz
  # already have 'Description'

  # simplify to join with 2015 data
  ts_data_2012 <-  points.all[,c('Description', 'Easting', 'Northing', 'Elevation')]
  ts_data_2012$year <- "2012"

return(ts_data_2012)

}


#' combine_2012_and_2015_total_station_data
#'
#' @param total_station_data_2012
#' @param total_station_data_2015
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr %>% filter
#'
combine_2012_and_2015_total_station_data <- function(total_station_data_2012,
                                                     total_station_data_2015){
  # combine 2012 and 2015 data --------------------------------------------------

  ts_data_both_years <- rbind(total_station_data_2012,
                              total_station_data_2015)

}


#' extract_points_in_main_excavation_area
#'
#' @param combined_2012_and_2015_total_station_data
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr %>% filter
extract_points_in_main_excavation_area <- function(combined_2012_and_2015_total_station_data){

  # still a few stray points way out of the excavation area, so remove those with Easting < 4995, Northing >5015
  combined_2012_and_2015_total_station_data_inside <-
    combined_2012_and_2015_total_station_data %>%
    filter(Easting > 4995, Northing < 5015)

# get only labelled finds and other points of interest
  excavation_area <- c("PF", "NAIL", "OSL", "EL", "P", "PL", "GAMMA", "MM", "SL", "SEC", "SECT")

  ts_data_both_years_PFs <- combined_2012_and_2015_total_station_data_inside %>%
    filter(grepl(paste(excavation_area, collapse = "|"), Description) )

  return(ts_data_both_years_PFs)

}


#' rotate_points_in_main_excavation_area
#'
#' @param extracted_points_in_main_excavation_area
#'
#' @return a data frame
#' @importFrom stats na.omit
#' @importFrom dplyr mutate
#' @export
#'
rotate_points_in_main_excavation_area <- function(extracted_points_in_main_excavation_area){

  # Rotate points from both seasons -----------------------------------------


  # get means

  lx <- mean(na.omit(extracted_points_in_main_excavation_area$Easting))
  ly <- mean(na.omit(extracted_points_in_main_excavation_area$Northing))

  # first translate all points to new origin

  extracted_points_in_main_excavation_area$X <-
    extracted_points_in_main_excavation_area$Easting - lx
  extracted_points_in_main_excavation_area$Y <-
    extracted_points_in_main_excavation_area$Northing - ly

  # check with a quick look, should
  # have 0,0 in the middle of the plot

  # rotate around new origin
  deg = 46 #
  theta = deg*pi/180
  extracted_points_in_main_excavation_area$Xnew <-
    (extracted_points_in_main_excavation_area$X * cos(theta)) - (extracted_points_in_main_excavation_area$Y * sin(theta))

  extracted_points_in_main_excavation_area$Ynew <-
    (extracted_points_in_main_excavation_area$Y * cos(theta)) + (extracted_points_in_main_excavation_area$X * sin(theta))


  # I want to flip it on the vertical axis --------------

  # change the sign on the Xnew variable
  extracted_points_in_main_excavation_area <-
    extracted_points_in_main_excavation_area %>%
    dplyr::mutate(Xnew_flipped = -Xnew)

  return(extracted_points_in_main_excavation_area)

}


#' clean_points_in_main_excavation_area
#'
#' @param rotated_points_in_main_excavation_area
#'
#' @return a data frame
#' @importFrom dplyr %>% filter
#' @import stringr
#' @importFrom purrr dmap_if
#' @import tidyr
#' @import readxl
#' @export
#'
#'
clean_points_in_main_excavation_area <- function(rotated_points_in_main_excavation_area){

  # Clean data to deal with non-artefact and mis-classifications-------------

  ts_data_both_years_PFs <- rotated_points_in_main_excavation_area

  # remove the list column
  ts_data_both_years_PFs$code <- NULL

  # remove some non-artefacts from CC's email

  # haematite to remove
  remove_haematite <- c("HM654", "HM1391")

  # lithics to remove
  col1 <- c(4488, 6868, 9285, 7378, 7379, 9104, 7615, 7745, 7553, 2599, 2723, 2532, 3103, 3244, 2623)

  col2 <- c(8263, 9001, 7817, 3086, 1775, 1813, 1841, 1852, 1853, 6138, 3827, 3873, 4318, 3864, 7075)

  col3 <- c(7524, 8701, 8446, 9190, 7164, 8547, 8857, 8533, 3081, 1848, 1815, 6976, 6794, 9031, 6084, 5992, 6712, 8548, 8494, 8560, 8503, 8995)

  col4 <- c(8704, 8462, 8868, 8475, 8977, 9017, 9047, 8721, 8065, 9200, 8605, 9026, 3083, 9023, 1830, 6098, 8574, 9143, 8666, 8669, 8626, 8703, 8719, 8789, 9007, 9020, 9045)

  col5 <- c(7957, 7928, 8712, 8058, 8707, 8592, 8972, 7841, 8558, 8356, 7906, 7944, 8596, 8372, 8587, 8668, 8609, 8625, 8595, 8579, 8752, 8705, 8729, 9085, 9046)

  col6 <- c(7724, 7347, 7538, 9009, 7550, 7528, 7783, 7846, 7938, 7979, 8033, 8197, 8229, 8396, 8401, 8488, 8501, 8502, 8611, 8612, 8638, 8658, 9075, 9151, 9199)

  email <- c(75, 131, 185, 1409, 1725, 1666)

  # another list from CC on 27 July 2016
  jul_26_list <- readxl::read_excel("data/stone_artefact_data/nonartefacts.xlsx", col_names = FALSE)
  jul_26_list_vec <- jul_26_list$X0
  jul_26_list_HM_GS <- toupper(jul_26_list_vec[grepl("hm|gs", jul_26_list_vec)])
  jul_26_list_L <- as.numeric(jul_26_list_vec[!grepl("hm|gs", jul_26_list_vec)])

  # L numbers to remove
  remove <- c(col1, col2, col3, col4, col5, col6, email, jul_26_list_L)

  # delete the very lowest putting HM and ART

  lowest_haematite <-  ts_data_both_years_PFs %>%
    filter(grepl("HM", Description)) %>%
    filter(Elevation == min(Elevation))
  # remove it
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(lowest_haematite$Description, Description))

  lowest_art <-  ts_data_both_years_PFs %>%
    filter(grepl("ART", Description)) %>%
    filter(Elevation == min(Elevation))

  # remove it (and all the points associated with it)
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(lowest_art$Description, Description))

  # add L to them
  remove_lithics <- paste0("L", remove)

  remove_stuff <- c(remove_lithics,
                    remove_haematite,
                    jul_26_list_HM_GS)

  # do the actual removal
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(paste(remove_stuff, collapse = "|"), Description))

  # L9060 and R299 are grindstones, so re-write description

  ts_data_both_years_PFs$Description <-  with(ts_data_both_years_PFs, gsub("L9060", "GS9060X", Description))

  ts_data_both_years_PFs$Description <-  with(ts_data_both_years_PFs, gsub("R299", "GS299X", Description))

  # check
  # grep("GS299X", ts_data_both_years_PFs$Description)


  # Split description column out into separate cols --------------


  ts_data_both_years_PFs$Description <- as.character(ts_data_both_years_PFs$Description)
  ts_data_both_years_PFs$code <- (strsplit(ts_data_both_years_PFs$Description, "_"))
  ts_data_both_years_PFs$type <- sapply(ts_data_both_years_PFs$code, "[", 1)
  ts_data_both_years_PFs$square <- gsub("^\\s+|\\s+$", "", sapply(ts_data_both_years_PFs$code, "[", 2))
  ts_data_both_years_PFs$spit <- sapply(ts_data_both_years_PFs$code, "[", 3)
  ts_data_both_years_PFs$cnr <- ifelse(nchar(sapply(ts_data_both_years_PFs$code, "[", 4)) == 1,  sapply(ts_data_both_years_PFs$code, "[", 4), "")
  ts_data_both_years_PFs$find <- ifelse(grepl("P", ts_data_both_years_PFs$type), str_extract(sapply(ts_data_both_years_PFs$code, "[", 4), "[A-Z]+"),"")
  ts_data_both_years_PFs$findn <- ifelse(grepl("P", ts_data_both_years_PFs$type), str_extract(sapply(ts_data_both_years_PFs$code, "[", 4), "[0-9]+"),"")

  # go over each character column and remove leading spaces
  tmp <-  ts_data_both_years_PFs
  ts_data_both_years_PFs <- dmap_if(ts_data_both_years_PFs,
                                    is.character,
                                    function(i) gsub("^ ", "", i))

  # check if we removed spaces
  # type_col_before <- tmp$type
  # type_col_before[grep("^ ", type_col_before)] # lots of spaces
  #
  # type_col_after <- ts_data_both_years_PFs$type
  # type_col_after[grep("^ ", type_col_after)] # none with spaces
  #


  # # if the row is an EL, and square column has 4 characters,
  # split them and put the last 2 in the spit col. Esp for C3 and C4 spit 14
  tmp_ <-
    ts_data_both_years_PFs %>%
    filter(grepl("EL", Description),
           nchar(square) == 4) %>%
    mutate(cnr = spit) %>%
    separate(square, c("square", "spit_"), sep = 2) %>%
    mutate(spit_ = as.numeric(gsub("[^0-9]", "", spit_))) %>%
    mutate(spit = spit_) %>%
    mutate(spit_ = NULL)

  # replace in main data frame
  ts_data_both_years_PFs$square <-
    ifelse(ts_data_both_years_PFs$Description %in% tmp_$Description,
           tmp_$square,
           ts_data_both_years_PFs$square)

  ts_data_both_years_PFs$spit <-
    ifelse(ts_data_both_years_PFs$Description %in%  tmp_$Description,
           tmp_$spit,
           ts_data_both_years_PFs$spit)

  # from CC email 22 APril 2016
  # OSL sample labels are wrong for the SW_A column.
  # They should all be one lower -
  # so 12A should be 11A, 11A should be 10A  etc.
  check_osl <- ts_data_both_years_PFs %>%
    filter(type == "OSL",
           square == "SW",
           grepl("A", spit)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # same
  check_osl <- ts_data_both_years_PFs %>%
    filter (grepl("OSL", Description) &
              grepl("SW", Description)  &
              grepl("A", Description)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # function that takes col, splits alpha-num, subtracts one from the num, and pastes them back together
  options(warn=-1)
  my_fn <- function(x){
    split_ <- separate(x, spit, c("spit_number", "spit_letter"),  sep = -2, remove = FALSE)
    # subtract one from the OSL number
    split_$spit_number <-  as.numeric(split_$spit_number) - 1
    # remake OSL sample ID with -1
    split_$spit <- NULL
    split_ <- unite(split_, spit, spit_number, spit_letter, sep ="")
    return(split_$spit)
  }
  options(warn=0)

  # select the cols and rows to change

  # we also need to update the description.
  # looking at ZJ's word doc of tables
  # ZJ has SW2A-11, 13A and 14A: no twelve!
  # I have SW3A-13A: no 14!
  # our 1-12 are one off

  conditions <- (with(ts_data_both_years_PFs, # conditions
                      grepl("OSL", Description) &
                        grepl("SW", Description)  &
                        grepl("A", Description) &
                        grepl(paste0("_", 1:12, "A", collapse = "|"), Description) ))

  # check
  # ts_data_both_years_PFs[ conditions, ]
  # check
  # sum(conditions)

  # first update desc

  ts_data_both_years_PFs$Description <-
    ifelse(conditions,
           paste0("OSL_SW_",
                  my_fn(ts_data_both_years_PFs)),
           ts_data_both_years_PFs$Description)

  # second update spit.
  # the ifelse function that selects the desired rows of the desired column, and changes them
  ts_data_both_years_PFs$spit <- ifelse(conditions, my_fn(ts_data_both_years_PFs), ts_data_both_years_PFs$spit)

  # remove duplicates
  # length(ts_data_both_years_PFs[duplicated(ts_data_both_years_PFs),])

  # check again

  ts_data_both_years_PFs$dup_string <- with(ts_data_both_years_PFs,
                                  paste0(Description,
                                         Easting,
                                         Northing,
                                         Elevation))
  #
  # length(ts_data_2012$dup_string)
  # length(unique(ts_data_2012$dup_string))

  ts_data_both_years_PFs <- ts_data_both_years_PFs[!duplicated(ts_data_both_years_PFs$dup_string),]

  ts_data_both_years_PFs$dup_string <- NULL

  # add depth below ground surface
  surf <- 100.693213   # NE_SEC_TAPE_1
  ts_data_both_years_PFs$depth_below_ground_surface <-
   surf -  ts_data_both_years_PFs$Elevation


  return(ts_data_both_years_PFs)


}


#' stone_artefacts
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return A data frame
#' @export
#'
#'
stone_artefacts <- function(cleaned_rotated_points_in_main_excavation_area){

  the_points <- cleaned_rotated_points_in_main_excavation_area

  # get only points for lithics, axes, axes flakes, haematite, grinding and art

  want <- c("_L", "_HM", "_GS", "_AX", "_AF", "_AR")

  the_points <- the_points[grepl(paste0(want,
                                        collapse = "|"),
                                 the_points$Description), ]

  # remove some 'find' codes that seem a bit odd
  not_want <- c("HMB", "LV", "LFEMP", "LFEMD",
                "LTAL", "LCAL", "LHUMP", "LHUMD",
                "LITHC")

  the_points <- the_points[!grepl(paste0(not_want,
                                        collapse = "|"),
                                 the_points$Description), ]

  # get 'find' col populated for a few odd records
  the_points <- na.omit(the_points)
  # look into artefacts with no 'find'
  the_points_no_find <-
    rbind(the_points[is.na(the_points$find),],
          the_points[the_points$find == "",])
  # get last bit of desc
  last_bit_of_desc <- sapply(the_points_no_find$code, function(i) i[length(i)])
  # update with newly extracted 'find' data
  the_points[the_points$Description %in% the_points_no_find$Description,]$find <-
    gsub("[0-9]", "", last_bit_of_desc)


  return(the_points)

}


#' plot_stone_artefacts
#'
#' @param stone_artefacts_only
#'
#' @return A plot
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import viridis
#' @import grid
#'
plot_stone_artefacts <- function(stone_artefacts_only){

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

  p <- ggplot() +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "L", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
                colour = viridis(6)[1],
                size = size-2.5) +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "HM", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = viridis(6)[2],
               size = size-2)  +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "GS", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = viridis(6)[3],
               size = size-1.5)  +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AXE", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = viridis(6)[4],
               size = size-1)  +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = "white",
               size = size + 0.5) +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = viridis(6)[5],
               size = size)  +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ], # halo
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = "white",
               size = size + 0.5) +
    geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ],
               aes(Xnew_flipped,
                   depth_below_ground_surface),
               colour = viridis(6)[6],
               size = size)  +

    scale_y_reverse(limits = c(3,0)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_line(colour = "black")) +
    scale_x_continuous(breaks = row_c,
                       labels = NULL) +
    xlab("") +
    ylab("Depth below \nground surface (m)") +
    scale_color_viridis(discrete=TRUE,
                        "Artefact\ntype") +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    coord_equal()

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

  # save copy
  png("figures/stone_artefacts_SW_section.png",
      height = 1200,
      width = 1200*1.92,
      res = 300,
      antialias = "cleartype")
  grid.draw(gt)
  dev.off()


}


#' refits
#'
#' @param stone_artefacts
#'
#' @return A data frame
#' @export
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggrepel
#'
refits <- function(stone_artefacts_only){

  # read in data
  refit_data <- read.csv("data/refit_data/Table_6_McNeil_Jessica_41449086_BA(Hons)_finalthesis.csv", stringsAsFactors = FALSE, header = TRUE)
  refit_data <- refit_data[-1,]

  # update artefact ID after CC's check
  refit_data[refit_data$artefact2 == " X210 B6-60 ", 5] <- "X210 B6-32"

  appendix_data <- read.csv("data/refit_data/Appendix McNeil_Jessica_41449086_BA(Hons)_finalthesis.csv", stringsAsFactors = FALSE)

  # get coords of refits with L numbers

  # wide to long
  names(refit_data)[2:3] <- c("set", "refit")

  refit_data_long <-
    gather(refit_data[,-1],
           key = artefact,
           value = descr,
           -set, -refit)

  refit_data_long$artefact_Lnums <-  gsub(" ", "", str_extract(refit_data_long$descr,
                                             "\\bL.* \\b"))

  refit_data_long$artefact_Lnums[ refit_data_long$artefact_Lnums == "L3024.2"] <- "L3024"

  L_nums_artefact <- gsub(" ", "",
                            na.omit(refit_data_long$artefact_Lnums))

  L_nums_artefact[L_nums_artefact == "L3024.2"] <- "L3024"

  artefact_L_details <- stone_artefacts_only[grepl(paste0(L_nums_artefact,
                                                     collapse = "|"),
                                                   stone_artefacts_only$Description),]

  # Jess has D2_41 for L3024, so exclude the duplicates
  # and C4_32 for L1718
  artefact_L_details <- artefact_L_details[!grepl(
"PF_C2_41_L3024|PF_C5_18_L3024|PF_E2_32A_L1718",
                                                  artefact_L_details$Description),]
  # for joining
  artefact_L_details$artefact_Lnums <- str_extract(artefact_L_details$Description,
                                                   "L[0-9]*")



  # for artefacts with no L number, get midpoint of spit end levels

  refit_data_long$artefact_sq_sp <-     gsub("-", "_",
                                        gsub(" ", "",
                                        str_extract(refit_data_long$descr,
                                              "[A-Z]{1}[0-9]{1}-[0-9]{1}.*")))


  # average location for all lithics in those squares and spits
  stone_artefacts_only$code <- NULL

  stone_artefacts_in_sqs <-
    stone_artefacts_only %>%
    dplyr::filter(grepl(paste0(refit_data_long$artefact_sq_sp,
                        collapse = "|"), Description)) %>%
    dplyr::mutate(artefact_sq_sp = paste0(square, "_", spit)) %>%
    dplyr::group_by(artefact_sq_sp) %>%
    dplyr::summarise(Easting = mean(Easting),
                     Northing = mean(Northing),
                     Elevation = mean(Elevation),
                     Xnew_flipped = mean(Xnew_flipped),
                     Ynew = mean(Ynew)) %>%
    arrange(Elevation)

  # put the coords onto the refit table, split, apply combine!

  # split table
  refit_data_long_Lnums <- refit_data_long[!is.na(refit_data_long$artefact_Lnums),]
  refit_data_long_no_Lnums <- refit_data_long[is.na(refit_data_long$artefact_Lnums),]

  # join coords on
  refit_data_long_Lnums <- left_join(refit_data_long_Lnums,
                                     artefact_L_details,
                                     by = "artefact_Lnums")

  refit_data_long_no_Lnums <- left_join(refit_data_long_no_Lnums,
                                        stone_artefacts_in_sqs,
                                     by = "artefact_sq_sp")

  # get the number of cols to be the same
  refit_data_long_Lnums <-
      refit_data_long_Lnums[,
                            colnames(refit_data_long_Lnums) %in%
                              names(refit_data_long_no_Lnums)]

  #names(refit_data_long_Lnums)
  #names(refit_data_long_no_Lnums)

  # combine into one table
  refit_data_long_coords <- rbind(refit_data_long_Lnums,
                                  refit_data_long_no_Lnums)

  refit_data_long_coords$descr <- gsub("^ | $", "", refit_data_long_coords$descr)

  # spit into start and end points to plot
  refit_data_long_coords_start <-
    refit_data_long_coords[duplicated(refit_data_long_coords$set), ]

  refit_data_long_coords_end <-
    refit_data_long_coords[!refit_data_long_coords$descr %in% refit_data_long_coords_start$descr, ]

  # long to wide for plotting lines connecting refits

  refit_data_coords_wide <-
  left_join(refit_data_long_coords_start,
            refit_data_long_coords_end,
            by = "set")

  # plot
  ggplot() +
    geom_point(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                        Elevation),
                    colour = "red") +

    geom_text_repel(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                  Elevation,
                  label = descr)) +

    geom_segment(data = refit_data_coords_wide,
                 aes(x = Xnew_flipped.x,
                     y = Elevation.x,
                     xend = Xnew_flipped.y,
                     yend = Elevation.y),
                 size = 1,
                 colour = "green") +

    coord_equal() +
    theme_minimal()

  ggsave("figures/refit_elev.png", width = 15, antialias = "cleartype")

  # plot
  ggplot() +
    geom_point(data = refit_data_long_coords,
               aes(Xnew_flipped,
                   Ynew),
               colour = "red") +

    geom_text_repel(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                        Ynew,
                        label = descr)) +

    geom_segment(data = refit_data_coords_wide,
                 aes(x = Xnew_flipped.x,
                     y = Ynew.x,
                     xend = Xnew_flipped.y,
                     yend = Ynew.y),
                 size = 1,
                 colour = "green") +

    coord_equal() +
    theme_minimal()

  ggsave("figures/refit_plan.png", width = 15, antialias = "cleartype")

  # what is the distance between the two refit pieces?
  refit_data_coords_wide$refit_dists_m <-
    sqrt((refit_data_coords_wide$Easting.x -
            refit_data_coords_wide$Easting.y)^2 +
           (refit_data_coords_wide$Northing.x -
              refit_data_coords_wide$Northing.y)^2 +
           (refit_data_coords_wide$Elevation.x -
              refit_data_coords_wide$Elevation.y)^2)

  #  plot
  ggplot(refit_data_coords_wide,
         aes(refit_dists_m)) +
    geom_histogram() +
    theme_minimal()

  ggsave("figures/refit_dists_histogram.png", width = 15, antialias = "cleartype")

  # what is the plunge angle of the refits?
  # compute from elevations and X (Easting.x or Xnewflipped)
  # plot the angles
  # start locations
  start_coords <- data.frame(E = refit_data_coords_wide$Easting.x,
                             N = refit_data_coords_wide$Northing.x,
                             Z = refit_data_coords_wide$Elevation.x)

  # end locations
  end_coords <-   data.frame(E = refit_data_coords_wide$Easting.y,
                             N = refit_data_coords_wide$Northing.y,
                             Z = refit_data_coords_wide$Elevation.y)

  # set end coords so that start is 0,0
  # assume that the higher point is the origin
  zero_origin <- data.frame(matrix(nrow = nrow(end_coords), ncol = ncol(end_coords)))
    for(i in 1:nrow(zero_origin)){

      if(start_coords$Z[i] < end_coords$Z[i]){
        zero_origin[i,] <-  end_coords[i,] - start_coords[i,]
      } else {
        zero_origin[i,] <- start_coords[i,] - end_coords[i,]
      }
    }


  # compute angle of displacement, as 0-360 deg
  refit_angle <- atan2(zero_origin[,1], zero_origin[,3]) / (pi/180)
  refit_angle_full_circle <- ifelse(refit_angle < 0, 360 + refit_angle, refit_angle)
  refit_angle_abs <- abs(refit_angle)
  # axial to bidir via the 'Angle Doubling Procedure'
  refit_angle_bidirectional <- c(refit_angle_abs,
                                 ifelse(refit_angle_abs * 2 > 360,
                                        (refit_angle_abs * 2) - 360,
                                        refit_angle_abs * 2))

  # sig test for orientations, these data are axial
  # Axial data are converted to angular data by multiplying by two.
  options(warn = -1)
  refit_angle_rads <- abs(refit_angle_bidirectional) * pi/180
  refit_angle_rads_test <- circular::rayleigh.test(refit_angle_rads)


  g1 <- ggplot() +
    geom_histogram(aes(x = refit_angle_abs),
                   colour = "black",
                   fill = "grey80") +
    theme(axis.text.x = element_text(size = 18)) +
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270)) +
    theme_minimal(base_size = 14) +
    xlab("") +
    ggtitle(paste0("MJB refit orientations \n (Rayleigh = ",
                   round(refit_angle_rads_test$statistic, 3),
                   " p = ",
                   round(refit_angle_rads_test$p.value, 3),
                   ")"))
  g1
  # grid::grid.newpage()
  # grid::pushViewport(grid::viewport(height=1, width=1, clip="on"))
  # grid::grid.draw(g1)
  # grid::grid.rect(x=0,y=0,height=2, width=1.05, gp=grid::gpar(col="white"))

  ggsave("figures/refits orientations.png", antialias = "cleartype")

  ## correlation between size of arefact and distance of refit?

  options(warn = 0)

}



#' prepare_geoarchaeology_data
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return A data frame
#' @export
#'
#' @importFrom Hmisc approxExtrap
#' @import dplyr
#' @import readxl

#'
#'
prepare_geoarchaeology_data <- function(cleaned_rotated_points_in_main_excavation_area){

  options(warn=-1)
  # mag sus from KL
  mag_sus_B2 <- read.csv("data/geoarchaeology_data/Kelsey_Lowe_mag_sus_B2.csv")
  # remove the C3 stuff
  mag_sus_B2$Square <- NULL
  mag_sus_B2$Depth <- NULL
  mag_sus_B2$Spit <- NULL

  mag_sus_B2$Depth.Below.Surface..m <-
    as.numeric(mag_sus_B2$Depth.Below.Surface..cm.) / 100

  # depths were measured on a tape, we shot some points on that tape. Let's find them
  # SW wall tape... actually NE_SEC_TAPE 1-4 (Not SECT)
  # are points on the tape on the SW wall,
  # as we can see from the 3d plot of section things

  # NE_SECT_TAPE 1 is the ground surface, then the other points are one meter apart


  NE_SEC_TAPE <-
    cleaned_rotated_points_in_main_excavation_area[grepl("NE_SEC_TAPE",
                      cleaned_rotated_points_in_main_excavation_area$Description), ]

  # put tape and total station depths together
  tape_depths <- data.frame(tape = 0:3,
                            total_station = NE_SEC_TAPE$Elevation)

  # need to interpolate to 5 cm to match with mag_sus_B2$Depth.Below.Surface..cm.

  xout <- seq(min(mag_sus_B2$Depth.Below.Surface..m),
              max(mag_sus_B2$Depth.Below.Surface..m),
              by = 0.05)

  # approx returns a list, convert to dataframe
  # we also need to extrapolate a little
  tape_depths_interp <- data.frame(do.call(cbind,
                                           Hmisc::approxExtrap(tape_depths$tape,
                                                        tape_depths$total_station,
                                                        n = length(xout),
                                                        xout = xout)))
  names(tape_depths_interp) <- names(tape_depths)


  # attach interpolated depths to mag sus data
  # there is a tiny difference in the vectors, so we convert to chr

  mag_sus_B2$Depth.Below.Surface..m  <-
    as.character(mag_sus_B2$Depth.Below.Surface..m)

  tape_depths_interp$tape <-  as.character(tape_depths_interp$tape)

  mag_sus_B2_total_station <-
    dplyr::left_join(mag_sus_B2,
              tape_depths_interp,
              by = c('Depth.Below.Surface..m' = 'tape'))

  mag_sus_B2_total_station$Depth.Below.Surface..m <-
    as.numeric(mag_sus_B2_total_station$Depth.Below.Surface..m)

  # depth below surf from total station
  surf <- 100.693213   # NE_SEC_TAPE_1
  mag_sus_B2_total_station$total_station_depth_m <-
    surf - mag_sus_B2_total_station$total_station

  # error in tape measure and total station
  mag_sus_B2_total_station$error <-
    mag_sus_B2_total_station$Depth.Below.Surface..m -
    mag_sus_B2_total_station$total_station_depth_m

  # done with mag sus

  # particle size data, sand-silt-clay
  # We have sediment columns from
  # SW Section S (closest to mag sus samples) and
  # SW Section Section W,
  # NE Section, and
  # NW Section
  #

  # This is from code I wrote for LRH

  MJB_SW_S_section_particle_size <- read.csv("data/geoarchaeology_data/MJB_SW_S_section_particle_size.txt", header = FALSE, stringsAsFactors = FALSE)


  # delete first row and first 23 columns to get only sample
  # names, size classes and sample data. We're also removing
  # column 25 and the very last column because they are empty
  MJB_SW_S_section_particle_size <-
    MJB_SW_S_section_particle_size[-1,-c(1:22, 25, ncol(MJB_SW_S_section_particle_size))]

  # convert a few errant characters to numbers
  MJB_SW_S_section_particle_size[,c(3:ncol(MJB_SW_S_section_particle_size))] <-
    as.numeric(as.character(unlist(MJB_SW_S_section_particle_size[,c(3:ncol(MJB_SW_S_section_particle_size))])))

  # reshape to long form
  MJB_SW_S_section_particle_size_long <-
    reshape(MJB_SW_S_section_particle_size,
            idvar=1:2,
            varying=list(size=colnames(MJB_SW_S_section_particle_size[seq(from=3,
                                                 to=ncol(MJB_SW_S_section_particle_size),
                                                 by=3)]),
            meas=colnames(MJB_SW_S_section_particle_size[seq(from=5,
                                    to=ncol(MJB_SW_S_section_particle_size),
                                    by=3)])),
            direction="long")
  # extract just measurements, size classes and sample labels
  size <- as.numeric(na.omit(unique(MJB_SW_S_section_particle_size_long$V26)))

  measurements <-
    MJB_SW_S_section_particle_size_long[,
                                        c(which( colnames(MJB_SW_S_section_particle_size_long)=="V24" ),
                                          which( colnames(MJB_SW_S_section_particle_size_long)=="V27" ) :
                                             which( colnames(MJB_SW_S_section_particle_size_long)=="V303"))]

  names(measurements) <- c("sample.id", size)

  measurements_long <- tidyr::gather(measurements, variable, value, -sample.id)
  measurements_long$variable <- as.numeric(as.character(measurements_long$variable))

  # compute mean values for each sample
  # replace the pattern of digit-digit-rs with nothing to group replicates together
  measurements$sample.id <- gsub("[[:digit:]]-[[:digit:]]-rs", "", measurements$sample.id)
  # get averages of multiple runs on the same sub-sample
  measurements_means <- aggregate(. ~ sample.id, data = measurements, mean)

  # reduce to sand-silt-clay

  # need to transpose table so sample = column and size class = row names
  measurements_means_t <-   data.frame(t(measurements_means), stringsAsFactors = FALSE)
  # put sample names as col names
  tmp <- as.vector(as.matrix(measurements_means_t[1,1:ncol(measurements_means_t)])[1,])
  # delete row with text
  measurements_means_t <- data.frame(measurements_means_t[-1,])
  # create column to order values
  measurements_means_t$size <- (as.numeric(size))
  # order
  measurements_means_t <- measurements_means_t[ order(-as.numeric(measurements_means_t$size)), ]
  # convert to data frame of numeric type
  measurements_means_t <- data.frame(apply(measurements_means_t, 2,
                                           function(x) as.numeric(as.character(x))))
  # delete size column, not needed anymore
  measurements_means_t <-
    measurements_means_t[, -which(names(measurements_means_t) == 'size'),
                                               drop=FALSE]
  # put colnames back on again
  colnames(measurements_means_t) <- tmp
  # add last size class of zero
  measurements_means_t <-
    rbind(measurements_means_t, c(rep(0, ncol(measurements_means_t))))
  # add size classes as row name
  rownames(measurements_means_t) <-
    round(c(rev(size),0),5)

  # Here we can compute sand-silt-clay %s
  # using Wentworth classes...
  # sand: 2000 - 62.5 um
  # silt: 62.5 - 4 um
  # clay: < 4 um

  # add size column to subset by
  measurements_means_t$size <-
    as.numeric(as.character(rownames(measurements_means_t)))

  sand <- colSums(measurements_means_t[ measurements_means_t$size >= 62.5
                                        & measurements_means_t$size < 2000, ] )

  silt <- colSums(measurements_means_t[ measurements_means_t$size >= 4
                                        & measurements_means_t$size < 62.5, ] )

  clay <- colSums(measurements_means_t[ measurements_means_t$size >= 0
                                        & measurements_means_t$size < 4, ] )

  # combine into one data frame
  three_classes <- data.frame(CLAY = clay,
                              SILT = silt,
                              SAND = sand)

  # remove size row
  three_classes <- three_classes[-nrow(three_classes),]
  row.names(three_classes) <- substr(row.names(three_classes), nchar(row.names(three_classes))-4,  nchar(row.names(three_classes))-1)

  # put depths in a column so we can calibrate with total station depths
  # need the depth col as character to ensure the join works
  three_classes$tape_depths <- as.character(as.numeric(row.names(three_classes)) - 1 )

  # we used the same tape as the mag sus data, so we can use that method to
  # get total station depths

  three_classes_B2_total_station <-
    dplyr::left_join(three_classes,
                     tape_depths_interp,
                     by = c('tape_depths' = 'tape'))

  # depth below surf from total station
  surf <- 100.693213   # NE_SEC_TAPE_1

  three_classes_B2_total_station$total_station_depth_m <- surf -
    as.numeric(three_classes_B2_total_station$total_station)

  # error in tape measure and total station
  three_classes_B2_total_station$error <-
    as.numeric(three_classes_B2_total_station$tape_depths) -
    three_classes_B2_total_station$total_station_depth_m

  # round depths so we can get a match

  three_classes_B2_total_station$depth_below_surf <-
    round( three_classes_B2_total_station$total_station_depth_m, 2)

  # done with particle size data


  # dC13 data from Mara Page

  dC13_data <- readxl::read_excel("data/geoarchaeology_data/Page_160404_CombinedResults final full.xlsx")

  # clean_and_tidydC13_data

  # data are in rows 4 to 78
  dC13_data_rows <- c(4:78)
  # sample IDs, etc
  sample_ID <- dC13_data[ dC13_data_rows, ][[2]]
  sample_mass <- as.numeric(dC13_data[ dC13_data_rows, ][[9]])
  d15N <- as.numeric(dC13_data[ dC13_data_rows, ][[15]])
  d13C_accuracy <- as.numeric(dC13_data[ dC13_data_rows, ][[23]])
  d13C_precision <- as.numeric(dC13_data[ dC13_data_rows, ][[24]])
  amount_corrected <- as.numeric(dC13_data[ dC13_data_rows, ][[25]])
  percent_C <- as.numeric(dC13_data[ dC13_data_rows, ][[19]])

  #  d13C corrected
  d13C_corrected <- amount_corrected
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
                           d13C_accuracy = d13C_accuracy,
                           d13C_precision = d13C_precision,
                           amount_corrected = amount_corrected,
                           percent_C = percent_C,
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
    mutate(depth = as.character(`factor(depth)`)) %>%
    select(-`factor(depth)`)

  # get the total station depths for the tape depths for dC13

  d13C_depth_means_total_station <-
    dplyr::left_join(d13C_depth_means,
                     tape_depths_interp,
                     by = c('depth' = 'tape'))

  # depth below surf from total station
  surf <- 100.693213   # NE_SEC_TAPE_1
  d13C_depth_means_total_station$total_station_depth_below_surface <-
    surf - as.numeric(d13C_depth_means_total_station$total_station)

  # for joining with other data
  d13C_depth_means_total_station$tape_depth <-
    d13C_depth_means_total_station$depth

  d13C_depth_means_total_station$depth <- NULL

  d13C_depth_means_total_station$total_station_depth_below_surface_rounded <-
    round(d13C_depth_means_total_station$total_station_depth_below_surface, 2)
  # that's all for d13C for now

  # Read in the other geoarch data

  burnt_earth_C3 <- read.csv("data/geoarchaeology_data/C3_burnt_earth_by_spit.csv")
  heated_per_litre_C3 <- read.csv("data/stone_artefact_data/C3_heated_per_litre_by_spit.csv")
  burnt_chert_C3 <- read.csv("data/stone_artefact_data/C3_burnt_chert_by_spit.csv")
  charcoal_per_litre_C3 <- read.csv("data/geoarchaeology_data/C3_charcoal_per_litre_by_spit.csv")
  charcoal_C3 <- read.csv("data/geoarchaeology_data/C3_charcoal_by_spit.csv")
  artefacts_per_litre_C3 <- read.csv("data/stone_artefact_data/C3_artefacts_per_litre_by_spit.csv")
  artefacts_C3 <- read.csv("data/stone_artefact_data/C3_artefacts_by_spit.csv")

  EL <-    cleaned_rotated_points_in_main_excavation_area[grepl("EL_",
                   cleaned_rotated_points_in_main_excavation_area$Description), ]

  EL$depth_below_surface <- surf - EL$Elevation


  # get spit depths from EL
  C3_spit_depths <-
    EL %>%
    filter(square == "C3") %>%
    group_by(spit) %>%
    dplyr::summarise(mean_depth_below_surface = mean(depth_below_surface)) %>%
    arrange(as.numeric(spit)) %>%
    ungroup %>%
    mutate(spit = as.numeric(spit)) %>%
    na.omit

  # we do need to interpolate a few missing spits
  xout <- seq(min(C3_spit_depths$spit),
              max(C3_spit_depths$spit),
              1)
  C3_spit_depths_interp <-
    data.frame(do.call(cbind,
               Hmisc::approxExtrap(C3_spit_depths$spit,
               C3_spit_depths$mean_depth_below_surface,
               xout = xout)))

  names(C3_spit_depths_interp) <- c("spit", "depth_below_surf")

  # first few spits are -ve, so lets fix that
  C3_spit_depths_interp[1:3, 2] <-  c(0, 0.05, 0.1)

  # combine all the things from C3
  things_in_C3 <-
    left_join(burnt_earth_C3,
              C3_spit_depths_interp,  # join with total station depths
              by = c("Spit" = "spit")) %>%
    left_join(.,
              heated_per_litre_C3) %>%
    left_join(.,
              burnt_chert_C3) %>%
    left_join(.,
              charcoal_per_litre_C3) %>%
    left_join(.,
              charcoal_C3) %>%
    left_join(.,
              artefacts_per_litre_C3) %>%
    left_join(.,
              artefacts_C3)

  # get rid of NA rows
  things_in_C3 <- things_in_C3[!is.na(things_in_C3$Spit),]

  # We also want mag sus data in here, join by depth,
  # since that's how the mag sus samples were recorded

  # round depths so we can get a match

  things_in_C3$depth_below_surf <-  round(things_in_C3$depth_below_surf, 2)

  mag_sus_B2_total_station$total_station_depth_m <- round(mag_sus_B2_total_station$total_station_depth_m, 2)

  # interpolate depth values for every 0.01 below the surface
  # for each data col in things_in_C3
  # don't interpolat data, just have gaps
  depths_0.01 <- seq(0,
                     max(d13C_depth_means_total_station$total_station_depth_below_surface),
                     0.01)

  # just match the rows and allow gaps
  all_rows <- data.frame(depths = depths_0.01)
  all_rows <- left_join(all_rows,
                        things_in_C3,
                        by = c("depths" = "depth_below_surf"))

  # subset to only depths that we have geoarch data for
  # all_rows <- all_rows[depths_0.01 <= max(things_in_C3$depth_below_surf), ]

  # add mag sus data
  all_rows <-
    left_join(all_rows,
              mag_sus_B2_total_station,
              by = c("depths" = "total_station_depth_m"))

  # and now add d13C data to all of that
  all_rows <-
    left_join(all_rows,
              d13C_depth_means_total_station,
              by = c("depths" = "total_station_depth_below_surface_rounded"))

  # and now add SW-S particle size data to all of that
  all_rows <-
    left_join(all_rows,
              three_classes_B2_total_station,
              by = c("depths" = "depth_below_surf"))
  options(warn=0)
  return(all_rows)

}


#' plot_geoarchaeology_data
#'
#' @param prepared_geoarchaeology_data
#'
#' @return plots
#' @export
#'
#' @import analogue
#' @import lattice
#' @importFrom latticeExtra doubleYScale
#'
#'
plot_geoarchaeology_data <- function(prepared_geoarchaeology_data){


  all_rows <- prepared_geoarchaeology_data
  # straigraphic panel plot
  depth <- all_rows$depths
  all_rows$Charcoal.Litre.log <- log(all_rows$Charcoal.Litre)
  all_rows$Charcoal.Litre.log[all_rows$Charcoal.Litre.log == -Inf] <- NA

  # subset variables for plotting
  plotting_data <- select(all_rows,
                          # Burnt.Earth,
                          # Heated.litre,
                          Artefacts,
                          Burnt.Chert,
                          # Charcoal.Litre,
                          #Charcoal.Litre.log,
                          # Charcoal.g,
                          # Artefacts.Litre,
                          Xlf..m.3.kg.,
                          mean_d13C_corrected,
                          SAND,
                          SILT,
                          CLAY)

  names(plotting_data) <- c(("Stone artefacts (n)"),
                            ("Burnt artefacts (n)"),
                            ("Magnetic susceptibility (SI units)"),
                            ("d13C per mill"),
                            ("Sand (%)"),
                            ("Silt (%)"),
                            ("Clay (%)"))


  #  lowest dense artefact band at between 2.1 and 2.3m depth near the back wall
  #  middle dense artefact band occurs between 100 and 150cm depth
  #  upper dense artefact band occurs between 40 and 70cm depth


  zones <- c(0.4, 1.05, # upper
                        # middle
             1.72, 2.3) # lower
  zoneNames <- c("","lower\nband",
                 "middle\nband",
                 "upper\nband", "")


plt1 <- analogue::Stratiplot(depth ~ .,
                  varTypes = "absolute",
                  labelRot = 90,
                  data = plotting_data,
                  type = c("l","g"),
                  col = "black",
                  lwd = 1.5,
                  zones = zones,
                  zoneNames = zoneNames)
plt1$x.scales$rot <- c(90,90)

plt2 <- Stratiplot(depth ~ .,
                   varTypes = "absolute",
                   labelRot = 90,
                   data = plotting_data,
                   type = c("l","g"),
                   col = "black",
                   lwd = 1.5,
                   yticks = c(0.77, 1.11, 1.75),
                   ylab =   c(34.2, 45.3, 56.3))
plt2$x.scales$rot <- c(90,90)


plt3 <- latticeExtra::doubleYScale(plt1,
                                   plt2,
                                   add.axis=T)

plt3 <- update(plt3, par.settings = simpleTheme(col = c("black", "black")))
plt3

# make space for the long titles at the top
# https://stat.ethz.ch/pipermail/r-help/2005-September/078334.html
trellis.par.set(theme = col.whitebg())
lw <- list(left.padding = list(x = 0.1, units = "inches"))
lw$right.padding <- list(x = 0.1, units = "inches")
lh <- list(bottom.padding = list(x = 0, units = "inches"))
lh$top.padding <- list(x = 0.15, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# save a copy
png("figures/geoarchaeology_stratigraphic_plot.png",
    width = 4000,
    height = 2000,
    res = 300,
    antialias = "cleartype")
print(plt3)
dev.off()

# put a copy on screen
# print(plt3)



}



#' get_osl_ages
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return a data frame
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import docxtractr
#'
get_osl_ages <-  function(cleaned_rotated_points_in_main_excavation_area){
  options(warn=-1)

  ts_data_both_years_PFs <-
    cleaned_rotated_points_in_main_excavation_area

  # fix problem with OSL sample numbering -------------------------------

  # from CC email 22 APril 2016
  # sample labels are wrong for the SW_A column.
  # They should all be one lower -
  # so 12A should be 11A, 11A should be 10A  etc.
  check_osl <- ts_data_both_years_PFs %>%
    filter(type == "OSL",
           square == "SW",
           grepl("A", spit)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # same
  check_osl <- ts_data_both_years_PFs %>%
    filter (grepl("OSL", Description) &
              grepl("SW", Description)  &
              grepl("A", Description)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # function that takes col, splits alpha-num, subtracts one from the num, and pastes them back together
  my_fn <- function(x){
    split_ <- separate(x, spit, c("spit_number", "spit_letter"),  sep = -2, remove = FALSE)
    # subtract one from the OSL number
    split_$spit_number <-  as.numeric(split_$spit_number) - 1
    # remake OSL sample ID with -1
    split_$spit <- NULL
    split_ <- unite(split_, spit, spit_number, spit_letter, sep ="")
    return(split_$spit)
  }

  # select the cols and rows to change

  # we also need to update the description.
  # looking at ZJ's word doc of tables
  # ZJ has SW2A-11, 13A and 14A: no twelve!
  # I have SW3A-13A: no 14!
  # our 1-12 are one off

  conditions <- (with(ts_data_both_years_PFs, # conditions
                      grepl("OSL", Description) &
                        grepl("SW", Description)  &
                        grepl("A", Description) &
                        grepl(paste0("_", 1:12, "A", collapse = "|"), Description) ))

  # check
  # ts_data_both_years_PFs[ conditions, ]
  # check
  # sum(conditions)

  # first update desc

  ts_data_both_years_PFs$Description <-
    ifelse(conditions,
           paste0("OSL_SW_",
                  my_fn(ts_data_both_years_PFs)),
           ts_data_both_years_PFs$Description)

  # second update spit.
  # the ifelse function that selects the desired rows of the desired column, and changes them
  ts_data_both_years_PFs$spit <- ifelse(conditions, my_fn(ts_data_both_years_PFs), ts_data_both_years_PFs$spit)

  # check, 'code' col holds original values
  # ts_data_both_years_PFs[ conditions, ]


  ### extract OSL points from Total station data --------------------------
  # OSL sample spits and squares --------------------------------------------

  # find the squares and spits for each OSL sample

  # extact OSL points
  OSL_and_micromorph <- c("OSL", "MM", "GAMMA")
  OSL_and_micromorph <- ts_data_both_years_PFs[grepl(paste(OSL_and_micromorph, collapse = "|"), ts_data_both_years_PFs$Description),]

  osl_points <- OSL_and_micromorph[grepl("OSL", OSL_and_micromorph$Description), ]
  end_levels <-
    ts_data_both_years_PFs[grepl("EL_", ts_data_both_years_PFs$Description),]

  end_levels$Description <- gsub(" ", "", end_levels$Description)
  gamma_points <- OSL_and_micromorph[grepl("GAMMA", OSL_and_micromorph$Description), ]

  # what are the spits associated with the 2012 OSL column on the SW section?
  # depths measured from surface, sampling started at bottom on MM1
  osl_column <-  read.csv("data/ages/MKII_2012_OSL_column_SW_section.csv")

  # seperate out upper and lower depths per sample

  osl_column_depths <- osl_column %>%
    separate(Depth, c("upper_depth", "lower_depth"), sep = "-") %>%
    dplyr::mutate_at(matches("depth"), as.numeric)

  # get total station point for MM1 of 2012
  mm1_2012 <- ts_data_both_years_PFs %>% filter(grepl("MM1", Description), year == "2012")
  # find lowest points
  mm1_2012_lowest <- mm1_2012 %>% slice(which.min(Elevation))

  # set the depth of the first sample to the mm1 depth, and calibrate the rest of the osl sample depths to that
  # ZJ's depth values increase with depth, but our total station values decrease with increasing depth...
  offset <- mm1_2012_lowest$Elevation + osl_column_depths$upper_depth[1]/100
  osl_column_depths_elevation <- data.frame(Sample = osl_column_depths$Sample,
                                            upper = -(osl_column_depths$upper_depth/100) + offset,
                                            lower = -osl_column_depths$lower_depth/100 + offset)
  # compute a mid-point for the osl sample depths
  osl_column_depths_elevation$Elevation <- with(osl_column_depths_elevation, (upper + (lower - upper)/2))

  # We'll arbitratily give the OSL column samples the same E and N
  # as the first gamma spec point, since the gamma points are in the
  # same column, and use the depths that we
  # computed from the MM1 location
  first_gamma <- gamma_points %>% filter(grepl("SW_SECT_GAMMA-1", Description))
  osl_column_depths_elevation_grid <- cbind(osl_column_depths_elevation, first_gamma[, c(2, 3, 5:10)])

  # so that gives us the depths of the OSL samples from the column.

  # now we can add these osl column points on the rest of the OSL points...
  all_osl_points <- full_join(osl_column_depths_elevation_grid, osl_points)
  all_osl_points$type <-  "OSL"

 # read in OSL dates from ZJ's word doc
  # these were delivered by ZJ in a series of tables in a Word doc.
  # LEts try to extract the tables from her word doc, using
  # https://github.com/hrbrmstr/docxtractr

  OSL_Data_tables_all_samples <- docxtractr::read_docx("data/ages/Data tables_all samples(250616)docx ZJ.docx")

  OSL_Data_tables_all_samples_tables <- docx_extract_all_tbls(OSL_Data_tables_all_samples)
  # combine tables into one big one
  OSL_Data_tables_all_samples_table <- do.call(rbind, OSL_Data_tables_all_samples_tables)
  # fix names
  names(OSL_Data_tables_all_samples_table) <-
    c("Sample",
      "Depth below surface (m)",
      "Water (%)",
      "EDR_Beta",
      "EDR_Gamma",
      "EDR_Cosmic",
      "EDR_total",
      "De value (Gy)",
      "Number of grains",
      "OD (%)",
      "Age (ka)")

  # get total station coods for these samples
  # make a col to match with
  all_osl_points$Description_short <- gsub("_", "", all_osl_points$Description)
  # make it match the IDs in the table from ZJ
  remove_me <- c("OSL", "B5", "B6")
  all_osl_points$Description_short <-
    gsub(paste0(remove_me, collapse = "|"), "",
         all_osl_points$Description_short)

  # join the ts points to the dates
  OSL_Data_tables_all_samples_table_ts_points <-
    left_join(OSL_Data_tables_all_samples_table,
              all_osl_points,
              by = c("Sample" = "Description_short"))

  # add depth below surf
  surf <- 100.693213   # NE_SEC_TAPE_1
  OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf <-
    OSL_Data_tables_all_samples_table_ts_points$Elevation - surf


  # compare these to the depths in ZJ's table (from me originally)
  # cbind(OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf , OSL_Data_tables_all_samples_table_ts_points$`Depth below surface (m)` )

  # excellent match, but we're missing:
  # all NE samples, SW14A, NE1C, SW8C
  # missing <- c("SW14A", "NE1C")
  # all_osl_points[grep(paste0(missing, collapse = "|"), all_osl_points$Description_short), ]
  # lost forever

  # get her NE sample depths, since we don't have points for those
  OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf <- as.numeric(
    ifelse(is.na(OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf),
           OSL_Data_tables_all_samples_table_ts_points$`Depth below surface (m)`,
           OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf))

  # what about XY for the NE?
  # get it from the QGIS project in
  # E:\My Documents\My UW\Research\1206 M2 excavation\Section photos
  osl_ne <- read.csv("data/ages/NE_section_from_QGIS_points.csv")
  osl_ne$NE_OSL_num <- paste0("NE", osl_ne$NE_OSL_num, "$") # exclude NE1C
  names(osl_ne) <-  c("Xnew_flipped", "Elevation", "id", "Sample")

  # update ZJ's table with data from QGIS
  OSL_Data_tables_all_samples_table_ts_points$Sample_dollar <-  paste0(OSL_Data_tables_all_samples_table_ts_points$Sample, "$")

  match_idx <-   match(osl_ne$Sample,
                       OSL_Data_tables_all_samples_table_ts_points$Sample_dollar)

  OSL_Data_tables_all_samples_table_ts_points$Elevation[match_idx] <-  osl_ne$Elevation

  OSL_Data_tables_all_samples_table_ts_points$Xnew_flipped[match_idx] <-  osl_ne$Xnew_flipped

  OSL_Data_tables_all_samples_table_ts_points$total_station_depth_below_surf[match_idx] <-  osl_ne$Elevation - surf

  OSL_Data_tables_all_samples_table_ts_points$Sample_dollar <- NULL

  # remove empty rows
  OSL_Data_tables_all_samples_table_ts_points <-
    OSL_Data_tables_all_samples_table_ts_points[OSL_Data_tables_all_samples_table_ts_points$Sample != "", ]

  # put age and error in separate cols
  OSL_Data_tables_all_samples_table_ts_points$osl_age <-
    as.numeric(gsub(" .*$", "", OSL_Data_tables_all_samples_table_ts_points$`Age (ka)`))
  OSL_Data_tables_all_samples_table_ts_points$osl_error <-
   as.numeric(gsub(".*± ", "", OSL_Data_tables_all_samples_table_ts_points$`Age (ka)`))

  # these are ready for plotting!
  OSL_Data_tables_all_samples_table_ts_points$code <- NULL
  write.csv(OSL_Data_tables_all_samples_table_ts_points, "data/ages/OSL_Data_tables_all_samples_table_ts_points.csv")

  options(warn=0)
  return(OSL_Data_tables_all_samples_table_ts_points)


}



#' get_c14_ages
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return
#' @export
#'
#' @import tidyr
#' @import Bchron
#'
get_c14_ages <- function(cleaned_rotated_points_in_main_excavation_area){


  ts_data_both_years_PFs <-  cleaned_rotated_points_in_main_excavation_area
  # depths of all the C14 dates that ZJ has (from her 14C results_Malakunanja II_2014 and 2016 results for Zen.xlsx)

  all_the_c14_dates <- read.csv("data/ages/14C results_Malakunanja II_2014 and 2016 results for Zen.csv")

  # edit depth of E3_5A_SF20, whic ZJ noticed is probably too deep, replace with mean spit depth
  # extract end levels and depth
  EL_E3_5A_mean_elev <- mean(ts_data_both_years_PFs[grepl("EL_E3_5A", ts_data_both_years_PFs$Description),]$Elevation)
  EL_E3_5A_mean_depth <- mean(ts_data_both_years_PFs[grepl("EL_E3_5A", ts_data_both_years_PFs$Description),]$depth_below_ground_surface)

   # replace in total station points
  ts_data_both_years_PFs[grepl("E3_5A_SF20", ts_data_both_years_PFs$Description),]$Elevation <- EL_E3_5A_mean_elev
  ts_data_both_years_PFs[grepl("E3_5A_SF20", ts_data_both_years_PFs$Description),]$depth_below_ground_surface <- EL_E3_5A_mean_depth



  # remove C14X bit that:
  # is before a -
  # is before a space
  # is after a space
  # really, we just have C14X plus a few digits, then space or -
  # so, remove 'word' beginning with C14X
  patterns <- c("C14X\\w+ *", "-", " ")
  all_the_c14_dates$c14_square_and_spit <- gsub(paste0(patterns, collapse = "|"), "", all_the_c14_dates$Sample.ID)

  # deal with the SF## samples by splitting them up
  all_the_c14_dates$SF <- ifelse(grepl("SF", all_the_c14_dates$Sample.ID),  gsub(".*SF", "", all_the_c14_dates$Sample.ID), 0)

  all_the_c14_dates$c14_square_and_spit <- gsub("SF.*", "", all_the_c14_dates$c14_square_and_spit)

  all_the_c14_dates$c14_square_and_spit <- gsub("/", "_", all_the_c14_dates$c14_square_and_spit)


  # get just the c14X code and the c14 number
  all_the_c14_dates$c14_sample_code <- gsub(".*(C14X\\w+).*$", "\\1", all_the_c14_dates$Sample.ID)
  all_the_c14_dates$c14_sample_code_num <- as.numeric(gsub("C14X", "", all_the_c14_dates$c14_sample_code))

  # split into square and spit
  all_the_c14_dates <- tidyr::separate(all_the_c14_dates, c14_square_and_spit, c("square", "spit"), sep = "_")
  all_the_c14_dates$square_spit <- with(all_the_c14_dates,
                                        paste0(square, "_", spit, "_"))

  all_the_c14_dates$c14_sample_code_num <-
    paste0("X", all_the_c14_dates$c14_sample_code_num)

  # first look for C14X## matches in the total station points...

  all_the_c14_dates$c14_sample_code_num <- gsub("^X4$", "X_4", all_the_c14_dates$c14_sample_code_num)

  c14x_matches <-
    ts_data_both_years_PFs[grep(
      paste0(
        paste0(all_the_c14_dates$c14_sample_code_num, "$"),
        collapse ="|"),
      ts_data_both_years_PFs$Description),]

  c14x_matches_summary <-
    c14x_matches %>%
    group_by(Description) %>%
    summarize_if(is.numeric, mean)

  c14x_matches_summary$to_match <- paste0("X",
                                          gsub(".*X", "", c14x_matches_summary$Description))

  all_the_c14_dates$to_match <- all_the_c14_dates$c14_sample_code_num
  all_the_c14_dates$to_match <- gsub("^X4$", "X_4", all_the_c14_dates$to_match)

  # second look for the SF matches... need to be careful about this because it seems like SF has two sets of numbers

  # all_the_c14_dates$square_spit
  all_the_c14_dates$tmp <- paste0(all_the_c14_dates$square_spit, paste0("SF", all_the_c14_dates$SF))

  want <- sort(all_the_c14_dates$tmp[!grepl("SF0", all_the_c14_dates$tmp)]) # 8

  have <- sort(gsub("PF_", "", unique(ts_data_both_years_PFs$Description[grepl(paste0(all_the_c14_dates$tmp, collapse = "|"), ts_data_both_years_PFs$Description)]))) # 6

  # what are we missing?
  # want[!want %in% have]
  # want "C4_9A_SF16"   only have  PF_C4_9_SF16
  # want "D3_16B_SF33"  only have  PF_D3_16_SF33

  # deal with it:
  all_the_c14_dates$tmp <- ifelse(all_the_c14_dates$tmp == "C4_9A_SF16",
                                  "C4_9_SF16",
                                  all_the_c14_dates$tmp)
  all_the_c14_dates$tmp <- ifelse(all_the_c14_dates$tmp == "D3_16B_SF33",
                                  "D3_16_SF33",
                                  all_the_c14_dates$tmp)



  sf_matches <-
    ts_data_both_years_PFs[grep(
      paste0(
        all_the_c14_dates$tmp,
        collapse ="|"),
      ts_data_both_years_PFs$Description),]

  # we have points for each SF, so we just need the mean location
  sf_matches_summary <-
    sf_matches %>%
    group_by(Description) %>%
    summarize_if(is.numeric, mean)

  sf_matches_summary$to_match <-
    gsub("PF_", "", sf_matches_summary$Description)

  all_the_c14_dates$to_match <-
    ifelse(all_the_c14_dates$to_match == "XNA",
           all_the_c14_dates$tmp,
           all_the_c14_dates$to_match)

  ## join it all up
  all_points <- rbind(c14x_matches_summary, sf_matches_summary)

  # bind back to C14 data
  all_the_c14_dates <-
    left_join(all_the_c14_dates,
              all_points,
              by = "to_match")

  # remove dups
  all_the_c14_dates <- all_the_c14_dates[!duplicated(all_the_c14_dates$Sample.ID),]

  # don't care if no age
  all_the_c14_dates <-
    all_the_c14_dates %>%
    filter(!is.na(Median))

  surf <- 100.693213   # NE_SEC_TAPE_1
  all_the_c14_dates$depth_below_surface <- all_the_c14_dates$Elevation - surf

  # because I'm not sure how the calibrations are done, I'll do it again myself
  # compute 95 credible interval cf. https://github.com/andrewcparnell/Bchron/issues/1
  c14_ages <- as.numeric(as.character(all_the_c14_dates$Mean.14C.Age..BP. ))
  c14_errors <- as.numeric(as.character(all_the_c14_dates$X1..14C.Age..BP.))


  ages_cal <- Bchron::BchronCalibrate(ages=c14_ages,
                             ageSds=c14_errors,
                             calCurves=rep('intcal13', length(c14_ages)))
  # First create age samples for each date
  age_samples = Bchron::SampleAges(ages_cal)
  # Now summarise them with quantile - this gives a 95% credible interval
  credible_ints <- apply(age_samples,2,quantile,prob=c(0.025,0.975))

  all_the_c14_dates$Bchron_95perc_cred_int_2.5 <- unname(credible_ints[1, ])
  all_the_c14_dates$Bchron_95perc_cred_int_97.5 <- unname(credible_ints[2, ])

  # midpoint of 95% credible interval
  all_the_c14_dates$Bchron_Median <- with(all_the_c14_dates,
                                          Bchron_95perc_cred_int_2.5 +
                                            (Bchron_95perc_cred_int_97.5 -
                                               Bchron_95perc_cred_int_2.5) / 2)


  C14_sample_depths_and_spit_depths <- all_the_c14_dates

  write.csv(C14_sample_depths_and_spit_depths, "data/ages/MKII_both_years_C14_samples_and_depths.csv")

  return(C14_sample_depths_and_spit_depths)

}


#' plot_ages_and_artefacts
#'
#' @param get_osl_ages
#' @param get_c14_ages
#' @param stone_artefacts_only
#'
#' @return
#' @export
#'
#' @import purrr
#' @import dplyr
#' @import ggpmisc
#' @import viridis
#' @import grid
#' @import ggrepel
#'
plot_ages_and_artefacts <- function(osl_ages, c14_ages, stone_artefacts_only){


  wanted <- "B3|B4|B5|C3|C4|C5|D3|D4|D5|E3|E4|E5"

  wanted_c14_sample_depths_and_spit_depths <-
    c14_ages %>%
    filter(grepl(wanted, Description))

  wanted_osl_ages_and_description <-
    osl_ages %>%
    filter(grepl("NW|SW", Description))

  # biplot
  wanted_C14 <-
    wanted_c14_sample_depths_and_spit_depths %>%
    select(Lab.ID, Sample.ID,  Bchron_Median, Elevation) %>%
    mutate(age = Bchron_Median/1000,
           Description = paste0(Lab.ID, " (", Sample.ID, ")"),
           method = "C14") %>%
    ungroup %>%
    select(-one_of(c('Lab.ID', 'Sample.ID', 'Bchron_Median')))


  wanted_OSL <-
    wanted_osl_ages_and_description %>%
    select(Description, `Age (ka)`, Elevation)  %>%
    mutate(method = "OSL",
           age = as.numeric(gsub(" ±.*$","", `Age (ka)`))) %>%
    select(-`Age (ka)`)

  wanted_C14_and_OSL <- rbind(wanted_C14, wanted_OSL)
  surf <- 100.693213   # NE_SEC_TAPE_1
  wanted_C14_and_OSL$depth_below_surface <-  -(wanted_C14_and_OSL$Elevation - surf)

  # investigate the distributions ----------------------------------------------------

  # draw plot with equations

  formula <- y ~ poly(x, 2)

  excluding <- c("Wk43605", # pit feature
                 #"Wk43606",
                 #"Wk43607",
                 #"Wk43610",
                 #"Wk43604",
                 #"Wk43611",
                 #"OZT591",
                 "OZT593")

  # exclude some points
  wanted_C14_and_OSL_plot_excluding <-
    wanted_C14_and_OSL %>%
    filter(!grepl(paste0(excluding, collapse ="|"), Description))

  # compute polynomial regression
  wanted_both_methods_model <- lm(age ~
                                   poly(depth_below_surface, 2),
                                 data = na.omit(wanted_C14_and_OSL_plot_excluding))
  # poly 2 is a very good fit

  # text size for labels
  size <- 2
  depth_age_biplot <-
    ggplot() +
    geom_smooth(data = wanted_C14_and_OSL_plot_excluding, # exclude some from curve fit
                aes(depth_below_surface,
                    age,
                    group = method,
                    colour = method),
                method = "lm",
                formula = formula,
                fullrange=TRUE,
                fill = viridis(1),
                alpha = 0.1) +
    geom_point(data = wanted_C14_and_OSL, # plot all points
               aes(depth_below_surface,
                   age,
               group = method,
               colour = method)) +
    stat_poly_eq(data = wanted_C14_and_OSL_plot_excluding, # exclude some from curve fit
                 aes(label = ..eq.label..), formula = formula,
                 parse = TRUE) +
    geom_text_repel(data = wanted_C14_and_OSL,
                    aes(x = depth_below_surface,
                        y = age),
                    colour = 'black',
                    size = size,
                    label = wanted_C14_and_OSL$Description) +
    annotate("text", x = 2.8,
             y = 10,
             label = paste0("Curve fit \nexcludes:\n",
                            paste0(excluding, collapse = "\n")),
             size = 2) +
    theme_minimal(base_size = 14) +
    scale_color_viridis(discrete=TRUE,
                        begin =0,
                        end= 1,
                        option = "viridis") +
    scale_fill_viridis() +
    xlab("Depth below surface (m)") +
    ylab("Age (ka)") +
    # ggtitle(paste0("MJB depth-age plot of C14 and OSL dates \nfrom ",  gsub("\\|", ", ", wanted))) +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_rect(colour = NA, fill = "white"),
          legend.key = element_rect(colour = NA, fill = "white")) +
    xlim(c(0,3))

  depth_age_biplot

  filename_ <- paste0("figures/depth_age_plot_of_C14_an_OSL_dates_from ", gsub("\\|", " ", wanted), ".png")

  ggsave(filename_,  width = 10, height = 10)

  # Plot lithic histogram ------------------------------------------------------------

  wanted_Lithics <- stone_artefacts_only[grepl(wanted, stone_artefacts_only$Description), ]
  wanted_Lithics$depth_below_surface <- - (wanted_Lithics$Elevation - surf)

  artefact_pulses <- c(0.05, 0.7, 1, 1.5, 2.15, 2.6)

  lithics_depth <- ggplot(wanted_Lithics,
                          aes(depth_below_surface)) +
    geom_histogram(binwidth = 0.05) +
    theme_minimal(base_size = 14) +
    theme(aspect.ratio = 1.61803/1) +
    xlim(c(0,3)) +
    geom_vline(xintercept=artefact_pulses,
               colour = "red",
               linetype="dotted",
               size = 1) +
  xlab("Depth below surface (m)") +
  ylab("Number of artefacts")  # +
    # ggtitle(paste0("Plotted lithics \nfrom ",  gsub("\\|", ", ", wanted)))

  lithics_depth

  filename_ <- paste0("figures/histogram_stone_artefacts_by_depth_from ", gsub("\\|", " ", wanted), ".png")

  ggsave(filename_,  width = 10, height = 10)

  # combine depth-age plot and lithic plot ------------------------------------------------

  # interpolate ages using age model fit of both c14 and OSL
  ages_of_pulses <- unname(predict(wanted_both_methods_model,
                                   data.frame(depth_below_surface = artefact_pulses)))

  # update age-depth plot with lines indicating pulses
  depth_age_biplot_ <-
    depth_age_biplot +
    theme(aspect.ratio = 1) +
    annotate("segment",
             x = 0,
             y = ages_of_pulses,
             xend = artefact_pulses,
             yend = ages_of_pulses,
             colour = "red",
             linetype="dotted",
             size = 1) +
    annotate("segment",
             x = artefact_pulses,
             y = 0,
             xend = artefact_pulses,
             yend = ages_of_pulses,
             colour = "red",
             linetype="dotted",
             size = 1) +
    annotate("text",
             x = 0,
             y = ages_of_pulses + 2.5,
             label = paste0(round(ages_of_pulses, 1), " ka"),
             size = 3) +
    theme(plot.margin = unit(c(1,3,1,1), "lines"))

  # combine age-depth and artefacts
  grid.newpage()
  grid.draw(rbind(ggplotGrob(depth_age_biplot_),
                  ggplotGrob(lithics_depth),
                  size = "first"))

  # save a copy

  dev.off()
  png(file = "figures/depth_age_plot_with_artefact_bands.png",
      width = 1600,
      height = 3200,
      res = 300,
      #antialias = "cleartype")
      type="cairo")
  grid::grid.draw(rbind(ggplot2::ggplotGrob(depth_age_biplot_),
                        ggplot2::ggplotGrob(lithics_depth),
                  size = "first"))
 dev.off()





}



