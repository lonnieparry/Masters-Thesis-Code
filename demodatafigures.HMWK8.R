library(EDIutils)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
#define dataset
scope <- "edi"
identifier <- "1318"

#get newest available revision
revision <- list_data_package_revisions(scope, identifier, filter = "newest")
package_id <- paste(scope, identifier, revision, sep = ".")

#available data entities to confirm names
entity_names <- read_data_entity_names(package_id)
print(entity_names)  # <-- just to visually confirm

#select the specific entity you want
entity_id <- entity_names$entityId[
  entity_names$entityName == "9_creelInterviewTable"
]

if (length(entity_id) == 0) stop("Entity name not found — check print(entity_names)")

#download & read it
raw_bytes <- read_data_entity(package_id, entity_id)
EDIcreeltable <- read_csv(file = raw_bytes)

#OK I mostly wanted to show that I could do this for the sake of this homework assignment, but now I am going to complete the rest of it with the revised version of this dataset so I can make some useable figure. You can find this still in my Github repo as creeltablecleaned.csv

creeltableall<-read.csv("creeltablecleaned.csv")


race_categories <- c(
  "asian" = "Asian",
  "indian" = "Indian",
  "white" = "White",
  "black_or_african_american" = "Black",
  "american_Indian_or_alaska_native" = "American Indian or Alaska Native",
  "hmong" = "Hmong",
  "multiracial" = "Multiracial",
  "refusal" = "Refusal",
  "hispanic" = "Hispanic or Latino (any race)",
  "other" = "Other",
  "arabic" = "Arabic",
  "Hispanic" = "Hispanic or Latino (any race)"
)

#I am removing any race categories I do not want as well as the ice fishing data

creeltabledemo <- creeltableall %>%
  filter(!is.na(race)) %>%                                  # remove NA races
  filter(partyActivity %in% c("shore_fishing", "boat_fishing")) %>%  # remove ice fishing
  mutate(
    race = recode(race, !!!race_categories)
  ) %>%
  filter(!race %in% c("Refusal", "Multiracial"))            # remove unwanted categories

# Set alphabetical race levels
race_levels <- sort(unique(creeltabledemo$race))

#creating a new varaible that stores what angler group they are in creeltabledemo <- creeltabledemo %>%
creeltabledemo <- creeltabledemo %>%
  mutate(
    angler_group = case_when(
      lakeID %in% c("EBP", "CEP", "CUP", "HUBP", "MM", "LSSP", "KLP", "RSP", "SSMP") ~ "Milwaukee",
      lakeID %in% c("MD", "ML", "KE", "WA", "WI", "YA", "SW", "FI", "UM") ~ "Dane County",
      lakeID %in% c("AQ", "AR", "BH", "BK", "BOT", "BY", "CA", "DS", "DY", "ER",
                    "FD", "HT", "ID", "IV", "JS", "LC", "LE", "LH", "LL", "LR",
                    "LT", "LV", "MS", "NH", "OB", "PK", "PN", "PT", "SA", "SE",
                    "SM", "SV", "TO", "UG", "WB", "WC", "WN", "WS", "BS", "BV",
                    "NT", "LJ", "TR", "BA", "EA", "BO", "BL", "BM", "LG", "ST", "HH") ~ "Vilas County",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(angler_group))


# Create combined angler group + activity for faceting
creeltabledemo <- creeltabledemo %>%
  mutate(
    group_activity = case_when(
      angler_group == "Vilas County" & partyActivity == "boat_fishing" ~ "Vilas County Boat Anglers",
      angler_group == "Milwaukee" & partyActivity == "boat_fishing" ~ "Milwaukee Boat Anglers",
      angler_group == "Milwaukee" & partyActivity == "shore_fishing" ~ "Milwaukee Shore Anglers",
      angler_group == "Dane County" & partyActivity == "boat_fishing" ~ "Dane County Boat Anglers",
      angler_group == "Dane County" & partyActivity == "shore_fishing" ~ "Dane County Shore Anglers",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group_activity))  # remove any rows that didn't match

valid_groups <- c(
  "Vilas County Boat Anglers",
  "Milwaukee Boat Anglers",
  "Milwaukee Shore Anglers",
  "Dane County Boat Anglers",
  "Dane County Shore Anglers"
)

#now we need to figue out the proportions for this graph
demoplot <- creeltabledemo %>%
  group_by(group_activity, race) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(group_activity) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup() %>%
  tidyr::complete(group_activity = valid_groups, race = race_levels, fill = list(count = 0, proportion = 0)) %>%
  mutate(race = factor(race, levels = race_levels))

# Color mapping for angler groups
color_map <- c(
  "Vilas County Boat Anglers" = "brown",
  "Milwaukee Boat Anglers" = "blue",
  "Milwaukee Shore Anglers" = "green",
  "Dane County Boat Anglers" = "blue",
  "Dane County Shore Anglers" = "green"
)

# Reorder group_activity so boat groups come first
demoplot$group_activity <- factor(demoplot$group_activity,
                                  levels = c("Vilas County Boat Anglers",
                                             "Milwaukee Boat Anglers",
                                             "Dane County Boat Anglers",
                                             "Milwaukee Shore Anglers",
                                             "Dane County Shore Anglers"))


ggplot(demoplot, aes(x = proportion, y = race, fill = group_activity)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = color_map) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  facet_wrap(~group_activity, nrow = 2, scales = "free_y") +
  labs(x = "Proportion", y = "Race") +
  ggtitle("Racial Composition of Wisconsin Anglers") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )

###OUTLIERS IN MY FIGURES###
#Looking at the actual figures I created here, there are certainly outliers in my data. This first obvious one is the lack of responses in My Vilas County Anglers. This figure definitely highlights the lack of diversity in that angler group, as there are no responses from any race other than white. This could be due to a variety of factors, including the demographics of the area, or even potential biases in who chose to respond to the survey.Other than that, the data looks like how I would expect it. 

####CHANGES OR IMBALANCES THAT COULD IMPACT INTERPRETATION OF THE DATA####
#1 I categorized multiple variations of some race categories like hispanic/black/multiracial categories, but that does mean some nuance is missing from a diversity standpoint. 
#2 Removed refusals and multiracial entirely. This ignores participants who declined to respond or who did not identify as a specific race, meaning it could lead to slight bias in the racial proportions. This is especially true when you notice just how many interviews I drop by doing so
#3 Proportions were calculated within each group_activity, which is correct for comparing racial composition within that group, but not for comparing across groups if sample sizes differ drastically
#4 Limited data on rural anglers resulted in an unclear big picture of the racial makeup of rural anglers in Wisconsin. This could skew the overall interpretation of racial diversity among anglers if rural areas have different demographics than urban areas.

# First, create the income_group variable and set it as a factor
creeltabledemo <- creeltabledemo %>%
  mutate(income_group = case_when(
    income %in% c("Less than $10,000", "$10,000 to $19,999",
                  "$20,000 to $29,999", "$30,000 to $39,999") ~ "Low (<$40k)",
    income %in% c("$40,000 to $49,999", "$50,000 to $59,999",
                  "$60,000 to $69,999", "$70,000 to $79,999") ~ "Middle ($40k-$79k)",
    income %in% c("$80,000 to $89,999", "$90,000 to $99,999",
                  "$100,000 to $149,999", "$150,000 or more") ~ "High (>$80k)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(income_group)) %>%   # remove any NAs
  mutate(income_group = factor(income_group, 
                               levels = c("High (>$80k)", "Middle ($40k-$79k)", "Low (<$40k)")))

# Define facet order (boats first row, shore second)
facet_order <- c(
  "Vilas County Boat Anglers",
  "Milwaukee Boat Anglers",
  "Dane County Boat Anglers",
  "Milwaukee Shore Anglers",
  "Dane County Shore Anglers"
)

# Make group_activity a factor
creeltabledemo$group_activity <- factor(creeltabledemo$group_activity, levels = facet_order)

# Calculate proportions only for existing combinations
income_plot_data <- creeltabledemo %>%
  group_by(group_activity, income_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(group_activity) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Color map: Vilas brown, boats blue, shore green
color_map <- c(
  "Vilas County Boat Anglers" = "brown",
  "Milwaukee Boat Anglers" = "blue",
  "Dane County Boat Anglers" = "blue",
  "Milwaukee Shore Anglers" = "green",
  "Dane County Shore Anglers" = "green"
)

# Plot
ggplot(income_plot_data, aes(x = proportion, y = income_group, fill = group_activity)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = color_map) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25)) +
  facet_wrap(~group_activity, ncol = 3, scales = "free_y") +  # 3 columns, 2 rows
  labs(x = "Proportion", y = "Income Group") +
  ggtitle("Income Composition of Wisconsin Anglers") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )


#MOVING ON TO MOTIVATIONS 
creeltableall <- creeltableall %>%
  rowwise() %>%
  mutate(
    Centrality = mean(c_across(c(c1LifeRevolves, c3MostEnjoyable, c4SignificantIncome,
                                 c6NoTimeHobbies, c8NoAlternative, c9FriendsFish,
                                 c11LoseFriends, c13OtherLeisure)), na.rm = TRUE),
    CatchOrientation = mean(c_across(c(c5Enjoyable, c7MaximizeNumbers, c14KeepFishing)), na.rm = TRUE),
    TrophyOrientation = mean(c_across(c(c2TrophySpots, c15SpecialGear, c12BigSmall)), na.rm = TRUE),
    HarvestOrientation = 6 - c10ReleaseFish
  ) %>%
  ungroup() %>%
  filter(if_any(c(Centrality, CatchOrientation, TrophyOrientation, HarvestOrientation), ~ !is.na(.)))

library(dplyr)
library(tidyr)
library(ggplot2)

# Prepare data
motivation_plot_data <- creeltableall %>%
  filter(site_group == "urban",
         partyActivity %in% c("shore_fishing", "boat_fishing")) %>%
  filter(if_any(c(Centrality, CatchOrientation, TrophyOrientation, HarvestOrientation), ~ !is.na(.))) %>%
  pivot_longer(
    cols = c(Centrality, CatchOrientation, TrophyOrientation, HarvestOrientation),
    names_to = "Motivation",
    values_to = "Score"
  ) %>%
  mutate(
    partyActivity = recode(partyActivity,
                           "boat_fishing" = "Boat Anglers",
                           "shore_fishing" = "Shore Anglers"),
    Motivation = factor(Motivation, levels = c("Centrality", "CatchOrientation", "TrophyOrientation", "HarvestOrientation"))
  )

# Calculate mean scores and max score per motivation & partyActivity for dynamic label positioning
means <- motivation_plot_data %>%
  group_by(Motivation, partyActivity) %>%
  summarise(mean_score = mean(Score, na.rm = TRUE),
            max_score = max(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(label_y = max_score + 0.05)  # offset a bit above the max

# Violin plot with dynamically positioned mean scores
ggplot(motivation_plot_data, aes(x = Motivation, y = Score, fill = partyActivity)) +
  geom_violin(trim = FALSE, alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), outlier.shape = NA) +
  geom_text(data = means, 
            aes(x = Motivation, y = label_y, label = round(mean_score, 2), group = partyActivity),
            position = position_dodge(width = 0.9), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Boat Anglers" = "steelblue", "Shore Anglers" = "green4")) +
  labs(x = "Motivation Category", y = "Score", fill = "Fishing Mode",
       title = "Motivation Scores of Urban Shore vs Boat Anglers") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )
