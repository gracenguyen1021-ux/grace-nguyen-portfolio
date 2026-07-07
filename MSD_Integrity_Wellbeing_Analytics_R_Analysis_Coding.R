# MBUA512 Data Analytics Report - MSD Case
library(tidyverse)
library(scales)
library(epitools)

# Load data
clients <- read_csv("/Downloads/clients.csv")
sanctions <- read_csv("/Downloads/sanctions.csv")
hardship <- read_csv("/Downloads/hardship_grants.csv")
casenotes <- read_csv("/Downloads/casenotes.csv")

# Fix date types
clients <- clients %>%
  mutate(
    EntryDate = as.Date(EntryDate),
    ExitDate  = as.Date(ExitDate)
  )

sanctions <- sanctions %>%
  mutate(SanctionDate = as.Date(SanctionDate))

hardship <- hardship %>%
  mutate(GrantDate = as.Date(GrantDate))

casenotes <- casenotes %>%
  mutate(NoteDate = as.Date(NoteDate))

# Reproducible "as-of" date (end of dataset window)
ASOF_DATE <- as.Date("2024-12-31")

# -----------------------------
# 1. Independence Baseline: Continuous Duration on Benefit
# -----------------------------
clients <- clients %>%
  mutate(
    ExitDateFilled = if_else(is.na(ExitDate), ASOF_DATE, ExitDate),
    ContinuousDurationWeeks = as.numeric(difftime(ExitDateFilled, EntryDate, units = "days")) / 7
  )

duration_summary <- clients %>%
  summarise(
    mean_duration   = mean(ContinuousDurationWeeks, na.rm = TRUE),
    median_duration = median(ContinuousDurationWeeks, na.rm = TRUE),
    sd_duration     = sd(ContinuousDurationWeeks, na.rm = TRUE),
    iqr_duration    = IQR(ContinuousDurationWeeks, na.rm = TRUE)
  )
print(duration_summary)

p1 <- ggplot(clients, aes(x = ContinuousDurationWeeks)) +
  geom_histogram(bins = 35, color = "white") +
  labs(
    title = "Figure 1. Distribution of Continuous Duration on Benefit (Weeks)",
    x = "Continuous duration (weeks)",
    y = "Number of clients"
  ) +
  theme_minimal()
print(p1)

p2 <- ggplot(clients, aes(y = ContinuousDurationWeeks)) +
  geom_boxplot() +
  labs(
    title = "Figure 2. Outliers in Continuous Duration on Benefit (Weeks)",
    y = "Continuous duration (weeks)"
  ) +
  theme_minimal()
print(p2)

# -----------------------------
# 2. Regional Duration: Long-Term Dependence Concentration
# -----------------------------
regional_duration <- clients %>%
  group_by(Region) %>%
  summarise(
    clients = n(),
    median_weeks = median(ContinuousDurationWeeks, na.rm = TRUE),
    p90_weeks = quantile(ContinuousDurationWeeks, 0.90, na.rm = TRUE)
  ) %>%
  arrange(desc(median_weeks))
print(regional_duration)

p3 <- ggplot(regional_duration, aes(x = reorder(Region, median_weeks), y = median_weeks)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 3. Median Duration on Benefit by Region",
    x = "Region",
    y = "Median duration (weeks)"
  ) +
  theme_minimal()
print(p3)

# -----------------------------
# 3. Integrity Pressure Point: Sanction Hotspots
# -----------------------------
clients_by_region <- clients %>%
  count(Region, name = "clients")

hotspots <- sanctions %>%
  left_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  group_by(Region) %>%
  summarise(
    sanctioned_clients = n_distinct(ClientID),
    total_sanctions    = n(),
    sanction_rate_per_100_clients = (sanctioned_clients / first(clients_by_region$clients[match(Region, clients_by_region$Region)])) * 100,
    grade3_pct_of_sanctions = mean(SanctionGrade == 3) * 100
  ) %>%
  arrange(desc(sanction_rate_per_100_clients))
print(hotspots)

p4 <- ggplot(hotspots, aes(x = reorder(Region, sanction_rate_per_100_clients), y = sanction_rate_per_100_clients)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 4. Sanction Rate by Region (per 100 clients)",
    x = "Region",
    y = "Sanction rate (clients sanctioned per 100 clients)"
  ) +
  theme_minimal()
print(p4)

p5 <- ggplot(hotspots, aes(x = reorder(Region, grade3_pct_of_sanctions), y = grade3_pct_of_sanctions)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 5. Severe (Grade 3) Sanctions as % of All Sanctions",
    x = "Region",
    y = "% of sanctions that are Grade 3"
  ) +
  theme_minimal()
print(p5)

# -----------------------------
# 4.Wellbeing Signal: Grade 3 sanction -> Food grant within 60 days (event-based)
# -----------------------------
# Food grants only
food_grants <- hardship %>%
  filter(GrantType == "Special Needs Grant - Food") %>%
  select(ClientID, GrantDate)

# Grade 3 sanctions only
g3 <- sanctions %>%
  filter(SanctionGrade == 3) %>%
  left_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  select(SanctionID, ClientID, Region, SanctionDate)

# Link each sanction to any food grant within 60 days, then collapse to one row per sanction
wellbeing_signal <- g3 %>%
  inner_join(food_grants, by = "ClientID", relationship = "many-to-many") %>%
  mutate(days = as.numeric(GrantDate - SanctionDate)) %>%
  filter(days >= 0, days <= 60) %>%
  distinct(ClientID) %>%
  mutate(food_within_60 = TRUE)

# Attach to client region + calculate regional rate
wellbeing_rate <- clients %>%
  select(ClientID, Region) %>%
  left_join(wellbeing_signal, by = "ClientID") %>%
  mutate(food_within_60 = if_else(is.na(food_within_60), FALSE, food_within_60)) %>%
  group_by(Region) %>%
  summarise(pct_clients = mean(food_within_60) * 100, .groups = "drop") %>%
  arrange(desc(pct_clients))
print(wellbeing_rate)

p6 <- ggplot(wellbeing_rate, aes(x = reorder(Region, pct_clients), y = pct_clients)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figure 6. Food Grants Within 60 Days of Grade 3 Sanctions (by Region)",
    x = "Region",
    y = "% of clients with food grant within 60 days"
  )
theme_minimal()
print (p6)

# Sanctioned clients in Northland
northland_sanctioned_clients <- sanctions %>%
  inner_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  filter(Region == "Northland") %>%
  distinct(ClientID)

northland_grade3_flag <- sanctions %>%
  filter(SanctionGrade == 3) %>%
  inner_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  filter(Region == "Northland") %>%
  distinct(ClientID) %>%
  mutate(ever_grade3 = TRUE)

northland_food_flag <- sanctions %>%
  inner_join(hardship %>% 
               filter(GrantType == "Special Needs Grant - Food"),
             by = "ClientID") %>%
  inner_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  filter(Region == "Northland") %>%
  mutate(days = as.numeric(GrantDate - SanctionDate)) %>%
  filter(days >= 0, days <= 60) %>%
  distinct(ClientID) %>%
  mutate(food_within_60 = TRUE)

northland_contingency <- northland_sanctioned_clients %>%
  left_join(northland_grade3_flag, by = "ClientID") %>%
  left_join(northland_food_flag, by = "ClientID") %>%
  mutate(
    ever_grade3 = if_else(is.na(ever_grade3), FALSE, ever_grade3),
    food_within_60 = if_else(is.na(food_within_60), FALSE, food_within_60)
  )

table_northland <- table(
  Grade3 = northland_contingency$ever_grade3,
  Food60 = northland_contingency$food_within_60
)
table_northland

oddsratio(table_northland)

# -----------------------------
# 5. Administrative Gridlock
# -----------------------------
sanctioned_client_ids <- sanctions %>% distinct(ClientID)

backlog <- casenotes %>%
  semi_join(sanctioned_client_ids, by = "ClientID") %>%
  left_join(clients %>% select(ClientID, Region), by = "ClientID") %>%
  mutate(action_age_days = as.numeric(ASOF_DATE - NoteDate),
         open_gt30 = (ActionStatus == "Open") & (action_age_days > 30)) %>%
  group_by(Region) %>%
  summarise(pct_open_gt30 = mean(open_gt30) * 100, .groups = "drop") %>%
  arrange(desc(pct_open_gt30))
print(backlog)

p7 <- ggplot(backlog, aes(x = reorder(Region, pct_open_gt30), y = pct_open_gt30)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 20, linetype = "dashed") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Figure 7. Administrative Gridlock: Open Actions >30 Days (Sanctioned Clients)",
    x = "Region",
    y = "% open actions older than 30 days"
  ) +
  theme_minimal()
print(p7)

# -----------------------------
# 6. Client Segmentation (Northland)
# -----------------------------
sanction_counts <- sanctions %>%
  count(ClientID, name = "SanctionCount")

northland_sanctioned <- clients %>%
  filter(Region == "Northland") %>%
  inner_join(sanction_counts, by = "ClientID") %>%
  select(ClientID, Age, Dependents_Count, CaseComplexityScore, SanctionCount) %>%
  drop_na()

# Scale numeric features
cluster_x <- northland_sanctioned %>%
  select(Age, Dependents_Count, CaseComplexityScore, SanctionCount) %>%
  scale()

set.seed(42)
km <- kmeans(cluster_x, centers = 3, nstart = 25)
northland_sanctioned$Cluster <- factor(km$cluster)

cluster_profile <- northland_sanctioned %>%
  group_by(Cluster) %>%
  summarise(
    n = n(),
    avg_age = mean(Age),
    avg_dependents = mean(Dependents_Count),
    avg_complexity = mean(CaseComplexityScore),
    avg_sanctions = mean(SanctionCount),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_complexity))
print(cluster_profile)

p8 <- ggplot(northland_sanctioned,
             aes(x = CaseComplexityScore,
                 y = Dependents_Count,
                 color = Cluster,
                 shape = Cluster)) +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(
    title = "Figure 8. Northland Sanctioned Clients: Personas by Complexity and Dependents",
    x = "Case complexity score",
    y = "Dependents count",
    color = "Cluster",
    shape = "Cluster"
  ) +
  theme_minimal()
print(p8)
