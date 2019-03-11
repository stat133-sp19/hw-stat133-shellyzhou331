## title: 
## Description:
## Input: Datasets contain every players shot informtion
## Output:

## Add the image as background
library(jpeg)
library(grid)
library(ggplot2)

getwd()
# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"
# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

## Shot Charts
# shot chart with court background
thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
ggsave(filename = "../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)

# shot chart with court background
iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
ggsave(filename = "../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)

# shot chart with court background
green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
ggsave(filename = "../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)

# shot chart with court background
durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
ggsave(filename = "../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)

# shot chart with court background
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
ggsave(filename = "../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)


## The big shot chart
total_data <- rbind(iguodala, green, thompson, durant, curry)
all_shot_chart <- ggplot(data = total_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() + facet_wrap(~name)
ggsave(filename = "../images/gsw-shot-charts.pdf", width = 8, height = 7)
ggsave(filename = "../images/gsw-shot-charts.png", width = 8, height = 7)
