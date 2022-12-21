---
title: ''
output:
  html_document:
    toc: yes
    toc_float: yes
    theme: yeti
  pdf_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE)
```

```{r, include = FALSE}
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(broom)
library(purrr)
library(tinytex)
library(ggrepel)
library(countrycode)
library(sf)
library(rnaturalearth)
```

[![UN_Logo](https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/2411px-UN_emblem_blue.svg.png "UN Logo"){align="right" width="15%"}](UN.org)

# Multilateralism is Well and Alive - Evidence from the UN General Assembly Votes

by Yannik Sassmann

*last generated: `r lubridate::today()`*

------------------------------------------------------------------------

[![General_Assembly](https://upload.wikimedia.org/wikipedia/commons/0/05/UN_General_Assembly_hall.jpg "General Assembly"){width="100%"}](UN.org)

### Background

Multilateralism and international cooperation are crucial to addressing the major global challenges of the 21st century. Yet the end of multilateralism is being invoked on all fronts. Especially after Russia's war of aggression on Ukraine, which violated international law, the world is threatened to fall back into geopolitical blocs.

But what is the actual state of multilateralism? To provide a data-driven underpinning to the debate on the future of multilateralism, this report looks at the voting record of the UN General Assembly in historical comparison. Of particular interest is the proportion of votes in favor of resolutions. This proportion can be used as a proxy to look at whether there is a shared understanding of the underlying issues at hand and whether the global community is moving closer together over the years.

### Approach

Building on the [United Nations General Assembly Voting Dataset](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ) by *Erik Voeten*, the endorsement rate of resolutions is being analyzed for certain issues, years, and countries. The results of this analysis are presented below. In a first step, the proportion of resolutions with "yes" votes in the entire UN history is presented (Chapter 1). This is followed by a disaggregation of the voting behavior according to one of the six included topics (Chapter 2):

-   Palestinian conflict
-   Nuclear weapons and nuclear material
-   Arms control and disarmament
-   Human rights
-   Colonialism
-   Economic development

Finally, the voting behavior of the P5 members of the UN Security Council and additionally of Germany and India is presented (Chapter 3).

#### Data Transformations performed

```{r, include = FALSE}
# Load data
UN_voting <- read_csv("https://dataverse.harvard.edu/api/access/datafile/6358426")
```

As can be derived from the table below, **endorsement votes** are labeled as `1`.

| Vote |              |
|------|--------------|
| 1    | Yes          |
| 2    | Abstain      |
| 3    | No           |
| 8    | Absent       |
| 9    | Not a member |

Hence, the **percentage of endorsing votes** can be calculated by using `mean(vote == 1)`. For the historical comparison over the years `group_by(year)` is used. Depending on the subject of interest `topic` or `Countryname` is also used as an argument for the `group_by()` function.

```{r, include = FALSE}
# Transform data and create dataframes
voting_per_year <- UN_voting %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

voting_by_country <- UN_voting %>%
  group_by(Countryname) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

voting_by_country_year <- UN_voting %>%
  group_by(year, Countryname) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

```

To better visualize the historical trend of voting behavior, the direction and steepness of the trend was calculated using **coefficients**. For the necessary data transformation of the topics to tidy data, the `nest()` function was used. This was followed by the determination of the coefficients with the help of linear regression models `(model = map(data, ~ lm(percent_yes ~ year, data = .)`.

------------------------------------------------------------------------

### (1) Historical Comparison

As the graph below illustrates, the trend in voting behavior at the UN General Assembly provides anecdotal evidence against a the narrative of a crisis of multilateralism. While resolutions received an average endorsement rate of only 15% in 1950, the proportion of votes in favor of resolutions was around 2/3 in 2000. Since 2020, this figure has risen by another 0.08 percentage points. That said, the **increasingly positive trend in shares of endorsement votes over the years is clearly evident**, even though it seems to have **stagnated since 2010**. This positive trend seems to indicate that the international community has moved closer together, as the content of the resolutions is met with greater endorsement and is thus more likely to achieve consensus.

```{r}
# Plot voting per year
ggplot(voting_per_year, aes(year, percent_yes)) +
    geom_line() +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    geom_label(data=voting_per_year %>% filter(year==1950), aes(label=round(percent_yes, digits = 2)),
                                                                size=3.5, label.size=NA, vjust=-1) +
    geom_label(data=voting_per_year %>% filter(year==2000), aes(label=round(percent_yes, digits = 2)),
                                                                size=3.5, label.size=NA, vjust=-0.5) +
    geom_label(data=voting_per_year %>% filter(year==2020), aes(label=round(percent_yes, digits = 2)),
                                                                size=3.5, label.size=NA, vjust=-0.5) +
    geom_vline(xintercept = 2019, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 2008, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 2001, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 1990, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 1973, linetype="dotted", size = 0.3) +
    geom_vline(xintercept = 1962, linetype="dotted", size = 0.3) +
    geom_text(aes(x=2019, label="\nCOVID-19", y=0.1), size=3, colour="grey") +
    geom_text(aes(x=2008, label="\nFinancial Crisis", y=0.05), size=3, colour="grey") +
    geom_text(aes(x=2001, label="\nWar on Terror", y=0.15), size=3, colour="grey") +
    geom_text(aes(x=1990, label="\nFall of Berlin Wall", y=0.1), size=3, colour="grey") +
    geom_text(aes(x=1973, label="\nOil Crisis", y=0.1), size=3, colour="grey") +
    geom_text(aes(x=1962, label="\n Cuban Missle Crisis", y=0.05), size=3, colour="grey") +
    labs(title = "Share of endorsement Votes in UN Resolutions by Year",
         caption = "based on data from United Nations General Assembly Voting Data",
         x = "Year",
         y = "Percentage of 'Yes' Votes") +
    theme_classic()
```

------------------------------------------------------------------------

### (2) Disaggregation by Topic

But then where does the dissatisfaction with multilateralism come from? But then where does the dissatisfaction with multilateralism come from? To investigate this question, we next look at the voting behavior of UN member states in different thematic areas.

```{r, include = FALSE}
# Transform topic data into tidy data

topics <- UN_voting %>%
  gather(topic, has_topic, me:ec) %>%
  filter(has_topic == 1) %>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))

by_year_topic <- topics %>%
    group_by(year, topic) %>%
    summarize(total = n(),
              percent_yes = mean(vote == 1)) %>%
    ungroup()
```

```{r}
ggplot(by_year_topic, aes(year, percent_yes)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1950, 1970, 1990, 2010)) +
  geom_vline(xintercept = 2010, linetype="solid", size = 0.3) +
  labs(title = "Share of endorsement Votes in UN Resolutions by Year and Topic",
         caption = "based on data from United Nations General Assembly Voting Data",
         x = "Year",
         y = "Percentage of 'Yes' Votes") +
  facet_wrap(~ topic)

```

This graph also shows that UN member states have moved closer together on all thematic areas covered in the dataset. Since 1950, the **proportion of votes in favor of resolutions has increased significantly in all thematic areas**. Solely human rights seem to be more contentious, when compared to the other thematic areas, as the endorsement rate for resolutions covering human rights has stagnated since the 1980s.

However, when looking at the graph, it is also apparent that the endorsement rate for resolutions in all topic areas has **stagnated since 2010 and has even decreased in 4 out of 6 cases**.

------------------------------------------------------------------------

### (3) Influential Member States

Another hypothesis to be investigated is whether influential states block the UN's ability to shape policy, which in turn would explain the dissatisfaction with multilateralism. Against this background, the next section examines the voting behavior of the P5 countries, as most influential members of UN. Out of personal interest, Germany and India are also included in the analysis.

```{r}
# Plotting for countries

p5_plus_voting <- voting_by_country_year %>%
  filter(Countryname %in% c("United States of America", "United Kingdom of Great Britain and Northern Ireland", "China", "France", "Russian Federation", "India", "Germany"))

# Shorten to UK
p5_plus_voting$Countryname[p5_plus_voting$Countryname == "United Kingdom of Great Britain and Northern Ireland"] <- "UK"

ggplot(p5_plus_voting, aes(year, percent_yes)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1950, 1970, 1990, 2010)) +
  geom_hline(yintercept = 0.75, linetype="solid", size = 0.1, color = "darkred") +
  geom_label(data=p5_plus_voting %>% filter(year==2018), aes(label=round(percent_yes, digits = 2)),
                                                                size=3, label.size=NA, vjust=-0.5) +
  labs(title = "Share of endorsement Votes in UN Resolutions by Year and Country",
         caption = "based on data from United Nations General Assembly Voting Data",
         x = "Year",
         y = "Percentage of 'Yes' Votes") +
  facet_wrap(~ Countryname, scales = "free_y")
```

```{r, include = FALSE}
# Compare to overall mean
mean_UN_voting_2018 <- voting_per_year %>%
  filter(year == 2018) %>%
  summarize(mean = round(percent_yes, digits=2))

mean_UN_voting_2018

mean_US_2018 <- voting_by_country_year %>%
  filter(year == 2018 & Countryname == "United States of America")%>%
  summarize(mean = mean(percent_yes))
            
mean_US_2018$mean
```

Looking at the chart below and comparing it to all member states, it is clear that the influential P5 states on average agree to fewer resolutions than non-P5-states. While across all member states, the endorsement rate for resolutions in 2018 was `r mean_UN_voting_2018 * 100` percent, the endorsement rate for resolutions of 4 out of 5 P5 states was significantly lower. The USA in particular has one of the lowest endorsement rates of all member states at just `r mean_US_2018$mean * 100` percent. Interestingly enough, as the only P5 member country, China's endorsement rating for resolutions is above the average of all member states.

The observations of this analysis suggest that through their low endorsement ratings, **influential P5 countries** **contribute to the UN deadlock** and that **Western countries**, such as the US, UK and France **are partly at fault for the crisis of multilateralism**.

Lastly, we can also take a look at the distribution of endorsement votes for the year 2021 on a map. It seems to confirm the impression that Western countries endorse fewer resolutions than UN member states from Latin America, Africa and Asia.

```{r, include=FALSE, warning=FALSE}
# Get world data
world <- ne_countries(scale = "small", returnclass = "sf")

# Plot empty world map
world %>%
  ggplot() +
  geom_sf()

# Change map projection
world %>%
    st_transform(crs = "+proj=robin") %>%
    ggplot() +
    geom_sf() +
    theme_minimal()
```

```{r, include=FALSE, warning=FALSE}
# Prepare data
countries_2021 <- UN_voting %>%
  group_by(year, Country, Countryname) %>%
  filter(year == 2021) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1)) %>%
  rename(iso3 = Country)

countries_2021_iso3 <- world %>%
  select(geometry, iso_a3) %>%
  left_join(countries_2021, by = c("iso_a3" = "iso3")) %>%
  filter(Countryname != "NA")

```



```{r}
world %>%
  st_transform(crs = "+proj=robin") %>%
  ggplot() +
  geom_sf(color = "darkgrey") +
  geom_sf(data = countries_2021_iso3, aes(fill = percent_yes)) +
  scale_fill_manual(values = "royalblue") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),     
        panel.grid.minor = element_blank(),
      	axis.text.x=element_blank(), #remove x axis labels
      	axis.ticks.x=element_blank(), #remove x axis ticks
      	axis.text.y=element_blank(),  #remove y axis labels
      	axis.ticks.y=element_blank(),  #remove y axis ticks,
      	rect = element_blank(),
        plot.margin=unit(c(0.0,0,0,0), "null"), 
		    plot.title = element_text(size = 16,
                              face = "bold",
                              hjust = 0.5,
                              vjust = 7),
	      legend.title = element_text(size = 8,
                                hjust = 0.5),
	      legend.key.width = unit(22, 'mm'),
	      legend.key.height = unit(4, 'mm')) +
  scale_fill_gradient(name = 'Endorsement Rates by Country in 2021', 
                        low = '#F7FBFF', 
                        high = '#08306B', 
                        na.value = 'grey75') +
  guides(fill = guide_colourbar(title.position = "top",
                                  title.hjust = 0.5, 
                                  title.vjust = 2))

```

------------------------------------------------------------------------

### (4) Most Significant Trends

By using the calculated coefficients, the member states with the largest changes in voting behavior were identified. However, the informative value of this analysis is extremely limited, since the strong change in voting behavior is often either related to the admission of the state to the UN or dissolution of the country (e.g. Czechoslovakia).

```{r, include = FALSE}
# Quantify trends through modelling with linear regressions finding a "best-fit-line" for each countries

# Get country coefficients for each country
country_coefficients <- voting_by_country_year %>%
  nest(-Countryname) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Filter for only statistically significant trends
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05) %>%
  arrange(desc(estimate))

filtered_countries <- filtered_countries[-c(87),]                                         
                                         
# Sort for the countries increasing most quickly
filtered_countries_top <- filtered_countries %>%
    arrange(desc(estimate)) %>%
    slice_head(n=5) %>%
    unnest(data)

# Sort for the countries decreasing most quickly
filtered_countries_worse <- filtered_countries %>%
    arrange(estimate) %>%
    slice_head(n=5) %>%
    unnest(data)

```

```{r, message=FALSE, warning=FALSE, fig.width= 15, fig.height= 26}
# Plot all countries
filtered_countries %>% 
  ggplot()+
    geom_point(aes(estimate, Countryname, color = p.adjusted), size = 5) +
    geom_text_repel(aes(estimate, Countryname, color = p.adjusted, label = Countryname),
                     fontface = 'bold', size = 4, box.padding = unit(0.25, "lines"), 
                    point.padding = unit(0.5, "lines"),segment.color = 'grey50') +
    ggtitle("Countries where Yes vote is changing most quickly over time")+ 
    labs(x="Estimate (related to change in % Yes votes)", y="Country") +
    theme(legend.title = element_text(face = "bold", size = 16)) + 
    theme(legend.text = element_text(size = 16)) +
    theme(legend.position = "right")+ labs(color = "P value") +  
    theme(plot.title = element_text(size = 24)) +
    theme(axis.text.x=element_text(size=14), axis.title=element_text(size=16))
```

Top 5 Countries with the Highest Increase in Endorsement Rating for Resolutions

```{r}
ggplot(filtered_countries_top, aes(year, percent_yes)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1950, 1970, 1990, 2010)) +
  labs(title = "Countries with Highest Increase in Endorsement Rating for Resolutions",
         caption = "based on data from United Nations General Assembly Voting Data",
         x = "Year",
         y = "Percentage of 'Yes' Votes") +
  facet_wrap(~ Countryname, scales = "free_y")
```

Worst 5 Countries with the Highest Decrease in Endorsement Rating for Resolutions

```{r}
ggplot(filtered_countries_worse, aes(year, percent_yes)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1950, 1970, 1990, 2010)) +
  labs(title = "Countries with Highest Decrease in Endorsement Rating for Resolutions",
         caption = "based on data from United Nations General Assembly Voting Data",
         x = "Year",
         y = "Percentage of 'Yes' Votes") +
  facet_wrap(~ Countryname, scales = "free_y")
```

------------------------------------------------------------------------

### (5) Key Takeaways

There is no sign of a crisis of multilateralism in the voting pattern of the UN General Assembly. The proportion of votes in favor of resolutions has risen steadily since the 1950s to 76% in 2020. However, this trend has stagnated since 2010, which could partly explain the growing frustration with multilateral forums such as the UN. The analysis nevertheless also shows that influential states, such as the P5 members, tend to block the UN's work through their comparatively low endorsement rate for resolutions. Against this background, the question arises, how influential states deal with the geopolitical shift towards a multipolar world order and whether they will succeed in reviving multilateralism by making concessions toe emerging economies.

------------------------------------------------------------------------

### (6) Further Research

Other interesting research questions could explore the influence of geographic region or income classification on voting behavior. It would also be interesting to get to investigate the reasons for why the endorsement rate for resolutions in P5 member states is comparatively low. Another interesting area of investigation is analyzing voting blocs (for instance through PCA analysis).


<center>

[![UN Logo](https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/2411px-UN_emblem_blue.svg.png){width=35%}

</center>
