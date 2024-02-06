[Link to R Markdown html](https://c6d36663b4624f3c8a19cef76c3456e4.app.posit.cloud/file_show?path=%2Ftmp%2FRtmpzyw5gh%2Fpreview-1942089a8c5.html)

title: "Diamonds_ggplot"\
author: "Daniel Capehorn"\
date: "2024-02-06"\
output: html_document


*This is a short exercise to demonstrate R techniques and plotting. It is not a full analysis and forms part of a wider portfolio.*

# Identify which cut is most popular by carat in diamonds data set

**First step is to install the diamonds dataset which is included with the ggplot2 package. We also need to install the below packages and open libraries that we will use to clean and transform the data.**

```{r package install, include=FALSE}
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)

install.packages("janitor")
library(janitor)
```

We then want to explore the data set, and understand its attributes:

```{r summary}
nrow(diamonds)
summary(diamonds)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/fc4d74fc-bc11-4281-a5ad-4c43524c7c0b)


This data set has information for nearly 54,000 diamonds (rows), with data for carat, cut, color, clarity, depth, *table*, price, *x*, *y*, *z*.

We do not know what table, x, y or z are, therefore in this instance, we would ask whoever has supplied the data to provide further insight:

```{r diamonds help}
?diamonds
```

**The results are as follows:**

-   carat: weight of the diamond (0.2--5.01)
-   cut: quality of the cut (Fair, Good, Very Good, Premium, Ideal)
-   color: diamond colour, from D (best) to J (worst)
-   clarity: a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
-   depth: total depth percentage = z / mean(x, y) = 2 \* z / (x + y) (43--79)
-   table: width of top of diamond relative to widest point (43--95)
-   price: price in US dollars (\$326--\$18,823)
-   x: length in mm (0--10.74)
-   y: width in mm (0--58.9)
-   z: depth in mm (0--31.8)

It would make sense to rename the columns so that they match the information provided by the author of the data set:

```{r}
diamonds_named <- diamonds %>%
  rename(length_mm = x, width_mm = y, depth_mm = z)
```

The next step is to understand whether there are any outliers. Grouping by values is a good way to establish what categories we should be working with and if there are any errors in spelling etc. in the strings:

```{r outliers}
cut_values <- 
  diamonds %>% 
  select(cut) %>% 
  distinct(cut)

glimpse(cut_values)
```

![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/78a43a0f-21b3-4052-9d60-6c5516bfee65)

```{r outliers}
color_values <- 
  diamonds %>% 
  select(color) %>% 
  distinct(color)

glimpse(color_values)
```

![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/f29fcebf-7937-4990-95d7-5ce7b7266d44)

```{r outliers}
clarity_values <- 
  diamonds %>% 
  select(clarity) %>% 
  distinct(clarity)

glimpse(clarity_values)
```

![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/5251974f-f931-4cea-91df-9b6c3d8c39b3)


Here we can clearly see that there are no misspelled values, or anything that looks out of place.

Next it is worth looking at the mean, min, max, and standard deviation of the numerical values to ensure there are no outliers. We can summarize by cut to provide context:

```{r statistics}
diamond_stats <- diamonds_named %>% 
  group_by(cut) %>% 
  summarize(mean_carat = mean(carat), mean_depth = mean(depth), mean_table = mean(table), mean_price = mean(price), mean_length = mean(length_mm), mean_width = mean(width_mm), mean_depth_mm = mean(depth_mm), min_carat = min(carat), min_depth = min(depth), min_table = min(table), min_price = min(price), min_length = min(length_mm), min_width = min(width_mm), min_depth_mm = min(depth_mm), max_carat = max(carat), max_depth = max(depth), max_table = max(table), max_price = max(price), max_length = max(length_mm), max_width = max(width_mm), max_depth_mm = max(depth_mm), sd_carat = sd(carat), sd_depth = sd(depth), sd_table = sd(table), sd_price = sd(price), sd_length = sd(length_mm), sd_width = sd(width_mm), sd_depth_mm = sd(depth_mm))
```

We then transform the data from wide to long data to enable us to see any patterns clearly:

```{r transpose}
transp_diamond_stats <- t(diamond_stats)
library(knitr)
kable(transp_diamond_stats)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/24bb314e-1de2-4a08-a714-727bd4544009)



Observations: \* There are minimum values of 0 for all cuts which would indicate an outlier\
\* Max width_mm for Premium and Ideal seem to fall outside of expectations\
\* Max depth_mm for Very Good falls outside of expectations\
\* Standard Deviation is relatively low for length, width and depth_mm, therefore we can assume that there the majority of the data set for these fields fall within acceptable parameters.\

We can see which values to strip out and at the same time from missing values:

```{r width outlier}
diamonds_named %>% 
  select(cut, width_mm) %>% 
  group_by(cut) %>% 
  drop_na() %>% 
  arrange(-width_mm)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/55c86c25-3158-4102-8503-4daa990792d6)


There are two clear outliers: width_mm Premium 58.9 and Ideal 31.80. We will filter these out of the data set.

```{r depth outlier}
diamonds_named %>% 
  select(cut, depth_mm) %>% 
  group_by(cut) %>% 
  drop_na() %>% 
  arrange(-depth_mm)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/24d34994-9c34-418e-8937-e6020c87ce13)


There is one clear outlier: depth_mm Very Good 31.80 which can be filtered out of the data set.

Finally for completeness, we will look at length_mm:

```{r length outlier}
diamonds_named %>% 
  select(cut, length_mm) %>% 
  group_by(cut) %>% 
  drop_na() %>% 
  arrange(-length_mm)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/b471bd4f-4e57-4d4c-8a67-25f34be39796)


The length seems to fall within an acceptable range, as we expected.

We can remove the outliers:

```{r}
clean_diamonds <- diamonds_named %>% 
  filter(width_mm <=30.00, depth_mm <=30.00, width_mm >0.00, depth_mm > 0.00, length_mm >0.00)
```

Then plot the most popular color and carat by cut:

```{r}

ggplot(data=clean_diamonds) + geom_bar(mapping = aes(x=carat, color=color)) + facet_wrap(~cut)
```
![image](https://github.com/danielcapehorn/diamonds_R/assets/158836200/83ecfee7-3313-4a90-80e2-bf143bd9a3ff)


Citations:

**R:** R Core Team (2023). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

**Tidyverse:** Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). "Welcome to the tidyverse." *Journal of Open Source Software*, *4*(43), 1686. <doi:10.21105/joss.01686> <https://doi.org/10.21105/joss.01686>.
