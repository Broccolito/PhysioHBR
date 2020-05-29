# PhysioHBR

As a high-performance, user-friendly and comprehensive data visualization tool, PhysioHBR uses R shiny and shinydashboard to visualize small- to middle- scaled research data.



### Supported Analysis

- Data summary
- Regression statistics
- Correlation heatmaps
- Boxplots
- Linear regression plots



### Usage

- Install R

  [R for Windows]https://cran.r-project.org/bin/windows/base/old/3.5.3/

  [R for Mac]https://cran.r-project.org/bin/macosx/

  In Ubuntu:

  ```bash
  sudo apt-get install r-base
  ```

- Install Shiny and required packages in R

  ```R
  install.packages("shiny")
  install.packages("shinydashboard")
  install.packages("shinydashboardPlus")
  install.packages("shinyWidgets")
  install.packages("reshape2")
  install.packages("ggplot2")
  install.packages("dplyr")
  library(shiny)
  ```

- Run GitHub instance

  ```R
  runGitHub(repo = "PhysioHBR", username = "Broccolito")
  ```

- Sample data can be downloaded from: https://github.com/Broccolito/PhysioHBR/blob/master/sample_data.csv

  | id    | var1   | var2    | var3    | var4    | var5    | var6    | …    |
  | ----- | ------ | ------- | ------- | ------- | ------- | ------- | ---- |
  | index | factor | numeric | numeric | numeric | numeric | factor  |      |
  | 1     | F      | 160.02  | 67.6    | 26.4    | 41      | Tibetan |      |
  | 2     | M      | 167.64  | 80.2    | 28.5    | 51      | Tibetan |      |
  | 3     | F      | 154.94  | 59      | 24.6    | 47      | Tibetan |      |
  | 4     | M      | 172.72  | 109.3   | 36.6    | 51      | Tibetan |      |
  | 5     | M      | 177.8   | 72.6    | 23      | 20      | Tibetan |      |
  | 6     | F      | NA      | NA      | NA      | 61      | Tibetan |      |
  | 7     | F      | 154.94  | 67.1    | 28      | 78      | Tibetan |      |
  | 8     | F      | 162.56  | 80.3    | 30.4    | 33      | Tibetan |      |
  | 9     | F      | 165.1   | 59.9    | 22      | 28      | Tibetan |      |
  | 10    | M      | NA      | NA      | NA      | 29      | Tibetan |      |
  | …     |        |         |         |         |         |         |      |

  - Fill the first row with variable names.
  - Fill the second row with variable types. Acceptable types include: Index, factor, numeric and character
  - Fill non-assigned values with NA, do not leave blank elements in the table.
  - Save the table as a ```.csv``` file.



### License

MIT



### Contributor

| Name      | Email           |
| --------- | --------------- |
| Wanjun Gu | wag001@ucsd.edu |



### Reference

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. ISBN 978-3-319-24277-4, [https://ggplot2.tidyverse.org](https://ggplot2.tidyverse.org/).

