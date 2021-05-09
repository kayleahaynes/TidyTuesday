# All recorded water sources in Ethiopia

[link to TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md). 

The data this week comes from [Water Point Data Exchange](https://data.waterpointdata.org/dataset/Water-Point-Data-Exchange-WPDx-Basic-/jfkt-jmqa). Note that the data is limited to some core columns as the full dataset is ~300 Mb. Also data is largely filtered down to African sources.

> The amount of water point data being collected is growing rapidly as governments and development partners increasingly monitor water points over time. Without harmonization among these different data sources, the opportunity for learning will be limited, with the true potential of this information remaining untapped. 

> By establishing a platform for sharing water point data throughout the global water sector, WPDx adds value to the data already being collected. By bringing together diverse data sets, the water sector can establish an unprecedented understanding of water services.

> Sharing this data has the potential to improve water access for millions of people as a result of better information available to governments, service providers, researchers, NGOs, and others.

## Choice of data visualisation 

*Hexbin map*: The nature of this data lends itself to using some sort of map. I chose a hexbin map as I wanted to keep the analysis at a high level. The map shows the proportion of water sources that had no water available on the day of the reporting. I chose just to look at Ethiopia as I saw there was already some [analysis done here](https://data.waterpointdata.org/dataset/Ethiopia-Water-Point-Map/2nct-ktzw). I used a bin size that covered the area of Ethiopia without too many missing locations but also highlighted some differences in the locations. 

*Bar charts*: I also added a couple of simple bar charts of the data to explore the water sources in a bit more detail by looking at the number of different types of water sources and the reasons why there wouldn't be any water available. 

I decided to use the viridis colour palette as it has many blue/green colours similar to the colour of water. 

## Data assumptions/notes 

* Not all of the water sources were recorded at the same time. The map just shows the proportion of ones that had no water available on the day of reporting, this doesn't necessarily indicate areas where there is a shortage of available water but rather the areas that should be investigated further and updated if water is now available. 

## Ideas for next steps

* Look at the proportions weighted by the number of water sources and also in relation to populations in the area. Essentially look at the areas where water sources with unavailable water would be most problematic and what consequences this has (for example how many people are affected and how much further do they need to travel for water). 

* Filter the data to just look at the map for the different water source types and unavailable water reasons. This data can be used to come up with the best course of actions to ensure water is avaialble. For example: 

    * If the water source is broken a plan to fix and maintain the water source can be put in place. A review of the water source types can help come up with a maintenance review schedule. 
    * If there's a water shortage then can look at the georgraphic region and decide whether a different type of source could be used to provide water. 
    * Poor management/lack of money: Can the water source be managed properly/is there any money to maintain the water source. 
    
Can extend the analysis to look at different areas. One thing I noticed was the data on reasons water wasn't available is quite messy in some countries (I got lucky that the country I picked here had it fairly tidy), it would be interesting to do some text analysis to combine similar types of water issues. 

<p align="center">
  <img src="https://github.com/kayleahaynes/TidyTuesday/blob/master/2021/week19/week19.png" width="1000">
</p>

# Water sources in Ethiopia recorded in 2021
<p align="center">
  <img src="https://github.com/kayleahaynes/TidyTuesday/blob/master/2021/week19/week19_2.png" width="1000">
</p>
