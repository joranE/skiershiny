## About this site

### Overview
The data on this site is compiled and maintained by me (Joran) based mainly on
the results publicly available on the [FIS website](fis-ski.com). I would say
that any errors are my own, except that the data published by FIS is not always
100% accurate itself. So, any errors are *probably* mine. If you happen to spot
something that you think is wrong feel free to let me know via email at 
statisticalskier at gmail dot com. 

Other data based skiing content by me can be found either on Twitter (@statskier)
or less frequently on my [website](statisticalskier.com).

In general, this site will allow you to produce a plot even if that plot is Bad
or Not A Good Idea. Instead of relying on my editorial judgement you will have to
exercise your own.

***

### PBM (Percent Back Median) Points
Some plots will display something called 'PBM Points'. These are analogous to
FIS points but instead of being based on a percent-back calculation from the 
winner they are based on a percent-back from the median skier. This includes
calculating a race penalty for each event, just like with FIS points. It is a 
system of my own devising whose main advantages are:

* allows for more meaningful discrimination between the best skiers,
* exceptional skiers (e.g. Marit Bjoergen) are less likely to distort everyone's points,
* more comparable across race formats (interval start, mass start, etc.).

Race penalties are calculated in a roughly analogous way as FIS points. However, 
instead of being set by the top five finishers it is set by the "middle five" 
finishers. Additionally, race penalties are not forced to be zero for WC and 
similar events. Instead, race penalties are applied to all races, but they are
inflated by a constant scaling factor for non-WC races.

Of course, it could also end up not working as well as I'd thought as I get to
see it in action over a real season. The plan is to evaluate and adjust next 
summer.

***

### Data Limitations
Anything that utilizes distance split times, or sprint heat times beyond the 
qualification round will have limited data. I only began accumulating that data
fairly recently, and of course it is only available for some FIS events. Future
work is planned that will (slowly) backfill as much of that data as possible. The
FIS live timing data can also itself be a little janky, so keeping it "clean" is
a moving target.

Published FIS results are fairly complete stretching back to the early 1990's.
Prior to that the results available online are severely truncated and typically
lack times which makes them of limited value for analysis. I have managed to supplement
the FIS data with paper records (thanks to Ruff Patterson & John Estle!) covering
a decent chunk of major international results between the late 1970's and 1990.
However, there are many gaps, including some entirely missing seasons. Additionally,
all this early data was hand entered by myself which involved a fair bit guesswork
on names, dates & locations. So mistakes in these older results really *are* mine.

***

### Gory Details
#### Skier Summary
This section is for looking at one skier at a time. Dragging over areas on the
graphs will show more details on the selected data points. Clicking on one of the
rows in the resulting table will show split time data for that race (if available).
The Data tab will let you download a single skier's raw data as either a csv or
an Excel file.

***

#### Head-to-head
This section is for comparing one skier to a collection of other skiers, specifically
when they have raced against each other. It will allow you to do stupid things like 
pick skiers who have never raced against each other (e.g. it will not prevent you from asking to 
look at Marit Bjoergen vs. Petter Northug) but will not display any data in those cases.
Again, selected regions on the graph will display additional details on those races
and clicking on one of the resulting rows will show split times (if available).

***

#### Development Progress
This section allows you to compare a collection of skiers to an "elite standard".
Specifically, it shows FIS points by age for the selected skiers with reference
lines for the 50th and 10th percentiles of FIS points for skiers who have achieved
at least two podium results in a major international race (WC/TdS/WSC/OWG). These
reference lines are calculated separately for men & women using quantile estimates
from a GAM (Generalized Additive Model). Fancy!

***

#### Race Snapshots
This section shows the standard race snapshot graphs that have been a staple of 
my Twitter feed for several years. Pick a date, then click on a race from that
day and it will produce a race snapshot for that race. The measure for distance 
events can be switched between the raw PBM skier, which is what I've traditionally
used for WC-level races, or PBM points or FIS points. Ther PBM points options will
tend to be more opinionated about field strength, but is more appropriate for non-WC
level races. Sprint races will always use the final finishing place. 
Normally I have only produced these plots for major international races, 
but this will allow you to make them for pretty much any race. For WC/TdS/WSC/OWG races it
uses only equivalent historical results from the same set of races. For any other
generic FIS race is uses all historical results for each athlete. This means that
these graphs for non-WC sprint races are essenitally meaningless. I'll keep thinking
about it and maybe come up with something.

***

#### Sprint Heat Progressions
This section is for displaying heat times as athletes progress through sprint
race rounds (qualification, quarterfinals, semifinals, finals). You can choose
from either the raw heat times, the difference from the median time in that round
and the difference from the advancement threshold time. This last measure is an
attempt to show how far people were from the minimum time necessary to advance to
the next round. (In the special case of the finals, the 'threshold' is the 3rd 
place time.) In general, all three measure look fairly similar.