fhxR
====

S. Brewster Malevich
malevich@email.arizona.edu

August 27, 2012

An experiment in fire history plotting with FHX data files in R.

I am going to assume that you, beloved reader, already have a recent version of [R](http://www.r-project.org) installed and are more-or-less familiar with what's what. Other than that, none of teh h4x0r skillz are required. If you are not familiar with R, hit Google with the search "R stat tutorial", so some variantion there of. You will find a plethora of useful learning material and helpful people from a wide array of backgrounds.

This document begins be describing the general capabilities for dealing with FHX fire history data in R. The code for this is found largely found in the "firehistory.R" file. I will show you how to read this code into your R session below. As you read this, I suggest you open a terminal and follow along. Additional, project-specific, tools have been included for example. These can be found in the "tools.R" file.

# General capabilities

## Reading and writing fire history data in R

To begin, open your script or R session and type:

    source("firehistory.R")

This simply loads the fire history code into your R session without the need for cut-n-paste. If you are having trouble, see the "Help!" section, below.

Next, simply load the desired .fhx file.

    d <- read.fhx("17I.fhx")

"17I.fhx" is included when I packaged this, so we will load this file and assign it to the `d` variable.

We can also load data from a comma separated-value (CSV) file. An example CSV file "establishment.csv" is included as an example. More specifically, not-available or "NA" values in the CSV file are given by the infamous value "-999". The code *requires* that the CSV file has the following columns and headings:

* `unique`
* `site`
* `plot`
* `tree`
* `species`
* `pith date`
* `inner ring date`
* `outer ring date`
* 'bark date'

The column headings need to be exact (capitalization and whitespace matters!). The order, however, can be arbitrary. Also, if the CSV file has more column headings, that is fine, but the columns above are required!

Now we load this "establishment" csv file into R as an `fhx` object with:

    est <- read.csv("establishment.csv", na.strings = c(-999), row.names = NULL)
    TARGET.SITE <- "LYT"
    TARGET.PLOT <- "15C"
    est.site <- subset(est, est$site == TARGET.SITE)
    est.plot <- subset(est.site, est.site$plot == TARGET.PLOT)
    est.plot.fhx <- fhx(est.plot)

As you can see, first, we read the CSV file into R. Next we take an extra step to parse this CSV file data to only include observations from plot "15C" of the site "LYT". Of course, subsetting is not always required but is a good tool to have on hand [1]. Finally, we turn the subsetting data into an `fhx` object, much like we did when we read-in the "17I.fhx" file, above. Even though we've subset the data before turning it into an `fhx` object, this process still may take a very long time, especially with large datasets.

  [1]: [] The astute R user will be quick to point out that we could have combined the `subset()` calls by using the `AND` operator "`&`".

We can also write simple FHX files from an `fhx` object in R. Note that any estimated pith values will be removed in order to maintain compatibility with other FHX-based software:

    write.fhx(est.plot.fhx, fname = "outputfilename.fhx")

## Manipulating fhx objects

A recent addition to this code gives us the ability to concatenate or combine `fhx` objects. So, lets say we want to combine the data from the "17I.fhx" file with data from our establishment CSV file. Continuing from our work above, this is a fairly straight-forward thing to code:

    all.fhx <- est.plot.fhx + d

This is a deceptively simple operation. Behind the curtain, each series' observation is checked for duplicates. If a conflict araises, R will attempt to resolve it in a reasonable manner. In this way we can combine data from establishment CSV files (which can contain fire event information, but no pith estimations). Is is important to keep in mind that there is a system to deal with duplicate or even conflicting results. It can be helpful, but has the potential to be frusturating, especially when dealing with unique or [unclean data](http://en.wikipedia.org/wiki/Data_cleansing). In general, I've tried to create a system that will lean towards the safety of uncertainty. For example, if one file says that 1580 has an "early firescar", and the other file says 1580 has a "dormant firescar", R will then conclude that 1580 has had an "unknown firescar". Conflict between any type of scar and injury results in an "unknown injury". Estimation rings to account for pith override any type of event and in the case of conflicting outer/inner dates and bark/pith, the out/inner will be used. Given this complexity and the relative youth of this concatenating code, if you run into trouble or bugs, this may be one of the first places you should check.

As before, (and as I'm sure you've noticed by now) concatenation is also very slow, especially with larger datasets. This is because no attempt has been made to vectorize the function, or better yet, impliment it in C. Let me know if you'd like to have a go at it.

## Plotting

Now that we have FHX fire history data in R, we can put this information to good use. To plot we use the `plot()` command.

    plot(d)

gives us a new-age interpretation of the classic fire history plot. Solid circles indicate "solid" starting and stopping dates (i.e. "pith-date", "bark-date"). Triangles indicate any kind of fire event. You will also notice that solid lines in a series are periods when the tree is sensitive to fire events. The dotted-line indicates periods when the tree is not sensitive to fire events. Previously, inner and outer years had been plotted for each of the series. Here, we simply let the dotted and solid line segments end. It is evident enough that this is the first or last year of the series and gives us a bit more plotting space to work with. If the distance to pith is estimated, it is shown as a faint, thin line leading to a solid circle (not shown here, but you can see it if you plot `est.plot.fhx`, which we created in an earlier section).

We can draw vertical lines across recorded fire events to highlight clustered fire events.

    plot(d, vline = TRUE)

Each fire event is given a transparent gray strip. When multiple fire events occur in the same year or in relatively close succession, the gray strip becomes progressively darker with each event.

# Project specific function

Lets now pick appart the code in "tools.R".

## `BFP`: for when you absolutely positively have to plot all your FHX data at once

The Big Friendly Plot (BFP; what else do you think it would be called?) is designed to help you quickly visualize complex stand and fire history data in a flexible format. For this example we begin by loading our project-specific functions from "tools.R", bringing in data from an FHX file and adding meta data from a scar-establishment file:

    source("tools.R")
    scar.fhx.file <- "LYT.fhx"
    scar <- read.csv("scarred_trees.csv", na.strings = c(-999), row.names = NULL)
    scar.fhx <- read.fhx("LYT.fhx")

It will take a minute or two to read this large FHX file. Because the scar establishment file is not yet clean, we'll give it a good scrubbing:

    # Fixing odd names in fire-scar establishment file.
    names(scar) <- c("unique", "site", "plot", "tree", "sect", "cut.ht..cm.",
                     "species", "live", "condition", "final..distance..m.",
                     "sample.UTM.E", "sample.UTM.N", "plot..UTM.E", "plot..UTM.N"
                     "photo", "comments")
    # So scar's unique matches unique in other datasets.
    scar$unique <- paste(scar$unique, scar$tree, sep = "")

Our first command is changing the data's heading to deal with spelling and white space inconsistencies. The second line makes changes to `scar`'s "unique" column so that the unique IDs include tree numbers. Previously, they only considered each observation's site and plot values. Having exactly matching "unique" values is *essential* because this is how connections are made between the different files we use. You'll notice that I've already done the cleaning for the file "scarred_trees_good.csv", which we will use for an example in a later section.

Now lets add the meta data that we can read from the scar-establishment file to the corresponding FHX file:

    scar.fhx <- PlaceMeta(scar.fhx, scar)

We will begin plotting with this data, but if we wanted, we could pull specific sites or plots from the meta-data-added-`fhx` object with:

    # Don't run this.
    scar.fhx.sub <- GetSite(scar.fhx, site = "site name")
    # or grab a specific plot - again don't run this
    scar.fhx.sub <- GetPlot(scar.fhx, plt = "plot name")

Now a friendly plot:

    BFP(scar.fhx)

I do not show the plot here because it is far too large, but as always, I encourage you to follow along on your own machine. You can see that we have a series of plots faceted by site and color coded for the serie's species. We are not, however, limited to the default plot.

`BFP()` has several options. We can make the plot more compact by removing the y-axis labels using the `ylabs` argument:

    BFP(scar.fhx, ylabs = FALSE)

We can show vertical lines for common fire events across all sites in a manner similar to `plot()` on an `fhx` object. Thi shelps to visualize temporal fire density:

    BFP(scar.fhx, ylabs = FALSE, vline = TRUE)

Finally, a feature which is especially helpful with publication and printing, we can specify either the number of columns or the number of rows we would like to plot (I am afraid that we cannot specify both):

    BFP(scar.fhx, ylabs = FALSE, ncol = 2)
    BFP(scar.fhx, ylabs = FLASE, nrow = 5)

THERE IS A BIT HERE THAT I LEFT OUT p 8 - 14

# What if I want to export these plots to certain specs?

Luckily, R is pretty good at this. Lets say you want a 5 in by 5 in PNG at 300 DPI (because low resolution and image compression are for chumps).

    png("filename.png", width = 5, height = 5, res = 300, units = "in")
    plot(d)
    dev.off()

Okay, but what if you want to edit this in vector image editing software? We can do something similar for SVG images. Need another format? This can be done with PDF, EPS, MBP or likely whatever other open format your heart desires. Just check the R man page for each function (e.g. `?svg').

# Help!

## Trouble loading the firehistory.R script

If loading the R scrip is creating trouble, it could be one of a few things. First, be sure that you have the `ggplot2` and `reshape` packages installed. You can do this with:

    install.packages("ggplot2", dependencies = TRUE)
    install.pakcages("reshape", dependencies = TRUE)

and follow the on-screen instructions.

Another possiblity is that you do not have the script within your R path, or the current working directory (CWD). `getwd()` will return R's CWD. We can then change R's CWD with `setwd()`.

## When (not "if") you find a bug...

If you do run into a problem, [file a bug report](https://github.com/ltrr-arizona-edu/fhxR/issues) or at the very least, write me a nice email. In either case be sure to describe exactly what you did, what happened and what you expected to happen. It also would be a good idea to include your data, if you can.
