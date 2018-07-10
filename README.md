# banter

### Description

*banter* is a package for creating hierarchical acoustic event classifiers out of multiple call type detectors.

### Installation

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('ericarcher/banter')
```

### Tutorial

The BANTER model is initialized with a data.frame of events. There is one row per event and it must have a column called `event.id` which is a unique id for each event, and a column called `species` which assigns each event to a given species. Every other column in the data.frame will be used as a predictor variable for the events.  
In the package, an example data.frame is in the `train.data` example data list as the `$events` element.
```r
data(train.data)
bant.mdl <- initBanterModel(train.data$events)
```

Next, detector data is added to the initialized BANTER model object. Each detector is a data.frame with a column called `event.id` that associates the detected call with an event that the model was initialized with, and a `call.id` column that provides a unique identifier for each call. Every other column will be used as a predictor variable for the calls.  
In the package, example data.frames for three detectors are provided in the `$detectors` element of the `train.data` example data list. Here is an example of adding the burst pulse (`bp`) detector.

```r
bant.mdl <- addBanterDetector(
  bant.mdl, 
  data = train.data$detectors$bp, 
  name = "bp",
  ntree = 10, 
  sampsize = 1
)
```

The `addBanterDetector` function can be called repeatedly to add additional detectors. Alternatively, if the detectors are all in a named list, they can be added at once:

```r
bant.mdl <- addBanterDetector(
  bant.mdl, 
  data = train.data$detectors, 
  ntree = 10, 
  sampsize = 1
)
```

Once all of the detectors have been added, then the full BANTER model is run:
```r
bant.mdl <- runBanterModel(bant.mdl, ntree = 5000, sampsize = 3)
```

The model can be easily summarized:
```r
summary(bant.mdl)
```

The actual `randomForest` model can be extracted for the event or detector models:
```r
# extract event Random Forest model
event.rf <- getBanterModel(bant.mdl, "event")

# extract burst pulse (bp) Random Forest model
bp.rf <- getBanterModel(bant.mdl, "bp")
```

These can then be visualized using other tools, such as those in the `rfPermute` package:
```r
library(rfPermute)
plotVotes(event.rf)
```

To predict novel data, it must be in a list with the event data in the `$events` element, and the detector data in a named list called `$detectors`:
```r
data(test.data)
predict(bant.mdl, test.data)
```

### Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/banter/issues>
* send a pull request: <https://github.com/ericarcher/banter/>
* e-mail: <eric.archer@noaa.gov>

### Reference
Rankin, S., Archer, F., Keating, J. L., Oswald, J. N., Oswald, M., Curtis, A. and Barlow, J. (2017) Acoustic classification of dolphins in the California Current using whistles, echolocation clicks, and burst pulses. Mar Mam Sci, 33: 520-540. [doi:10.1111/mms.12381](https://onlinelibrary.wiley.com/doi/abs/10.1111/mms.12381)

### version 0.9.2

* Initial release