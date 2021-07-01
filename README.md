<!-- badges: start -->
[![Travis build status](https://travis-ci.org/joel-becker/stRong.svg?branch=master)](https://travis-ci.org/joel-becker/stRong)
[![Codecov test coverage](https://codecov.io/gh/joel-becker/stRong/branch/master/graph/badge.svg)](https://codecov.io/gh/joel-becker/stRong?branch=master)
<!-- badges: end -->

# stRong

Package to visualise data from the "strong" app.

## Metrics

Arbitrarily, I set my exercise production function to be

```
Y = F(reps, sets, exercise_quantity, exercise_quality)
Y = (reps * sets * exercise_quantity * exercise_quality) ^ 0.5
```

and cumulative volume on a given day to be

```
Y_t = Y + 0.9*Y_{t-1}
```
