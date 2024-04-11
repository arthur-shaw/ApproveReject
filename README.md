
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ApproveReject

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of ApproveReject is to provide a simple graphical interface for
approving and rejecting interviews.

## Installation

You can install the development version of ApproveReject like so:

``` r
pak::pak("arthur-shaw/ApproveReject")
```

## Usage

### Prepare the input file

The application requires an Excel file with a sheet containing the at
least the following columns:

- **`interviewId`.** Case identifier produced by Survey Solutions and
  stored in the `interview__id` in exported data. See more
  [here](https://docs.mysurvey.solutions/headquarters/export/identify_records/).
- **`decision`.** Expected values: either an `reject`, `approve`, or an
  empty cell.
- **`status`.** Status code produced by Survey Solutions and stored in
  `interview__status` in exported data. See more details
  [here](https://docs.mysurvey.solutions/headquarters/export/system-generated---export-file-anatomy/#coding_status)
  on the possible values.
- **`comment`.** Optional comment to provide upon approval / rejection.
  In the case of rejection, this is the message that field teams will
  see upon synching and receiving a rejected interview.

### Launch the app

To run the app, open RStudio and execute the the following lines of
code:

``` r
library(ApproveReject)
run_app()
```

This will launch the application in a new window.

In that window, the app will guide users through three tasks:

1.  Supply credentials for the Survey Solutions server
2.  Provide an Excel file containing details on interviews to reject
3.  Launch approval and/or rejection of interviews in the Excel file