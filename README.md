
# chevron

<!-- badges: start -->
<!-- badges: end -->

The goal of `chevron` is to allow anyone to easily access Rugby League data.

## Installation

You can install the development version of chevron like so:

``` r
devtools::install_github("thecrucherau/chevron")
```
This requires the `devtools` package to be installed.

## Example

This is a basic example which shows you how to get information for a single match from 
Rugby League Project:

``` r
library(chevron)

get_rlp_match(39999)
#> $match_info
#> $match_info$rlp_match_id
#> [1] 39999

#> $match_info$rlp_round_name
#> [1] "Grand Final"

#> $match_info$referee_info
#> # A tibble: 2 x 4
#>   rlp_referee_id name          type     origin       
#>            <dbl> <chr>         <chr>    <chr>        
#> 1             80 Ben Cummins   on-field Cairns       
#> 2            551 Gerard Sutton on-field Coonabarabran

#> $match_info$status
#> [1] "Completed"

#> $match_info$date
#> [1] "Sunday, 6th October, 2019"

#> $match_info$crowd
#> [1] 82922


#> $team_info
#> # A tibble: 2 x 9
#>  rlp_team_id team_name   rlp_coach_id coach   ht_score ft_score scrums penalties is_home
#>        <dbl> <chr>              <dbl> <chr>      <dbl>    <dbl>  <dbl>     <dbl> <lgl>  
#> 1          11 Sydney Roo~         2924 Trent ~        8       14     NA        NA TRUE   
#> 2           2 Canberra R~          211 Ricky ~        6        8     NA        NA FALSE  

#> $player_info
#> # A tibble: 34 x 11
#>    rlp_player_id name     number position team_name  is_captain tries goals goal_attempts
#>            <dbl> <chr>     <dbl> <chr>    <chr>      <lgl>      <dbl> <dbl>         <dbl>
#>  1         19474 James T~      1 FB       Sydney Ro~ FALSE          1     0             0
#>  2         20005 Daniel ~      2 W        Sydney Ro~ FALSE          0     0             0
#>  3         22609 Latrell~      3 C        Sydney Ro~ FALSE          0     3             5
#>  4         22779 Joseph ~      4 C        Sydney Ro~ FALSE          0     0             0
#>  5          3701 Brett M~      5 W        Sydney Ro~ FALSE          0     0             0
#>  6         20149 Luke KE~      6 FE       Sydney Ro~ FALSE          0     0             0
#>  7          1595 Cooper ~      7 HB       Sydney Ro~ FALSE          0     0             0
#>  8         13876 Jared W~      8 FR       Sydney Ro~ FALSE          0     0             0
#>  9         28452 Sam VER~      9 HK       Sydney Ro~ FALSE          1     0             0
#> 10         20316 Isaac L~     10 FR       Sydney Ro~ FALSE          0     0             0
#> # ... with 24 more rows, and 2 more variables: sin_bins <dbl>, send_offs <dbl>

```

