# Retrieve all the auxiliary variables from a workspace

Lists all the variables in a modelling context.

## Usage

``` r
get_named_variables(context = NULL)
```

## Arguments

- context:

  a modelling context

## Value

a list with all the groups and named variables

## Examples

``` r
context_FR <- create_insee_context()
get_named_variables(context_FR)
#> $REG1
#> [1] "REG1.week"
#> 
#> $REG2
#> [1] "REG2.week"     "REG2.Saturday"
#> 
#> $REG3
#> [1] "REG3.Monday"            "REG3.Tuesday_to_Friday" "REG3.Saturday"         
#> 
#> $REG5
#> [1] "REG5.Monday"    "REG5.Tuesday"   "REG5.Wednesday" "REG5.Thursday" 
#> [5] "REG5.Friday"   
#> 
#> $REG6
#> [1] "REG6.Monday"    "REG6.Tuesday"   "REG6.Wednesday" "REG6.Thursday" 
#> [5] "REG6.Friday"    "REG6.Saturday" 
#> 
#> $LY
#> [1] "LY.LY"
#> 
#> $REG1_LY
#> [1] "REG1_LY.LY"        "REG1_LY.REG1_week"
#> 
#> $REG2_LY
#> [1] "REG2_LY.LY"       "REG2_LY.week"     "REG2_LY.Saturday"
#> 
#> $REG3_LY
#> [1] "REG3_LY.LY"                "REG3_LY.Monday"           
#> [3] "REG3_LY.Tuesday_to_Friday" "REG3_LY.Saturday"         
#> 
#> $REG5_LY
#> [1] "REG5_LY.LY"        "REG5_LY.Monday"    "REG5_LY.Tuesday"  
#> [4] "REG5_LY.Wednesday" "REG5_LY.Thursday"  "REG5_LY.Friday"   
#> 
#> $REG6_LY
#> [1] "REG6_LY.LY"        "REG6_LY.Monday"    "REG6_LY.Tuesday"  
#> [4] "REG6_LY.Wednesday" "REG6_LY.Thursday"  "REG6_LY.Friday"   
#> [7] "REG6_LY.Saturday" 
#> 
```
