#
# Example using R's Fuzzy sets facilities
#
# Taken from http://www.facstaff.bucknell.edu/mastascu/eControlHTML/Fuzzy/Fuzzy1.html
# See also http://stackoverflow.com/questions/15197613/fuzzy-logic-function-in-r-as-in-matlab

library(sets)

## set universe, i.e. the domain variables can range over.
sets_options("universe", seq(from = -200, to = 200, by = 1))

#------------------------------------------------------------------------------------------
# Membership Functions for measured temp
temp_cold_memshp_fn = fuzzy_trapezoid(corners=c(-101, -100, 40, 70))
temp_ok_memshp_fn = fuzzy_triangular(corners=c(40,70,100))
temp_hot_memshp_fn = fuzzy_trapezoid(corners=c(70, 100, 200, 201))

plot_temp_memshp_fns <- function () {
  plot(temp_cold_memshp_fn, xlim=c(0,120), col="blue")
  plot(temp_ok_memshp_fn, xlim=c(0,120), col="orange", add=TRUE)
  plot(temp_hot_memshp_fn, xlim=c(0,120), col="red", add=TRUE)
}

#------------------------------------------------------------------------------------------
# Membership Functions for heating control effort
heat_low_memshp_fn =  fuzzy_triangular(corners=c(-50,0,50))
heat_med_memshp_fn =  fuzzy_triangular(corners=c(0, 50, 100))
heat_high_memshp_fn = fuzzy_triangular(corners=c(50, 100, 150))

plot_heat_memshp_fns <- function() {
  plot(heat_low_memshp_fn, xlim=c(-50,150), col="yellow")
  plot(heat_med_memshp_fn, xlim=c(-50,150), col="green", add=TRUE)
  plot(heat_high_memshp_fn, xlim=c(-50,150), col="purple", add=TRUE)
}

#------------------------------------------------------------------------------------------
# Fuzzy Variables
variables <-
  set (
    temp = fuzzy_variable(cold   = temp_cold_memshp_fn,
                          ok     = temp_ok_memshp_fn,
                          hot    = temp_hot_memshp_fn),
    heat = fuzzy_variable(low  = heat_low_memshp_fn,
                          med    = heat_med_memshp_fn,
                          high  = heat_high_memshp_fn)
  )

#------------------------------------------------------------------------------------------
# Fuzzy Rules
rules <-
  set(
    fuzzy_rule(temp %is% cold, heat %is% high),
    fuzzy_rule(temp %is% ok,   heat %is% med),
    fuzzy_rule(temp %is% hot,  heat %is% low)
  )

#------------------------------------------------------------------------------------------
# Set up Fuzzy System
system <- fuzzy_system(variables, rules)
print(system)
plot(system)

get_heat_response <- function(fuzzy_system, temp) {
  fi <- fuzzy_inference(fuzzy_system, list(temp=temp))
#  plot(fi)
  return(gset_defuzzify(fi, "centroid"))
}


#------------------------------------------------------------------------------------------
# Examples
# If the measured temperature is 80 degrees, what's the control effort?
get_heat_response(system, 80)

#What does the heat response look like over temp range?
temps <- seq(from= 0, to=100, by=1)
heats <- NULL
for (i in temps) {
  heats[i] = get_heat_response(system, temps[i])
}
plot(heats)

## reset universe
sets_options("universe", NULL)

