# This is an example script for the Chi-squared Goodness of Fit test.  It follows Example 1, Section 23.7
# in the Kreyszeg text.  The hypothesis is that the data are from a normal distribution.

# Citation:
# Kreyszig, E. (1998). Advanced Engineering Mathematics (8th ed.). New York: John Wiley & Sons, Inc.

# Example 1: You have test results from the splitting tensile strength of concrete cylinders (lb/in^2).  
# You wish to determine if they are normally distributed.  The Chi-squared Goodness of fit test 
# determines the extent that the differences in the sample distribution are likely to represent 
# differences from the normal distribution.

# Packages used:
library(e1071)

x = c(320,350,370,320,400,420,390,360,370,340,380,340,390,350,360,400,330,390,400,360,340,350,390,360,350,350,360,350,360,390,410,360,440,340,390,370,380,370,350,400,380,370,330,340,400,330,350,370,380,370,340,350,390,350,350,320,330,350,380,410,360,380,330,350,360,390,360,390,360,360,350,370,360,390,340,380,300,370,340,400,320,300,400,380,370,400,360,370,330,340,370,420,370,340,420,370,360,340,370,360)
n = length(x)     # = 100
m = mean(x)       # = 364.7
s = sd(x)         # = 26.83489 this is the function for the bias-corrected sample standard deviation, otherwise,
s1 = (sum(((x-m)^2))/(n-1))^(1/2) # = 26.83489
s2 = (sum(((x-m)^2))/(n))^(1/2)   # = 26.70037 this is the conventional standard deviation.
sk = skewness(x)  # = 0.157014
k = kurtosis(x)   # = -0.106061

hist(x, breaks = c(250,295,305,315,325,335,345,355,365,375,385,395,405,415,425,435,450), xlim = c(250,450))

# Normalize data
y = (x-m)/s

# Break up data into intervals such that each interval has at least five data elements x_j.
h = hist(x, breaks = c(250,325,335,345,355,365,375,385,395,405,450))
min(h$counts)     # = 6, this satisfies the requirement.  Interval is 10
b = h$counts
k = length(h$breaks)-1 # number of intervals
x1 = h$breaks

# Assemble table of X_0^2
v = array(0, dim = c(k))             # Array for the normalized value
p = v                                # Array for the F(x) or CDF of the normalized variable, v
e = v                                # Variable, e_j = n * p_j, where n is the number of variables/degrees of freedom
for (j in 1:k) {
      v[j] = (x1[j+1] - m)/s2        # Normalized variable
      p[j] = pnorm(v[j])             # F(x_j)
      if (j==k) {
            p[j] = 1
      }
      e[j] = n*p[j]
      if (j>1) {
            e[j] = e[j] - (n*p[j-1]) # adjustment for integral of p(j) from v(j-1) to v(j)
      }
}
X_0 = sum(((b-e)^2)/e)               # Summation.  Equation (1) from section 23.7 box.
