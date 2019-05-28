######################
# Loren Collingwood  #
# UC Riverside       #
# UCI Panel Analysis #
# W1-W2 Panels       #
# For Pop Panels     #
# Date: 5/28/2018    #
######################

# Clear Working Environment #
rm(list=ls())

#######################################
# Install and Load Necessary Packages # 
#######################################

check.packages <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then load them
packages<-c("car", "data.table", "descr", "devtools", "stringr", 
            "tidyverse",  "xtable", "Zelig", "ZeligChoice",)

check.packages(packages)

###########################################################
# Load/Install Rmturkcheck (may need newest version of R) #
###########################################################

if("Rmturkcheck" %in% installed.packages()[, "Package"] == TRUE) { # Installed
        
       library(Rmturkcheck)
                
} else {
                
        install_github("lorenc5/Rmturkcheck")
        library(Rmturkcheck) # Note console may ask you some questions
                
}

# Check Package is Loaded (brings up help file)        
?Rmturkcheck

####################################################################
# Compare Relevant covariates across waves 1 and 2                 #
# 'Balance Table' -- to ensure response rate is effectively random #
####################################################################

#####################
# Load Package Data #
#####################

data(w1)
data(w2)

######################
# Examine top 6 rows #
######################

head(w1)
head(w2)

#############################################
#   Difference of Means T-Test: Education   #
#############################################

ttest <- t.test(w1$educ_r, w2$educ_r); ttest

###############################################
#         Generalize to All Covariates        #
###############################################

# Logical Test: Verify column names are same #
colnames(w1) == colnames(w2)

# Gather column labels #
cnam <- colnames(w1)

# Look at cnam vector #
cnam

# Calculate generalize t-stats for all comparisons #
t_generalize(w1, w2, cnames1= cnam, cnames2 = cnam)

###############################
# Write out to xtable (latex) #
###############################

x <- t_generalize(w1, w2, cnames1= cnam, cnames2 = cnam)
row.names(x) <- c("Blacks more violent", "Gun Scale", "Religious Attendance",
                  "Party ID", "Democrat", "Republican", "Independent",
                  "Gun Ownership", "Income", "Education")
xtable(x, 
       caption="My T-Tests, I'm so amazing I did it", 
       label = "my_t_tests",
       align = c("l", rep("r", 5)))


################################################
# Estimate W1 - W2 models: Toy Muslim Ban Data #
# All Covariates measured Wave 1               #
################################################

#############
# Load Data #
#############

data("ban_dummy")

##########################################
#   Examine Distribution; Descriptives   #
##########################################

summary(ban_dummy)

##########################################
# Main IV: Create Min/Max for Simulation #
##########################################

amer_min <- min(ban_dummy$american_identity_r, na.rm=T); amer_min
amer_max <- max(ban_dummy$american_identity_r, na.rm=T); amer_max

#####################################################################
#  Model 1: Wave 1 Ban Attitude; Main IV= Wave 1 American Identity  #
#####################################################################

summary(model_z <- zelig(ban_w1_r ~ american_identity_r +
                                    trump_app_r + 
                                    muslim_scale,
                                    data = ban_dummy, 
                                    model="ls"))

################################
#  Post-Estimation Simulation  #
################################

#############################################
# Set low and high american_identity values #
#############################################

x.low <- setx(model_z, american_identity_r = amer_min); x.low
x.high <- setx(model_z, american_identity_r = amer_max); x.high

#######################################################################################
# Simulate 'Ban' expected values for low american_identity and high american_identity #
#######################################################################################

s.out1 <- Zelig::sim(model_z, x = x.low)
s.out2 <- Zelig::sim(model_z, x = x.high)

######################################
# Extract Simulation Expected Values #
######################################

min_max1 <- c(sim_extract(s.out1), sim_extract(s.out2))

######################################################################
#  Model 2: Wave 2 Ban Attitude; Main IV = Wave 1 American Identity  #
######################################################################

summary(model_z <- zelig(ban_w2_r ~ american_identity_r +
                                    trump_app_r + 
                                    muslim_scale,
                                    data = ban_dummy, 
                                    model="ls"))

# Set low and high american_identity values
x.low <- setx(model_z, american_identity_r = amer_min); x.low
x.high <- setx(model_z, american_identity_r = amer_max); x.high

#######################################################################################
# Simulate 'Ban' expected values for low american_identity and high american_identity #
#######################################################################################

s.out1 <- Zelig::sim(model_z, x = x.low); s.out1
s.out2 <- Zelig::sim(model_z, x = x.high); s.out2

######################################
# Extract Simulation Expected Values #
######################################

min_max2 <- c(sim_extract(s.out1), sim_extract(s.out2))

#################################
# Model 3: Ban change T1 to T2  #
#################################

summary(model_z <- zelig(ban_change_r ~ american_identity_r +
                                        trump_app_r + 
                                        muslim_scale,
                                        data = ban_dummy, 
                                        model="ls"))

#############################################
# Set low and high american_identity values #
#############################################

x.low <- setx(model_z, american_identity_r = amer_min)
x.high <- setx(model_z, american_identity_r = amer_max)

#######################################################################################
# Simulate 'Ban' expected values for low american_identity and high american_identity #
#######################################################################################

s.out1 <- Zelig::sim(model_z, x = x.low)
s.out2 <- Zelig::sim(model_z, x = x.high)

######################################
# Extract Simulation Expected Values #
######################################

min_max3 <- cbind ( sim_extract(s.out1, conf_band=T), 
                  sim_extract(s.out2, conf_band=T))
min_max3

######################################
#  Extract out 90% Confidence Bands  #
######################################

# Low American Identity #
min_vec <- sort(s.out1$get_qi())
min_low_90 <- min_vec[50]
min_high_90 <- min_vec[950]

# High American Identity #
max_vec <- sort(s.out2$get_qi())
max_low_90 <- max_vec[50]
max_high_90 <- max_vec[950]

#################
# Plot Creation #
#################

par(mfrow=c(1,2))

##################
# Panel 1 T1, T2 #
##################

x <- 1:2
plot(x,
     min_max1,
     pch=5,
     bty = "n",
     xaxt="n",
     main="Attitudes on Muslim Ban and American Identity",
     ylab = "E(Support Muslim Ban)",
     xlab = "Strength of American Identity\n(Measured Wave 1)",
     ylim = c(2,4))
points(x, min_max2, pch=6)
lines(x, min_max1, lwd=2)
lines(x, min_max2, lty=2, lwd=2, col="brown")
axis(1, at=c(1,1.5,2), labels=c("Low", "Mid", "High"))
text(1.7,3.3, "Wave 1")
text(1.9,2.7, "Wave 2")

# Panel 2 Plot -- Difference #

plot(x,
     min_max3[1,],
     pch=17,
     xaxt="n",
     bty="n",
     ylim=c(-1, 1),
     main="Change in Attitudes on Muslim Ban\n(Wave 1 to Wave 2)",
     ylab = "E(Muslim Ban Shift)",
     xlab = "Strength of American Identity\n(Measured Wave 1)")
lines(x, min_max3[1,], lwd=2)

segments(1, min_max3[2,1], 1, min_max3[3,1])
segments(2, min_max3[2,2], 2, min_max3[3,2])

segments(1, min_low_90, 1, min_high_90, lwd=2)
segments(2, max_low_90, 2, max_high_90, lwd=2)

axis(1, at=c(1,1.5,2), labels=c("Low", "Mid", "High"))
abline(h=0, col="grey", lty=2, lwd=2)



