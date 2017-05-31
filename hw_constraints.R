# load base data from south africa
load("hw_data_sa")
future.years <- c(2016:2050)
# fit linear models to hw size and capacity
nurse.model <- lm(n_nu ~ poly(year, 2), data = hw.data)
nurse.art.model <- lm(art_per_nu ~ n_nu, data = hw.data)
dr.model <- lm(n_dr ~ poly(year, 2), data = hw.data)
dr.art.model <- lm(art_per_dr ~ n_dr, data = hw.data)
# predict future hw size
hw.data <- rbind(
  hw.data, data.frame(
    year = future.years,
    n_nu = predict(nurse.model, data.frame(year=future.years)),
    art_per_nu = NA,
    n_dr = predict(dr.model, data.frame(year=future.years)),
    art_per_dr = NA
  )
)
is.future <- hw.data$year %in% future.years
# predict future hw capacity
hw.data$art_per_nu[is.future] <- predict(nurse.art.model, data.frame(n_nu = hw.data$n_nu[is.future]))
hw.data$art_per_dr[is.future] <- predict(dr.art.model, data.frame(n_dr = hw.data$n_dr[is.future]))

# add earlier years, assuming constant capacity
old.years <- rbind(hw.data[c(1,1,1,1), ])
old.years$year <- 2000:2003
hw.data <- rbind(old.years, hw.data)

# calculate total capacity
hw.data <- cbind(hw.data, art.capacity = hw.data$n_nu * hw.data$art_per_nu + hw.data$n_dr * hw.data$art_per_dr)

par(mfcol=c(2,2))
plot(hw.data$year, hw.data$n_nu, main="N nurses")
plot(hw.data$year, hw.data$art_per_nu, main="ART / nurse")
plot(hw.data$year, hw.data$n_dr, main="N clinicians")
plot(hw.data$year, hw.data$art_per_dr, main="ART / clinician")
par(mfcol=c(1,1))
