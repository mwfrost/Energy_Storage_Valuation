percent <- function(x, digits = 2, format = "f", ...)
{
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}

calc.benefits <- function(input.df){
  c <- 'default'
  b.energy <- (input.df['on.peak.rate',c] - input.df['off.peak.rate',c]) * input.df['capacity',c] * input.df['load.shift.duration',c] * input.df['load.shifting.periods',c]
  
  b.demand <- input.df['demand.charge',c] * input.df['capacity',c] * 12 * 1000 # 100 is KW to MW
  
  b.quality <- input.df['cost.per.volt.sag',c] * input.df['volt.sag.count',c]
  
  b.total <- b.energy + b.demand + b.quality
  b.total
}

calc.sensitivity <- function(dat, var, delta) {
  var.a <- dat[var,'default']
  result.a <- calc.benefits(dat)
  dat[var,'default'] <- dat[var,'default'] * delta
  var.b <- dat[var,'default']
  result.b <- calc.benefits(dat)
  ((result.b - result.a) / result.a) / ((var.b - var.a) / var.a) 
  (result.b - result.a) / (var.b - var.a) 
}


exercise <- function(input.df, var.name, offset){
  input.df[var.name,1] <- input.df[var.name,1] * offset
  
  b.energy <- (input.df['on.peak.rate',1] - input.df['off.peak.rate',1]) * 
    input.df['capacity',1] * input.df['load.shift.duration',1] * 
    input.df['load.shifting.periods',1]
  
  b.demand <- input.df['demand.charge',1] * input.df['capacity',1] * 12 * 1000 # 100 is KW to MW
  b.quality <- input.df['cost.per.volt.sag',1] * input.df['volt.sag.count',1]
  b.total <- b.energy + b.demand + b.quality
  b.total
  
}

mc.benefits <- function(dat, iterations){
  mc.results <- c()
  dat <- dat[,c('default','var.name','sd')]
  dat$iteration <- 0
  for(i in 1:iterations) {
    mc.dat <-  ddply(dat, .(var.name, default, sd) , function(x) c(mcval=rnorm(1 , x$default, x$sd )))
    mc.dat$iteration <- i
    mc.dat$default <- mc.dat$mcval
    mc.dat$mcval <- NULL
    row.names(mc.dat) <- mc.dat$var.name
    mc.results[i] <- calc.benefits(mc.dat)
  }
  mc.results
}

calc.sensitivity.mc <- function(dat, var, delta, iterations) {
  var.a <- dat[var,'default']
  result.a <- mc.benefits(dat, iterations)
  dat[var,'default'] <- dat[var,'default'] * delta
  var.b <- dat[var,'default']
  result.b <- mc.benefits(dat, iterations)
  ((result.b - result.a) / result.a) / ((var.b - var.a) / var.a) 
  mean( (result.b - result.a) / (var.b - var.a)  )
}


# http://www.mathepi.com/comp/discounting.html 
# Compute net present value of payments received at the 
# beginning of each time interval given the discount.rate. 
# Payment 0 happens at time 0 (not time 1). 
npv <- function(discount.rate, payments, nn=length(payments)) {
  
  sum(payments * (1/(1+discount.rate))^(0:(nn-1))) }

# Payment 0 happens at time 1 (not time 0). 
npv1 <- function(discount.rate, payments, nn=length(payments)) {
  
  sum(payments * (1/(1+discount.rate))^(1:nn)) }

# npv function with optional zero shift 
npv.s = function(discount.rate, payments, npv0=0, fn) {
  
  fn(discount.rate, payments) - npv0 
}

# Solve IRR 
# npv must cross zero in the search range 
irr = function(payments, npv0=0, fn=npv) {
  
  uniroot(npv.s, c(0.0, 1), payments=payments, npv0=npv0, fn=fn)$root }

# Plot npv for range of discount rates 
# Can be used as diagnostic and to find range where npv crosses 
# zero (requirement for uniroot). 

p.npv = function(t, r=c(0,1), fn=npv) {
  
  s = seq(r[1], r[2], (r[2]-r[1])/30) 
  plot(s, sapply(s, fn, t, simplify=TRUE), type='l',
       
       xlab="discount rate", ylab="NPV", main="NPV vs discount rate"     ) 
  grid() 
  # abline(0, 0, col='blue') 
  
}

# Annotate with IRR 

p.irr = function(t, fn=npv) {
  
  r = irr(t) 
  points(r, 0) 
  text(r, 0, sprintf('IRR = %2.2f%%', 100*r), pos=4, col='blue') }
