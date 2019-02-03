// elephant dynamics by site 

// NOTATION
// ...

data {
	// dimensions
	int N[2]; 
	int C; 
	int S[C];
	int Y;
	// design matrices
	int XY[N[1]];
	int XC[N[1]];
	int XS[N[1]];
	int ZY[N[2]];
	int ZC[N[2]];
	// empirical data
	real y[N[1]]; 
	int  k[N[2]];
	int  n[N[2]];
	// fixed covariates
	real r;
	vector[Y] area[sum(S)];
	
}
transformed data {

	vector[Y] year;
	vector[Y] relative_area[sum(S)];
	
	for(i in 1:Y)
		year[i] = i - 1;
	
	{
		int ii = 0;
		for(i in 1:C) {
			for (j in 1:S[i]) {
				ii += 1;
				relative_area[ii] = area[ii] / area[ii][1];
			}
		}
	}
}
parameters {

	real x0_log[C];
	real x0_re_log[sum(S)];
	real mu[C];
	real alpha[C];
	real xi[C];
	real<lower=0> sigma[2];
	real<lower=0, upper=1> theta[C];	
	real<lower=0, upper=1> omega;	
}
transformed parameters {
	
	// linear predictors
	vector[Y] xt_log[sum(S)];
	vector[Y] lambda[C];
	
	for(i in 1:C) {
		
		for(j in 1:Y) {
	
			// define model for lambda
			lambda[i, j] = mu[i] + alpha[i] * year[j];
		}
	}
	
	{ 
		int ii = 0;
		for (i in 1:C) {
			for (j in 1:S[i]) {
				ii += 1;
				xt_log[ii] = x0_log[i] + x0_re_log[ii] + (r - lambda[i]) .* year - log(relative_area[ii]) - square(sigma[1])/2;
			}
		}
	}	
	
	
}
model {
		
	// likelihood
	for (i in 1:N[1]) {
	
		(y[i] > 0) ~ bernoulli(theta[XC[i]]);
		
		if (y[i] > 0) {
			y[i] ~ lognormal(xt_log[XS[i]][XY[i]], sigma[1]);
		}
	}
	
	for(i in 1:N[2]) {
	
		k[i] ~ binomial_logit(n[i], xi[ZC[i]] + lambda[ZC[i]][ZY[i]]);
	}
	
	x0_re_log ~ normal(0, sigma[2]);
	
	// ssvs prior
	target += omega * normal_lpdf(alpha | 0, 1) + (1 - omega) * normal_lpdf(alpha | 0, 0.001);
	omega ~ beta(0.5, 0.5);
	
	// error terms
	sigma  ~ cauchy(0, 1);

}
generated quantities {

	vector[Y] nt[sum(S)];
	vector[Y] numbers[C];
	vector[Y] numbers_region;
	vector[Y] mortality[C];
	vector[Y] mortality_region;
	vector[Y] pike[C];
	
	int ii;
	
	for(i in 1:C) {
		for(j in 1:Y) {
	
			mortality[i, j] = lambda[i, j];
			pike[i, j]      = inv_logit(xi[i] + lambda[i, j]);
		}
	}
	
	ii = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
			ii += 1;
			nt[ii] = theta[i] * exp(xt_log[ii]) .* area[ii];
		}
	}
	
	for (i in 1:C) {
		for (j in 1:Y) {
			numbers[i, j] = 0.0;
		}
	}
	
	for (i in 1:Y) {
		numbers_region[i] = 0.0;
		mortality_region[i] = 0.0;
	}
	
	ii = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
			ii += 1;
			for (l in 1:Y) {
				numbers[i, l] += nt[ii, l];
			}
		}
	}
	
	for (i in 1:C) {
		for (j in 1:Y) {
		
			numbers_region[j] += numbers[i, j];
			mortality_region[j] += mortality[i, j] * numbers[i, j];
		}
	}
	
	for (i in 1:Y) {
		mortality_region[i] /= numbers_region[i];
	}
}

