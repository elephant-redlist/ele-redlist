// elephant dynamics by site 

// NOTATION
// DIMENSIONS
// C: number of countries
// N: number of records
// S: number of sites or zones per country
// Y: number of years
// LOOK-UP VECTORS
// XY; XC; XS
// OBSERVATIONAL DATA
// y: observed densities
// COVARIATES
// r: intrinsic growth rate
// area: area size per site or zone

functions {

	int to_integer(real x) {
	
		int i = 1;
		while(i < x) {
			i += 1;
		}
		return i;
	}

	real median(vector x) {
	
		real y;
		
		if (fmod(num_elements(x), 2.0) == 0.0) {
		
			// num_elements(x) is even return the mean of the middle two numbers
			real i = num_elements(x) / 2.0;
			
			int j[2];
			j[1] = to_integer(i);
			j[2] = j[1] + 1;
			
			y = mean(sort_asc(x)[j]);
		
		} else {
		
			// num_elements(x) is odd return the middle number
			real i = ceil(num_elements(x) / 2.0);
			int  j = to_integer(i);
			
			y = sort_asc(x)[j];
		}
		
		return y;
	}
}
data {
	int C;
	int N;  
	int S[C];
	int Y;
	int XY[N];
	int XC[N];
	int XS[N];
	real y[N]; 
	real r;
	vector[Y] area[sum(S)];
	
}
transformed data {

	vector[Y] year;
	vector[Y] relative_area[sum(S)];
	
	vector[sum(S)] loc_prior;
	
	for(i in 1:Y)
		year[i] = i - 1;
	
	{
		int k = 0;
		for(i in 1:C) {
			for (j in 1:S[i]) {
				k += 1;
				relative_area[k] = area[k] / area[k][1];
			}
		}
	}
	
	{
		// lognormal intercept
		int n[sum(S)];
		
		for (i in 1:sum(S)) {
			n[i] = 0;
		}
		
		loc_prior = rep_vector(0.0, sum(S));
		for (i in 1:N) {
			if (y[i] > 0) {
				n[XS[i]] += 1;
				loc_prior[XS[i]] += log(y[i]);
			}
		}
		
		for (i in 1:sum(S)) {
			if (n[i] > 0) {
				loc_prior[i] /= n[i];
			}
		}
	}
}
parameters {

	real x0_log[sum(S)];
	real mu[C];
	real alpha[C];
	real alpha_re[sum(S)];
	real<lower=0> sigma[2];
	real<lower=0, upper=1> theta[C];	
	real<lower=0, upper=1> rho;	
}
transformed parameters {
	
	// linear predictors
	vector[Y] xt_log[sum(S)];
	vector[Y] lambda[sum(S)];
	
	{
		int k = 0;
		for(i in 1:C) {
			for (j in 1:S[i]) {
				k += 1;
				for (l in 1:Y) {
					// define model for lambda
					lambda[k, l] = mu[i] + (alpha[i] + alpha_re[k]) * year[l];
				}
			}
		}
	}
	
	{ 
		int k = 0;
		for (i in 1:C) {
			for (j in 1:S[i]) {
				k += 1;
				xt_log[k] = x0_log[k] + (r - lambda[k]) .* year - log(relative_area[k]) - square(sigma[1])/2;
			}
		}
	}	
	
	
}
model {
		
	// likelihood
	for (i in 1:N) {
	
		(y[i] > 0) ~ bernoulli(theta[XC[i]]);
		
		if (y[i] > 0) {
			y[i] ~ lognormal(xt_log[XS[i]][XY[i]], sigma[1]);
		}
	}
	
	// augmented density intercept prior
	x0_log ~ normal(loc_prior, 1);
	
	// mortality rate parameters
	alpha    ~ normal(0, sigma[2]);
	alpha_re ~ normal(0, sigma[2] * rho);
	
	// error terms
	rho   ~ beta(1, 1);
	sigma ~ cauchy(0, 1);

}
generated quantities {

	vector[Y] numbers_zone[sum(S)];
	vector[Y] numbers[C];
	vector[Y] numbers_region;
	vector[Y] mortality[C];
	vector[Y] mortality_region;
	real      decline[C];
	real      decline_region;
	
	real median_numbers_sum = 0.0;
	
	int k;
	
	for (i in 1:C) {
		mortality[i] = mu[i] + alpha[i] * year;
	}
	
	k = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
		
			k += 1;
			numbers_zone[k] = theta[i] * exp(xt_log[k]) .* area[k];
		}
	}
	
	for (i in 1:C) {
		for (j in 1:Y) {
			numbers[i, j] = 0.0;
		}
		decline[i] = 0.0;
	}
	
	for (i in 1:Y) {
		numbers_region[i] = 0.0;
		mortality_region[i] = 0.0;
	}
	decline_region = 0.0;
	
	k = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
			k += 1;
			for (l in 1:Y) {
				numbers[i, l] += numbers_zone[k, l];
			}
		}
		decline[i] = numbers[i, Y] / numbers[i, 1];
	}	
	
	for (i in 1:C) {
		for (j in 1:Y) {
		
			numbers_region[j] += numbers[i, j];
			mortality_region[j] += mortality[i, j] * numbers[i, j];
		}
		decline_region += decline[i] * median(numbers[i]);
	}
	
	for (i in 1:C) {
		median_numbers_sum += median(numbers[i]);
	}
	
	for (i in 1:Y) {
		mortality_region[i] /= numbers_region[i];
	}
	 
	decline_region /= median_numbers_sum; 
}

