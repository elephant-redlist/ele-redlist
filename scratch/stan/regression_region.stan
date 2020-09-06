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

	real median(real[] x) {
	
		real y;
		
		if (fmod(size(x), 2.0) == 0.0) {
		
			// size(x) is even return the mean of the middle two numbers
			real i = size(x) / 2.0;
			
			int j[2];
			j[1] = to_integer(i);
			j[2] = j[1] + 1;
			
			y = mean(sort_asc(x)[j]);
		
		} else {
		
			// size(x) is odd return the middle number
			real i = ceil(size(x) / 2.0);
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

	int M;
	vector[Y] year;
	vector[Y] relative_area[sum(S)];
	
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
}
parameters {

	real x0_log[C];
	real x0_re_log[sum(S)];
	real mu[C];
	real alpha[C];
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
		int k = 0;
		for (i in 1:C) {
			for (j in 1:S[i]) {
				k += 1;
				xt_log[k] = x0_log[i] + x0_re_log[k] + (r - lambda[i]) .* year - log(relative_area[k]) - square(sigma[1])/2;
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
	real      numbers_region_median;
	real      decline[C];
	real      decline_region;
	
	int k;
	
	for(i in 1:C) {
		for(j in 1:Y) {
	
			mortality[i, j] = lambda[i, j];
		}
	}
	
	k = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
			k += 1;
			nt[k] = theta[i] * exp(xt_log[k]) .* area[k];
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
	numbers_region_median = 0.0;
	decline_region = 0.0;
	
	k = 0;
	for(i in 1:C) {
		for (j in 1:S[i]) {
			k += 1;
			for (l in 1:Y) {
				numbers[i, l] += nt[k, l];
			}
		}
		decline[i] = numbers[i, Y] / numbers[i, 1];
	}	
	
	for (i in 1:C) {
		for (j in 1:Y) {
		
			numbers_region[j] += numbers[i, j];
			mortality_region[j] += mortality[i, j] * numbers[i, j];
		}
		//numbers_region_median += median(numbers[i])
		decline_region += decline[i];// * median(numbers[i]);
	}
	
	for (i in 1:Y) {
		mortality_region[i] /= numbers_region[i];
	}
	
	//decline_region /= numbers_region_median; 
	decline_region /= C; 
}

