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
	
	int num_nonzero(real[] y) {
		int np = 0;
		for (n in 1:size(y))
			if (y[n] > 0)
				np += 1;
		return np;
	}
	
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
		
			// num_elements(x) is even 
			// return the mean of the middle two numbers
			real i = num_elements(x) / 2.0;
			
			int j[2];
			j[1] = to_integer(i);
			j[2] = j[1] + 1;
			
			y = mean(sort_asc(x)[j]);
		
		} else {
		
			// num_elements(x) is odd 
			// return the middle number
			real i = ceil(num_elements(x) / 2.0);
			int  j = to_integer(i);
			
			y = sort_asc(x)[j];
		}
		
		return y;
	}
	
	real vector_norm(vector x) {
	    
	    real i = 0.0;
	    
	    for (j in 1:num_elements(x))
	        i += pow(x[j], 2.0);
	        
	    return pow(i, 0.5);
	}
	
	real matrix_norm(vector[] x) {
	    
		vector[size(x)] i;
		
	    for (j in 1:size(x))
	        i[j] = vector_norm(x[j]);
	        
	    return sum(i);
	}
}
data {
	
	// dimensions
	int N;
	int Y;
	int C; 
	int S[C];
	
	// look-up vectors
	int XY[N];
	int XC[N];
	int XS[N];
	
	// site level 
	// survey density 
	// data
	real y[N]; 
	
	// covariates
	vector[Y]      survey_area[sum(S)];
	vector[sum(S)] range_area;
	
	// logicals
	int use_relative_survey_area;
	
}
transformed data {

	// number of positive 
	// survey records
	int N_nz = num_nonzero(y);
	
	// number of sites
	int sumS = sum(S);
			
	// positive catch vector
	// and look-up vectors
	real y_nz[N_nz];
	int XY_nz[N_nz];
	int XC_nz[N_nz];
	int XS_nz[N_nz];
	
	// probability of non-zero survey
	// estimate per site
	real theta[sumS];

	vector[Y] year;
	vector[Y] log_relative_survey_area[sumS];
		
	for(i in 1:Y) {
		year[i] = i - 1;
	}
	
	{
		int k = 0;
		for(i in 1:C) {
			for (j in 1:S[i]) {
				k += 1;
				log_relative_survey_area[k] = use_relative_survey_area ? log(survey_area[k] / survey_area[k][1]) : rep_vector(0.0, Y);
			}
		}
	}
	
	// re-define look-up 
	// vectors for non-zero
	// positive survey vector
	// (with zeros stripped out)
	{
		int loc = 1;
		for (i in 1:N) {
			if (y[i] > 0) {
			
				// positive catch record
				y_nz[loc] = y[i];
				
				// look-up vectors
				XY_nz[loc] = XY[i];
				XC_nz[loc] = XC[i];
				XS_nz[loc] = XS[i];
				
				loc += 1;
			}
		}
	}
	
	// calculate ML theta per site
	// (probability of positive survey estimate)
	{
		real bin[sumS];
		int  n[sumS];
		
		for (i in 1:sumS) {
			bin[i] = 0;
			n[i]   = 0;
		}
		
		for (i in 1:N) {
			n[XS[i]] += 1;
			if (y[i] > 0)
				bin[XS[i]] += 1.0;
		}
		
		for (i in 1:sumS) {
			theta[i] = bin[i] / n[i];
		}
	}
	
}
parameters {
	
    // density intercept parameters
	vector[sumS] x0_log;
	
	// mortality parameters
	real         alpha0;
	vector[C]    alphaC;
	vector[sumS] alphaS;
	
	// log-normal observation
	// error
	real<lower=0> sigma;
	
	// random effects error
	real<lower=0> tau[2];
}
transformed parameters {
	
	// linear predictors
	vector[Y] xt_log[sumS];
	vector[Y] lambda[sumS];
	
	vector[sumS] alpha_re;
		
	{
		int k = 0;
		for(i in 1:C) {			
			for (j in 1:S[i]) {
			
				k += 1;
				
				// centred random effects
				alpha_re[k]  = alpha0 - tau[2] * alphaC[i] - tau[1] * alphaS[k];
				
				// define model for growth rate (lambda)
				lambda[k] = alpha_re[k] * year;
				
				// define model for density
				xt_log[k] = x0_log[k] + lambda[k] - log_relative_survey_area[k];
			}
		}
	}
}
model {
		
	// likelihood
	{
		vector[N_nz] mu_log_nz;
		
		for (i in 1:N_nz) {
			mu_log_nz[i] = xt_log[XS_nz[i]][XY_nz[i]];
		}
			
		y_nz ~ lognormal(mu_log_nz, sigma);
	}
	
	// random effects
	alphaC ~ std_normal();
	alphaS ~ std_normal();
	
	// error terms
	sigma ~ std_normal();
	tau   ~ std_normal();
}
generated quantities {
		
	real decline[2];
	
	real parameter_summary[4];
	real error_summary[2];
	real output_summary[1];
	
	vector[N] y_sim = rep_vector(0.0, N);
	
	{
		int k = 0;
		
		real T = Y - 1;
				
		// standard error for decline
		real tau_sum = pow(T * (square(tau[1]) + square(tau[2])), 0.5);
		
		real n0_site;
		real n0_site_sum;
		
		real decline_site;
		real decline_site_sum;
				
		n0_site_sum = 0.0;
		decline_site_sum = 0.0;
		
		for(i in 1:C) {		
			for (j in 1:S[i]) {
			
				k += 1;
				n0_site = theta[k] * exp(xt_log[k][1] + square(sigma) / 2) * range_area[k];
				
				n0_site_sum += n0_site;
				
				decline_site = exp(alpha_re[k] * T);
				
				decline_site_sum += n0_site * decline_site;
			}			
		}
		decline[1] = decline_site_sum / n0_site_sum;
		decline[2] = exp(alpha0 * T + square(tau_sum) / 2.0);
	}
		
	// posterior prediction of the survey density data
	for (i in 1:N) {
		if (y[i] > 0) {
			y_sim[i] = lognormal_rng(xt_log[XS[i]][XY[i]], sigma);
		}
	}
	
	// SUMMARY STATISTICS FOR TRACE DIAGNOSTICS
	parameter_summary[1] = vector_norm(x0_log);
	parameter_summary[2] = alpha0;
    parameter_summary[3] = vector_norm(alphaC);
	parameter_summary[4] = vector_norm(alphaS);
	
	error_summary[1] = sigma;
	error_summary[2] = vector_norm(to_vector(tau));
	
	output_summary[1] = vector_norm(to_vector(decline));
}
