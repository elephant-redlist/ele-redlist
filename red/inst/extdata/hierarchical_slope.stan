// elephant dynamics by site 

// NOTATION
// DIMENSIONS
// C: number of countries
// N: number of records
// S: number of sites per country
// Y: number of years
// LOOK-UP VECTORS
// XY; XC; XS
// OBSERVATIONAL DATA
// y: observed densities
// COVARIATES
// survey_area: survey area per year
// range_area: assumed range area across years

functions {
	
	int num_nonzero(real[] y) {
		int np = 0;
		for (n in 1:size(y))
			if (y[n] > 0)
				np += 1;
		return np;
	}
	
	real vector_norm(vector x) {
	    
	    real i = 0.0;
	    
	    for (j in 1:num_elements(x))
	        i += pow(x[j], 2.0);
	        
	    return pow(i, 0.5);
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
    int  start;
	
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
		
	real decline;
    real expected_decline;
    real MPE;
	
	real parameter_summary[4];
	real error_summary[2];
	real output_summary[2];
    
    vector[sumS] N0 = rep_vector(0.0, sumS);
    vector[sumS] NT = rep_vector(0.0, sumS);
    
    vector[sumS] decline_site = rep_vector(0.0, sumS);
        
    vector[N] y_sim = rep_vector(0.0, N);
    
 	{
		int k = 0;
        
        real T = Y - start;
        
        // standard error for decline
		real tau_sum = pow(T * (square(tau[1]) + square(tau[2])), 0.5);
				
		real site_weight_sum = 0.0;
		real decline_site_sum = 0.0;
		
		for(i in 1:C) {		
			for (j in 1:S[i]) {
			
				k += 1;
                
				N0[k] = theta[k] * exp(xt_log[k][start] + square(sigma) / 2) * range_area[k];
                NT[k] = theta[k] * exp(xt_log[k][Y]     + square(sigma) / 2) * range_area[k];
				
				site_weight_sum += N0[k];
				
				decline_site[k] = N0[k] > 0 ? 1.0 - NT[k] / N0[k] : 0.0;
				
				decline_site_sum += N0[k] * decline_site[k];
			}			
		}
		decline = decline_site_sum / site_weight_sum;
        expected_decline = 1.0 - exp(alpha0 * T + square(tau_sum) / 2.0);
	}
		
	// posterior prediction of the survey density data
	for (i in 1:N) {
		if (y[i] > 0) {
			y_sim[i] = lognormal_rng(xt_log[XS[i]][XY[i]], sigma);
		}
	}
    
    // MEAN PREDICTION ERROR
    MPE = 0.0;
    for (i in 1:N) {
        
        MPE += y[i] > 0.0 ? fabs(y_sim[i] - y[i]) / y[i] : 0.0;
    }
    MPE /= N;
	
	// SUMMARY STATISTICS FOR TRACE DIAGNOSTICS
	parameter_summary[1] = vector_norm(x0_log);
	parameter_summary[2] = alpha0;
    parameter_summary[3] = vector_norm(alphaC);
	parameter_summary[4] = vector_norm(alphaS);
	
	error_summary[1] = sigma;
	error_summary[2] = vector_norm(to_vector(tau));
	
	output_summary[1] = decline;
    output_summary[2] = expected_decline;
}
