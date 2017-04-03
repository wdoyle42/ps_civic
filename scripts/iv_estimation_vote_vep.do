capture log close
log using geo_educ_civic_vote_vep.log, replace

////////////////////////////////////////////////////////////////////////////////
//
// <FILE> iv_estimation_.do
// <AUTH> Will Doyle and Benjamin Skinner
//
// REPLICATION for:
//
// REPLICATION for:
//
// Doyle, W.R. and Skinner, B.T. (2016). Does Postsecondary Education Result
//      in Civic Benefits? Journal of Higher Education.
//
// PURPOSE:
//
// This file performs the primary analyses found in the paper. It loops through
// all combinations of outcome, instrument, and sample. With each iteration,
// csv file in the format of the paper tables is stored in the ../output
// directory.
//
////////////////////////////////////////////////////////////////////////////////

// clear everything
clear all

// set directories
global datdir "../data/"
global outdir "../output/"

// load data
use ${datdir}geo_vote.dta, clear

// subset to HS/GED or above (PreHS == 1)
drop if highattain == 1

// Create/recode variables (as needed)

replace mothed = . if mothed == 95

capture confirm variable instatecost_2yr_mod
if _rc {
    gen instatecost_2yr_mod = instatecost_2yr / 1000
}

capture confirm variable allfte_2yr_mod
if _rc {
    gen allfte_2yr_mod = allfte_2yr / 1000
}

capture confirm variable neardist_ins_pub_sq
if _rc {
    gen neardist_ins_pub_sq = neardist_ins_pub^2
}

// set locals: years
local yearlist 2004 2006 2008 2010

// set locals: outcome variables
local yvars didvote

// set locals: primary independent variable
local x ccol

// set locals: control variables
local controls asvab black hispanic multiracial female south smsa vep_pct i.quarter

// set locals: instruments (Card instruments first); names of vars of interest
local z1 i.haveschool_4yr##c.mothed
local z1v1 "1.haveschool_4yr"
local z1v2 "1.haveschool_4yr#c.mothed"

local z2 i.haveschool_2yr##c.mothed
local z2v1 "1.haveschool_2yr"
local z2v2 "1.haveschool_2yr#c.mothed"

local z3 c.isild_2yr##c.mothed
local z3v1 "isild_2yr"
local z3v2 "c.isild_2yr#c.mothed"

local z4 c.instatecost_2yr_mod##c.mothed
local z4v1 "instatecost_2yr_mod"
local z4v2 "c.instatecost_2yr_mod#c.mothed"

local z5 c.allfte_2yr_mod##c.mothed
local z5v1 "allfte_2yr_mod"
local z5v2 "c.allfte_2yr_mod#c.mothed"

local z6 c.allild_allinst##c.mothed
local z6v1 "allild_allinst"
local z6v2 "c.allild_allinst#c.mothed"

local z7 c.neardist_ins_pub##c.mothed neardist_ins_pub_sq c.neardist_ins_pub_sq#c.mothed
local z7v1 "neardist_ins_pub"
local z7v2 "c.neardist_ins_pub#c.mothed"

local zvars `z1' `z2' `z3' `z4' `z5' `z6' `z7'

// summarize variables
sum `yvars'
sum `x'
sum `controls'
sum `zvars'

// store length of locals for out matrices
local yearlength: word count `yearlist'
local zlength: word count `zvars'
local zlength = `zlength' - 2

//////////////////////////////////////////////////////////////////////
// PRODUCE ESTIMATES
//////////////////////////////////////////////////////////////////////

// loop through each outcome variable
foreach y of local yvars {

    // init matrix for first stage (unique matrix for each outcome)
    // rows = number of IVs * (betas, s.e., endogpv, overidpv, Fstat, MinEig, N)
    // cols = years
    local cell = 2 + 2 + 5
    local rows = `zlength' * `cell'
    local cols = `yearlength'
    matrix fsmat = J(`rows', `cols', .)

    // init matrix for second stage
    local rows = `zlength' * 2 + 2
    local cols = `yearlength' * 2
    matrix ssmat = J(`rows', `cols', .)

    // init counter for matrix rows
    local i = 1

    // NB: to use parallel local lists, need nested locals and forvalues loop
    forvalues zz = 1/`zlength' {

        // adjust counter so it conforms to matrix as we loop
        local fsi = `i' * `cell' - `cell' + 1
        local ssi = `i' * 2 - 1 + 2

        // init counter for matrix columns
        local j = 1

        // loop through each year
        foreach year of local yearlist {

            // push second stage column counter forward
            local ssj = `j' * 2 - 1

            // subsetting data for each year so preserve
            preserve
            keep if year == `year'

            // Reduced Form
            di ""
            di "------------------------------------------------------------"
            di "Reduced form"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            reg `y' `x' `controls' `z`zz''

            // get estimation sample; check; get sample size
            gen cw = e(sample)

            // OLS
            di "------------------------------------------------------------"
            di "OLS"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            reg `y' `x' `controls' `z`zz'' if cw == 1

            // store N
            local N = e(N)

            // store beta and s.e. of ccol for second stage table
            local ssbetaols = _b[ccol]
            local ssbseols = _se[ccol]

            // OLS First Stage
            di ""
            di "------------------------------------------------------------"
            di "OLS First-stage"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            reg `x' `controls' `z`zz'' if cw == 1

            // get beta and beta s.e. for excluded instruments
            local fsbeta1 = _b[`z`zz'v1']
            local fsbse1 = _se[`z`zz'v1']
            local fsbeta2 = _b[`z`zz'v2']
            local fsbse2 = _se[`z`zz'v2']

            // IV estimates using 2SLS
            di ""
            di "------------------------------------------------------------"
            di "2SLS"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            ivregress 2sls `y' `controls' (`x' = `z`zz'') if cw == 1, robust first

            // get beta and beta s.e. for endogenous regressor (ccol)
            local ssbetaiv = _b[ccol]
            local ssbseiv = _se[ccol]

            //  Test of endogneity
            di ""
            di "------------------------------------------------------------"
            di "Test of endogeneity"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            estat endogenous ccol

            // store p-value of Robust regression F
            local endogFpv = r(p_regF)

            // Test of overidentifying restrictions
            di ""
            di "------------------------------------------------------------"
            di "Test of overidentification restrictions"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            estat overid

            // store p-value of score
            local overidpv = r(p_score)

            // Test of first stage instrument strength
            di ""
            di "------------------------------------------------------------"
            di "Test of strength of first stage instruments"
            di "for `y' `year' with instruments `z`zz''"
            di "------------------------------------------------------------"
            di ""
            estat first, forcenonrobust

            // store f-stat. minimun Eigenvalue and list of minimum bias values
            mat singleres = r(singleresults)
            local Fstat = singleres[1,4]
            local minEig = r(mineig)

            // store values in matrices
            di ""
            di "------------------------------------------------------------"
            di "Store values in matrices"
            di "------------------------------------------------------------"
            di ""

            // first stage: betas and s.e.s of instruments
            matrix fsmat[`fsi',`j']      = `fsbeta1'
            matrix fsmat[`fsi' + 1, `j'] = `fsbse1'
            matrix fsmat[`fsi' + 2, `j'] = `fsbeta2'
            matrix fsmat[`fsi' + 3, `j'] = `fsbse2'

            // first stage: endogeneity p-value
            matrix fsmat[`fsi' + 4, `j'] = `endogFpv'

            // first stage: overidentification p-value
            matrix fsmat[`fsi' + 5, `j'] = `overidpv'

            // first stage: first stage F-stat and minimum Eigenvalue
            matrix fsmat[`fsi' + 6, `j'] = `Fstat'
            matrix fsmat[`fsi' + 7, `j'] = `minEig'

            // first stage: observations
            matrix fsmat[`fsi' + 8, `j'] = `N'

            // second stage: betas/se OLS, betas/se IV
            matrix ssmat[1, `ssj'] = `ssbetaols'
            matrix ssmat[2, `ssj'] = `ssbseols'
            matrix ssmat[`ssi', `ssj' + 1]     = `ssbetaiv'
            matrix ssmat[`ssi' + 1, `ssj' + 1] = `ssbseiv'

            // print matrix for progress
            matrix list fsmat
            matrix list ssmat

            // drop cw so can be used in next loop
            drop cw

            // restore data before next subset (R is still way better)
            restore

            // add to column counter
            local j = `j' + 1

        } // end year loop

    // add to row counter
    local i = `i' + 1

    } //  end z loop

    // save matrices
    matrix `y'_fsmat = fsmat
    matrix `y'_ssmat = ssmat

    // save matrices to output directory
    di ""
    di "===================================================================="
    di "Outsheet matrices to output directory"
    di "===================================================================="
    di ""

    preserve

    drop _all
    svmat `y'_fsmat
    outsheet using ${outdir}`y'_vep_fsmat.csv, c replace

    drop _all
    svmat `y'_ssmat
    outsheet using ${outdir}`y'_vep_ssmat.csv, c replace

    restore

} //  end y loop

capture log close
exit

////////////////////////////////////////////////////////////////////////////////
// END SCRIPT
////////////////////////////////////////////////////////////////////////////////
