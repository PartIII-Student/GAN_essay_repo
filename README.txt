For the simulations in Section 1.4.2.4 Score Adaptive Learning Rates:

* SALR.py is the main script it will generate data for score adaptive learning rates 
including for psi = 0.
* SALR_control_adam.py and SALR_control_momentum.py generate data on the same
problem using Adam and Momentum respectively.
* All data is saved to the subfolders in the folder SALR_data
* The code displayed in the Appendix is a simplified version of these scripts
to ease exposition, these scripts were used to generate the data
which was analysed



For the simulations in Section 1.5.1 Mode Collapse:

* mode.py is the only script, it generates data for all 4 combinations of objective 
function and minibatches i.e. 	
	minibtaches ; min log(1-D(G(x)))
	no minibtaches ; min log(1-D(G(x)))
	minibtaches ; max log(D(G(x)))
	no minibtaches ; max log(D(G(x)))
* All the data is stored in the subfolders in mode_data folder, note 
objective function 1 corresponds to min log(1-D(G(x)).
* The code displayed in the Appendix is a simplified version of this script
to ease exposition, this script was used to generate the data
which was analysed



Analysis:

* The analysis of the generated data was done in R and uses the scripts SALR_analysis.R 
and Mode_collapse_analysis.R