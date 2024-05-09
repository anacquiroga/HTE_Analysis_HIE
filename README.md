# HTE_Analysis_HIE
Heterogeneous treatment effect of health insurance experiment using ML (causal and instrumental forests)

README

This folder contains data files related to the research paper titled “Picture this: Making health insurance choices easier for those who need it”

1. Data Files:

    - data2811.dta: This file contains participant behavior and choices during the experiment, as well as information including demographic variables and responses to various questions and quizzes. Below is a description of the variables contained in this file that were used for the analysis presented in this paper, the way each variable is referred to in the manuscript is written between parenthesis:

•	saw_pinfo (Visualizing personalized information): =1 if participant visualized optional personalized information
•	two_infotypes (Two personalized information options): = 1 if participant had two optional formats of personalized information, graphical and numerical (treat == 4)
•	distance_best_plan (Distance to plan with the lowest expected cost): Distance to plan with lowest expected costs (Expected costs of the current plan - Expected costs of cheapest plan)
•	treat (Treatment): Arm to which experiment participant was randomized (=0 control, =1 general information; =2 general information and optional graphical information; =3 general information and personalized numerical information; =4 general information and optional graphical or numerical information)
•	female (Female): Binary variable indicating whether the participant is female (1) or not (0).
•	age (Age): Participant's age.
•	uni (Uni): Binary variable indicating whether the participant has a university degree (1) or not (0).
•	s_doc (Doctor visits): Self-reported number of doctor visits in the last 12 months.
•	total_ok (Objective HI knowledge): Total score on the health insurance objective knowledge quiz.
•	hilm_beh_avg (HILM-CH: Behavior): Average of behavior using and behavior choosing health insurance domains of the HILM-CH.
•	hilm_conf_avg (HILM-CH: Confidence): Average of confidence using and confidence choosing health insurance domains of the HILM-CH.
•	m_income (Income): Median income category selected by the participant.
•	healthstat (Health status): Reported health status on a scale of 0-5, where 1 is very poor and 5 is very good.
•	ra_gains (Risk averse for gains): Binary variable indicating whether the participant is risk averse in gain lotteries (1) or not (0).
•	rs_loss (Risk seeking for loss): Binary variable indicating whether the participant is risk seeking in loss lotteries (1) or not (0).
•	reflection (Reflection effect): Binary variable indicating whether the participant exhibits the reflection effect (1) or not (0).
•	pw1060 (Probability overweighting 10-60%): Binary variable indicating whether the participant shows probability overweighting between 10-60% (1) or not (0).
•	pw6090 (Probability overweighting 60-90%): Binary variable indicating whether the participant shows probability overweighting between 60-90% (1) or not (0).
•	pw1090 (Probability overweighting 10-90%): Binary variable indicating whether the participant shows probability overweighting between 10-60% and/or between 60-90% (1) or not (0).
•	c_current (Major event in current round): Major event took place on current experiment round.
•	c_previous (Major event in previous round): Major event took place on previous experiment round.
•	minor_event (Minor event): factor variable indicating type of minor event that took place during an experiment round. 0=no minor event, 1=minor event A, 2=minor event B
•	plan (Plan): Health insurance plan chosen by the participant on a given round.
•	deductible (Deductible): Participant's deductible level for own actual health insurance (1-6).
•	s_canton (Canton): Canton of residency.
•	s_study_area (Study area): Participant's study area.
•	s_general_risk (General risk question): 0-10 Question on willingness to take risk going from "Not at all willing to take risks" to "Very willing to take risks"
•	sure (DCS scale): 0-4 Decisional conflict scale score
•	s_swiss (Swiss): =1 if participant is Swiss
•	s_ded (Deductible): Participant’s deductible level for actual own health insurance
•	s_supplementary (Supplementary insurance): Participant's actual supplementary health insurance status.
•	s_switchedhi (Last time switching HI): Last time participant switched actual health insurance plan.
•	total_qs_answered (Total trivia questions answered): Total trivia questions answered during experiment up to a given round)
•	correct_quiz (Correct trivia questions): Correct trivia questions in a given round
•	newbalance (Balance): Number of points accumulated at the end of a given round of the experiment
•	round_qs (Round trivia questions): Trivia questions answered in a given round of the experiment


2. Running the Programs:

•	The data files provided can be used for various analyses as described in the research paper. The data can be loaded into statistical software packages such as R or Stata. This particular analysis was performed using R. 
•	For further information on the analysis and result interpretation, please refer to the the research paper.

For any inquiries or clarifications regarding the data, please contact ana.quirogagutierrez@bfh.ch

