# EPIC's CEJST Evaluation Script 

# About
Between January 2023 and March 2023, in partnership with WE ACT and The Center for Neighborhood Technology, EPIC produced a series of analyses on the White House's Climate and Economic Justice Screening Tool - [CEJST.](https://screeningtool.geoplatform.gov/en/#8/0/0)
The results were presented in the blog [CEJST is a simple map, with big implications and attention to cumulative burdens matters](https://www.policyinnovation.org/blog/cejst-simple-map-big-implications) and a [webinar](https://www.youtube.com/watch?v=iVJoK32waCw).

# Disclaimer 
EPIC makes no assurance to the accuracy of analysis or outputs found in this repository and is shared 'As is'. Use is limited to non-commercial and with proper attribution. 

# Details 
The script [cejest-eval-script-public.R contains](https://github.com/Environmental-Policy-Innovation-Center/cejest-eval/blob/main/cejest-eval-script-public.R) all code necessary to produce the base visualizations, and many of the facts generated for the blog. The script pulls CEJST data (downloaded January 2022) from EPIC's AWS server.
The analysis was conducted using R version 4.2.2 - Innocent and Trusting

# Key Outputs 
* Visualizations are stored in [results/plots](https://github.com/Environmental-Policy-Innovation-Center/cejest-eval/tree/main/results/plots). 
* Dataset with demographics by threshold exceedance is stored as [results/Thresholds_PopCharacteristics_v1.csv](https://github.com/Environmental-Policy-Innovation-Center/cejest-eval/blob/main/results/Thresholds_PopCharacteristics_v1.csv)
* Dataset with state summaries of Disadvantaged Communities is stored as [results/state-dac-summary_v1.csv](https://github.com/Environmental-Policy-Innovation-Center/cejest-eval/blob/main/results/state-dac-summary_v1.csv)

# Contact and Acknowledgements 
EPIC would like to thank Bob Dean of CNT and Manuel Salgado of WE ACT for their collaboration. EPIC also appreciates the work of the White House's Council for Environmental Quality (CEQ) and the US Digital Services (USDS) for their work on CEJST.
For questions, please contact Gabriel Watson, Manager of Data Science and Applications at EPIC - gabe@policyinnovation.org 
