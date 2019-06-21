#!/bin/sh
Rscript install.R
Rscript Main.R --discretisation=advanced --save_discretised=y --discretised_filename=kddcup.test.discretised --threads=4 --samples=4 --optimal_k_samples=3 --sample_size=1000 --start_from_anomaly=n --min_clusters=4 --max_clusters=8 --input_file=kddcup.test --column_types=kddcup.column_types --results_file=bootstrap_skmeans_svm_anomalies_simple_4.csv --results_file_with_data=bootstrap_skmeans_svm_anomalies_sorted_data_simple_4.csv
Rscript Anomalies.R --roc_curve=roc_curve_4.rds --auc=auc_4.txt --original=kddcup.test --file_with_anomalies=bootstrap_skmeans_svm_anomalies_simple_4.csv

