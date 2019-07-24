
******************************************************************************************************;
* COMMON DEFINITIONS FOR MODELING
******************************************************************************************************;
%inc '/mktg/cppma/data10/fair_price_fy16_prog/FPM_Init.sas';

*options mlogic mprint;

*Input dataset: Profile;
%let integrated_profile_ds     = fpm05.integrated_profile2;
/*%let integrated_profile_ds     = fpm05.integrated_profile2_test;*/

*Output dataset: Data Prep;
%let segment_model_output_lib = wrk02;
%let segment_model_output_dsn = model;
%let segment_model_ds         = &segment_model_output_lib..&segment_model_output_dsn.;
%macro get_segment_model_dsname (cluster, model); &segment_model_ds._&cluster._&model. %mend;

*REGIONS;
%let region_list = &profile_regions.;

*PRODUCT CODES FOR PREDICTOR VARIABLES;
%let pred_prod_global_level   =	G_GC_ON  G_GC_DF  G_GC_MW  G_GC_RET G_GC_OTH G_HD_FHD G_SP_SPT
								F_PRTY   F_ECON
								D_HIOB   
								RESI
								;
%let pred_prod_region_level   =	E_CP_ONL E_CP_ONB E_CP_DF  E_CP_PRL E_CP_PRB E_CP_EC 
								E_CF_ONF E_CF_DFF E_CF_FGT
								E_XP_PRL E_XP_PRB E_XP_EC
								E_XF_FGT
								E_OTH    E_CN     E_UK
								;
%let pred_prod_fxf_level      =	F_PRTY   F_ECON
								;
%let pred_prod_fxf_level2     =	PRTY   ECON
								; *TODO LATER: STANDARIZE DATASET TO INCLUDE F;

%let model_opco_list= E G F;

*SEGMENT VARIABLE;
%let cluster_var = clus26; **cluster_nbr; *clus19;

*TARGET VARIABLES (FOR BOTH FXS AND FXF);
%let target_variables = %metric_list(     ypp_nf, P1, &target_prod_global_level.)
						%metric_list(     ypp_nf, P1, &target_prod_region_level., &region_list.)
						%metric_list(FXF, ypp_nf, P1, &target_prod_fxf_level.)
						;

/*%let target_prod_global_level = T E G G_GC G_HD G_SP;*/
/*%let target_prod_region_level =   E E_CF E_CP E_XF E_XP;*/
/*%let target_prod_fxf_level          = F;*/


*PREDICTOR VARIABLES FOR FXS;
*Note: LOH_list class_list wgt_list Surcharge_list defined in fpm_init file;
%let size_predictors =	%metric_list(     adv wgt tcost vcost, P1,  &pred_prod_global_level.)
						%metric_list(     adv wgt tcost vcost, P1,  &pred_prod_region_level., &region_list.)
						%metric_list(FXF, adv wgt tcost vcost, P1,  &pred_prod_fxf_level.)
						%metric_list(FXF, vol wgt,             P1,   TF)
						%query_vars(&integrated_profile_ds., ^lane_vol, type=N, not_pattern1=_prop_, not_pattern2=_E$, not_pattern3=_G$, not_pattern4=_S$)
						%query_var_range (&integrated_profile_ds., vol_d_DECV, vol_g_DGDS, ^vol_, type=N)
				;

%let ratio_predictors =	%metric_list(     wpp tcpp vcpp, P1, &target_prod_global_level. &pred_prod_global_level.)
						%metric_list(     wpp tcpp vcpp, P1, &target_prod_region_level. &pred_prod_region_level., &region_list.)
						%metric_list(FXF, wpp tcpl vcpl, P1, &target_prod_fxf_level.    &pred_prod_fxf_level.)
						;
%let dist_predictors =	%metric_list(dist, P1, &target_prod_global_level. &pred_prod_global_level.)
						%metric_list(dist, P1, &target_prod_region_level. &pred_prod_region_level., &region_list.)
						;

%let prop_predictors =	%metric_list(     vol_prop wgt_prop, P1, &pred_prod_global_level.)
						%metric_list(     vol_prop wgt_prop, P1, &pred_prod_region_level., &region_list.)
						%metric_list(FXF, vol_prop wgt_prop, P1, &pred_prod_fxf_level.)
						%query_vars(&integrated_profile_ds., ^lane_vol_prop_, type=N, not_pattern1=_S$, not_pattern2=_E$, not_pattern3=_G$)
						%query_vars(&integrated_profile_ds., ^ind_prop_)
/*						%query_vars(&integrated_profile_ds., ^zn_prop_)*/

						;
%put 	&prop_predictors	;

%let lf_predictors =	%query_vars(&integrated_profile_ds., ^tot_lf, type=N)
						;

*PREDICTOR VARIABLES FOR FXF;
%let FXF_size_predictors =		adv_P1_E adv_P1_G
								%metric_list(FXF, adv wgt tcost vcost, P1, F &pred_prod_fxf_level.)
								%metric_list(FXF, vol wgt,             P1 ,  TF)
								;
%let FXF_ratio_predictors =		%metric_list(FXF, wpp tcpl vcpl, P1, &target_prod_fxf_level.    &pred_prod_fxf_level.)
                                ;
%let FXF_dist_predictors = 		%metric_list(FXF, milew, P1, &target_prod_fxf_level.    &pred_prod_fxf_level.)
                        		;
%let FXF_cls_predictors =		%metric_list(FXF, clsw, P1, &target_prod_fxf_level.    &pred_prod_fxf_level.)
                       			;
%let FXF_prop_predictors =		%metric_list(FXF, vol_prop wgt_prop, P1, &pred_prod_fxf_level.)
								%metric_list(FXF, vol_prop wgt_prop, P1, &FXF_region_list.)
								%metric_list(FXF, vol_prop wgt_prop, P1, &pred_prod_fxf_level2., &fxf_delday_list. )
								%metric_list(FXF, vol_prop wgt_prop, P1, &pred_prod_fxf_level2., &fxf_head_haul_list. )
								%metric_list(FXF, VP WP, &Surcharge_list.,Y)
								%metric_list(FXF, VP WP, &LOH_list. &class_list. &wgt_list. )
								FXF_rev_prop_use_3pl_Y 
								rev_prop_P1_E rev_prop_P1_F rev_prop_P1_G
								fxf_lfc_stat_cd_g fxf_lfc_stat_cd_s
								;
								*%query_vars(&integrated_profile_ds., ^ind_prop_)
								;


		/*TODO: ADD cap_adv_lgwth cap_wgt_lgwth */
		/*TODO: ADD MIXES TO PROPORTIONS*/

*CUSTOMER VARIABLES TO PRINT (FOR BOTH FXS AND FXF);
%let additional_vars =	level_nm
						excp_type_cd
						cntry_cd
						Prim_Seg_CD
						%metric_list(rev, P1, T &model_opco_list)
						; 

*Apply transformations;
%let target_variables_log     = %metric_list(log, &target_variables.);
%let size_predictors_log      = %metric_list(log, &size_predictors.);
%let ratio_predictors_log     = %metric_list(log, &ratio_predictors.);
%let fxf_size_predictors_log  = %metric_list(log, &fxf_size_predictors.);
%let fxf_ratio_predictors_log = %metric_list(log, &fxf_ratio_predictors.);

*Clean variable lists;
%let target_variables     =	%query_vars_exists(&integrated_profile_ds., &target_variables.);
%let target_variables_log =	%query_vars_exists(&integrated_profile_ds., &target_variables_log.);
%let size_predictors      =	%query_vars_exists(&integrated_profile_ds., &size_predictors.);
%let size_predictors_log  =	%query_vars_exists(&integrated_profile_ds., &size_predictors_log.);
%let ratio_predictors     =	%query_vars_exists(&integrated_profile_ds., &ratio_predictors.);
%let ratio_predictors_log =	%query_vars_exists(&integrated_profile_ds., &ratio_predictors_log.);
%let dist_predictors      =	%query_vars_exists(&integrated_profile_ds., &dist_predictors.);
%let prop_predictors      =	%query_vars_exists(&integrated_profile_ds., &prop_predictors.);
%let lf_predictors        =	%query_vars_exists(&integrated_profile_ds., &lf_predictors.);

%let fxf_size_predictors      =  %query_vars_exists(&integrated_profile_ds., &fxf_size_predictors.);
%let fxf_size_predictors_log  =  %query_vars_exists(&integrated_profile_ds., &fxf_size_predictors_log.);
%let fxf_ratio_predictors     =  %query_vars_exists(&integrated_profile_ds., &fxf_ratio_predictors.);
%let fxf_ratio_predictors_log =  %query_vars_exists(&integrated_profile_ds., &fxf_ratio_predictors_log.);
%let fxf_dist_predictors      =  %query_vars_exists(&integrated_profile_ds., &fxf_dist_predictors.);
%let fxf_prop_predictors      =  %query_vars_exists(&integrated_profile_ds., &fxf_prop_predictors.);
%let FXF_cls_predictors       =  %query_vars_exists(&integrated_profile_ds., &FXF_cls_predictors.);


*MODELING PARAMETERS;
%let model_min_sample_size =	500; 
%let model_training_pct    =    0.80;
%let model_validation_pct  =    0.10;
%let model_list            =	&target_prod_global_level.
								%metric_list(&target_prod_region_level., &region_list.)
								F
								;

*MODEL FINE TUNING PARAMETERS;
%let max_iterations_reg     = 10;
%let max_infuential_checked = 10;
%let COOKD_ratio_cutoff     = 1.5;  *2;
%let count_test_influential_obs=10;

*PREDICTION FACTOR EXTRACTION;
%let num_pls_factors = 80;

*PREDICTION FACTOR SELECTION;
%let model_stop_criteria1 = 150; *NONE;
%let model_stop_criteria2 = 145; *NONE;
%let model_stop_criteria3 = 140; *NONE;
%let min_test_df          = 30;

*Check variable lists;
%macro check_vars ();
%log(check_vars);
%put model_list                = {&model_list.};           %put;
%put target_variables          = {&target_variables.};     %put;
%put target_variables_log      = {&target_variables_log.}; %put;
%put;
%put size_predictors           = {&size_predictors.};      %put;
%put size_predictors_log       = {&size_predictors_log.};  %put;
%put ratio_predictors          = {&ratio_predictors.};     %put;
%put ratio_predictors_log      = {&ratio_predictors_log.}; %put;
%put dist_predictors           = {&dist_predictors.};      %put;
%put prop_predictors           = {&prop_predictors.};      %put;
%put lf_predictors             = {&lf_predictors.};        %put;
%put;
%put fxf_size_predictors        = {&fxf_size_predictors.};      %put;
%put fxf_size_predictors_log    = {&fxf_size_predictors_log.};   %put;
%put fxf_ratio_predictors       = {&fxf_ratio_predictors.};     %put;
%put fxf_ratio_predictors_log   = {&fxf_ratio_predictors_log.}; %put;
%put fxf_dist_predictors        = {&fxf_dist_predictors.};      %put;
%put fxf_prop_predictors        = {&fxf_prop_predictors.};      %put;
%put FXF_cls_predictors         = {&FXF_cls_predictors.};       %put;
%put;
%put model_min_sample_size  = {&model_min_sample_size.};  %put;
%put model_training_pct     = {&model_training_pct.};     %put;
%put model_validation_pct   = {&model_validation_pct.};   %put;
%put max_iterations_reg     = {&max_iterations_reg.};     %put;
%put max_infuential_checked = {&max_infuential_checked.}; %put;
%put COOKD_ratio_cutoff     = {&COOKD_ratio_cutoff.};     %put;
%put num_pls_factors        = {&num_pls_factors.};        %put;
%put model_stop_criteria1   = {&model_stop_criteria1.};   %put;
%put model_stop_criteria2   = {&model_stop_criteria2.};   %put;
%put model_stop_criteria3   = {&model_stop_criteria3.};   %put;
%put min_test_df            = {&min_test_df.};            %put;
%put;
%log(check_vars end);
%mend check_vars;
%check_vars;


******************************************************************************************************;
* TARGET VARIABLE DATA EXPLORATION
******************************************************************************************************;

*Return the target variable name for each model;
*Plot targets;
%macro plot_segment_targets (input_ds, model_list, cluster);
	%local ix vx vy;

	%let ix = 2;
	%let vy = %scan(&model_list., 1,    %str( ));
	%let vx = %scan(&model_list., &ix., %str( ));
	%do %while(%length(&vx.));

		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc sgplot data=&input_ds.;
			scatter y = %get_target_variable(&vy.) 
					x = %get_target_variable(&vx.) 
					/	group        = excl_flg_&vx.  
						MARKERATTRS  = (SYMBOL=circle) 
						TRANSPARENCY = 0.3;
			title "Segment &cluster.: Correlation between Target Variables [&vx.] & [&vy.]";
			where (excl_flg_&vy. = '');
		run;
		ods graphics off;

		%let ix = %eval(&ix. + 1);
		%let vx = %scan(&model_list., &ix., %str( ));
	%end;
%mend plot_segment_targets;


******************************************************************************************************;
* MACROS AT THE SEGMENT LEVEL
******************************************************************************************************;

%macro segment_driver (cluster);

	*Get the records for this segment;
	*Get the attributes for modeling;
	%log(segment_driver: &cluster_var.=&cluster.: start);
	data work.segment_data;
		set &integrated_profile_ds.;
		where (&cluster_var. = &cluster.);
		keep	&additional_vars.
				&profile_roll_up_var.
				&cluster_var.

				&target_variables. &target_variables_log.

				&size_predictors.  &size_predictors_log.
				&ratio_predictors. &ratio_predictors_log.
				&dist_predictors.
				&prop_predictors.
				&lf_predictors.

				&fxf_size_predictors.  &fxf_size_predictors_log.
				&fxf_ratio_predictors. &fxf_ratio_predictors_log.
				&fxf_dist_predictors.    
				&fxf_prop_predictors.     				
				&FXF_cls_predictors.

				adv_P1_:
				PRIM_SEG_CD
				excl_flg:
				hard_excl_rc:
				soft_excl_rc:
				;
	run;

	*START MANUAL FIX -----------------------------------;
	*TODO: REMOVE THIS MANUAL FIX IN THE NEXT VERSION!!!!;
	data	work.segment_data 
			work.segment_data_deleted
			;
		set work.segment_data;

		*Exception-type exclusions;
		if (	excp_type_cd in (&excluded_excp_type_cd.) or
				&profile_roll_up_var. in (&exclude_modeling_geni.)
			)
		then 
			output work.segment_data_deleted;
		else
			output work.segment_data;
	run;

	proc print data=work.segment_data_deleted (obs=100) noobs;
		title "MANUAL FIX (segment_data_deleted, max=100)";
		var &additional_vars.;
	run;
	*END MANUAL FIX -----------------------------------;


	*Missing value imputations at segment level (so imputed values are more appropiate);
	%log(segment_driver: &cluster_var.=&cluster.: imputations);
	%impute_missing_with_stat(
			work.segment_data, 
			&dist_predictors. &lf_predictors. &FXF_cls_predictors., 

			median
			);


	*Count customers without exclusions;
	%log(segment_driver: &cluster_var.=&cluster.: count customers without exclusions);

	%let excl_flg_list  = %query_vars_exists(work.segment_data, %metric_list(excl_flg,     &model_list.));
	%let hard_excl_list = %query_vars_exists(work.segment_data, %metric_list(hard_excl_rc, &model_list.));
	%let soft_excl_list = %query_vars_exists(work.segment_data, %metric_list(soft_excl_rc, &model_list.));

	%put excl_flg_list  = {&excl_flg_list.};
	%put hard_excl_list = {&hard_excl_list.};
	%put soft_excl_list = {&soft_excl_list.};

	proc sql noprint;
	create table work.segment_counts1 as
	(	select &cluster_var., %apply_macro(missingcounts, &hard_excl_list., sep=COMMA)
		from work.segment_data
		group by &cluster_var.
	);
	quit;

	proc sql noprint;
	create table work.segment_counts2 as
	(	select  &cluster_var., %apply_macro(missingcounts, &soft_excl_list., sep=COMMA)
		from work.segment_data
		group by &cluster_var.
	);
	quit;

	proc print data=work.segment_counts1 noobs; title "Segment &cluster.: Model Counts after Hard Exclusions";        run;
	proc print data=work.segment_counts2 noobs; title "Segment &cluster.: Model Counts after Hard & Soft Exclusions"; run;


	*Find models with good sample sizes;
	%log(segment_driver: &cluster_var.=&cluster.: find models with good sample sizes);

	proc transpose 
			data=work.segment_counts2 
			out=work.segment_counts2_tx (rename=(col1=count _NAME_=model))
			;
		var soft_: ;
	run;

	data work.segment_counts2_tx; 
		set work.segment_counts2_tx; 
		where (count >= &model_min_sample_size.);
		label model = "Model Code";
		label count = "Customers Without Exclusions";
		model = tranwrd(strip(model), 'soft_excl_rc_', ''); 
	run;

	%let model_list_passed  = ;
	%let model_list_counts  = ;
	proc sql;
	title  "Segment &cluster.: Models with Enough Sample Size";
	title2 "(&model_min_sample_size.+ customers)";
	select model, count
	into :model_list_passed separated by ' ', :model_list_counts separated by ' '
	from work.segment_counts2_tx;
	quit;
	%put model_list_passed = {&model_list_passed.};
	%put model_list_counts = {&model_list_counts.};

	%delete_ds(work.segment_counts1 work.segment_counts2 work.segment_counts2_tx);


	*Get target vars for the trimmed list of models;
	%let target_variables_passed = %apply_macro(get_target_variable, &model_list_passed., sep=);
	%put target_variables_passed = {&target_variables_passed.};

	%if %length(&target_variables_passed.) %then %do;

		*Check target vars for the segment;
		%log(segment_driver: &cluster_var.=&cluster.: check target vars for the segment);
		proc corr  data=work.segment_data outp=corr0 noprint; var &target_variables_passed.; run;
		data corr0; set corr0; where (_TYPE_='CORR'); drop _TYPE_; run;
		proc print data=corr0 noobs;  title "Segment &cluster.: Correlation between Target Variables"; run;
		%plot_segment_targets(work.segment_data, &model_list_passed., &cluster.);
		%delete_ds(corr0);

		*Execute modeling;
		%log(segment_driver: &cluster_var.=&cluster.: start modeling);
		%segment_model_driver( &model_list_passed. );

	%end;

	*Cleanup;
	%log(segment_driver: &cluster_var.=&cluster.: end);
	%delete_ds(work.segment_data);

%mend segment_driver;

******************************************************************************************************;
* MACROS AT THE SEGMENT-MODEL LEVEL
******************************************************************************************************;

%macro segment_model_driver (model_list);
	%local ix vx;
	%let ix = 1;
	%let vx = %scan(&model_list., &ix., %str( ));
	%do %while(%length(&vx.));

		%segment_model_process_detail (&vx.);

		%let ix = %eval(&ix. + 1);
		%let vx = %scan(&model_list., &ix., %str( ));
	%end;
%mend segment_model_driver;


%macro segment_model_process_detail (model);

	*Input vars;
	%log(segment_model_process_detail: segment &cluster. model &model. start);
	%script_switch (&script_name., MOD_&cluster._&model._BEGIN, &directory_output.);

	*Set model variables;
	%local target_var excl_flg_var;
	%let target_var   = %get_target_variable(&model.);
	%let excl_flg_var = excl_flg_&model.;
	%put target_var   = {&target_var.};
	%put excl_flg_var = {&excl_flg_var.};

	*Generate model dataset;
	%local output_ds;
	%let output_ds = %get_segment_model_dsname(&cluster., &model.);
	%put output_ds = {&output_ds.};
	%segment_model_generate_data (
		work.segment_data, 
		&output_ds.
		);

	*Set training target (leave missing for validation & test sets so they dont inform model);
	data &output_ds.;
		set &output_ds.;
		label model_target = 'Training Target';
		if model_status = 'M' then model_target=&target_var.; else model_target=.;
	run;

	proc means data=&output_ds. n nmiss min mean max  nolabels;
		title "Model &model. Segment &cluster.: Target Variable check....";
		class model_status;
		var &target_var. model_target;
	run;


	*Set predictors (use all size and proportion predictors, but only the relevant ratios and distaces);
	%if &model=F %then %do;
		%let predictor_vars =   &FXF_size_predictors_log.
								&fxf_ratio_predictors_log.
								&fxf_dist_predictors.
								&fxf_prop_predictors.
								&FXF_cls_predictors.
								;
	%end; %else %do;
		*%let predictor_vars =	&size_predictors_log.
								%metric_list(log, wpp tcpp vcpp, P1, &model.)
								%metric_list(dist, P1, &model.)
								&prop_predictors.
								&lf_predictors.
								;
		%let predictor_vars =	&size_predictors_log.
								&ratio_predictors_log.
								&dist_predictors.
								&prop_predictors.
								&lf_predictors.
								;
	%end;
	%let predictor_vars = %query_vars_exists(&output_ds., &predictor_vars.);
	%let predictor_num  = %count_elements(&predictor_vars.);
	%put predictor_vars (BEFORE TRIM) = {&predictor_vars.};
	%put predictor_num  (BEFORE TRIM) = {&predictor_num.};

	*Eliminate uninformative or fully-missing predictors;
	%log(segment_model_process_detail: segment &cluster. model &model. trim predictors);
	%stats_ds(&output_ds., &predictor_vars., out=tmp_stats_minmax, metrics=n nmiss min p1 p5 q1 mean q3 p95 p99 max sum std, print=N);
	data tmp_stats_minmax; 
		set tmp_stats_minmax;
		length var_status $ 32;
		if      (n = 0)             then var_status = 'Missing (all)';
		else if (nmiss > 0)         then var_status = 'Missing (partial)';
		else if (min = max)         then var_status = 'No Info';
		else if (min=p95 or p5=max) then var_status = 'Min Info';
		else                             var_status = 'OK';
	run;

	proc sort  data=tmp_stats_minmax; by _VARIABLE_; run;
	proc print data=tmp_stats_minmax noobs; title  "Fully-Missing Predictors ==> DROPPED";      where (var_status = 'Missing (all)');     run;
	proc print data=tmp_stats_minmax noobs; title  "Unimputed Missing Predictors ==> DROPPED";  where (var_status = 'Missing (partial)');  run;
	proc print data=tmp_stats_minmax noobs; title  "No-Information Predictors ==> DROPPED";     where (var_status = 'No Info');  run;
	proc print data=tmp_stats_minmax noobs; title  "Minimal-Info Predictors ==> DROPPED";       where (var_status = 'Min Info'); run;
	proc print data=tmp_stats_minmax noobs; title  "Stats for Remaining Predictors";            where (var_status = 'OK');       run;

	data tmp_drop_vars; set tmp_stats_minmax; where (var_status ~= 'OK'); keep _VARIABLE_; rename _VARIABLE_ = _VARNAME_; run;
	%drop_variable_with_dataset (&output_ds., tmp_drop_vars); 
	%delete_ds(tmp_full_vars tmp_drop_vars tmp_stats_minmax);

	%let predictor_vars = %query_vars_exists(&output_ds., &predictor_vars.);
	%let predictor_num  = %count_elements(&predictor_vars.);
	%put predictor_vars (AFTER TRIM) = {&predictor_vars.};
	%put predictor_num  (AFTER TRIM) = {&predictor_num.};


	%if %length(&predictor_vars.) %then %do;
	
		*Rotate predictors;
		%script_switch (&script_name., MOD_&cluster._&model._FACTORS, &directory_output.);
		%segment_model_rotate_predictors();
		%let rotated_predictor_vars = %query_vars(&output_ds._pls_pred, ^factor);
		%let rotated_predictor_num  = %count_elements(&rotated_predictor_vars.);
		%put rotated_predictor_vars = {&rotated_predictor_vars.};
		%put rotated_predictor_num  = {&rotated_predictor_num.};


		%if (&rotated_predictor_num. > 0) %then %do;

			*Check predictors;
			%script_switch (&script_name., MOD_&cluster._&model._FACTOR_CHECK, &directory_output.);
			%segment_model_check_predictors();

			*Run models;
			%segment_model_run_paralell() /*%segment_model_run();*/ 

			*Check models;
			%script_switch (&script_name., MOD_&cluster._&model._MODEL_RESULTS, &directory_output.);
			%segment_model_check_results();

		%end;
	%end;

%mend segment_model_process_detail;


%macro segment_model_generate_data (input_ds, output_ds);

	*Remove the hard exclusions and separate the soft exclusions;	
	data	tmp_OK 
			tmp_SOFTEXCL;
		set &input_ds.;
		if      &excl_flg_var.=''  then output tmp_OK;
		else if &excl_flg_var.='S' then output tmp_SOFTEXCL;
	run;

	*Split the non-exclusion group in three parts (training, validation, testing);
	%sample_target_validation_ds (
		tmp_OK, 
		tmp_vali, 
		tmp_test, 
/*		Prim_Seg_CD, */
,		
		adv_P1_&model. adv_P1_T,
		rate_sample = %sysevalf(1 - &model_training_pct.) , 
		rate_split  = %sysevalf(&model_validation_pct. / (1 - &model_training_pct.)) ,
		seed=43210
		);

	%merge_ds(tmp_OK, tmp_vali (keep=&profile_roll_up_var.), tmp_OK, &profile_roll_up_var., in1=Y, in2_flg=in_vali);
	%merge_ds(tmp_OK, tmp_test (keep=&profile_roll_up_var.), tmp_OK, &profile_roll_up_var., in1=Y, in2_flg=in_test);
	%delete_ds(tmp_vali tmp_test);

	*Bring all groups together and flag them appropiately;
	data &output_ds.;
		set tmp_SOFTEXCL (in=in_soft) tmp_OK (in=in_OK);

		label model_status = "Modeling group"; *(M=Model Train, V=Valid, T=Test, X=Soft Exclusion);
		drop in_vali in_test;
		if      (in_soft)               then model_status = 'X';
/*		else if (in_OK and in_vali='Y') then model_status = 'V';*/
		else if (in_OK and in_vali='Y') then model_status = 'T';

		else if (in_OK and in_test='Y') then model_status = 'T';

		else                                 model_status = 'M';
	run;

	%delete_ds(tmp_OK tmp_SOFTEXCL);

	%freq_ds(&output_ds., model_status, p=Y, desc=DATA SPLIT FOR MODEL &model.);

%mend segment_model_generate_data;	


%macro segment_model_rotate_predictors ();

	%delete_ds(&output_ds._pls_pred);
	%delete_ds(&output_ds._pls_xscr);

	%pls_train (
		&output_ds.,
		&output_ds._pls_pred,
		model_target,
		&predictor_vars.,
		&profile_roll_up_var.,
		out_score      = &output_ds._pls_xscr,
		nfac           = &num_pls_factors.,
		desc           = Segment &cluster.: PLS model for &target_var.
		);

%mend segment_model_rotate_predictors;


%macro segment_model_check_predictors ();

	proc corr data=&output_ds._pls_pred outp=corr0 noprint; var &predictor_vars.;         with &target_var.;     where (model_status in ('M','V','T')); run;
	proc corr data=&output_ds._pls_pred outp=corr1 noprint; var &rotated_predictor_vars.; with &target_var.;     where (model_status in ('M','V','T')); run;	
	proc corr data=&output_ds._pls_pred outp=corr2 noprint; var &rotated_predictor_vars.; with &predictor_vars.; where (model_status in ('M','V','T')); run;

	data corr0; set corr0; where (_TYPE_='CORR'); drop _TYPE_; run;
	data corr1; set corr1; where (_TYPE_='CORR'); drop _TYPE_; run;
	data corr2; set corr2; where (_TYPE_='CORR'); drop _TYPE_; run;

	proc print data=corr0 noobs;  title "Model &model. Segment &cluster.: Target-Predictor Correlations"; title2 "For Train + Valid + Test groups"; run;
	proc print data=corr1 noobs;  title "Model &model. Segment &cluster.: Target-Factor Correlations";    title2 "For Train + Valid + Test groups"; run;
	proc print data=corr2 noobs;  title "Model &model. Segment &cluster.: Predictor-Factor Correlations"; title2 "For Train + Valid + Test groups"; run;

	%delete_ds(corr0 corr1 corr2);
	
	%segment_model_plot_predictors(
			&output_ds._pls_pred, 
			&model., 
			&target_var., 
			&rotated_predictor_vars. &predictor_vars.
			);

%mend segment_model_check_predictors;


%macro segment_model_plot_predictors (input_ds, model, target_var, predictor_var);
	%local ix vx;

	%let ix = 1;
	%let vx = %scan(&predictor_var., &ix., %str( ));
	%do %while(%length(&vx.));
		
		ods graphics on; */ IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc sgplot data=&input_ds.;
			scatter y = &target_var. 
					x = &vx. 
					/	group        = excl_flg_&model.  
						MARKERATTRS  = (SYMBOL=circle) 
						TRANSPARENCY = 0.3;
			title "Model &model. Segment &cluster.: &target_var. vs. &vx.";
		run;
		ods graphics off;

		%let ix = %eval(&ix. + 1);
		%let vx = %scan(&predictor_var., &ix., %str( ));
	%end;
%mend segment_model_plot_predictors;

%macro segment_model_run ();
	%local num_iter num_iter_z2 num_influential cookd_train_cutoff;

	%let num_influential = 0;

	%do num_iter = 1 %to &max_iterations_reg.;
		%let num_iter_z2 = %sysfunc(putn(&num_iter., z2.));

		%log(segment_model_run: segment &cluster_var.=&cluster. model &model.: iteration &num_iter. start);

		*Select rotated predictors;
		%script_switch (&script_name., MOD_&cluster._&model._FACTOR_SELECT_ITER&num_iter_z2., &directory_output.);
		%let select_rotated_pred_vars = ;
		%let select_rotated_pred_num  = ;
		%segment_model_select_factors();
		%put select_rotated_pred_vars = {&select_rotated_pred_vars.};
		%put select_rotated_pred_num  = {&select_rotated_pred_num.};


		
		*RUN THE REGRESSION MODEL;
		%script_switch (&script_name., MOD_&cluster._&model._MODEL_ITER&num_iter_z2., &directory_output.);
		%segment_model_run_iteration();

		*GET COOKSD RESULTS (WORST CUSTOMERS FIRST);
		proc sort 
			data = &output_ds._reg_out (keep=&profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z model_status where=(model_status='M') )
			out  = tmp_influential_obs; 
			by descending COOKD;
		run;

		proc sort 
			data = &output_ds._reg_out (keep=&profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z model_status where=(model_status in ('V','T','X')) )
			out  = tmp_influential_obs2; 
			by descending COOKD;
		run;

		data tmp_influential_obs; 
			set tmp_influential_obs (obs=%eval(&max_infuential_checked.+1)); 
			if (find(influence_flg,'COOKD','it') > 0);
		run;

		data tmp_influential_obs2; 
/*			set tmp_influential_obs2; */
/*			if (find(influence_flg,'COOKD','it') > 0);*/

			set tmp_influential_obs2; 
		   if _n_<=&count_test_influential_obs;
		run;

		%let cookd_train_cutoff =;
		proc sql;
		title "Largest COOKD in Training Group (iteration &num_iter.)";
		select max(COOKD) as max_training_cookd into :cookd_train_cutoff from tmp_influential_obs (obs=1);
		quit;
		%put cookd_train_cutoff = {&cookd_train_cutoff.};


		*FIND COOKD SPIKES (EXTREME INFLUNTIAL OBSERVATIONS) FOR THE TRAINING GROUP;
		proc sort data=tmp_influential_obs; by COOKD; run;

		data tmp_influential_obs; 
			set tmp_influential_obs;
			drop COOKD_prev;
			COOKD_prev = lag(COOKD);
			if (COOKD_prev ~= .); 
			if (COOKD_prev ~= 0) then COOKD_ratio = COOKD / COOKD_prev;
			if (COOKD_ratio > &COOKD_ratio_cutoff.) then spike + 1;
		run;
		
		proc sort data=tmp_influential_obs (where=(spike>0)); by descending COOKD; run;

		%let num_influential = %nobs_ds(tmp_influential_obs);
		%put num_influential = {&num_influential.};


		*TAKE OUT SPIKES;
		%if (&num_influential. > 0) and (&num_iter. < &max_iterations_reg.) %then %do;

			*TAKE OUT SPIKES IN ALL GROUPS (TRAINING + VALIDATION + TESTING);
			%let cookd_train_cutoff =;
			proc sql;
			title "COOKD Cutoff for Training Group (iteration &num_iter.)";
			select min(COOKD) as cookd_training_cutoff into :cookd_train_cutoff from tmp_influential_obs;
			quit;
			%put cookd_train_cutoff = {&cookd_train_cutoff.};

			data tmp_influential_obs2; set tmp_influential_obs2; if (COOKD>=&cookd_train_cutoff.); run;

			%merge_ds(&output_ds._reg_out,  tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._reg_out,  &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			%merge_ds(&output_ds._pls_pred, tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._pls_pred, &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			data &output_ds._reg_out;  set &output_ds._reg_out;  drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;
			data &output_ds._pls_pred; set &output_ds._pls_pred; drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;
			
			%merge_ds(&output_ds._pls_pred, tmp_influential_obs (keep=&profile_roll_up_var.), &output_ds._pls_pred, &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			data &output_ds._pls_pred;
				set &output_ds._pls_pred;
				drop exclude_infl_flg;
				if exclude_infl_flg='Y' then do;
					model_status = 'I';
					model_target = .;
				end;
			run;

		%end; %else %do;

			*NO TRAINING MODIFICATIONS;
			*BUT DO TAKE OUT COOKD VALUES IN VALIDATION OR TESTING THAT EXCEED TRAINING;
			%put cookd_train_cutoff = {&cookd_train_cutoff.};
			data tmp_influential_obs2; set tmp_influential_obs2; if (COOKD>&cookd_train_cutoff.); run;


			%merge_ds(&output_ds._reg_out,  tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._reg_out,  &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			%merge_ds(&output_ds._pls_pred, tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._pls_pred, &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			data &output_ds._reg_out;  set &output_ds._reg_out;  drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;
			data &output_ds._pls_pred; set &output_ds._pls_pred; drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;

		%end;

		*IF EXTREME INFLUENTIAL OBSERVATIONS WERE FOUND, THEN PRINT WHO ARE THOSE CUSTOMERS;
		proc print data=tmp_influential_obs noobs;
			title "Most Extreme Influential Observations Found in Training Group (iteration &num_iter.)";
			%if (&num_iter. = &max_iterations_reg.) %then %do; title2 'Not taken out (this was the last iteration)...'; %end;
			var model_status &profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z COOKD_ratio ;
		run;

		proc print data=tmp_influential_obs2 noobs;
			title "Most Extreme Influential Observations Found in Validation & Testing Groups (iteration &num_iter.)";
			var model_status &profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z ;
		run;

/*		%let num_influential_obs2 = %nobs_ds(tmp_influential_obs2);*/

		%delete_ds(tmp_influential_obs);
		%delete_ds(tmp_influential_obs2);

		*IF NO EXTREME INFLUENTIAL OBSERVATIONS, THEN EXIT WITH THE LAST MODEL;
		*OR IF REACHED THE LIMIT OF ITERATIONS, THEN ALSO EXIT WITH THE LAST MODEL;
		%if (&num_influential. = 0) %then %return;
	%end;
    
%mend segment_model_run;



%macro select_variables(method,selection, select, stop);

		%ucounts_ds(
			&output_ds._pls_pred_&method., 
			&profile_roll_up_var, 
			rev_P1_T, 
			by_var=model_status, 
			desc=Model &model. Segment &cluster.: Customer Counts (iteration &num_iter.)
			);


				
		*Select model using 10-fold cross validation - Iter&num_iter.;
		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc glmselect 
				data          = &output_ds._pls_pred_&method.
				plots(unpack) = (aseplot COEFFICIENTPANEL)
				;
			title "Model &model. Segment &cluster.: Select Factors - Method=&method: iteration=&num_iter";
			model model_target = &rotated_predictor_vars.
						/	SELECTION=&selection.(%if &selection='stepwise' %then %do; select=&select %end; STOP=&stop choose=CV )

						CVMETHOD=RANDOM(10)
						CVDETAILS=ALL
						STATS=(SL FVALUE RSQUARE ADJRSQ AICC)
						;
			output out=_tmp_glmselect_output p=pred r=res ;
		run;
	
		ods graphics off;


		%let selected_list_PC_&method. = &_GLSIND.;
		%let selected_num_PC_&method. = %count_elements(&&selected_list_PC_&method.);
		%put selected_list_PC_&method. = {&&selected_list_PC_&method.};
		%put selected_num_PC_&method.  = {&&selected_num_PC_&method.};

		*Cleanup;
		%delete_ds(_tmp_glmselect_output);		
%mend;


%macro calculate_metrics (method);
		
		*Run regression model with selected variables;
		%delete_ds(&output_ds._reg_out_&method.);
		%delete_ds(&output_ds._reg_outest_&method.);
		ods graphics on;  */ IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc reg	data   = &output_ds._pls_pred_&method.
					outest = &output_ds._reg_outest_&method.
					rsquare covout outseb
					PLOTS(MAXPOINTS=60000) = ( RStudentByLeverage(label) CooksD(label) DFFITS(label) )
					; *DFBETAS(label);
			title "Model &model. Segment &cluster.: Fit Regression (iteration &num_iter.)";
			id &profile_roll_up_var.; 
			model model_target = &&selected_list_PC_&method. / rsquare vif;
			output out=&output_ds._reg_out_&method. p=pred stdi=pred_stderr r=res rstudent=rstudent cookd=cookd dffits=dffits h=leverage;
		run;
		quit;
		ods graphics off;

		*Calculate z-scores and influence;
		%let train_N = ;
		proc sql noprint; select count(*) into :train_N from &output_ds._pls_pred_&method. where model_target~=.; quit;
		%put train_N = {&train_N.};

		%let train_RMSE = ;
		proc sql noprint; select _RMSE_ into :train_RMSE from &output_ds._reg_outest_&method. (obs=1); quit;
		%put train_RMSE = {&train_RMSE.};

		data &output_ds._reg_out_&method.;
			set &output_ds._reg_out_&method. (rename=(COOKD=COOKD0 RSTUDENT=RSTUDENT0));

		*Save number of predictors;
		label model_p = "Number of model predictors";
		model_p = &&selected_num_PC_&method. + 1;

		*Calculate residual for everybody (including validation and scored customers);
		label res = "Model residual";
		res = &target_var. - pred;

		*Calculate COOKD for everybody (including validation and scored customers);
		label COOKD = "Cook's D influence statistic";
		if (leverage~=0 and leverage~=1 and leverage~=.) then
			COOKD = (res**2 / &train_RMSE.**2) * (leverage / (1-leverage)**2) / model_p;

		*Calculate RSTUDENT for everybody (including validation and scored customers);
		drop MSEi;
		label RSTUDENT = "Studentized model resisual";
		if (leverage~=0 and leverage~=1 and leverage~=.) then do;
			MSEi = ( &train_RMSE.**2 * (&train_N. - model_p) -  res**2/(1-leverage) ) / (&train_N. - model_p - 1) ;
			if (MSEi * (1-leverage)) > 0 then 
				RSTUDENT = res / sqrt(MSEi * (1-leverage));
		end;

		*Determine z-score;
		format target_z comma20.2;
		label  target_z = "z-score of model resisual";
		if (res~=. and pred_stderr>0) then 
			target_z = res / pred_stderr;

		*Determine z-score band (0=within far price, >0 above fair price, <0 below fair price);
		format target_z_band comma20.0;
		label  target_z_band = "fair price band based on model resisual";
		if      target_z<-3 then target_z_band = -3;
		else if target_z<-2 then target_z_band = -2;
		else if target_z<-1 then target_z_band = -1;
		else if target_z<=1 then target_z_band =  0;
		else if target_z<=2 then target_z_band =  1;
		else if target_z<=3 then target_z_band =  2;
		else                     target_z_band =  3;

		*Prediction (e.g. fair price) in the original scale;
		format pred_orig dollar20.2;
		label pred_orig = "Model prediction (fair price) in original scale (un-transformed)";

		if pred>20 then pred=20;/**added by meng to prevent overflow**/
	 	pred_orig = exp(pred)-1;

		*Flag outliers and influencial observations;
		length influence_flg $ 64;
		label influence_flg = "Stats to check (cutoffs exceeded)":
		influence_flg = '';
		if COOKD>1                                    then %strconcatspace(influence_flg,'COOKD1');
		if 1>=COOKD>(4/&train_N.)                     then %strconcatspace(influence_flg,'COOKD2');
		if abs(DFFITS)>2                              then %strconcatspace(influence_flg,'DFFITS1');
		if 2>=abs(DFFITS)>(2*sqrt(model_p/&train_N.)) then %strconcatspace(influence_flg,'DFFITS2');
		if abs(RSTUDENT)>2                            then %strconcatspace(influence_flg,'RSTUD');
		if influence_flg = '' then do;
			if model_status='M' then influence_flg = 'NO FLAG';
			else                     influence_flg = 'N/A';
		end;

		*For testing only;
		if      (COOKD  = .)                then COOKD_check='MISS!!!';
		else if (COOKD0 = .)                then COOKD_check='ok2';
		else if abs(COOKD - COOKD0)<0.0001  then COOKD_check='ok1';    
		else                                     COOKD_check='ERR!!!';

		if      (RSTUDENT  = .)                  then RSTUDENT_check='MISS!!!';
		else if (RSTUDENT0 = .)                  then RSTUDENT_check='ok2';
		else if abs(RSTUDENT - RSTUDENT0)<0.0001 then RSTUDENT_check='ok1'; 
		else                                          RSTUDENT_check='ERR!!!';
		run;

		

%mend;


%macro influence_out_training (method);

	      *GET COOKSD RESULTS (WORST CUSTOMERS FIRST);
		
               proc sort 
			data = &output_ds._reg_out_&method. (keep=&profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z model_status where=(model_status='M') )
			out  = tmp_influential_obs; 
			by descending COOKD;
		run;

		data tmp_influential_obs; 
			set tmp_influential_obs (obs=%eval(&max_infuential_checked.+1)); 
			if (find(influence_flg,'COOKD','it') > 0);
		run;

		*FIND COOKD SPIKES (EXTREME INFLUNTIAL OBSERVATIONS) FOR THE TRAINING GROUP;
		proc sort data=tmp_influential_obs; by COOKD; run;

		data tmp_influential_obs; 
			set tmp_influential_obs;
			drop COOKD_prev;
			COOKD_prev = lag(COOKD);
			if (COOKD_prev ~= .); 
			if (COOKD_prev ~= 0) then COOKD_ratio = COOKD / COOKD_prev;
			if (COOKD_ratio > &COOKD_ratio_cutoff.) then spike + 1;
		run;
		
		proc sort data=tmp_influential_obs (where=(spike>0)); by descending COOKD; run;

		%let num_influential = %nobs_ds(tmp_influential_obs);
		%put num_influential = {&num_influential.};


		*TAKE OUT SPIKES;
		%if (&num_influential. > 0) %then %do;
			%merge_ds(&output_ds._pls_pred_&method., tmp_influential_obs (keep=&profile_roll_up_var.), &output_ds._pls_pred_&method., &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
			data &output_ds._pls_pred_&method.;
				set &output_ds._pls_pred_&method.;
				drop exclude_infl_flg;
				if exclude_infl_flg='Y' then do;
					model_status = 'I';
					model_target = .;
				end;
			run;

		%end; 

		*IF EXTREME INFLUENTIAL OBSERVATIONS WERE FOUND, THEN PRINT WHO ARE THOSE CUSTOMERS;
		proc print data=tmp_influential_obs noobs;
			title "Most Extreme Influential Observations Found in Training Group (iteration &num_iter.)";
			%if (&num_iter. = &max_iterations_reg.) %then %do; title2 'Not taken out (this was the last iteration)...'; %end;
			var model_status &profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z COOKD_ratio ;
		run;

		%delete_ds(tmp_influential_obs);
		
%mend;

%macro influence_out_test (method);
	
	%script_switch (&script_name., MOD_&cluster._&model._MODEL_Final_results., &directory_output.);


	data _tmp_testdata;
		set &output_ds._reg_out_&method. (where=(model_status='T'));
		res = abs(res);
	run;

	PROC MEANS DATA=_tmp_testdata q1 q3 noprint;
		var res;
	output out=_tmp_out q1=q1 q3=q3;
	run; 
	

	proc print data=_tmp_out;
	title "tmp_out";


	%let _tmp_q1 = ;
	%let _tmp_q3 = ;
	proc sql noprint;
		select q1, q3 
		into :_tmp_q1, :_tmp_q3
		from _tmp_out;
	quit;
	%put _tmp_q1 = {&_tmp_q1.};
	%put _tmp_q3 = {&_tmp_q3.};


	proc sort data=_tmp_testdata;  by descending res; run;
    data tmp_influential_obs2;
                set _tmp_testdata;
                if (_n_<6 or res>(&_tmp_q3+3*(&_tmp_q3.- &_tmp_q1.)));
    run;

	%merge_ds(&output_ds._reg_out_&method.,  tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._reg_out_&method.,  &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);
	%merge_ds(&output_ds._pls_pred_&method., tmp_influential_obs2 (keep=&profile_roll_up_var.), &output_ds._pls_pred_&method., &profile_roll_up_var., in1=Y, in2_flg=exclude_infl_flg);

	data &output_ds._reg_out_&method.;  set &output_ds._reg_out_&method.;  drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;
	data &output_ds._pls_pred_&method.; set &output_ds._pls_pred_&method.; drop exclude_infl_flg; if exclude_infl_flg='Y' then model_status = 'J'; run;


	*IF EXTREME INFLUENTIAL OBSERVATIONS WERE FOUND, THEN PRINT WHO ARE THOSE CUSTOMERS;
	proc print data=tmp_influential_obs2 noobs;
		title "Most Extreme Influential Observations Found in Test Group ";
		var model_status &profile_roll_up_var. &additional_vars. res target_z ;
	run;

	%delete_ds(_tmp_testdata);
	%delete_ds(_tmp_out);
	%delete_ds(tmp_influential_obs2);


%mend;

%macro test_vs_training(method,selection, select, stop);
		
		%script_switch (&script_name., MOD_&cluster._&model._MODEL_Final_results., &directory_output.);


		data _tmp_testdata;
			set &output_ds._pls_pred_&method. (where=(model_status='T'));
			model_target = &target_var.;
		run;
											
		%let _tmp_test_count = ;
		proc sql noprint;
		select count(*) 
		into :_tmp_test_count
		from _tmp_testdata;
		quit;
		%put Test Set Count = {&_tmp_test_count.};
		
		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;

		proc glmselect 
			data          = &output_ds._pls_pred_&method.
			testdata      = _tmp_testdata
			plots(unpack) = (aseplot COEFFICIENTPANEL)
				;
			title "Model &model. Segment &cluster.: Test VS Training: Method=&method";
			model model_target = &rotated_predictor_vars.
						/	SELECTION=&selection.( %if &selection='stepwise' %then %do; select=&select %end; STOP=&stop choose=CV )

						CVMETHOD=RANDOM(10)
	
						CVDETAILS=ALL
						STATS=(SL FVALUE RSQUARE ADJRSQ AICC)
						;
			run;
	
		ods graphics off;
		
		
		*Cleanup;
		%delete_ds(_tmp_testdata);
%mend;

%macro cal_test_data_error(method);
		
		%script_switch (&script_name., MOD_&cluster._&model._MODEL_Final_results., &directory_output.);

		
		*Calculate target mean for this cluster (training observations only);
		%let train_target_mean = ;
		proc sql noprint;
		select mean(&target_var.) as train_target_mean format 24.6
		into :train_target_mean
		from &output_ds._reg_out_&method.
		where (model_status = 'M');
		quit;
		%put train_target_mean = {&train_target_mean.};

		*Compare searches using training data;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Training Group)";
		select
			count(*)                                                                      as cust  format 20.0,
			(&&selected_num_PC_&method.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
			sqrt(sum((&target_var.-pred)**2)/(count(*)))           as rmse  format 20.6,
			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		from &output_ds._reg_out_&method.
		where model_status='M';
		quit;
		
		
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Test Group) ";
		select
			count(*)                                                                      as cust  format 20.0,
			(&&selected_num_PC_&method.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
			sqrt(sum((&target_var.-pred)**2)/(count(*)))           as rmse  format 20.6,

			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		into :comp_cust_&method, :comp_param_&method, :comp_sse_&method, :comp_rmse_&method, :comp_r2_&method
		from &output_ds._reg_out_&method.
		where model_status='T';

		quit;
		%put comp_cust_&method  = {&&comp_cust_&method};
		%put comp_param_&method = {&&comp_param_&method};
		%put comp_sse_&method   = {&&comp_sse_&method};
		%put comp_rmse_&method  = {&&comp_rmse_&method};
		%put comp_r2_&method    = {&&comp_r2_&method};				
		
%mend;

%macro seg_mod_run(method,selection, select, stop);
	
	%script_switch (&script_name., MOD_&cluster._&model._MODEL_By_Method&method., &directory_output.);

	%local num_iter num_influential;

	data &output_ds._pls_pred_&method;
		set &output_ds._pls_pred;
	
		%let num_influential = ;

        %do num_iter = 1 %to &max_iterations_reg.;
		
		%select_variables(&method,&selection, &select, &stop);
		
		%calculate_metrics(&method.);

		%influence_out_training(&method.);

		*IF NO EXTREME INFLUENTIAL OBSERVATIONS, THEN EXIT WITH THE LAST MODEL;
		%if (&num_influential. = 0) %then %goto Done;
	%end;	
	
	%Done:

	%influence_out_test(&method.);

	%cal_test_data_error(&method.);

	%test_vs_training(&method,&selection, &select, &stop);
		
%mend;

%macro choose_model();
	
	%script_switch (&script_name., MOD_&cluster._&model._MODEL_Final_results., &directory_output.);

	*Pick the best model using test data;
	%let best_selection_method    = ;
	%let select_rotated_pred_vars = ;
	%let select_rotated_pred_num  = ;
	%if %sysevalf(&comp_rmse_1. <= &comp_rmse_2.) and %sysevalf(&comp_rmse_1. <= &comp_rmse_3.) %then %do;
		%let best_selection_method    = 1;
		%let select_rotated_pred_vars = &selected_list_PC_1.;
		%let select_rotated_pred_num  = &selected_num_PC_1.;
	%end; %else %if %sysevalf(&comp_rmse_2. <= &comp_rmse_3.) %then  %do;
		%let best_selection_method    = 2;
		%let select_rotated_pred_vars = &selected_list_PC_2.;
		%let select_rotated_pred_num  = &selected_num_PC_2.;
	%end; %else %do;
		%let best_selection_method    = 3;
		%let select_rotated_pred_vars = &selected_list_PC_3.;
		%let select_rotated_pred_num  = &selected_num_PC_3.;
	%end;
	%put FINAL best_selection_method    = {&best_selection_method.};
	%put FINAL select_rotated_pred_vars = {&select_rotated_pred_vars.};
	%put FINAL select_rotated_pred_num  = {&select_rotated_pred_num.};

	data &output_ds._pls_pred;
		set &output_ds._pls_pred_&best_selection_method.;
	run;
	data &output_ds._reg_out;
		set &output_ds._reg_out_&best_selection_method.;

	*Save to the HTML;
	title  "Model &model. Segment &cluster.: Best Factor Selection Method";
	title2 "Based on Test Results";
	%print_macro_var(best_selection_method);

%mend;

%macro segment_model_run_paralell ();
	
	%let comp_cust_1  = ;
	%let comp_param_1 = ;
	%let comp_sse_1   = ;
	%let comp_rmse_1  = ;
	%let comp_r2_1    = ;

	%let comp_cust_2  = ;
	%let comp_param_2 = ;
	%let comp_sse_2   = ;
	%let comp_rmse_2  = ;
	%let comp_r2_2    = ;

	%let comp_cust_3  = ;
	%let comp_param_3 = ;
	%let comp_sse_3   = ;
	%let comp_rmse_3  = ;
	%let comp_r2_13   = ;


	%let selected_list_PC_1 = ;
	%let selected_num_PC_1  = ;

	%let selected_list_PC_2 = ;
	%let selected_num_PC_2  = ;

	%let selected_list_PC_3 = ;
	%let selected_num_PC_3  = ;

	%seg_mod_run(1,stepwise,SBC,ADJRSQ);
	%seg_mod_run(2,lar,X,ADJRSQ);
	%seg_mod_run(3,lar,X,sbc);

	%choose_model();
	
%mend;



%macro segment_model_select_factors ();

		*SHOW CUSTOMER COUNTS GOING INTO THE MODEL;
		%ucounts_ds(
			&output_ds._pls_pred, 
			&profile_roll_up_var, 
			rev_P1_T, 
			by_var=model_status, 
			desc=Model &model. Segment &cluster.: Customer Counts (iteration &num_iter.)
			);
		*Prepare a test dataset with the target populated (necessary just to generate the plots);
		data _tmp_testdata;
/*			set &output_ds._pls_pred (where=(model_status='V'));*/
			set &output_ds._pls_pred (where=(model_status='T'));

			model_target = &target_var.;
		run;
		*Stop Criteria: If a maximum is set, ensure that it does not exceed the test count;

		%let _tmp_test_count = ;
		proc sql noprint;
		select count(*) 
		into :_tmp_test_count
		from _tmp_testdata;
		quit;
		%put Test Set Count = {&_tmp_test_count.};
    *****modified by meng start****;
		%if (&_tmp_test_count <&min_test_df.) %then %do;
			%let model_stop_criteria1 = NONE;
			%let model_stop_criteria2 = NONE;
			%let model_stop_criteria3 = NONE;
		%end;

		%if (&model_stop_criteria1. ~= NONE) %then %do;
			%if (&_tmp_test_count. - &model_stop_criteria1.) < &min_test_df. and (&_tmp_test_count. - &model_stop_criteria1.) >=1 %then 
				%let model_stop_criteria1 = %eval(&_tmp_test_count. - &min_test_df.);
			 %else %let model_stop_criteria1 =&_tmp_test_count.;
		%end;
		%if (&model_stop_criteria2. ~= NONE) %then %do;
			%if (&_tmp_test_count. - &model_stop_criteria2.) < &min_test_df. and (&_tmp_test_count. - &model_stop_criteria2.) >=1 %then 
				%let model_stop_criteria2 = %eval(&_tmp_test_count. - &min_test_df.);
			%else %let model_stop_criteria2 =&_tmp_test_count.;
	%end;
		%if (&model_stop_criteria3. ~= NONE) %then %do;
			%if (&_tmp_test_count. - &model_stop_criteria3.) < &min_test_df. and (&_tmp_test_count. - &model_stop_criteria3.) >=1 %then 
				%let model_stop_criteria3 = %eval(&_tmp_test_count. - &min_test_df.);
			 %else %let model_stop_criteria3 =&_tmp_test_count.;
		%end;

  *****modified by meng end ***;
		%put model_stop_criteria1 = {&model_stop_criteria1.};
		%put model_stop_criteria2 = {&model_stop_criteria2.};
		%put model_stop_criteria3 = {&model_stop_criteria3.};

		*Select model using 10-fold cross validation - First search;
		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc glmselect 
				data          = &output_ds._pls_pred
				testdata      = _tmp_testdata
				plots(unpack) = (aseplot COEFFICIENTPANEL)
				;
			title "Model &model. Segment &cluster.: Select Factors - First Search";
			model model_target = &rotated_predictor_vars.
					/*/	SELECTION=STEPWISE(SELECT=SBC STOP=&model_stop_criteria1. CHOOSE=CV)*/
					/	SELECTION=STEPWISE(SELECT=SBC STOP=ADJRSQ CHOOSE=CV)

						CVMETHOD=RANDOM(10)

						CVDETAILS=ALL
						STATS=(SL FVALUE RSQUARE ADJRSQ AICC)
						;
			output out=_tmp_glmselect_output_1 p=pred r=res;
		run;
		ods graphics off;
		%let selected_list_PC1 = &_GLSIND.;
		%let selected_num_PC1  = %count_elements(&selected_list_PC1.);
		%put selected_list_PC1 = {&selected_list_PC1.};
		%put selected_num_PC1  = {&selected_num_PC1.};

		*Select model using 10-fold cross validation - Second search;
		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc glmselect 
				data          = &output_ds._pls_pred
				testdata      = _tmp_testdata
				plots(unpack) = (aseplot COEFFICIENTPANEL)
				;
			title "Model &model. Segment &cluster.: Select Factors - Second Search";
			model model_target = &rotated_predictor_vars.
					/*/	SELECTION=STEPWISE(SELECT=SL STOP=&model_stop_criteria2. CHOOSE=CV)*/
					/	SELECTION=LAR( STOP=SBC CHOOSE=CV)

						CVMETHOD=RANDOM(10)
	
						CVDETAILS=ALL
						STATS=(SL FVALUE RSQUARE ADJRSQ AICC)
						;
			output out=_tmp_glmselect_output_2 p=pred r=res;
		run;
		ods graphics off;
		%let selected_list_PC2 = &_GLSIND.;
		%let selected_num_PC2  = %count_elements(&selected_list_PC2.);
		%put selected_list_PC2 = {&selected_list_PC2.};
		%put selected_num_PC2  = {&selected_num_PC2.};

		*Select model using 10-fold cross validation - Third search;
		ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc glmselect 
				data          = &output_ds._pls_pred
				testdata      = _tmp_testdata
				plots(unpack) = (aseplot COEFFICIENTPANEL)
				;
			title "Model &model. Segment &cluster.: Select Factors - Third Search";
			model model_target = &rotated_predictor_vars.
						/*/	SELECTION=STEPWISE(SELECT=AICC STOP=&model_stop_criteria3. CHOOSE=CV)*/
					/	SELECTION=LAR(STOP=ADJRSQ CHOOSE=CV)

						CVMETHOD=RANDOM(10)

						CVDETAILS=ALL
						STATS=(SL FVALUE RSQUARE ADJRSQ AICC)
						;
			output out=_tmp_glmselect_output_3 p=pred r=res;
		run;
		ods graphics off;
		%let selected_list_PC3 = &_GLSIND.;
		%let selected_num_PC3  = %count_elements(&selected_list_PC3.);
		%put selected_list_PC3 = {&selected_list_PC3.};
		%put selected_num_PC3  = {&selected_num_PC3.};

		%delete_ds(_tmp_testdata);

		*Calculate target mean for this cluster (training observations only);
		%let train_target_mean = ;
		proc sql noprint;
		select mean(&target_var.) as train_target_mean format 24.6
		into :train_target_mean
		from &output_ds._pls_pred
		where (model_status = 'M');
		quit;
		%put train_target_mean = {&train_target_mean.};

		*Compare searches using training data;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Training Group) - First Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC1.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC1.+1)))           as rmse  format 20.6,
			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		from _tmp_glmselect_output_1
		where model_status='M';
		quit;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Training Group) - Second Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC2.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC2.+1)))           as rmse  format 20.6,
			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		from _tmp_glmselect_output_2
		where model_status='M';
		quit;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Training Group) - Third Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC3.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC3.+1)))           as rmse  format 20.6,
			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		from _tmp_glmselect_output_3
		where model_status='M';
		quit;


		                                  *Compare searches using validation data;
		*Compare searches using test data;

		*Summarize first search;
		%let comp1_cust  = ;
		%let comp1_param = ;
		%let comp1_sse  = ;
		%let comp1_rmse  = ;
		%let comp1_r2    = ;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Validation Group) - First Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC1.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
/*			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC1.+1)))           as rmse  format 20.6,*/
			sqrt(sum((&target_var.-pred)**2)/(count(*)))           as rmse  format 20.6,

			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		into :comp1_cust, :comp1_param, :comp1_sse, :comp1_rmse, :comp1_r2
		from _tmp_glmselect_output_1
/*		where model_status='V';*/
		where model_status='T';

		quit;
		%put comp1_cust  = {&comp1_cust.};
		%put comp1_param = {&comp1_param.};
		%put comp1_sse   = {&comp1_sse.};
		%put comp1_rmse  = {&comp1_rmse.};
		%put comp1_r2    = {&comp1_r2.};

		*Summarize second search;
		%let comp2_cust  = ;
		%let comp2_param = ;
		%let comp2_sse  = ;
		%let comp2_rmse  = ;
		%let comp2_r2    = ;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Validation Group) - Second Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC2.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
/*			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC2.+1)))           as rmse  format 20.6,*/
			sqrt(sum((&target_var.-pred)**2)/(count(*)))           as rmse  format 20.6,
			sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		into :comp2_cust, :comp2_param, :comp2_sse, :comp2_rmse, :comp2_r2
		from _tmp_glmselect_output_2
/*		where model_status='V';*/
		where model_status='T';
		quit;
		%put comp2_cust  = {&comp2_cust.};
		%put comp2_param = {&comp2_param.};
		%put comp2_sse   = {&comp2_sse.};
		%put comp2_rmse  = {&comp2_rmse.};
		%put comp2_r2    = {&comp2_r2.};

		*Summarize third search;
		%let comp3_cust  = ;
		%let comp3_param = ;
		%let comp3_sse  = ;
		%let comp3_rmse  = ;
		%let comp3_r2    = ;
		proc sql;
		title "Model &model. Segment &cluster.: Factor Selection Results (Validation Group) - Third Search";
		select
			count(*)                                                                      as cust  format 20.0,
			(&selected_num_PC3.+1)                                                        as param format 20.0,
			sum((&target_var. - pred)**2)                                                 as sse   format 20.4,
/*			sqrt(sum((&target_var.-pred)**2)/(count(*)-(&selected_num_PC3.+1)))           as rmse  format 20.6,*/
	    	sqrt(sum((&target_var.-pred)**2)/(count(*)))           as rmse  format 20.6,

		sum((pred-&train_target_mean.)**2)/sum((&target_var.-&train_target_mean.)**2) as r2    format 20.4
		into :comp3_cust, :comp3_param, :comp3_sse, :comp3_rmse, :comp3_r2
		from _tmp_glmselect_output_3
/*		where model_status='V';*/
		where model_status='T';
		quit;
		%put comp3_cust  = {&comp3_cust.};
		%put comp3_param = {&comp3_param.};
		%put comp3_sse   = {&comp3_sse.};
		%put comp3_rmse  = {&comp3_rmse.};
		%put comp3_r2    = {&comp3_r2.};

		*Pick the best model using validation data;
		%let best_selection_method    = ;
		%let select_rotated_pred_vars = ;
		%let select_rotated_pred_num  = ;
		%if %sysevalf(&comp1_rmse. <= &comp2_rmse.) and %sysevalf(&comp1_rmse. <= &comp3_rmse.) %then %do;
			%let best_selection_method    = 1;
			%let select_rotated_pred_vars = &selected_list_PC1.;
			%let select_rotated_pred_num  = &selected_num_PC1.;
		%end; %else %if %sysevalf(&comp2_rmse. < &comp3_rmse.) %then  %do;
			%let best_selection_method    = 2;
			%let select_rotated_pred_vars = &selected_list_PC2.;
			%let select_rotated_pred_num  = &selected_num_PC2.;
		%end; %else %do;
			%let best_selection_method    = 3;
			%let select_rotated_pred_vars = &selected_list_PC3.;
			%let select_rotated_pred_num  = &selected_num_PC3.;
		%end;
		%put FINAL best_selection_method    = {&best_selection_method.};
		%put FINAL select_rotated_pred_vars = {&select_rotated_pred_vars.};
		%put FINAL select_rotated_pred_num  = {&select_rotated_pred_num.};

		*Save to the HTML;
		title  "Model &model. Segment &cluster.: Best Factor Selection Method";
		title2 "Based on Validation Results";
		%print_macro_var(best_selection_method);

		*Cleanup;
		%delete_ds(_tmp_glmselect_output_1 _tmp_glmselect_output_2 _tmp_glmselect_output_3);

%mend segment_model_select_factors;


%macro segment_model_run_iteration ();

	*SHOW CUSTOMER COUNTS GOING INTO THE MODEL;
	%ucounts_ds(
		&output_ds._pls_pred, 
		&profile_roll_up_var, 
		rev_P1_T, 
		by_var=model_status, 
		desc=Model &model. Segment &cluster.: Customer Counts (iteration &num_iter.)
		);

	*Run regression model with selected variables;
	%delete_ds(&output_ds._reg_out);
	%delete_ds(&output_ds._reg_outest);
	ods graphics on;  */ IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
	proc reg	data   = &output_ds._pls_pred
				outest = &output_ds._reg_outest
				rsquare covout outseb
				PLOTS(MAXPOINTS=60000) = ( RStudentByLeverage(label) CooksD(label) DFFITS(label) )
				; *DFBETAS(label);
		title "Model &model. Segment &cluster.: Fit Regression (iteration &num_iter.)";
		id &profile_roll_up_var.; 
		model model_target = &select_rotated_pred_vars. / rsquare vif;
		output out=&output_ds._reg_out p=pred stdi=pred_stderr r=res rstudent=rstudent cookd=cookd dffits=dffits h=leverage;
	run;
	quit;
	ods graphics off;

	*Calculate z-scores and influence;
	%let train_N = ;
	proc sql noprint; select count(*) into :train_N from &output_ds._pls_pred where model_target~=.; quit;
	%put train_N = {&train_N.};

	%let train_RMSE = ;
	proc sql noprint; select _RMSE_ into :train_RMSE from &output_ds._reg_outest (obs=1); quit;
	%put train_RMSE = {&train_RMSE.};

	data &output_ds._reg_out;
		set &output_ds._reg_out (rename=(COOKD=COOKD0 RSTUDENT=RSTUDENT0));

		*Save number of predictors;
		label model_p = "Number of model predictors";
		model_p = &select_rotated_pred_num. + 1;

		*Calculate residual for everybody (including validation and scored customers);
		label res = "Model residual";
		res = &target_var. - pred;

		*Calculate COOKD for everybody (including validation and scored customers);
		label COOKD = "Cook's D influence statistic";
		if (leverage~=0 and leverage~=1 and leverage~=.) then
			COOKD = (res**2 / &train_RMSE.**2) * (leverage / (1-leverage)**2) / model_p;

		*Calculate RSTUDENT for everybody (including validation and scored customers);
		drop MSEi;
		label RSTUDENT = "Studentized model resisual";
		if (leverage~=0 and leverage~=1 and leverage~=.) then do;
			MSEi = ( &train_RMSE.**2 * (&train_N. - model_p) -  res**2/(1-leverage) ) / (&train_N. - model_p - 1) ;
			if (MSEi * (1-leverage)) > 0 then 
				RSTUDENT = res / sqrt(MSEi * (1-leverage));
		end;

		*Determine z-score;
		format target_z comma20.2;
		label  target_z = "z-score of model resisual";
		if (res~=. and pred_stderr>0) then 
			target_z = res / pred_stderr;

		*Determine z-score band (0=within far price, >0 above fair price, <0 below fair price);
		format target_z_band comma20.0;
		label  target_z_band = "fair price band based on model resisual";
		if      target_z<-3 then target_z_band = -3;
		else if target_z<-2 then target_z_band = -2;
		else if target_z<-1 then target_z_band = -1;
		else if target_z<=1 then target_z_band =  0;
		else if target_z<=2 then target_z_band =  1;
		else if target_z<=3 then target_z_band =  2;
		else                     target_z_band =  3;

		*Prediction (e.g. fair price) in the original scale;
		format pred_orig dollar20.2;
		label pred_orig = "Model prediction (fair price) in original scale (un-transformed)";

		if pred>20 then pred=20;/**added by meng to prevent overflow**/
	 	pred_orig = exp(pred)-1;

		*Flag outliers and influencial observations;
		length influence_flg $ 64;
		label influence_flg = "Stats to check (cutoffs exceeded)":
		influence_flg = '';
		if COOKD>1                                    then %strconcatspace(influence_flg,'COOKD1');
		if 1>=COOKD>(4/&train_N.)                     then %strconcatspace(influence_flg,'COOKD2');
		if abs(DFFITS)>2                              then %strconcatspace(influence_flg,'DFFITS1');
		if 2>=abs(DFFITS)>(2*sqrt(model_p/&train_N.)) then %strconcatspace(influence_flg,'DFFITS2');
		if abs(RSTUDENT)>2                            then %strconcatspace(influence_flg,'RSTUD');
		if influence_flg = '' then do;
			if model_status='M' then influence_flg = 'NO FLAG';
			else                     influence_flg = 'N/A';
		end;

		*For testing only;
		if      (COOKD  = .)                then COOKD_check='MISS!!!';
		else if (COOKD0 = .)                then COOKD_check='ok2';
		else if abs(COOKD - COOKD0)<0.0001  then COOKD_check='ok1';    
		else                                     COOKD_check='ERR!!!';

		if      (RSTUDENT  = .)                  then RSTUDENT_check='MISS!!!';
		else if (RSTUDENT0 = .)                  then RSTUDENT_check='ok2';
		else if abs(RSTUDENT - RSTUDENT0)<0.0001 then RSTUDENT_check='ok1'; 
		else                                          RSTUDENT_check='ERR!!!';
	run;

	*For testing only;
	*%freq_ds(&output_ds._reg_out, COOKD_check RSTUDENT_check);

%mend segment_model_run_iteration;

%macro segment_model_check_results ();
	%local total_OK_cust total_cust rev_P1_T;


	*Plot target vs. predicted value;
	%macro plot_pred_act (status_list);
		ods graphics on; */ IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
		proc sgpanel data=&output_ds._reg_out noautolegend;
			panelby model_status;
			series x=&target_var. y=&target_var. / lineattrs=GraphPrediction(color=gray);
			scatter x=pred y=&target_var. / MARKERATTRS=(symbol=X);
			where model_status in (  %field(&status_list.)  );
			colaxis label= "Predicted &target_var.";
			rowaxis label= "Actual &target_var.";
			title     "Predicted vs. Actual Plot for Model &model. Segment &cluster.";
			footnote  "Modeling groups: M=Training, V=Validation, T=Testing, X=Soft Exclusions";
			footnote2 "I=Influential (Training), J=Influential (Validation/Testing)";
		run;
		ods graphics off;
		footnote;
	%mend plot_pred_act;
/*	%plot_pred_act('M' 'V' 'T');*/
	%plot_pred_act('M' 'T');

	%plot_pred_act('I' 'J' 'X');

	*Print counts for fair price bands;
	proc sql;
	create table _tmp_detail_bands as
	select 
		case 
			when target_z<-3 then 'A) BELOW -3s'
			when target_z<-2 then 'B) BETWEEN -3s AND -2s'
			when target_z<-1 then 'C) BETWEEN -2s AND -1s'
			when target_z<=1 then 'D) BETWEEN -1s AND +1s'
			when target_z<=2 then 'E) BETWEEN +1s AND +2s'
			when target_z<=3 then 'F) BETWEEN +2s AND +3s'
			else                  'G) ABOVE +3s'
		end as target_z,
		sum(case when (model_status in ('M'))         then 1 else 0 end) as train_cust            format comma20.0,
		sum(case when (model_status in ('V'))         then 1 else 0 end) as valid_cust            format comma20.0,
		sum(case when (model_status in ('T'))         then 1 else 0 end) as test_cust             format comma20.0,
		sum(case when (model_status in ('M','V','T')) then 1 else 0 end) as TOTAL_OK_CUST         format comma20.0,
		sum(case when (model_status in ('X'))         then 1 else 0 end) as flag_X_cust           format comma20.0,
		sum(case when (model_status in ('I','J'))     then 1 else 0 end) as influ_cust            format comma20.0,
		count(*)                                                         as TOTAL_CUST            format comma20.0,
		sum(rev_P1_T)                                                    as rev_P1_T              format dollar20.0,
		sum(rev_P1_T)/(calculated TOTAL_CUST)                            as avg_rev_P1_T_per_cust format dollar20.0,
		%apply_macro(simplesum, %metric_list(rev, P1, &model_opco_list), dollar20.0, sep=COMMA)

	from &output_ds._reg_out
	group by 1 ;
	quit;

	proc sql;
	create table _tmp_total_bands as
	select 
		'TOTAL' as target_z,
		sum(train_cust)                       as train_cust            format comma20.0,
		sum(valid_cust)                       as valid_cust            format comma20.0,
		sum(test_cust)                        as test_cust             format comma20.0,
		sum(TOTAL_OK_CUST)                    as TOTAL_OK_CUST         format comma20.0,
		sum(flag_X_cust)                      as flag_X_cust           format comma20.0,
		sum(influ_cust)                       as influ_cust            format comma20.0,
		sum(TOTAL_CUST)                       as TOTAL_CUST            format comma20.0,
		sum(rev_P1_T)                         as rev_P1_T              format dollar20.0,
		sum(rev_P1_T)/(calculated TOTAL_CUST) as avg_rev_P1_T_per_cust format dollar20.0,
		%apply_macro(simplesum, %metric_list(rev, P1, &model_opco_list), dollar20.0, sep=COMMA)

	from _tmp_detail_bands;

	quit;

	%let total_OK_cust =;
	%let total_cust    =;
	%let rev_P1_T      =;
	proc sql noprint;
	select 
		total_OK_cust as total_OK_cust format 24.0,
		total_cust    as total_cust    format 24.0,
		rev_P1_T      as rev_P1_T      format 24.8
	into :total_OK_cust, :total_cust, :rev_P1_T
	from _tmp_total_bands;
	quit;
	%put total_OK_cust = {&total_OK_cust.};
	%put total_cust       = {&total_cust.};
	%put rev_P1_T         = {&rev_P1_T.};

	data _tmp_total_bands; 
		set _tmp_detail_bands _tmp_total_bands;

		format PCT_OK_CUST PCT_TOTAL_CUST pct_rev_P1_T percent8.1;
		PCT_OK_CUST    = total_OK_cust / &total_OK_cust.;
		PCT_TOTAL_CUST = total_cust    / &total_cust.;
		pct_rev_P1_T   = rev_P1_T      / &rev_P1_T.;
	run;

	proc print data=_tmp_total_bands noobs;
		title "Price Bands for Model &model. Segment &cluster.";
		var	target_z 
			train_cust   valid_cust    test_cust     TOTAL_OK_CUST   PCT_OK_CUST
			flag_X_cust  influ_cust    TOTAL_CUST    PCT_TOTAL_CUST
			rev_P1_T     pct_rev_P1_T  avg_rev_P1_T_per_cust              
			%metric_list(rev, P1, &model_opco_list);
	run;

	%delete_ds(_tmp_detail_bands _tmp_total_bands);



	*Print largest customers above and below fair price;
	proc sort data=&output_ds._reg_out (where=(target_z<-1)) out=_tmp_fair_price; by descending rev_P1_T;
	proc print data=_tmp_fair_price (obs=30) noobs;
		title "Largest 30 Customers (by rev_P1_T) BELOW Fair Price Range for Model &model. Segment &cluster.";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE; 
	run;

	proc sort data=&output_ds._reg_out (where=(target_z>+1)) out=_tmp_fair_price; by descending rev_P1_T;
	proc print data=_tmp_fair_price (obs=30) noobs;
		title "Largest 30 Customers (by rev_P1_T) ABOVE Fair Price Range for Model &model. Segment &cluster.";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE; 
	run;
	
	proc sort data=&output_ds._reg_out (where=(-1<=target_z<=+1)) out=_tmp_fair_price; by descending rev_P1_T;
	proc print data=_tmp_fair_price (obs=30) noobs;
		title "Largest 30 Customers (by rev_P1_T) WITHIN Fair Price Range for Model &model. Segment &cluster.";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE; 
	run;
	%delete_ds(_tmp_fair_price);



	*Print influence results;
	proc template;
		define statgraph sgplot;
		begingraph / collation=binary;
		layout overlay / yaxisopts=(labelFitPolicy=Split) y2axisopts=(labelFitPolicy=Split);
		ScatterPlot X='leverage'n Y='rstudent'n / subpixel=off primary=true Group='influence_flg'n 
					Markerattrs=( Symbol=X) DataTransparency=0.2 LegendLabel="Studentized model resisual" NAME="SCATTER"
					rolename=(Label=&profile_roll_up_var. COOKD=COOKD influence_flg=influence_flg) tip=(Label X Y COOKD influence_flg)
					;
		DiscreteLegend "SCATTER"/ title="influence_flg";
		endlayout;
		endgraph;
		end;
	run;

	ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
	proc sgrender data=&output_ds._reg_out(where=(model_status in ('M'))) template=sgplot; 
		title  "Infuence Results for Model &model. Segment &cluster. - TRAINING Group";
		title2 "After extreme influential observations were removed";
	run;
	ods graphics off;

	ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
	proc sgrender data=&output_ds._reg_out(where=(model_status in ('V','T'))) template=sgplot; 
		title  "Infuence Results for Model &model. Segment &cluster. - VALIDATION & TESTING Group";
		title2 "After extreme influential observations were removed";
	run;
	ods graphics off;

	ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;
	proc sgrender data=&output_ds._reg_out(where=(model_status in ('I','J') and rstudent~=.)) template=sgplot; 
		title "Infuence Results for Model &model. Segment &cluster. - EXTREME INFLUENTIAL OBSERVATIONS";
	run;
	ods graphics off;

	*ORIGINAL PLOTTING CODE;
	/*ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;*/
	/*proc sgplot data=&output_ds._reg_out; *tmplout="/mktg/cppma/data10/fair_price_fy16_prog/plot_test.txt";*/
	/*	title "Infuence Results for Model &model. Segment &cluster. - TRAINING Group";*/
	/*	scatter x=leverage y=rstudent / group=influence_flg markerattrs=(symbol=X) transparency=0.2;*/
	/*	where model_status in ('M');*/
	/*run;*/
	/*ods graphics off;*/
	/**/
	/*ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;*/
	/*proc sgplot data=&output_ds._reg_out;*/
	/*	title "Infuence Results for Model &model. Segment &cluster. - VALIDATION & TESTING Groups";*/
	/*	scatter x=leverage y=rstudent / group=influence_flg markerattrs=(symbol=X) transparency=0.2;*/
	/*	where model_status in ('V','T');*/
	/*run; */
	/*ods graphics off;*/
	/**/
	/*ods graphics on / IMAGEMAP=on LABELMAX=60000 TIPMAX=60000 DISCRETEMAX=60000;*/
	/*proc sgplot data=&output_ds._reg_out;*/
	/*	title "Infuence Results for Model &model. Segment &cluster. - INFLUENTIAL OBSERVATIONS REMOVED";*/
	/*	scatter x=leverage y=rstudent / group=influence_flg markerattrs=(symbol=X) transparency=0.1;*/
	/*	where model_status in ('I','J');*/
	/*run; */
	/*ods graphics off;*/

	%ucounts_ds(
		&output_ds._reg_out, 
		&profile_roll_up_var., 
		by_var = influence_flg, 
		filter = (model_status = 'M'),
		desc = Infuence Results for Model &model. Segment &cluster.  - TRAINING Group
		);	

	proc sort 
		data=&output_ds._reg_out (
				where = ((model_status = 'M') and (influence_flg ~= ''))
				keep =	&profile_roll_up_var. &cluster_var. &additional_vars.
						&target_var. model_status 
						pred res target_z 
						influence_flg COOKD DFFITS RSTUDENT LEVERAGE
				)
		out=_tmp_INFLUENCE; 
		by descending COOKD;
	run;

	proc print data=_tmp_INFLUENCE (obs=20) noobs;
		title "Infuence Results for Model &model. Segment &cluster. --- Worst COOKD Values in TRAINING Group (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z;
		where find(influence_flg,'COOKD')>0;
	run;

	data _tmp_INFLUENCE; 
		set _tmp_INFLUENCE; 
		abs_DFFITS=abs(DFFITS);
	run;

	proc sort data=_tmp_INFLUENCE; by descending abs_DFFITS;

	proc print data=_tmp_INFLUENCE (obs=20) noobs;
		title "Infuence Results for Model &model. Segment &cluster. --- Worst DFFITS Values in TRAINING Group (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. influence_flg COOKD DFFITS RSTUDENT LEVERAGE target_z;
		where find(influence_flg,'DFFITS')>0;
	run;

	%delete_ds(_tmp_INFLUENCE);
	%delete_ds( &output_ds._reg_out_tmp); /**added by Meng**/

	data &output_ds._reg_out_tmp;
	set &output_ds._reg_out (where=(model_status in ('M')));
	run;

	%numobs (_tmp_model_y_obs, &output_ds._reg_out_tmp ); 
   
%if &_tmp_model_y_obs>0 %then %do;

	*QQ Plot for trainign and validation;
	%qqplot(
		&output_ds._reg_out (where=(model_status = 'M')), 
		res, 
		&profile_roll_up_var. &additional_vars., 
		list_num_obs=20, 
		desc=QQ Plot of Residuals for Model &model. Segment &cluster. - TRAINING Group
		);

	*Print customers with most negative residual (below fair price);
	data _tmp_RES2_Y;
		set  &output_ds._reg_out;
		where model_status in ('M');
		keep	&profile_roll_up_var. &cluster_var. &additional_vars.
				&target_var. model_status 
				pred res target_z 
				influence_flg COOKD DFFITS RSTUDENT LEVERAGE
				;
	run;
	

	
	proc sort data=_tmp_RES2_Y; by res; run;
	
	proc print data=_tmp_RES2_Y (obs=20) noobs;
		title "Customers with Most Negative Residual --- In TRAINING (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE RSTUDENT;
	run;


	proc sort data=_tmp_RES2_Y; by descending res; run;
	
	proc print data=_tmp_RES2_Y (obs=20) noobs;
		title "Customers with Most Positive Residual --- In TRAINING (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE RSTUDENT;
	run;

%end;
	%delete_ds( &output_ds._reg_out_tmp); /**added by Meng**/

	data &output_ds._reg_out_tmp;
	set &output_ds._reg_out (where=(model_status in ('V','T')));
	run;

	%numobs (_tmp_model_v_obs, &output_ds._reg_out_tmp ); 
   
%if &_tmp_model_v_obs>0 %then %do;

	%qqplot(
		&output_ds._reg_out (where=(model_status in ('V','T'))), 
		res, 
		&profile_roll_up_var. &additional_vars., 
		list_num_obs=20, 
		desc=QQ Plot of Residuals for Model &model. Segment &cluster. - VALIDATION/TESTING Group
		);

  	data _tmp_RES2_V;
		set  &output_ds._reg_out;
		where model_status in ('V','T');
		keep	&profile_roll_up_var. &cluster_var. &additional_vars.
				&target_var. model_status 
				pred res target_z 
				influence_flg COOKD DFFITS RSTUDENT LEVERAGE
				;
	run;
	proc sort data=_tmp_RES2_V; by res; run;

	proc print data=_tmp_RES2_V (obs=20) noobs;
		title "Customers with Most Negative Residual --- In TESTING & VALIDAITON (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE RSTUDENT;
	run;
   	proc sort data=_tmp_RES2_V; by descending res; run;

	proc print data=_tmp_RES2_V (obs=20) noobs;
		title "Customers with Most Positive Residual --- In TESTING & VALIDAITON (max=20)";
		var model_status &profile_roll_up_var. &additional_vars. &target_var. pred res target_z influence_flg COOKD LEVERAGE RSTUDENT;
	run;


%end;

	%delete_ds( &output_ds._reg_out_tmp);


	
	%delete_ds(_tmp_RES2_Y _tmp_RES2_V);
		
%mend segment_model_check_results;




