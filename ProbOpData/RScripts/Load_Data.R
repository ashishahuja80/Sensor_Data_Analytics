#**************************************************************************************
# STEP_03_PROJECT_TEMPLATE_for_analysis_of_Power_Plants_R
#**************************************************************************************

# COMMENTS
{
  # SALI_Analysis_of_Plant_Data = function(INPUT_Turbine_Data_Explorer_CSV_Path,
  #                                                    INPUT_PROJECT_INPUT_TXT_FILE, INPUT_NUMBER_OF_LINES_SKIPPED,
  #                                                    INPUT_Project_Name, 
  #                                                    INPUT_Analysis_Title, 
  #                                                    INPUT_Analysis_Title_underscored, 
  #                                                    INPUT_VECTOR_VXX_Title_Type_Text, 
  #                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, 
  #                                                    INPUT_VECTOR_VXX, 
  #                                                    INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
  #                                                    INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
  #                                                    INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE,
  #                                                    INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE,
  #                                                    INPUT_LOGICAL_BDC_STRING,
  #                                                    INPUT_NUMBER_OF_EBCs,
  #                                                    INPUT_VECTOR_EBC,
  #                                                    INPUT_LOGICAL_EBC_STRING,
  #                                                    INPUT_VECTOR_REMOVE_NA,
  #                                                    INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data,
  #                                                    INPUT_VECTOR_UNIT, 
  #                                                    INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS,
  #                                                    INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES,
  #                                                    INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC, 
  #                                                    INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
  #                                                    INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
  #                                                    INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
  #                                                    INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
  #                                                    INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
  #                                                    INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_plus_minus_seconds_time_frame,
  #                                                    INPUT_MY_time_frame_string,
  #                                                    INPUT_1D_STATISTICS_create_TRUE_OR_FALSE, 
  #                                                    INPUT_1D_STATISTICS_TYPE_VECTOR, INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS ,
  #                                                    INPUT_VECTOR_breaks_01,
  #                                                    INPUT_VECTOR_breaks_02,
  #                                                    INPUT_VECTOR_breaks_03,
  #                                                    INPUT_CREATE_EVENT_PERIOD_Values_TRUE_OR_FALSE,
  #                                                    INPUT_CREATE_TIME_CURVES_TRUE_OR_FALSE,
  #                                                    INPUT_PRESET_NUMBER_OF_TIME_SERIES_TYPE, 
  #                                                    INPUT_TIME_AXES_FORMAT,
  #                                                    INPUT_TIME_CURVES_my_width,
  #                                                    INPUT_TIME_CURVES_my_height,
  #                                                    INPUT_TIME_CURVES_my_left_oma_factor,
  #                                                    INPUT_TIME_CURVES_my_nbr_of_left_y_axes,
  #                                                    INPUT_TIME_CURVES_my_right_oma_factor,
  #                                                    INPUT_TIME_CURVES_my_nbr_of_right_y_axes,
  #                                                    INPUT_TIME_CURVES_my_left_y_axis_x_lines_position,
  #                                                    INPUT_TIME_CURVES_my_right_y_axis_x_lines_position,
  #                                                    INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines,
  #                                                    INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines,
  #                                                    INPUT_TIME_CURVES_y_axes_Pixel_Factor,
  #                                                    INPUT_MAXIMUM_NUMBER_OF_ALLOWED_LEFT_Y_AXES_PER_TIME_SERIES_PLOT,
  #                                                    INPUT_MAXIMUM_NUMBER_OF_ALLOWED_RIGHT_Y_AXES_PER_TIME_SERIES_PLOT,
  #                                                    INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor,
  #                                                    INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor,
  #                                                    INPUT_TIME_CURVES_plot_text_UNITS_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_01_X_Y_GRAPHS_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_02_X_Y_COUNT_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_02_X_Y_PROBABILITY_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_02_X_Y_KERNEL_DENSITY_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_02_X_Y_ECDF_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_02_X_Y_1_MINUS_ECDF_create_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS_TRUE_OR_FALSE, 
  #                                                    INPUT_2D_STATISTICS_04_X_Y_COUNT_SENSITIVITY_ANALYSIS_TRUE_OR_FALSE,
  #                                                    INPUT_2D_STATISTICS_05_P_GTGT_CUMULATIVE_create_TRUE_OR_FALSE,
  #                                                    INPUT_NUMBER_OF_INTERVALLS_FOR_2D_SENSITIVITY_ANALYSIS,
  #                                                    INPUT_2D_STATISTICS_20_CUMULATIVE_CONTOURS_DISTRIBUTIONS_create_TRUE_OR_FALSE,
  #                                                    INPUT_Correlation_Scatter_MAIN_COLOR_SEPARATION_FACTOR_GTE_0_LTE_1,
  #                                                    INPUT_Correlation_Scatter_SHIFT_ALPHA_FACTOR_GTE_0_LTE_1,
  #                                                    INPUT_set_rgb_1_2_value_LOW_between_0_and_1_DEFAULT_IS_1, 
  #                                                    INPUT_set_rgb_1_2_value_HIGH_between_0_and_1_DEFAULT_IS_0,
  #                                                    INPUT_OUTPUT_CLEANED_PROJECT_DATA)
  # {	
  
  #***************************************************************************************
  # HINTS
  #
  # V1 is YYYY-MM-DD 
  # V2 is hh-mm-ss
  #
  # V1 and V2 are always combined to MY_DATE , such that
  # V1 and V2 are always set to NULL
  #
  # -------------------------------------------------------------------------------------
  #
  # The next Columns V3, V4, Vn are the only-for-analysis-used-input-columns
  # 
  # Inputs:
  # 
  # e.g. in example Temple Plant columns V3, V4 , ... V12
  #
  # V3  := GT11 Normalized Load in %
  # V4  := GT11 MW Load in MW
  # V5  := HRSG11 HP BYP STEAM CTRL VLV in % - as condition < 0.5%
  # V6  := HRSG11 HRH STM BYP VLV in %       - as condition condition < 0.5%
  # V7  := GT12 Normalized Load in %
  # V8  := GT12 MW Load in MW
  # V9  := HRSG12 HP BYP STEAM CTRL VLV in % - as condition condition < 0.5%
  # V10 := HRSG12 HRH STM BYP VLV in %       - as condition condition < 0.5%
  # V11 := ST Load in MW
  # V12 := Plant Total MW Load in MW
  #
  # -------------------------------------------------------------------------------------
  # After this Columns Vn+1 , Vn+2, .... Vm are the condition-columns
  #
  # Conditions:
  # 
  # e.g. in example Temple Plant column V13 , condition : plantmaster Selected 0/1
  #
  # V13 := Plant Master - Selected 0/1       - as condition 0->1
  #
  # for each Condition there are Condition types to be selected, availabe condiation-types:
  # 	GT_LT : greater-then-less-then
  #	B_SWITCH_01: binary-switch 0->1
  #	B_SWITCH_10: binary-switch 1->0
  #
  # e.g. in example Temple Plant column V13 , condition : plantmaster Selected 0/1
  #	SWITCH_01
  #	
  # -------------------------------------------------------------------------------------
  #
  # INPUT_NUMBER_OF_LAST_but_UNUSUABLE_COLUMN ist 14 for PROJECT DATA
  # V14 := last (unusuable) column
  #
  #****************************************************************************************
}

#===========================================================================
# if you want to reuse the last data 
{
  #===========================================================================
  # INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data =
  #     "keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data"
  # then these preparation steps can be skipped
  # 
  # otherwise if
  # INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data =
  #     "reload_complete_data"
  # then these preparation steps must be processed
  #===========================================================================
}
# - then
{
  #   => Prepare Data for analysis / create directories etc.
  #
  #   => General Preparations:
  #     - Read file
  #     - evtly. delete LAST but UNUSUABLE COLUMN
  #     - only keep columns of INPUT_VECTOR_VXX
  #     - remove (case based) ERRORS in PROJECT_DATA_VXX
  #     - combine Date-Time to Data-Time_entries
  #
  #   => find 
  #     NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
  #     COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR
  #
  #   => find
  #     k_MY_DATE_COLUMN
}
#===========================================================================
VXX_EVENT_START_DATA<-list()
# START_EVENT_TIME<-list()
PROJECT_EVENT_DATA<-list()
VXX_EVENT_END_DATA<-list()
# END_EVENT_TIME<-list()

if (INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data == "reload_complete_data")
{
  #mkdirs(Correlations_03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS_path)
  #mkdirs(Correlations_04_X_Y_COUNT_SENSITIVITY_ANALYSIS_path)
  
  setwd(Input_Turbine_DATA_Explorer_CSV_Path)
  
  print("Data loading, conversion of Date and other checks")
  st<-Sys.time()
  
  # evtly. check 
  # INPUT_PROJECT_INPUT_TXT_FILE
  
  #------------------------------------------------------------------------------------
  # 01: General Preparations: 
  #  - Read file 
  #  - evtly. delete LAST but UNUSUABLE COLUMN
  #  - combine Date-Time to Data-Time_entries
  #------------------------------------------------------------------------------------
  {
    #....................................
    # 01: Read file 
    #....................................
    {
      header_info <- fread(INPUT_PROJECT_INPUT_TXT_FILE,
                           header=TRUE,
                           nrows=2,
                           skip = 0,
                           dec=".",
                           na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),
                           blank.lines.skip=TRUE,
                           showProgress = TRUE,
                           verbose = FALSE)  
      
      txt_file_source<-NULL
      time_check_tde<-length(which(colnames(header_info)=="Time:")) #checks for time column coming in TDE txt file
      date_check_tde<-length(which(colnames(header_info)=="Date:")) # checks for Date column coming in TDE txt file
      time_check_probopdata<-length(which(colnames(header_info)=="Time")) # checks for Time column coming in ProbopData txt output
      mydate_col<-length(which(colnames(header_info)=="MYDATE"))#checks for presence of MYDATE column (only for temporary use)
      if ((time_check_tde>0)&(date_check_tde>0)){
        txt_file_source<-"TDE"
        INPUT_NUMBER_OF_LINES_SKIPPED<-2
        columnstoSkip<-2 # Date and Time column
      }else if(time_check_probopdata>0){
        txt_file_source<-"ProbOpData"
        INPUT_NUMBER_OF_LINES_SKIPPED<-1
        columnstoSkip<-1 # Time column
      }else if (mydate_col>0){
        txt_file_source<-"Shiny" #represents txt files that were loaded in Shiny once and dataset saved as txt again for temporary use
      }else {
        showNotification("Unrecognized txt file.",duration=2)
        return()
      }
      
      PROJECT_DATA_VXX <- fread(INPUT_PROJECT_INPUT_TXT_FILE, 
                                skip = INPUT_NUMBER_OF_LINES_SKIPPED, 
                                dec=".", 
                                na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE )
      
      gc()
      
      #str(PROJECT_DATA_VXX)
      #summary(PROJECT_DATA_VXX)
      #head(PROJECT_DATA_VXX)
      #tail(PROJECT_DATA_VXX)
    }
    #summary(PROJECT_DATA_VXX)
    
    #---------------------------------------------
    # evtly delete LAST but superfluous COLUMN
    # of
    # => 01: Read file 
    #
    # explanation/comment to the 2 cases:
    {
      #
      # there are two cases:
      # case 1: PROJECT_DATA_VXX contains 
      #         INPUT_MY_NUMBER_OF_INPUTS columns 
      #         plus one date column
      #         plus one time column
      #         plus one superfluous empty column
      #
      #         i.e. PROJECT_DATA_VXX contains
      #         INPUT_MY_NUMBER_OF_INPUTS + 3 columns
      #         => then this last empty column has to be removed
      #
      # case 2: PROJECT_DATA_VXX contains 
      #         INPUT_MY_NUMBER_OF_INPUTS columns 
      #         plus one date column
      #         plus one time column
      #
      #         i.e. PROJECT_DATA_VXX contains
      #         INPUT_MY_NUMBER_OF_INPUTS + 2 columns
      #         => nothing has to be removed !
      #
    }
    #---------------------------------------------
    if (txt_file_source=="TDE"){
      #--- case 1 : last empty column has to be removed ------
      if(dim(PROJECT_DATA_VXX)[2] == INPUT_MY_NUMBER_OF_INPUTS + 3)
      {
        mycol = paste("V" , dim(PROJECT_DATA_VXX), sep="")[2]
        PROJECT_DATA_VXX[,c(mycol):=NULL]
        gc()
      }
    }
    #summary(PROJECT_DATA_VXX)
    #head(PROJECT_DATA_VXX)
    
    #..................................................
    #  combine Date-Time to Data-Time_entries
    # of
    # => 01: Read file 
    #..................................................
    if (txt_file_source=="TDE"){
      PROJECT_DATA_VXX[, MY_DATE:= ymd_hms(paste(V1,V2,sep=" "))]
      PROJECT_DATA_VXX[,V1:= NULL]
      PROJECT_DATA_VXX[,V2:= NULL]
    }else if(txt_file_source=="ProbOpData"){
      PROJECT_DATA_VXX[, MY_DATE:= ymd_hms(V1)]
      PROJECT_DATA_VXX[,V1:= NULL]
    }
    gc()
    #summary(PROJECT_DATA_VXX)
    
    #=======================================================================
    # 23.08.2017
    #---------------------------------------------------------------------------
    # re-enumerate columns in PROJECT_DATA_VXX and in INPUT_VECTOR_VXX
    #   V3 V4 V5 .... Vn+3 , NOT  MY_DATE
    # to
    #   V1 V2 V3 .... Vn   , kept MY_DATE
    #---------------------------------------------------------------------------
    for (j in 1:dim(PROJECT_DATA_VXX)[2])
    {
      
      if(colnames(PROJECT_DATA_VXX)[j] != "MY_DATE")
      {
        colnames(PROJECT_DATA_VXX)[j] = paste("V", j, sep="")
        INPUT_VECTOR_VXX[j]= paste("V", j, sep="")
      }
      
    }
    
    
    #COPY_OF_READ_PROJECT_DATA_VXX=copy(PROJECT_DATA_VXX)
    
    #.......................................................
    # Create Data_Quality-files for 01: Read file 
    #.......................................................
    {
    #   TOTAL_OPERATION_HOURS_OF_STEP_FILE = "01_TOTAL_OPERATION_HOURS_OF_ALL_READ_DATA.csv"
    #   DATA_SUMMARY_OF_STEP_FILE= "01_DATA_SUMMARY_OF_ALL_READ_DATA.xlsx"
    #   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "01_VALIDATED_DATA_PER_PARAMETER_OF_ALL_READ_DATA.xlsx"
    #   
    #   TOTAL_OPERATION_HOURS_OF_THIS_STEP =
    #     ifelse(exists('INPUT_MY_FREQUENCY'), 
    #            my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                     INPUT_VECTOR_VXX,
    #                                     TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                     DATA_SUMMARY_OF_STEP_FILE,
    #                                     VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                     BASIC_Project_path,
    #                                     INPUT_MY_FREQUENCY
    #            ), 
    #            my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                     INPUT_VECTOR_VXX,
    #                                     TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                     DATA_SUMMARY_OF_STEP_FILE,
    #                                     VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                     BASIC_Project_path
    #            ))
    }
    
  }    
  
  # ====================================================================================
  # 02: General Preparations: 
  #  - only keep columns of INPUT_VECTOR_VXX
  #------------------------------------------------------------------------------------
  # Create as small as possible Data-files for data analysis :
  #
  # only keep columns of INPUT_VECTOR_VXX
  #
  # normally you should organize the analysis with no superfluous columns in INPUT_VECTOR_VXX
  #
  # ====================================================================================
  {
    {
      my_len_of_INPUT_VECTOR_VXX <- length(INPUT_VECTOR_VXX)
      my_len_of_PROJECT_DATA_VXX = dim(PROJECT_DATA_VXX)[2]
      
      mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS = ""
      
      #===============================================================================
      # 23.08.2017
      #===============================================================================
      
      for(i in 1 :my_len_of_PROJECT_DATA_VXX[1])
      {
        if (colnames(PROJECT_DATA_VXX)[i] != "MY_DATE") 
        {
          for(j in 1 :my_len_of_INPUT_VECTOR_VXX)
          {
            if (colnames(PROJECT_DATA_VXX)[i] != INPUT_VECTOR_VXX[j])
            { 
              mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS_append = TRUE
              
            }else
            {
              mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS_append = FALSE
              break
            }
          }
          if(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS_append == TRUE)
          {
            if(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS == "")
            {
              mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS = colnames(PROJECT_DATA_VXX)[i]
            }else
            {
              mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS = c(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS,colnames(PROJECT_DATA_VXX)[i] )
            }
            
          }
          
        }
      }
      
      
      if(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS=="")
      {
        len_mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS = 0
      }else
      {
        len_mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS = length(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS)
      }
      
      #=======================================================================
      # exactly HERE
      #   unnecessary columns are REMOVED
      #=======================================================================
      if (len_mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS != 0)
      {
        for(i in 1 : len_mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS)
        {
          PROJECT_DATA_VXX[,c(mycol_vec_of_NOT_NEEDED_INPUT_VECTOR_VXX_PARTS[i]):=NULL]
        }
      }
      
      #=======================================================================
      # 23.08.2017
      #---------------------------------------------------------------------------
      # re-enumerate 
      #
      #     !!! AGAIN - after having removed unnecessary INPUT-Columns !!! 
      #
      # columns in PROJECT_DATA_VXX and in INPUT_VECTOR_VXX
      #   V3 V4 V5 .... Vn+3 , NOT  MY_DATE
      # to
      #   V1 V2 V3 .... Vn   , kept MY_DATE
      #---------------------------------------------------------------------------
      for (j in 1:dim(PROJECT_DATA_VXX)[2])
      {
        
        if(colnames(PROJECT_DATA_VXX)[j] != "MY_DATE")
        {
          colnames(PROJECT_DATA_VXX)[j] = paste("V", j, sep="")
          INPUT_VECTOR_VXX[j]= paste("V", j, sep="")
        }
        
      }
      
      gc()
      
    }
    #summary(PROJECT_DATA_VXX)
    #head(PROJECT_DATA_VXX)
    
    #COPY_OF_READ_PROJECT_DATA_VXX_without_superfluous_columns=copy(PROJECT_DATA_VXX)
    
    #.......................................................
    # Create Data_Quality-files for 02: Read file without superfluous columns in INPUT_VECTOR_VXX
    #.......................................................
    {
      # TOTAL_OPERATION_HOURS_OF_STEP_FILE = "02_TOTAL_OPERATION_HOURS_OF_ALL_READ_DATA_without_superfluous_columns.csv"
      # DATA_SUMMARY_OF_STEP_FILE= "02_DATA_SUMMARY_OF_ALL_READ_DATA_without_superfluous_columns.xlsx"
      # VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "02_VALIDATED_DATA_PER_PARAMETER_OF_ALL_READ_DATA_without_superfluous_columns.xlsx"
      # 
      # TOTAL_OPERATION_HOURS_OF_THIS_STEP =
      #   ifelse(exists('INPUT_MY_FREQUENCY'), 
      #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
      #                                   INPUT_VECTOR_VXX,
      #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
      #                                   DATA_SUMMARY_OF_STEP_FILE,
      #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
      #                                   BASIC_Project_path,
      #                                   INPUT_MY_FREQUENCY
      #          ), 
      #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
      #                                   INPUT_VECTOR_VXX,
      #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
      #                                   DATA_SUMMARY_OF_STEP_FILE,
      #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
      #                                   BASIC_Project_path
      #          ))
    }
  }   
  
  #..................................................
  # 03: General Preparations: 
  # remove (case based) ERRORS in PROJECT_DATA_VXX
  #-------------------------------------------------------
  # ERRORS are supposed to be the following entries
  #  NULL
  #  ERROR
  #  -1.#INF
  #   1.#INF
  #  NA
  #-------------------------------------------------------
  {
    {
      for(i in 1 :my_len_of_INPUT_VECTOR_VXX)
      {
        V_INDEX <- which(colnames(PROJECT_DATA_VXX)==INPUT_VECTOR_VXX[i])
        
        if(INPUT_VECTOR_REMOVE_NA[i] == "YES")
        {
          PROJECT_DATA_VXX <- PROJECT_DATA_VXX[!is.na(PROJECT_DATA_VXX[[V_INDEX]])]
        }
        #summary(PROJECT_DATA_VXX)
        gc()
      }
      
      #COPY_OF_PROJECT_DATA_VXX_AFTER_REMOVEMENT_OF_CASE_BASED_NA_DATA=copy(PROJECT_DATA_VXX)
      
      gc()
    }
    
    #.......................................................
    # Create Data_Quality-files for 03: Read file without superfluous columns and case based removed errors
    #.......................................................
    {
      # TOTAL_OPERATION_HOURS_OF_STEP_FILE = "03_TOTAL_OPERATION_HOURS_AFTER_REMOVEMENT_OF_CASE_BASED_NA_DATA.csv"
      # DATA_SUMMARY_OF_STEP_FILE= "03_DATA_SUMMARY_OF_DATA_AFTER_REMOVEMENT_OF_CASE_BASED_NA_DATA.xlsx"
      # VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "03_VALIDATED_DATA_PER_PARAMETER_OF_DATA_AFTER_REMOVEMENT_OF_CASE_BASED_NA_DATA.xlsx"
      # 
      # TOTAL_OPERATION_HOURS_OF_THIS_STEP =
      #   ifelse(exists('INPUT_MY_FREQUENCY'), 
      #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
      #                                   INPUT_VECTOR_VXX,
      #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
      #                                   DATA_SUMMARY_OF_STEP_FILE,
      #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
      #                                   BASIC_Project_path,
      #                                   INPUT_MY_FREQUENCY
      #          ), 
      #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
      #                                   INPUT_VECTOR_VXX,
      #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
      #                                   DATA_SUMMARY_OF_STEP_FILE,
      #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
      #                                   BASIC_Project_path
      #          ))
      
    }
    #summary(PROJECT_DATA_VXX)
    #head(PROJECT_DATA_VXX)
    
    
    
  }
  #str(PROJECT_DATA_VXX)
  #summary(PROJECT_DATA_VXX)
  TOTAL_OPERATION_HOURS_OF_THIS_STEP = (dim(PROJECT_DATA_VXX)[1]/3600) * INPUT_MY_FREQUENCY
  en<-Sys.time()
  print(en-st)
  
  # ====================================================================================
  # find 
  #   NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
  #   COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR
  #
  # find
  #   k_MY_DATE_COLUMN
  # ====================================================================================
  # {
  #   my_dim_of_PROJECT_DATA_XXX = dim(PROJECT_DATA_VXX)[2]
  #   
  #   if(INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS == 0)
  #   {
  #     my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS=0
  #   }else
  #   {  
  #     
  #     my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS   = length(INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS)
  #     
  #     my_outer_break = FALSE
  #     
  #     for(NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS in 1 :my_dim_of_PROJECT_DATA_XXX )
  #     {	
  #       
  #       if (my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS   == 1)	
  #       {
  #         if (colnames(PROJECT_DATA_VXX)[NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS] == INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS){break}
  #       }else
  #       {
  #         for(j in 1 : my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS  )
  #         {
  #           if (colnames(PROJECT_DATA_VXX)[NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS] == INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j])
  #           {	
  #             my_outer_break = TRUE
  #             break
  #           }
  #           # -- should stop as soon as found at j=1 because of hint (*1) --------
  #         }
  #         if(my_outer_break == TRUE){break}	
  #       }
  #     }
  #     
  #     
  #     for(k_MY_DATE_COLUMN in 1 :my_dim_of_PROJECT_DATA_XXX )
  #     {	
  #       
  #       if (colnames(PROJECT_DATA_VXX)[k_MY_DATE_COLUMN] == "MY_DATE" ){break}
  #     }
  #     
  #     COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR=""
  #     for(COLUMN_NBR_WITHIN_PROJECT_DATA_VXX in 1 :my_dim_of_PROJECT_DATA_XXX )
  #     {
  #       for(my_BOUNDARY_CONDITIONS_counter in 1: my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS)
  #       {
  #         if (colnames(PROJECT_DATA_VXX)[COLUMN_NBR_WITHIN_PROJECT_DATA_VXX] == 
  #             INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[my_BOUNDARY_CONDITIONS_counter])
  #         {
  #           if(COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR=="")
  #           {
  #             COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR = COLUMN_NBR_WITHIN_PROJECT_DATA_VXX
  #           }else
  #           {
  #             COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR = 
  #               c(COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR,COLUMN_NBR_WITHIN_PROJECT_DATA_VXX)
  #             
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  
  #NBR_OF_first_Column_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
  # COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR
  #k_MY_DATE_COLUMN
  
  # COPY_OF_PROJECT_DATA_VXX_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions=copy(PROJECT_DATA_VXX)
  
  gc()
  
}else
{
  #.......................................................
  # Create Data_Quality-files for 01: Read file 
  #.......................................................
  
  
  #.......................................................
  # Create Data_Quality-files for 02: Read file without superfluous columns in INPUT_VECTOR_VXX
  #.......................................................
  
  
  #.......................................................
  # Create Data_Quality-files for 03: Read file without superfluous columns and case based removed errors
  #.......................................................
  
  
  # PROJECT_DATA_VXX=copy(COPY_OF_PROJECT_DATA_VXX_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions)
  gc()
}

#summary(PROJECT_DATA_VXX)

#=======================================================================
# 17.03.2017 -> 23.08.2017
#---------------------------------------------------------------------------
# re-enumerate columns in PROJECT_DATA_VXX and in INPUT_VECTOR_VXX
#   V3 V4 V5 .... Vn+3 , NOT  MY_DATE
# to
#   V1 V2 V3 .... Vn   , kept MY_DATE
#---------------------------------------------------------------------------
# for (j in 1:dim(PROJECT_DATA_VXX)[2])
# {
#   
#   if(colnames(PROJECT_DATA_VXX)[j] != "MY_DATE")
#   {
#     colnames(PROJECT_DATA_VXX)[j] = paste("V", j, sep="")
#     INPUT_VECTOR_VXX[j]= paste("V", j, sep="")
#   }
#   
# }  

#summary(PROJECT_DATA_VXX)
gc()


#---------------------------------------------------------------------------
# set up DOMAIN BASED and EVENT BASED conditions & set up event-time-frames
#---------------------------------------------------------------------------
{
  
  #----------------------------------------------------------------------------------
  # SET UP MATHEMARICALLY DOMAIN BASED GENERAL BOUNDARY CONDITIONS
  # ---------------------------------------------------------------------------------
  # 04: DOMAIN BASED GENERAL BOUNDARY CONDITIONS are without date column
  # ---------------------------------------------------------------------------------
  {
    #---------------------------------------------------------------------------------------------------
    # 170305 :  V22-> V22A -> V22B -> V23D2
    #---------------------------------------------------------------------------


    ######################################
    # dynamic version
    #
    #   BDC_VECTOR
    #   my_read_INPUT_LOGICAL_BDC_STRING
    #   my_for_eval_INPUT_LOGICAL_BDC_STRING
    #
    #   my_for_eval_analyse_string
    #
    # in case of error: stop
    ######################################
    # if (INPUT_LOGICAL_BDC_STRING!="")###modification
      {
      # BDC_VECTOR=""
      # 
      # for(k in 1 : (my_dim_of_PROJECT_DATA_XXX - 1 ) )
      # {
      #   if(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE[k] == "GET")
      #   {
      #     if(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE[k] == "LET")
      #     {
      #       BDC_K = paste('(',
      #                     'PROJECT_DATA_VXX[[', k, ']]' ,
      #                     ' >= ',
      #                     "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[", k,']',
      #                     ' & ',
      #                     'PROJECT_DATA_VXX[[', k, ']]',
      #                     ' <= ',
      #                     "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[", k,']',
      #                     ')',
      #                     sep="")
      #     }else
      #     {
      # 
      #       BDC_K  = paste( '(',
      #                       'PROJECT_DATA_VXX[[', k, ']]' ,
      #                       ' >= ',
      #                       "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[", k,']',
      #                       ' & ',
      #                       'PROJECT_DATA_VXX[[', k, ']]',
      #                       ' < ',
      #                       "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[", k,']',
      #                       ')',
      #                       sep="")
      #     }
      #   }else
      #   {
      #     if(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE[k] == "LET")
      #     {
      # 
      #       BDC_K  = paste( '(',
      #                       'PROJECT_DATA_VXX[[', k, ']]' ,
      #                       ' > ',
      #                       "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[", k,']',
      #                       ' & ',
      #                       'PROJECT_DATA_VXX[[', k, ']]',
      #                       ' <= ',
      #                       "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[", k,']',
      #                       ')',
      #                       sep="")
      # 
      #     }else
      #     {
      #       BDC_K  = paste( '(',
      #                       'PROJECT_DATA_VXX[[', k, ']]' ,
      #                       ' > ',
      #                       "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[", k,']',
      #                       ' & ',
      #                       'PROJECT_DATA_VXX[[', k, ']]',
      #                       ' < ',
      #                       "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[", k,']',
      #                       ')',
      #                       sep="")
      # 
      #     }
      #   }
      # 
      # 
      #   BDC_VECTOR[k] = BDC_K
      # 
      #   gc()
      # }
      # 
      # #-------------------------------------
      # # get BDC1, BDC2, BDC3, ....
      # #-------------------------------------
      # for(k in 1 : (my_dim_of_PROJECT_DATA_XXX - 1 ) )
      # {
      #   my_for_eval_BC = paste( 'BDC', k , '=',
      #                           '\"' ,
      #                           BDC_VECTOR[k],
      #                           '\"',
      #                           sep="")
      # 
      #   eval(parse(text=my_for_eval_BC))
      # 
      # }
      # 
      # #-------------------------------------
      # # analyse INPUT_LOGICAL_BDC_STRING
      # #
      # #   my_read_INPUT_LOGICAL_BDC_STRING
      # #   my_for_eval_INPUT_LOGICAL_BDC_STRING
      # #
      # # in case of error: stop
      # #-------------------------------------
      # {
      #   string_lenght_of_INPUT_LOGICAL_BDC_STRING = nchar(INPUT_LOGICAL_BDC_STRING)
      # 
      #   my_read_INPUT_LOGICAL_BDC_STRING = ""
      #   my_for_eval_INPUT_LOGICAL_BDC_STRING = ""
      # 
      #   my_BDC =""
      # 
      #   last_char_was_BDC_related = FALSE
      # 
      #   for(i in 1 : string_lenght_of_INPUT_LOGICAL_BDC_STRING )
      #   {
      #     my_char=right(left(INPUT_LOGICAL_BDC_STRING,i),1)
      # 
      #     if(my_char=="(" | my_char==")" | my_char==" " |
      #        my_char=="&" | my_char=="|" | my_char=="x" | my_char=="o" | my_char=="r"
      #     )
      #     {
      #       if(last_char_was_BDC_related==FALSE)
      #       {
      #         my_read_INPUT_LOGICAL_BDC_STRING = paste(my_read_INPUT_LOGICAL_BDC_STRING, my_char, sep ="")
      #         my_for_eval_INPUT_LOGICAL_BDC_STRING = paste(my_for_eval_INPUT_LOGICAL_BDC_STRING, my_char, sep ="")
      #       }else
      #       {
      # 
      #         my_read_INPUT_LOGICAL_BDC_STRING = paste(my_read_INPUT_LOGICAL_BDC_STRING,
      #                                                  my_BDC,
      #                                                  my_char,
      #                                                  sep ="")
      # 
      #         my_for_eval_INPUT_LOGICAL_BDC_STRING = paste(my_for_eval_INPUT_LOGICAL_BDC_STRING,
      #                                                      eval(parse(text=my_BDC)),
      #                                                      my_char,
      #                                                      sep ="")
      #       }
      #     }
      # 
      # 
      #     if(my_char=="B")
      #     {
      #       my_BDC = my_char
      #       last_char_was_BDC_related = TRUE
      #     }else
      #     {
      #       last_char_was_BDC_related = FALSE
      #     }
      # 
      #     if(my_char=="D" |
      #        my_char=="C" |
      #        my_char=="0" |
      #        my_char=="1" |
      #        my_char=="2" |
      #        my_char=="3" |
      #        my_char=="4" |
      #        my_char=="5" |
      #        my_char=="6" |
      #        my_char=="7" |
      #        my_char=="8" |
      #        my_char=="9"
      #     )
      #     {
      #       my_BDC = paste(my_BDC, my_char, sep ="")
      #       last_char_was_BDC_related = TRUE
      #     }else
      #     {
      #       last_char_was_BDC_related = FALSE
      #     }
      # 
      #     if(last_char_was_BDC_related==TRUE & i==string_lenght_of_INPUT_LOGICAL_BDC_STRING)
      #     {
      #       my_read_INPUT_LOGICAL_BDC_STRING = paste(my_read_INPUT_LOGICAL_BDC_STRING,
      #                                                my_BDC,
      #                                                sep ="")
      # 
      #       my_for_eval_INPUT_LOGICAL_BDC_STRING = paste(my_for_eval_INPUT_LOGICAL_BDC_STRING,
      #                                                    eval(parse(text=my_BDC)),
      #                                                    sep ="")
      #     }
      # 
      #   }
      # 
      #   #---------------------------------------------------------------
      #   # check if INPUT_LOGICAL_BDC_STRING == my_read_INPUT_LOGICAL_BDC_STRING
      #   #
      #   # in case of error : stop !
      #   #---------------------------------------------------------------
      #   {
      #     if(INPUT_LOGICAL_BDC_STRING == my_read_INPUT_LOGICAL_BDC_STRING)
      #     {
      #       # ok
      #     }else
      #     {
      #       # error in analysis
      #       INPUT_LOGICAL_BDC_STRING_ERROR_MESSAGE =
      #         paste('INPUT_LOGICAL_BDC_STRING-ERROR:',
      #               "\n",
      #               'INPUT_LOGICAL_BDC_STRING=',
      #               "\n",
      #               INPUT_LOGICAL_BDC_STRING,
      #               "\n",
      #               '!=' ,
      #               "\n",
      #               my_read_INPUT_LOGICAL_BDC_STRING ,
      #               "\n",
      #               '=',
      #               'my_read_INPUT_LOGICAL_BDC_STRING',
      #               sep="")
      # 
      #       #cat(INPUT_LOGICAL_BDC_STRING_ERROR_MESSAGE)
      # 
      #       stop(INPUT_LOGICAL_BDC_STRING_ERROR_MESSAGE)
      # 
      #     }
      #   }
      # 
      # 
      # }
      # 
      # #--------------------------------------
      # # create my_for_eval_analyse_string
      # # and run analysis ...
      # #--------------------------------------
      # {
      #   #PROJECT_DATA_VXX_COPY=copy(PROJECT_DATA_VXX)
      # 
      #   INPUT_LOGICAL_BDC_STRING_START ="PROJECT_DATA_VXX <- PROJECT_DATA_VXX["
      #   INPUT_LOGICAL_BDC_STRING_END =",]"
      # 
      #   my_for_eval_analyse_string=paste(INPUT_LOGICAL_BDC_STRING_START,
      #                                    my_for_eval_INPUT_LOGICAL_BDC_STRING,
      #                                    INPUT_LOGICAL_BDC_STRING_END,
      #                                    sep="")
      # 
      #   eval(parse(text=my_for_eval_analyse_string))
      # }


    }
  #   
  }
  #str(PROJECT_DATA_VXX)
  #summary(PROJECT_DATA_VXX)
  
  #COPY_OF_PROJECT_DATA_VXX_WITH_DOMAIN_BASED_GENERAL_BOUNDARY_CONDITIONS=copy(PROJECT_DATA_VXX)
  
  #...............................................................................................................
  # Create Data_Quality-files for 04: TOTAL_OPERATION_HOURS_AFTER_SETTING_DOMAIN_BASED_BOUNDARY_CONDITIONS
  #...............................................................................................................
  {
    # TOTAL_OPERATION_HOURS_OF_STEP_FILE = "04_TOTAL_OPERATION_HOURS_AFTER_SETTING_DOMAIN_BASED_BOUNDARY_CONDITIONS.csv"
    # DATA_SUMMARY_OF_STEP_FILE= "04_DATA_SUMMARY_OF_DATA_AFTER_SETTING_DOMAIN_BASED_BOUNDARY_CONDITIONS.xlsx"
    # VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "04_VALIDATED_DATA_PER_PARAMETER_OF_DATA_AFTER_SETTING_DOMAIN_BASED_BOUNDARY_CONDITIONS.xlsx"
    # 
    # TOTAL_OPERATION_HOURS_OF_THIS_STEP =
    #   ifelse(exists('INPUT_MY_FREQUENCY'), 
    #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                   INPUT_VECTOR_VXX,
    #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                   DATA_SUMMARY_OF_STEP_FILE,
    #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                   BASIC_Project_path,
    #                                   INPUT_MY_FREQUENCY
    #          ), 
    #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                   INPUT_VECTOR_VXX,
    #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                   DATA_SUMMARY_OF_STEP_FILE,
    #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                   BASIC_Project_path
    #          ))
    
  }
  
  #----------------------------------------------------------------------------------
  # 05: SET UP EVENT BASED BOUNDARY CONDITIONS - 
  #
  # and create EVENT BASED PROJECT_DATA_VXX
  #
  # HINT: time consuming !! 
  # ---------------------------------------------------------------------------------
  {
    
    # #-----------------------------------------------------------------------------------
    # # if NO time series are needed, then the following ccalculations are NOT needed
    # # i.e.
    # #   following ccalculations are ONLY needed
    # #   if INPUT_CREATE_TIME_CURVES_TRUE_OR_FALSE==TRUE
    # #-----------------------------------------------------------------------------------
    # if((INPUT_CREATE_TIME_CURVES_TRUE_OR_FALSE==TRUE) & (INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS != 0))
    # {
    #   {
    #     
    #     my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS   <- length(INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS)
    #     
    #     #--------------------------------------------------------
    #     # if diff(MY_DATE) > 1, then some rows contained errors
    #     #
    #     # in case of a considered SWITCH from 0-> 1
    #     # these swiches will be ignored , if the allowed 
    #     #   INPUT_ALLOWED_SWITCH_ERROR_TOLERANCE (e.g. 1, 2,3,4, ...)
    #     # is exceeded
    #     #
    #     # I.e. ifINPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES  = 1, then switches will be ignored, if 
    #     # the time tifference between these switches was greater than 1 time units (seconds)
    #     #
    #     # Or e.g. ifINPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES = 10, then switches will be ignored, if 
    #     # the time tifference between these switches was greater than 10 seconds (seconds)
    #     #
    #     #--------------------------------------------------------
    #     
    #     PROJECT_DATA_VXX[, delta_DATE_TIME:= diff(MY_DATE) ]
    #     # PROJECT_DATA_VXX[, delta_DATE_TIME:= c(diff(MY_DATE),1) ] #modification
    #     PROJECT_DATA_VXX[, my_found_row_names:=row.names(PROJECT_DATA_VXX)]
    #     
    #     # PROJECT_DATA_VXX_copy = copy(PROJECT_DATA_VXX)
    #     
    #     #---------------------------------------------------------------------------------------------------------------
    #     # pocessing EVENT BASED BOUNDARY CONDITIONS
    #     # reducing PROJECT_DATA_VXX with respect to EVENT BASED BOUNDARY CONDITIONS 
    #     #   => PROJECT_DATA_VXX_copy = event_based_PROJECT_DATA_VXX
    #     #---------------------------------------------------------------------------------------------------------------
    #     for(j in 1 :my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS  )
    #     {
    #       PROJECT_DATA_VXX_copy = copy(PROJECT_DATA_VXX)
    #       #-------------------------------------------------------------------
    #       # pocessing EVENT BASED BOUNDARY CONDITIONS in step j ...
    #       #   => VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #       #   => VXX_VALUE_at_found_EVENT_END_TIME_POINT
    #       #-------------------------------------------------------------------
    #       {
    #         #-------------------------------------------------------------------
    #         # SWITCH_Condition 0->1 at time points i->i+1
    #         #-------------------------------------------------------------------
    #         if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "B_SWITCH_01")
    #         {
    #           
    #           my_diff_predecessor = PROJECT_DATA_VXX_copy[[
    #             COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR[j]]][1]
    #           
    #           #--------------------------------------------------------------------------------------------
    #           # diff(", INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] 
    #           # is the difference between i+1 -th and i -th CONSIDERED switch
    #           #
    #           # i.e. either
    #           #   -1   if SWITCH was 1->0
    #           #   0    if switch was 0->0
    #           #   1    if switch was 0->1
    #           #
    #           # for row nbr 1 the absolute switch value 
    #           #   my_diff_predecessor
    #           # is taken instead of diff(", INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] 
    #           #
    #           # ##############################################################
    #           
    #           my_for_eval <-  paste("PROJECT_DATA_VXX_copy[, delta_",
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] , 
    #                                 ":=c(my_diff_predecessor, 
    #                                 diff(", 
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], "))]", sep="")
    #           
    #           eval(parse(text=my_for_eval))
    #           
    #           #--------------------------------------------------------------------------------------------
    #           # condition
    #           #   == 1 effects filtering events from 0 -> 1 :  Difference of i+1 th minus i th switch is 1
    #           #
    #           # condition
    #           #  my_found_row_names > 1
    #           #
    #           # insures that in case that considered switch in first row 
    #           #   IS 1
    #           # that this event is IGNORED - of course it has: because there was no event before the first row !!!!
    #           # such that it is not possible, that there has been a sitch from 0 -> 1
    #           #
    #           # condition
    #           #  delta_DATE_TIME <=INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # 
    #           # insures, that e.g. switches 0-> 1 exceeding a time difference of tolareted secnds 
    #           #     >INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # will be IGNORED !
    #           #
    #           ####################################################################################################
    #           my_for_eval <-  paste("PROJECT_DATA_VXX_copy[delta_", 
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], 
    #                                 "==1", 
    #                                 " &  my_found_row_names > 1",
    #                                 " & delta_DATE_TIME <=INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES" ,
    #                                 ",]" ,
    #                                 sep="")
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT  <- eval(parse(text=my_for_eval))
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT$my_found_row_names)
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT <- VXX_ROWs_at_found_EVENT_END_TIME_POINT -1
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <- 
    #             PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_START_TIME_POINT == TRUE,]
    #           #---------------------------------------------------------------------------------------------------------------------
    #           # there are still large delta_DATE_TIME > INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # possible in VXX_VALUE_at_found_EVENT_START_TIME_POINT because of ERRORS 
    #           #
    #           # these records must be removed
    #           #
    #           # and then
    #           #
    #           # VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           #
    #           # must be adapted again !!
    #           #---------------------------------------------------------------------------------------------------------------------
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <-  VXX_VALUE_at_found_EVENT_START_TIME_POINT[delta_DATE_TIME <= INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES,]
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_START_TIME_POINT$my_found_row_names)
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT <- VXX_ROWs_at_found_EVENT_START_TIME_POINT + 1
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT <- PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_END_TIME_POINT == TRUE,]
    #           #--------------------------------------------------------------------------------------------
    #           
    #           VXX_EVENT_START_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           VXX_EVENT_END_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_END_TIME_POINT
    #           
    #           EVENT_START_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_START_TIME_POINT$MY_DATE
    #           EVENT_END_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE
    #           
    #         }
    #         
    #         
    #         #-------------------------------------------------------------------
    #         # SWITCH_Condition 1->0 at time points i->i+1
    #         #-------------------------------------------------------------------
    #         if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "B_SWITCH_10")
    #         {
    #           
    #           my_diff_predecessor = PROJECT_DATA_VXX_copy[[
    #             COLUMN_NBR_WITHIN_PROJECT_DATA_VXX_of_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_VECTOR[j]]][1]
    #           
    #           #--------------------------------------------------------------------------------------------
    #           # diff(", INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] 
    #           # is the difference between i+1 -th and i -th CONSIDERED switch
    #           #
    #           # i.e. either
    #           #   -1   if SWITCH was 1->0
    #           #   0    if switch was 0->0
    #           #   1    if switch was 0->1
    #           #
    #           # for row nbr 1 the absolute switch value 
    #           #   my_diff_predecessor
    #           # is taken instead of diff(", INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] 
    #           #
    #           # ##############################################################
    #           
    #           #--------------------------------------------------------------------------------------------
    #           my_for_eval <-  paste("PROJECT_DATA_VXX_copy[, delta_",
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] , 
    #                                 ":=c(my_diff_predecessor, diff(", 
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], "))]", sep="")
    #           
    #           eval(parse(text=my_for_eval))
    #           
    #           #--------------------------------------------------------------------------------------------
    #           # condition
    #           #   == -1 effects filtering events from 1 -> 0 :  Difference of i+1 th minus i th switch is -1
    #           #
    #           # condition
    #           #  my_found_row_names > 1
    #           #
    #           # insures that in case that considered switch in first row 
    #           #   IS 1
    #           # that this event is IGNORED - of course it has: because there was no event before the first row !!!!
    #           # such that it is not possible, that there has been a sitch from 0 -> 1
    #           #
    #           # condition
    #           #  delta_DATE_TIME <=INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # 
    #           # insures, that e.g. switches 0-> 1 exceeding a time difference of tolareted secnds 
    #           #     >INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # will be IGNORED !
    #           #
    #           ####################################################################################################
    #           
    #           my_for_eval <-  paste("PROJECT_DATA_VXX_copy[delta_", 
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], 
    #                                 "==-1", 
    #                                 " &  my_found_row_names > 1",
    #                                 " & delta_DATE_TIME <=INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES" ,
    #                                 ",]" ,
    #                                 sep="")
    #           
    #           
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT  <- eval(parse(text=my_for_eval))
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT$my_found_row_names)
    #           
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT <- VXX_ROWs_at_found_EVENT_END_TIME_POINT -1
    #           
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <- 
    #             PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_START_TIME_POINT == TRUE,]
    #           
    #           #---------------------------------------------------------------------------------------------------------------------
    #           # there are still large delta_DATE_TIME >INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES
    #           # possible in VXX_VALUE_at_found_EVENT_START_TIME_POINT because of ERRORS 
    #           #
    #           # these records must be removed
    #           #
    #           # and then
    #           #
    #           # VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           #
    #           # must be adapted again !!
    #           #---------------------------------------------------------------------------------------------------------------------
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <-
    #             VXX_VALUE_at_found_EVENT_START_TIME_POINT[delta_DATE_TIME <=INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_SWITCHES,]
    #           
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_START_TIME_POINT$my_found_row_names)
    #           
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT <- VXX_ROWs_at_found_EVENT_START_TIME_POINT + 1
    #           
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT <- 
    #             PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_END_TIME_POINT == TRUE,]
    #           
    #           #--------------------------------------------------------------------------------------------
    #           VXX_EVENT_START_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           VXX_EVENT_END_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_END_TIME_POINT
    #           
    #           EVENT_START_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_START_TIME_POINT$MY_DATE
    #           EVENT_END_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE
    #           
    #         }
    #         
    #         
    #         #-------------------------------------------------------------------
    #         # event based conditions
    #         #
    #         #       GT
    #         #       GET
    #         #       LT
    #         #       LET
    #         #
    #         #       GT_LT
    #         #       GET_LT
    #         #       GET_LET
    #         #       GT_LET
    #         #-------------------------------------------------------------------
    #         if(
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LT") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LET") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT_LT") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET_LT") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET_LET") |
    #           ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT_LET")
    #         )
    #         {
    #           
    #           #--------------------------------------------------------------------------------------------
    #           # condition
    #           #   PROJECT_DATA_VXX$INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] 
    #           #       GT
    #           #       GET
    #           #       LT
    #           #       LET
    #           #
    #           #       GT_LT
    #           #       GET_LT
    #           #       GET_LET
    #           #       GT_LET
    #           #   INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]
    #           #     
    #           #   effects filtering events 
    #           #
    #           #   e.g.:    PROJECT_DATA_VXX$V12>0
    #           #   
    #           #
    #           # condition
    #           #  my_found_row_names > 1
    #           #
    #           # insures that in case that considered switch in first row 
    #           #   IS 1
    #           # that this event is IGNORED - of course it has: because there was no event before the first row !!!!
    #           # such that it is not possible, that there has been a sitch from 0 -> 1
    #           #
    #           #
    #           ####################################################################################################
    #           
    #           if(
    #             ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT") |
    #             ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET") |
    #             ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LT") |
    #             ( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LET") )
    #           {        
    #             
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT"){my_condition =">"}
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET"){my_condition =">="}
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LT"){my_condition ="<"}
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "LET"){my_condition ="<="}
    #             
    #             my_for_eval = paste("PROJECT_DATA_VXX_REDUCED_BY_EVENT_BOUNDARY_CONDITIONS <-",
    #                                 "PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$",
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], 
    #                                 my_condition,
    #                                 INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j],
    #                                 " &  my_found_row_names > 1",
    #                                 ",", 
    #                                 "]", sep="")
    #           }else
    #           {
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT_LT")
    #             { 
    #               my_leftcondition =">"
    #               my_rightcondition ="<"
    #             }
    #             
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET_LT")
    #             { 
    #               my_leftcondition =">="
    #               my_rightcondition ="<"
    #             }
    #             
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GET_LET")
    #             { 
    #               my_leftcondition =">="
    #               my_rightcondition ="<="
    #             }
    #             
    #             
    #             if( INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j] == "GT_LET")
    #             { 
    #               my_leftcondition =">="
    #               my_rightcondition ="<="
    #             }
    #             
    #             my_for_eval = paste("PROJECT_DATA_VXX_REDUCED_BY_EVENT_BOUNDARY_CONDITIONS <-",
    #                                 "PROJECT_DATA_VXX_copy[",
    #                                 "(",
    #                                 "PROJECT_DATA_VXX_copy$",
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], 
    #                                 my_leftcondition,
    #                                 INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j],
    #                                 ")",
    #                                 " & ",
    #                                 "(",
    #                                 "PROJECT_DATA_VXX_copy$",
    #                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j], 
    #                                 my_leftcondition,
    #                                 INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j],
    #                                 ")",
    #                                 " & ",
    #                                 "(",
    #                                 " my_found_row_names > 1",
    #                                 ")",
    #                                 ",", 
    #                                 "]", sep="")
    #             
    #             
    #           }
    #           
    #           eval(parse(text=my_for_eval))  
    #           
    #           ##############################################################################################
    #           # determine VXX_VALUE_at_found_EVENT_END_TIME_POINT for the first time (first step)
    #           ##############################################################################################
    #           
    #           my_for_eval <-  paste("PROJECT_DATA_VXX_REDUCED_BY_EVENT_BOUNDARY_CONDITIONS[", 
    #                                 "delta_DATE_TIME >= INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC" ,
    #                                 ",]" ,
    #                                 sep="")
    #           
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT  <- eval(parse(text=my_for_eval))
    #           
    #           #-----------------------------------------------------------------------------
    #           # determine VXX_ROWs_at_found_EVENT_END_TIME_POINT
    #           #
    #           #-----------------------------------------------------------------------------
    #           # Explanation
    #           {
    #             ###############################################################################################
    #             #
    #             # based on
    #             # INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC
    #             #
    #             # example: if
    #             # INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC = 5
    #             #
    #             # then this means , that the time events are considered to be continuos as long as the 
    #             # time-difference from time slot to time slot is <= 5 seconds
    #             #
    #             # that means as long as time slot differences are <= 5, the related events are considered to belong to the same
    #             # time intervall. 
    #             #
    #             # as soon as a slot difference is greater then 5, a new time intervall begins.
    #             #
    #             # This rule was defined because in almost all the time the considered events are observed to happen in more or
    #             # less smooth time slots of one second within an event intervall.
    #             # But sometimes it happens that a signal is missing 2,3,4, or say maximum 5 seconds and then comes back again.
    #             # For exactly these situations the parameter
    #             #  INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC
    #             # was invented: it preserves, that associated events stay assiciated in time series if the dead time between the time slots is short enough.
    #             ######################################################################################################################
    #           }
    #           #-----------------------------------------------------------------------------
    #           
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT=""
    #           
    #           for (my_counter_k in 1: dim(VXX_VALUE_at_found_EVENT_END_TIME_POINT)[1])
    #           {
    #             
    #             if (VXX_VALUE_at_found_EVENT_END_TIME_POINT[my_counter_k]$delta_DATE_TIME <= 
    #                 INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC)
    #             {
    #               if(my_counter_k == 1)
    #               {
    #                 VXX_ROWs_at_found_EVENT_END_TIME_POINT =
    #                   as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT[my_counter_k]$my_found_row_names) + 1
    #               }else
    #               {
    #                 VXX_ROWs_at_found_EVENT_END_TIME_POINT =
    #                   c(VXX_ROWs_at_found_EVENT_END_TIME_POINT ,
    #                     as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT[my_counter_k]$my_found_row_names) + 1)
    #               }
    #             }else
    #             {
    #               if(my_counter_k == 1)
    #               {
    #                 VXX_ROWs_at_found_EVENT_END_TIME_POINT =
    #                   as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT[my_counter_k]$my_found_row_names) 
    #               }else
    #               {
    #                 VXX_ROWs_at_found_EVENT_END_TIME_POINT =
    #                   c(VXX_ROWs_at_found_EVENT_END_TIME_POINT ,
    #                     as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT[my_counter_k]$my_found_row_names) )
    #               }
    #               
    #             }
    #             
    #           }
    #           
    #           
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT <- 
    #             PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_END_TIME_POINT == TRUE,]
    #           
    #           #-----------------------------------------------------------------------------
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT=""
    #           
    #           for (my_counter_k in 1: dim(VXX_VALUE_at_found_EVENT_END_TIME_POINT)[1])
    #           {
    #             if(my_counter_k==1)
    #             {
    #               tmp_intervall_data_of_PROJECT_DATA_VXX <- 
    #                 PROJECT_DATA_VXX_REDUCED_BY_EVENT_BOUNDARY_CONDITIONS[MY_DATE <= 
    #                                                                         VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE[1],]
    #               
    #               VXX_ROWs_at_found_EVENT_START_TIME_POINT=
    #                 as.integer(tmp_intervall_data_of_PROJECT_DATA_VXX$my_found_row_names[1])
    #               
    #             }else
    #             {
    #               tmp_intervall_data_of_PROJECT_DATA_VXX <- 
    #                 PROJECT_DATA_VXX_REDUCED_BY_EVENT_BOUNDARY_CONDITIONS[MY_DATE > 
    #                                                                         VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE[my_counter_k-1]
    #                                                                       & MY_DATE  <= 
    #                                                                         VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE[my_counter_k],]
    #               
    #               VXX_ROWs_at_found_EVENT_START_TIME_POINT =
    #                 c(VXX_ROWs_at_found_EVENT_START_TIME_POINT,
    #                   as.integer(tmp_intervall_data_of_PROJECT_DATA_VXX$my_found_row_names[1]))
    #             }
    #           }
    #           
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <- 
    #             PROJECT_DATA_VXX_copy[PROJECT_DATA_VXX_copy$my_found_row_names %in% VXX_ROWs_at_found_EVENT_START_TIME_POINT == TRUE,]
    #           
    #           #--------------------------------------------------------------------
    #           # define common_row_names_of_EVENT_START_AND_TIME_POINTs
    #           #--------------------------------------------------------------------
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT[, common_row_names_of_EVENT_START_AND_TIME_POINTs:=row.names(VXX_VALUE_at_found_EVENT_START_TIME_POINT)]
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT[, common_row_names_of_EVENT_START_AND_TIME_POINTs:=row.names(VXX_VALUE_at_found_EVENT_END_TIME_POINT)]
    #           
    #           #---------------------------------------------------------------------------------------------------------------------
    #           # there are still large delta_DATE_TIME > INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC
    #           # possible in VXX_VALUE_at_found_EVENT_START_TIME_POINT because of ERRORS 
    #           #
    #           # these records must be removed
    #           #
    #           # and then
    #           #
    #           # VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           #
    #           # must be adapted again !!
    #           #---------------------------------------------------------------------------------------------------------------------
    #           VXX_VALUE_at_found_EVENT_START_TIME_POINT <-
    #             VXX_VALUE_at_found_EVENT_START_TIME_POINT[delta_DATE_TIME <= 
    #                                                         INPUT_ALLOWED_EVENT_BASED_BOUNDARY_CONDITIONS_ERROR_TOLERANCE_in_seconds_for_GT_LT_ETC,]
    #           
    #           VXX_ROWs_at_found_EVENT_START_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_START_TIME_POINT$my_found_row_names)
    #           
    #           #VXX_VALUE_at_found_EVENT_END_TIME_POINT$common_row_names_of_EVENT_START_AND_TIME_POINTs
    #           #VXX_VALUE_at_found_EVENT_START_TIME_POINT$common_row_names_of_EVENT_START_AND_TIME_POINTs
    #           
    #           VXX_VALUE_at_found_EVENT_END_TIME_POINT <-
    #             VXX_VALUE_at_found_EVENT_END_TIME_POINT[
    #               VXX_VALUE_at_found_EVENT_END_TIME_POINT$common_row_names_of_EVENT_START_AND_TIME_POINTs %in% 
    #                 VXX_VALUE_at_found_EVENT_START_TIME_POINT$common_row_names_of_EVENT_START_AND_TIME_POINTs == TRUE,]
    #           
    #           VXX_ROWs_at_found_EVENT_END_TIME_POINT <- as.integer(VXX_VALUE_at_found_EVENT_END_TIME_POINT$my_found_row_names)
    #           
    #           #--------------------------------------------------------------------------------------------
    #           
    #           VXX_EVENT_START_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_START_TIME_POINT
    #           VXX_EVENT_END_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-VXX_VALUE_at_found_EVENT_END_TIME_POINT
    #           
    #           EVENT_START_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_START_TIME_POINT$MY_DATE
    #           EVENT_END_TIME_POINT_OF_VXX_VALUE = VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE
    #           
    #           
    #         }
    #       }    
    #       
    #       #------------------------------------------------------------------------------------
    #       # reducing PROJECT_DATA_VXX with respect to EVENT BASED BOUNDARY CONDITIONS of step j
    #       #   => PROJECT_DATA_VXX_copy = event_based_PROJECT_DATA_VXX
    #       #------------------------------------------------------------------------------------
    #       # explanation
    #       {
    #         # at start j=1 make copy
    #         #  reduced_PROJECT_DATA_VXX = copy(PROJECT_DATA_VXX)
    #         # 
    #         # combine for fragmented time-intervalls
    #         #   i in 1 :dim(VXX_VALUE_at_found_EVENT_START_TIME_POINT)[1]
    #         #
    #         # EVENT BASED PROJECT_DATA_VXX per step i
    #         #   = reduced_PROJECT_DATA_VXX
    #         #
    #         # to actualized 
    #         #   event_based_PROJECT_DATA_VXX
    #         #
    #         # i.e. reduce PROJECT_DATA_VXX with respect to 
    #         # EVENT BASED BOUNDARY CONDITIONS of step j
    #         #   => event_based_PROJECT_DATA_VXX  at step j
    #         #      
    #         #
    #       }
    #       #-----------------------------------------------------------------------------------
    #       # this is the time consuming part !
    #       #------------------------------------------------------------------------------------
    #       {
    #         #-------------------------------------------------
    #         # determine event_based_PROJECT_DATA_VXX
    #         #-------------------------------------------------
    #         {
    #           
    #           event_based_PROJECT_DATA_VXX<-NULL
    #           for(i in 1 :dim(VXX_VALUE_at_found_EVENT_START_TIME_POINT)[1])
    #           {
    #             reduced_PROJECT_DATA_VXX <-	
    #               PROJECT_DATA_VXX_copy[
    #                 PROJECT_DATA_VXX_copy$MY_DATE >= (VXX_VALUE_at_found_EVENT_START_TIME_POINT$MY_DATE[i] - 
    #                                                     INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_plus_minus_seconds_time_frame) &
    #                   PROJECT_DATA_VXX_copy$MY_DATE <= (VXX_VALUE_at_found_EVENT_END_TIME_POINT$MY_DATE[i] + INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_plus_minus_seconds_time_frame) ,]
    #             
    #             if (i==1)
    #             {
    #               event_based_PROJECT_DATA_VXX = reduced_PROJECT_DATA_VXX
    #             }else
    #             {
    #               event_based_PROJECT_DATA_VXX = rbind(event_based_PROJECT_DATA_VXX , reduced_PROJECT_DATA_VXX )
    #             }
    #           }
    #         }
    #         
    #         PROJECT_DATA_VXX_copy = event_based_PROJECT_DATA_VXX
    #         PROJECT_EVENT_DATA[[INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS[j]]]<-event_based_PROJECT_DATA_VXX
    #         # PROJECT_EVENT_DATA[[j]]<-event_based_PROJECT_DATA_VXX
    #       }
    #     }
    #     
    #     #--------------------------------------------------------------------------------------------------------------
    #     # when all j=1 ... my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
    #     # are taken in account, 
    #     # then finally
    #     # assign  PROJECT_DATA_VXX = event_based_PROJECT_DATA_VXX
    #     #--------------------------------------------------------------------------------------------------------------
    #     # PROJECT_DATA_VXX_EVENT = event_based_PROJECT_DATA_VXX
    #     
    #     
    #   }
    # }else # 170222 :  V22-> V22A -> V22B
    # {
    #   my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS=0
    # }
    # 
    # # 170222 :  V22-> V22A -> V22B
    # if(INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS == 0)
    # { my_len_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS=0 }
    # 
  }
  
  #COPY_OF_PROJECT_DATA_VXX_WITH_EVENT_BASED_GENERAL_BOUNDARY_CONDITIONS=copy(PROJECT_DATA_VXX)
  
  #...............................................................................................................
  # Create Data_Quality-files for 05: TOTAL_OPERATION_HOURS_AFTER_SETTING_EVENT_BASED_BOUNDARY_CONDITIONS
  #...............................................................................................................
  {
    # TOTAL_OPERATION_HOURS_OF_STEP_FILE = "05_TOTAL_OPERATION_HOURS_AFTER_SETTING_EVENT_BASED_BOUNDARY_CONDITIONS.csv"
    # DATA_SUMMARY_OF_STEP_FILE= "05_DATA_SUMMARY_OF_DATA_AFTER_SETTING_EVENT_BASED_BOUNDARY_CONDITIONS.xlsx"
    # VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "05_VALIDATED_DATA_PER_PARAMETER_OF_DATA_AFTER_SETTING_EVENT_BASED_BOUNDARY_CONDITIONS.xlsx"
    # 
    # TOTAL_OPERATION_HOURS_OF_THIS_STEP =
    #   ifelse(exists('INPUT_MY_FREQUENCY'),
    #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                   INPUT_VECTOR_VXX,
    #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                   DATA_SUMMARY_OF_STEP_FILE,
    #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                   BASIC_Project_path,
    #                                   INPUT_MY_FREQUENCY
    #          ),
    #          my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
    #                                   INPUT_VECTOR_VXX,
    #                                   TOTAL_OPERATION_HOURS_OF_STEP_FILE,
    #                                   DATA_SUMMARY_OF_STEP_FILE,
    #                                   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
    #                                   BASIC_Project_path
    #          ))
  }
  
}

#summary(PROJECT_DATA_VXX)
gc()


#----------------------------------------------------------------------------------
# SET UP EQUATION_BASED_BOUNDARY_CONDITIONS
# ---------------------------------------------------------------------------------
{
  #----------------------------------------------------------------------------------
  # 06: EQUATION_BASED_BOUNDARY_CONDITIONS 
  # ---------------------------------------------------------------------------------
  {
    # if (INPUT_NUMBER_OF_EBCs !=0)
    # {
    #   ######################################
    #   # determine 
    #   #
    #   #   EBC1 , EBC2 , ... EBCn
    #   #   my_read_INPUT_LOGICAL_EBC_STRING
    #   #   my_for_eval_INPUT_LOGICAL_EBC_STRING
    #   # 
    #   #   my_for_eval_analyse_string
    #   #
    #   # in case of error: stop
    #   #
    #   # create my_for_eval_analyse_string
    #   # and run analysis ...
    #   ######################################
    #   {
    #     
    #     #-------------------------------------
    #     # get EBC1, EBC2, EBC3, ....
    #     #-------------------------------------
    #     for(k in 1 : INPUT_NUMBER_OF_EBCs )
    #     {
    #       my_for_eval_BC = paste( 'EBC', k , '=',
    #                               '\"' , 
    #                               INPUT_VECTOR_EBC[k],
    #                               '\"', 
    #                               sep="")
    #       
    #       eval(parse(text=my_for_eval_BC))
    #       
    #     }
    #     
    #     #-------------------------------------
    #     # analyse INPUT_LOGICAL_EBC_STRING
    #     #
    #     #   my_read_INPUT_LOGICAL_EBC_STRING
    #     #   my_for_eval_INPUT_LOGICAL_EBC_STRING
    #     #
    #     # in case of error: stop
    #     #-------------------------------------
    #     {      
    #       string_lenght_of_INPUT_LOGICAL_EBC_STRING = nchar(INPUT_LOGICAL_EBC_STRING)
    #       
    #       my_read_INPUT_LOGICAL_EBC_STRING = ""
    #       my_for_eval_INPUT_LOGICAL_EBC_STRING = ""
    #       
    #       my_EBC =""
    #       
    #       last_char_was_EBC_related = FALSE
    #       
    #       for(i in 1 : string_lenght_of_INPUT_LOGICAL_EBC_STRING )
    #       {
    #         my_char=right(left(INPUT_LOGICAL_EBC_STRING,i),1)
    #         
    #         if(my_char=="(" | my_char==")" | my_char==" " | 
    #            my_char=="&" | my_char=="|" | my_char=="x" | my_char=="o" | my_char=="r"
    #         )
    #         {
    #           if(last_char_was_EBC_related==FALSE)
    #           {
    #             my_read_INPUT_LOGICAL_EBC_STRING = paste(my_read_INPUT_LOGICAL_EBC_STRING, my_char, sep ="")
    #             my_for_eval_INPUT_LOGICAL_EBC_STRING = paste(my_for_eval_INPUT_LOGICAL_EBC_STRING, my_char, sep ="")
    #           }else
    #           {
    #             
    #             my_read_INPUT_LOGICAL_EBC_STRING = paste(my_read_INPUT_LOGICAL_EBC_STRING, 
    #                                                      my_EBC,
    #                                                      my_char, 
    #                                                      sep ="")
    #             
    #             my_for_eval_INPUT_LOGICAL_EBC_STRING = paste(my_for_eval_INPUT_LOGICAL_EBC_STRING, 
    #                                                          eval(parse(text=my_EBC)),
    #                                                          my_char, 
    #                                                          sep ="")
    #           }
    #         }
    #         
    #         
    #         if(my_char=="E")
    #         { 
    #           my_EBC = my_char
    #           last_char_was_EBC_related = TRUE
    #         }else
    #         {
    #           last_char_was_EBC_related = FALSE
    #         }
    #         
    #         if(my_char=="B" |
    #            my_char=="C" | 
    #            my_char=="0" |
    #            my_char=="1" |
    #            my_char=="2" |
    #            my_char=="3" |
    #            my_char=="4" |
    #            my_char=="5" |
    #            my_char=="6" |
    #            my_char=="7" |
    #            my_char=="8" |
    #            my_char=="9" 
    #         )
    #         { 
    #           my_EBC = paste(my_EBC, my_char, sep ="")
    #           last_char_was_EBC_related = TRUE
    #         }else
    #         {
    #           last_char_was_EBC_related = FALSE
    #         }
    #         
    #         if(last_char_was_EBC_related==TRUE & i==string_lenght_of_INPUT_LOGICAL_EBC_STRING)
    #         {
    #           my_read_INPUT_LOGICAL_EBC_STRING = paste(my_read_INPUT_LOGICAL_EBC_STRING, 
    #                                                    my_EBC,
    #                                                    sep ="")
    #           
    #           my_for_eval_INPUT_LOGICAL_EBC_STRING = paste(my_for_eval_INPUT_LOGICAL_EBC_STRING, 
    #                                                        eval(parse(text=my_EBC)),
    #                                                        sep ="")
    #         }
    #         
    #       }
    #       
    #       #--------------------------------------------------------------- 
    #       # check if INPUT_LOGICAL_EBC_STRING == my_read_INPUT_LOGICAL_EBC_STRING
    #       #
    #       # in case of error : stop !
    #       #---------------------------------------------------------------
    #       {
    #         if(INPUT_LOGICAL_EBC_STRING == my_read_INPUT_LOGICAL_EBC_STRING)
    #         {
    #           # ok 
    #         }else
    #         {
    #           # error in analysis
    #           INPUT_LOGICAL_EBC_STRING_ERROR_MESSAGE =
    #             paste('INPUT_LOGICAL_EBC_STRING-ERROR:',
    #                   "\n",
    #                   'INPUT_LOGICAL_EBC_STRING=',
    #                   "\n",
    #                   INPUT_LOGICAL_EBC_STRING, 
    #                   "\n",
    #                   '!=' , 
    #                   "\n",
    #                   my_read_INPUT_LOGICAL_EBC_STRING ,
    #                   "\n",
    #                   '=',
    #                   'my_read_INPUT_LOGICAL_EBC_STRING',
    #                   sep="")
    #           
    #           #cat(INPUT_LOGICAL_EBC_STRING_ERROR_MESSAGE)
    #           
    #           stop(INPUT_LOGICAL_EBC_STRING_ERROR_MESSAGE)
    #           
    #         }
    #       }
    #       
    #       
    #     }
    #     
    #     #--------------------------------------
    #     # create my_for_eval_analyse_string
    #     # and run analysis ...
    #     #--------------------------------------
    #     {
    #       #PROJECT_DATA_VXX_COPY=copy(PROJECT_DATA_VXX)
    #       
    #       INPUT_LOGICAL_EBC_STRING_START ="PROJECT_DATA_VXX <- PROJECT_DATA_VXX["
    #       INPUT_LOGICAL_EBC_STRING_END =",]"
    #       
    #       my_for_eval_analyse_string=paste(INPUT_LOGICAL_EBC_STRING_START, 
    #                                        my_for_eval_INPUT_LOGICAL_EBC_STRING, 
    #                                        INPUT_LOGICAL_EBC_STRING_END,
    #                                        sep="")
    #       
    #       eval(parse(text=my_for_eval_analyse_string))
    #     }
    #     
    #     
    #   }
    # }
    
  }
  #str(PROJECT_DATA_VXX)
  #summary(PROJECT_DATA_VXX)
  
  #COPY_OF_PROJECT_DATA_VXX_WITH_DOMAIN_BASED_GENERAL_BOUNDARY_CONDITIONS=copy(PROJECT_DATA_VXX)
  
  #...............................................................................................................
  # Create Data_Quality-files for 06: TOTAL_OPERATION_HOURS_AFTER_SETTING_EQUATION_BASED_BOUNDARY_CONDITIONS
  #...............................................................................................................
  {
  #   TOTAL_OPERATION_HOURS_OF_STEP_FILE = "06_TOTAL_OPERATION_HOURS_AFTER_SETTING_EQUATION_BASED_BOUNDARY_CONDITIONS.csv"
  #   DATA_SUMMARY_OF_STEP_FILE= "06_DATA_SUMMARY_OF_DATA_AFTER_SETTING_EQUATION_BASED_BOUNDARY_CONDITIONS.xlsx"
  #   VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE = "06_VALIDATED_DATA_PER_PARAMETER_OF_DATA_AFTER_SETTING_EQUATION_BASED_BOUNDARY_CONDITIONS.xlsx"
  #   
  #   TOTAL_OPERATION_HOURS_OF_THIS_STEP =
  #     ifelse(exists('INPUT_MY_FREQUENCY'), 
  #            my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
  #                                     INPUT_VECTOR_VXX,
  #                                     TOTAL_OPERATION_HOURS_OF_STEP_FILE,
  #                                     DATA_SUMMARY_OF_STEP_FILE,
  #                                     VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
  #                                     BASIC_Project_path,
  #                                     INPUT_MY_FREQUENCY
  #            ), 
  #            my_WRITE_DATA_QUALITIES( PROJECT_DATA_VXX,
  #                                     INPUT_VECTOR_VXX,
  #                                     TOTAL_OPERATION_HOURS_OF_STEP_FILE,
  #                                     DATA_SUMMARY_OF_STEP_FILE,
  #                                     VALIDATED_DATA_PER_PARAMETER_OF_STEP_FILE,
  #                                     BASIC_Project_path
  #            ))
  #   
  }
}
