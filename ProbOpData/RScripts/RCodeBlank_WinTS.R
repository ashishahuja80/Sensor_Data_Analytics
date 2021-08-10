#%
#
#/
#******************************************************
# HINTS
#
# V1 is YYYY-MM-DD
# V2 is hh-mm-ss
#
# V1 and V2 are always combined to MY_DATE , such that
# V1 and V2 are always set to NULL
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Inputs: 
#
# V1="11MPR30CT001_XQ01_C" = ambient temperature
# V2="11MBY10CE901_XQ01_MW" = active power
#
# -------------------------------------------------------------
# 
# Conditions:  !!!! have always to be read in file-columns AFTER Input-Columns !!!!
# 
# V3 := 
#************************************************************
#===========================================================================
# DEFINE PATHS
#===========================================================================
Input_Turbine_DATA_Explorer_CSV_Path = "C:/Shiny/Data/LV_Transformer/PL1029_Plock/"
INPUT_PROJECT_INPUT_H5_FILE = "PL1029_Plock.h5"
# INPUT_NUMBER_OF_LINES_SKIPPED = 2
INPUT_MY_FREQUENCY = 60
INPUT_START_DATE = ""
INPUT_END_DATE = ""
INPUT_WINTS_PLANTNAME = ""
INPUT_WINTS_DOWNLOAD_STATUS = ""
INPUT_CYCLIC_FILE_STATUS = ""
BASIC_Project_path = "C:/Shiny/Output/LV_Transformer/PL1029_Plock/"
#
#
#===========================================================================
# DEFINE Project Name and Titles
#===========================================================================
INPUT_Project_Name ="LV_Transformer"
INPUT_Analysis_Title ="PL1029_Plock"
INPUT_Analysis_Title_underscored = "PL1029_Plock"
#===========================================================================
#===========================================================================
# DEFINE KKS INPUTS
#===========================================================================
#
INPUT_MY_NUMBER_OF_INPUTS = 18
#
#
#
#===========================================================================
# DEFINE DOMAIN_BASED_BOUNDARY_CONDITIONS
#===========================================================================
#
#
#
#
#.................................
# DEFINE DOMAIN_BASED_CONDITION_TYPES
#
# possible INPUT_VECTOR_DOMAIN_LEFT_CONDITION_TYPEs
#  Greater Equal than = "GET" 
#  Greater than = "GT" 
#  DEFAULT => "GET" 
#
#
# possible INPUT_VECTOR_DOMAIN_RIGHT_CONDITION_TYPEs
#  LESS Equal than = "LET" 
#  LESS than = "LT" 
#  DEFAULT => "LET" 
#.................................
#
#
#===========================================================================
# Define logical Boundary-Conditions (BDC) string
# Logical Boundary conditions are named as
#   BDC1, BDC2, ... BDCN
#
# Logical Parameters are
#  AND  = &
#   OR  = |
#   xor(BDC2, BDC3)
#
# and of course brackets
#   (   
#   )
#
# Logical Boundary-Conditions DEFAULT String is
#   BDC1 & BDC2 & BDC3 & BDC4 & ... & BDCN
#
#===========================================================================
#
INPUT_LOGICAL_BDC_STRING = "BDC1 & BDC2 & BDC3 & BDC4 & BDC5 & BDC6 & BDC7"
#
#
#
#===========================================================================
# DEFINE EQUATION_BASED_BOUNDARY_CONDITIONS
#
# default
#  INPUT_NUMBER_OF_EBCs = 0
#===========================================================================
#
#
INPUT_NUMBER_OF_EBCs = 0
INPUT_LOGICAL_EBC_STRING = ""
#
#
#=============================================================================================
# define for which Parameters NA shall be REMOVED
#   this is to be done if cases are defined, where each case describes a vector of case associated variables
#
# DEFAULT 
#   INPUT_VECTOR_REMOVE_NA_V1 = "NO"
#   INPUT_VECTOR_REMOVE_NA_V2 = "NO"
#   INPUT_VECTOR_REMOVE_NA_V3 = "NO"
#   ...
#   INPUT_VECTOR_REMOVE_NA_Vn = "NO"
# #=============================================================================================
#===========================================================================
# DEFINE EVENT_BASED_BOUNDARY_CONDITIONS
#
# if INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS = 0 then
# no EVENT_BASED_BOUNDARY_CONDITIONS will be considered !
# Possibilies
#
# GT
# GET
# LT
# LET
#
# GT_LT
# GET_LT
# GET_LET
# GT_LET
#
# B_SWITCH_01
# B_SWITCH_10
#
#===========================================================================
#
INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS = 0
INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE = TRUE
#
#
#
#===========================================================================
# say 
# if you want to keep last loaded data with last boundary conditions
# or reload complete data ..
# 
#   INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data =
#     "keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data"
# or
#     "reload_complete_data"
#===========================================================================
INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data = "reload_complete_data"
#
#
#===========================================================================
# Set desired statistical evaluations
#===========================================================================
# ---- 1D -------------------------------------------
INPUT_1D_STATISTICS_create_TRUE_OR_FALSE = TRUE
INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS = 100
#
INPUT_Number_of_1D_STATISTICS_TYPEs = 19
#
INPUT_1D_STATISTICS_TYPE_1 = "FREQUENCY"
INPUT_1D_STATISTICS_TYPE_2 = "DENSITY"
INPUT_1D_STATISTICS_TYPE_3 = "PROBABILITY"
INPUT_1D_STATISTICS_TYPE_4 = "ECDF"
INPUT_1D_STATISTICS_TYPE_5 = "1-ECDF"
INPUT_1D_STATISTICS_TYPE_6 = "BoxPlot24hours"
INPUT_1D_STATISTICS_TYPE_7 = "BoxPlotWeekDays"
INPUT_1D_STATISTICS_TYPE_8 = "BoxPlotMonths"
INPUT_1D_STATISTICS_TYPE_9 = "BoxPlotCalenderWeeks"
INPUT_1D_STATISTICS_TYPE_10 = "BoxPlotSeasons"
INPUT_1D_STATISTICS_TYPE_11 = "BoxPlotBusinessDaysWeekends"
INPUT_1D_STATISTICS_TYPE_12 = "BoxPlotYears"
INPUT_1D_STATISTICS_TYPE_13 = "violinBoxPlot24hours"
INPUT_1D_STATISTICS_TYPE_14 = "violinBoxPlotMonths"
INPUT_1D_STATISTICS_TYPE_15 = "violinBoxPlotWeekDays"
INPUT_1D_STATISTICS_TYPE_16 = "violinBoxPlotBusinessDaysWeekends"
INPUT_1D_STATISTICS_TYPE_17 = "violinBoxPlotCalendarWeeks"
INPUT_1D_STATISTICS_TYPE_18 = "violinBoxPlotSeasons"
INPUT_1D_STATISTICS_TYPE_19 = "violinBoxPlotYears"
#
#
# ---- 2D -------------------------------------------
#
INPUT_2D_STATISTICS_create_TRUE_OR_FALSE = FALSE
#
INPUT_2D_STATISTICS_01_X_Y_GRAPHS_create_TRUE_OR_FALSE = TRUE
#
INPUT_2D_STATISTICS_02_X_Y_COUNT_create_TRUE_OR_FALSE = TRUE
#
INPUT_2D_STATISTICS_02_X_Y_PROBABILITY_create_TRUE_OR_FALSE  = TRUE
#
INPUT_2D_STATISTICS_02_X_Y_KERNEL_DENSITY_create_TRUE_OR_FALSE  = TRUE
#
INPUT_2D_STATISTICS_02_X_Y_ECDF_create_TRUE_OR_FALSE = TRUE
#
INPUT_2D_STATISTICS_02_X_Y_1_MINUS_ECDF_create_TRUE_OR_FALSE = TRUE
#
INPUT_2D_STATISTICS_03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS_TRUE_OR_FALSE  = FALSE
#
INPUT_2D_STATISTICS_04_X_Y_COUNT_SENSITIVITY_ANALYSIS_TRUE_OR_FALSE = FALSE
#
INPUT_2D_STATISTICS_05_P_GTGT_CUMULATIVE_create_TRUE_OR_FALSE = FALSE
#
INPUT_NUMBER_OF_INTERVALLS_FOR_2D_SENSITIVITY_ANALYSIS = 100
#
INPUT_2D_STATISTICS_20_CUMULATIVE_CONTOURS_DISTRIBUTIONS_create_TRUE_OR_FALSE = FALSE
#
#
#===============================================================================================
# switch on/off time series evaluation
#===============================================================================================
INPUT_CREATE_TIME_CURVES_TRUE_OR_FALSE = FALSE
#
INPUT_TIME_CURVES_plot_text_UNITS_TRUE_OR_FALSE = FALSE
#
INPUT_CREATE_EVENT_PERIOD_Values_TRUE_OR_FALSE = FALSE
#
#-----------------------------------------------------------
# INPUT_PRESET_NUMBER_OF_TIME_SERIES_TYPE 
#   ALL_TIME_INTERVALLS
#   ONLY_ONE_ALL_OVER_TIME_INTERVALL
#
# INPUT_TIME_AXES_FORMAT
#   YYMM
#   hhmm
#   hhmmss
#-----------------------------------------------------------
#
INPUT_PRESET_NUMBER_OF_TIME_SERIES_TYPE = "ALL_TIME_INTERVALLS"
#
INPUT_TIME_AXES_FORMAT = "hhmmss"
#
#----------------------------------------------------------
# Graphic Parameters
#----------------------------------------------------------
INPUT_MAXIMUM_NUMBER_OF_ALLOWED_LEFT_Y_AXES_PER_TIME_SERIES_PLOT = 3
#
INPUT_MAXIMUM_NUMBER_OF_ALLOWED_RIGHT_Y_AXES_PER_TIME_SERIES_PLOT = 3
#
INPUT_TIME_CURVES_my_left_oma_factor = 4
#
INPUT_TIME_CURVES_my_right_oma_factor  = 3
#
#
#-----------------------------------------------------------
#
#
#
########################################################################################################################
########################################################################################################################
########################################################################################################################
##
##
##  BLOCK OF FIXED PROGRAM-PARTS - no more entries necessary from here on .........
##
##
########################################################################################################################
########################################################################################################################
########################################################################################################################


{

##### superfluous - fixed anyway ##########################################
INPUT_Correlation_Scatter_MAIN_COLOR_SEPARATION_FACTOR_GTE_0_LTE_1 = 0.9
INPUT_Correlation_Scatter_SHIFT_ALPHA_FACTOR_GTE_0_LTE_1 = 1

INPUT_set_rgb_1_2_value_LOW_between_0_and_1_DEFAULT_IS_1 = 0.6
INPUT_set_rgb_1_2_value_HIGH_between_0_and_1_DEFAULT_IS_0 = 0.0


#####################################################################################################

# Setting up Time-curves Graphical INPUT
# "FIX": 
#   dependig on geneneral boundary conditions
#   dependig on event based boundary conditions
#####################################################################################################


############################################################
# general pre-settings
############################################################
INPUT_TIME_CURVES_my_width = 1800
INPUT_TIME_CURVES_my_height = 1000
INPUT_TIME_CURVES_y_axes_Pixel_Factor = 18750


##########################################################################
# setting up y axes for general boundary conditions on the LEFT side
##########################################################################
{

#---- get INPUT_TIME_CURVES_my_nbr_of_left_y_axes = INPUT_TIME_CURVES_my_left_oma_factor
  {
INPUT_TIME_CURVES_my_nbr_of_left_y_axes = 
      INPUT_MY_NUMBER_OF_INPUTS -
      INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS
  }
  
  ##### Original Version ############################################################
  # INPUT_TIME_CURVES_my_left_y_axis_x_lines_position <- c(0,4,8)
  # INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- c(-2,2,6)
  # INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- c(-0.0,1,2)
  ###################################################################################
  
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_left_y_axis_x_lines_position <- c(0,4,8)
  #----------------------------------------------------------------------
  {
    my_left_y_axis_x_lines_position_step_size=4
    my_left_y_axis_x_lines_position_step_size_counter=-4
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_left_y_axis_x_lines_position <- 0
      INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- -2
      INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- 0.0
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position = "INPUT_TIME_CURVES_my_left_y_axis_x_lines_position <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
      {
        my_left_y_axis_x_lines_position_step_size_counter=
          my_left_y_axis_x_lines_position_step_size_counter + my_left_y_axis_x_lines_position_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position , 
                  my_left_y_axis_x_lines_position_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position , 
                  my_left_y_axis_x_lines_position_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_left_y_axis_x_lines_position))
    }
    
  }
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- c(-2,2,6)
  #----------------------------------------------------------------------
  {
    my_left_y_axes_labelling_x_lines_step_size=4
    my_left_y_axes_labelling_x_lines_step_size_counter=-6
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- 0
      INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- -2
      INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- 0.0
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines = "INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
      {
        my_left_y_axes_labelling_x_lines_step_size_counter=
          my_left_y_axes_labelling_x_lines_step_size_counter + my_left_y_axes_labelling_x_lines_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines , 
                  my_left_y_axes_labelling_x_lines_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines , 
                  my_left_y_axes_labelling_x_lines_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_labelling_x_lines))
    }
  }
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- c(0,1,2)
  #----------------------------------------------------------------------
  {
    my_left_y_axes_header_text_x_coords_factor_step_size=1
    my_left_y_axes_header_text_x_coords_factor_step_size_counter=-1
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- 0
      INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- -2
      INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- 0.0
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor = "INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
      {
        my_left_y_axes_header_text_x_coords_factor_step_size_counter=
          my_left_y_axes_header_text_x_coords_factor_step_size_counter + my_left_y_axes_header_text_x_coords_factor_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_left_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor , 
                  my_left_y_axes_header_text_x_coords_factor_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor , 
                  my_left_y_axes_header_text_x_coords_factor_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_left_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_left_y_axes_header_text_x_coords_factor))
    }
  }
  
}

##########################################################################
# setting up y axes for event based boundary conditions on the RIGHT side
##########################################################################
{
  #---- get INPUT_TIME_CURVES_my_nbr_of_right_y_axes = INPUT_TIME_CURVES_my_right_oma_factor
  {
    INPUT_TIME_CURVES_my_nbr_of_right_y_axes = INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS
  }
  
  ##### Original Version ############################################################
  # INPUT_TIME_CURVES_my_right_y_axis_x_lines_position <- 0
  # INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- 0
  # INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- c(19.2,20.2)
  ###################################################################################
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_right_y_axis_x_lines_position <- c(0,4,8)
  #----------------------------------------------------------------------
  {
    my_right_y_axis_x_lines_position_step_size=4
    my_right_y_axis_x_lines_position_step_size_counter=-4
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_right_y_axis_x_lines_position <- 0
      INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- 0
      INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- 19.2
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position = "INPUT_TIME_CURVES_my_right_y_axis_x_lines_position <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
      {
        my_right_y_axis_x_lines_position_step_size_counter=
          my_right_y_axis_x_lines_position_step_size_counter + my_right_y_axis_x_lines_position_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position , 
                  my_right_y_axis_x_lines_position_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position , 
                  my_right_y_axis_x_lines_position_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_right_y_axis_x_lines_position))
    }
    
  }
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- c(0,4,8)
  #----------------------------------------------------------------------
  {
    my_right_y_axes_labelling_x_lines_step_size=4
    my_right_y_axes_labelling_x_lines_step_size_counter=-4
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- 0
      INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- 0
      INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- 19.2
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines = "INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
      {
        my_right_y_axes_labelling_x_lines_step_size_counter=
          my_right_y_axes_labelling_x_lines_step_size_counter + my_right_y_axes_labelling_x_lines_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines , 
                  my_right_y_axes_labelling_x_lines_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines , 
                  my_right_y_axes_labelling_x_lines_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_labelling_x_lines))
    }
  }
  
  #----------------------------------------------------------------------
  # INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- c(19.2,20.2,21.2)
  #----------------------------------------------------------------------
  {
    my_right_y_axes_header_text_x_coords_factor_step_size=1
    my_right_y_axes_header_text_x_coords_factor_step_size_counter=18.2
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes == 1)
    {
      INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- 0
      INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- 0
      INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- 19.2
    }else
    {
      my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor = "INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor <- c("
      for (INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter in 1:INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
      {
        my_right_y_axes_header_text_x_coords_factor_step_size_counter=
          my_right_y_axes_header_text_x_coords_factor_step_size_counter + my_right_y_axes_header_text_x_coords_factor_step_size
        
        if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes_counter < INPUT_TIME_CURVES_my_nbr_of_right_y_axes)
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor , 
                  my_right_y_axes_header_text_x_coords_factor_step_size_counter, "," , sep="")
        }
        else
        {
          my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor = 
            paste(my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor , 
                  my_right_y_axes_header_text_x_coords_factor_step_size_counter, ")" , sep="")
        }
      }
    }  
    
    if(INPUT_TIME_CURVES_my_nbr_of_right_y_axes > 1)
    {
      eval(parse(text=my_for_eval_INPUT_TIME_CURVES_my_right_y_axes_header_text_x_coords_factor))
    }
  }
  
}


#======================================================================================================
#GENERATE INPUT_VECTORS
#======================================================================================================
{
# INPUT_VECTOR_VXX
# INPUT_VECTOR_UNIT # Measurement Unit
# INPUT_VECTOR_PLANT_UNIT # plant unit eg. GT41
# INPUT_VECTOR_WINTS_DESCRIPTION
# INPUT_VECTOR_USERDEFINED_DESCRIPTION
# INPUT_VECTOR_SIGNAL_ID
# INPUT_VECTOR_START_DATE_WINTS
# INPUT_VECTOR_VXX_Title_Type_Text
# INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT 
# INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE
# INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE
# INPUT_VECTOR_breaks_01
# INPUT_VECTOR_breaks_02
# INPUT_VECTOR_breaks_03
#
#MY_INPUT_VECTOR
#MY_INPUT_VECTOR_UNIT
#======================================================================================================


############# normally automatically predefined in next for loop #####################


# MY_INPUT_VECTOR_V1= "V3"
# MY_INPUT_VECTOR_V2= "V4"
#....
# 
# MY_INPUT_VECTOR_UNIT_V1 = ""
# MY_INPUT_VECTOR_UNIT_V2 = ""
#....

##################################################

# INPUT_VECTOR_breaks_01_V1 = 20
# INPUT_VECTOR_breaks_01_V2 = 20
# ...
# 
# INPUT_VECTOR_breaks_02_V1 = 40
# INPUT_VECTOR_breaks_02_V2 = 40
# ...
# 
# INPUT_VECTOR_breaks_03_V1 = 200
# INPUT_VECTOR_breaks_03_V2 = 200
# INPUT_VECTOR_breaks_03_V3 = 200
# ...

  
  # INPUT_VECTOR_PLANT_UNIT # plant unit eg. GT41
  # INPUT_VECTOR_WINTS_DESCRIPTION
  # INPUT_VECTOR_USERDEFINED_DESCRIPTION
  # INPUT_VECTOR_SIGNAL_ID
  
##################################################

for (i in 1:INPUT_MY_NUMBER_OF_INPUTS)
{
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_V', i, '= ' , '\"', "V", i, '\"', sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_UNIT_V', i, '= ' , as.name(paste0("INPUT_VECTOR_MEASUREMENT_UNIT_V",i)), sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_PLANT_UNIT_V', i, '= ' , as.name(paste0("INPUT_VECTOR_PLANT_UNIT_V",i)) , sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_WINTS_DESCRIPTION_V', i, '= ' , as.name(paste0("INPUT_VECTOR_WINTS_DESCRIPTION_V",i)), sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_USERDEFINED_DESCRIPTION_V', i, '= ' , as.name(paste0("INPUT_VECTOR_USERDEFINED_DESCRIPTION_V",i)), sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_SIGNAL_ID_V', i, '= ' , as.name(paste0("INPUT_VECTOR_SIGNAL_ID_V",i)), sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  
  my_for_eval_MY_INPUT_VECTOR  = paste('MY_INPUT_VECTOR_START_DATE_WINTS_V', i, '= ' , as.name(paste0("INPUT_VECTOR_START_DATE_WINTS_V",i)), sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('INPUT_VECTOR_breaks_01_V', i, '=' , '20', sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('INPUT_VECTOR_breaks_02_V', i, '=' , '40', sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
  
  my_for_eval_MY_INPUT_VECTOR  = paste('INPUT_VECTOR_breaks_03_V', i, '=' , '200', sep="")
  eval(parse(text=my_for_eval_MY_INPUT_VECTOR))
}

my_for_eval_INPUT_VECTOR_VXX = "INPUT_VECTOR_VXX <- c("
my_for_eval_INPUT_VECTOR_UNIT = "INPUT_VECTOR_UNIT <- c("
my_for_eval_INPUT_VECTOR_PLANT_UNIT = "INPUT_VECTOR_PLANT_UNIT <- c("
my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION = "INPUT_VECTOR_WINTS_DESCRIPTION <- c("
my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION = "INPUT_VECTOR_USERDEFINED_DESCRIPTION <- c("
my_for_eval_INPUT_VECTOR_SIGNAL_ID = "INPUT_VECTOR_SIGNAL_ID <- c("
my_for_eval_INPUT_VECTOR_START_DATE_WINTS = "INPUT_VECTOR_START_DATE_WINTS <- c("

my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text = "INPUT_VECTOR_VXX_Title_Type_Text <- c("
my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT = "INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT <- c("
my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE = "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE <- c("
my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE = "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE <- c("

my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE = "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE <- c("
my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE = "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE <- c("
my_for_eval_INPUT_VECTOR_REMOVE_NA = "INPUT_VECTOR_REMOVE_NA <- c("

my_for_eval_INPUT_VECTOR_breaks_01 = "INPUT_VECTOR_breaks_01 = c("
my_for_eval_INPUT_VECTOR_breaks_02 = "INPUT_VECTOR_breaks_02 = c("
my_for_eval_INPUT_VECTOR_breaks_03 = "INPUT_VECTOR_breaks_03 = c("

for (i in 1:INPUT_MY_NUMBER_OF_INPUTS)
{
  if(i==INPUT_MY_NUMBER_OF_INPUTS)
  {
    my_for_eval_INPUT_VECTOR_VXX = paste(my_for_eval_INPUT_VECTOR_VXX , "MY_INPUT_VECTOR_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_UNIT = paste(my_for_eval_INPUT_VECTOR_UNIT , "MY_INPUT_VECTOR_UNIT_V", i , ")" , sep="")
    
    my_for_eval_INPUT_VECTOR_PLANT_UNIT = paste(my_for_eval_INPUT_VECTOR_PLANT_UNIT , "MY_INPUT_VECTOR_PLANT_UNIT_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION = paste(my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION , "MY_INPUT_VECTOR_WINTS_DESCRIPTION_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION = paste(my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION , "MY_INPUT_VECTOR_USERDEFINED_DESCRIPTION_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_SIGNAL_ID = paste(my_for_eval_INPUT_VECTOR_SIGNAL_ID , "MY_INPUT_VECTOR_SIGNAL_ID_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_START_DATE_WINTS = paste(my_for_eval_INPUT_VECTOR_START_DATE_WINTS , "MY_INPUT_VECTOR_START_DATE_WINTS_V", i , ")" , sep="")
    
    my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text  = paste(my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text  , "INPUT_VECTOR_VXX_Title_Type_Text_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT  = paste(my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT  , "INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE, "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE, "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V", i , ")" , sep="")

    my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE, "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE, "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE_V", i , ")" , sep="")
    
    my_for_eval_INPUT_VECTOR_REMOVE_NA = paste( my_for_eval_INPUT_VECTOR_REMOVE_NA, "INPUT_VECTOR_REMOVE_NA_V", i , ")" , sep="")
    
    my_for_eval_INPUT_VECTOR_breaks_01 = paste( my_for_eval_INPUT_VECTOR_breaks_01 , "INPUT_VECTOR_breaks_01_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_breaks_02 = paste( my_for_eval_INPUT_VECTOR_breaks_02 , "INPUT_VECTOR_breaks_02_V", i , ")" , sep="")
    my_for_eval_INPUT_VECTOR_breaks_03 = paste( my_for_eval_INPUT_VECTOR_breaks_03 , "INPUT_VECTOR_breaks_03_V", i , ")" , sep="")
    
  } else
  {
    my_for_eval_INPUT_VECTOR_VXX = paste(my_for_eval_INPUT_VECTOR_VXX , "MY_INPUT_VECTOR_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_UNIT = paste(my_for_eval_INPUT_VECTOR_UNIT , "MY_INPUT_VECTOR_UNIT_V", i , "," , sep="")
    
    my_for_eval_INPUT_VECTOR_PLANT_UNIT = paste(my_for_eval_INPUT_VECTOR_PLANT_UNIT , "MY_INPUT_VECTOR_PLANT_UNIT_V", i ,",", sep="")
    my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION = paste(my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION , "MY_INPUT_VECTOR_WINTS_DESCRIPTION_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION = paste(my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION , "MY_INPUT_VECTOR_USERDEFINED_DESCRIPTION_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_SIGNAL_ID = paste(my_for_eval_INPUT_VECTOR_SIGNAL_ID , "MY_INPUT_VECTOR_SIGNAL_ID_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_START_DATE_WINTS = paste(my_for_eval_INPUT_VECTOR_START_DATE_WINTS , "MY_INPUT_VECTOR_START_DATE_WINTS_V", i , "," , sep="")
    
    my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text  = paste(my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text  , "INPUT_VECTOR_VXX_Title_Type_Text_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT  = paste(my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT  , "INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE  , "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE  , "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V", i , "," , sep="")
    
    my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE  , "INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE = paste( my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE  , "INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE_V", i , "," , sep="")
    
    my_for_eval_INPUT_VECTOR_REMOVE_NA = paste( my_for_eval_INPUT_VECTOR_REMOVE_NA  , "INPUT_VECTOR_REMOVE_NA_V", i , "," , sep="")
    
    my_for_eval_INPUT_VECTOR_breaks_01 = paste( my_for_eval_INPUT_VECTOR_breaks_01  , "INPUT_VECTOR_breaks_01_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_breaks_02 = paste( my_for_eval_INPUT_VECTOR_breaks_02  , "INPUT_VECTOR_breaks_02_V", i , "," , sep="")
    my_for_eval_INPUT_VECTOR_breaks_03 = paste( my_for_eval_INPUT_VECTOR_breaks_03  , "INPUT_VECTOR_breaks_03_V", i , "," , sep="")
    
  }
  
}


eval(parse(text=my_for_eval_INPUT_VECTOR_VXX))
eval(parse(text=my_for_eval_INPUT_VECTOR_UNIT))

eval(parse(text=my_for_eval_INPUT_VECTOR_PLANT_UNIT))
eval(parse(text=my_for_eval_INPUT_VECTOR_WINTS_DESCRIPTION))
eval(parse(text=my_for_eval_INPUT_VECTOR_USERDEFINED_DESCRIPTION))
eval(parse(text=my_for_eval_INPUT_VECTOR_SIGNAL_ID))
eval(parse(text=my_for_eval_INPUT_VECTOR_START_DATE_WINTS))

eval(parse(text=my_for_eval_INPUT_VECTOR_VXX_Title_Type_Text))
eval(parse(text=my_for_eval_INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT))
eval(parse(text=my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE))
eval(parse(text=my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE))

eval(parse(text=my_for_eval_INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE))
eval(parse(text=my_for_eval_INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE))

eval(parse(text=my_for_eval_INPUT_VECTOR_REMOVE_NA))

eval(parse(text=my_for_eval_INPUT_VECTOR_breaks_01))
eval(parse(text=my_for_eval_INPUT_VECTOR_breaks_02))
eval(parse(text=my_for_eval_INPUT_VECTOR_breaks_03))


#--------- InPUT-Vector-BACK UP in case of errors --------------------------------------
# INPUT_VECTOR_VXX <- c( MY_INPUT_VECTOR_V1,
#MY_INPUT_VECTOR_V2,
#MY_INPUT_VECTOR_V3,
#MY_INPUT_VECTOR_V4,
#MY_INPUT_VECTOR_V5,
#MY_INPUT_VECTOR_V6,
#MY_INPUT_VECTOR_V7,
#MY_INPUT_VECTOR_V8,
#            MY_INPUT_VECTOR_V9,
#MY_INPUT_VECTOR_V10)
# 
# INPUT_VECTOR_UNIT <- c(MY_INPUT_VECTOR_UNIT_V1,
#                        MY_INPUT_VECTOR_UNIT_V2,
#                        MY_INPUT_VECTOR_UNIT_V3,
#                        MY_INPUT_VECTOR_UNIT_V4,
#                        MY_INPUT_VECTOR_UNIT_V5,
#                        MY_INPUT_VECTOR_UNIT_V6,
#                        MY_INPUT_VECTOR_UNIT_V7,
#                        MY_INPUT_VECTOR_UNIT_V8,
#                        MY_INPUT_VECTOR_UNIT_V9,
#                        MY_INPUT_VECTOR_UNIT_V10)
# 
# INPUT_VECTOR_VXX_Title_Type_Text <- c( INPUT_VECTOR_VXX_Title_Type_Text_V1,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V2,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V3,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V4,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V5,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V6,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V7,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V8,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V9,
#                                        INPUT_VECTOR_VXX_Title_Type_Text_V10)
# 
# INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT <- c( INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V1,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V2,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V3,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V4,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V5,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V6,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V7,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V8,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V9,
#                                                    INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V10)
# 
# INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE <- c(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V1,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V2,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V3,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V4,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V5,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V6,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V7,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V8,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V9,
#                            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V10)
# 
# INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE <- c(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V1,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V2,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V3,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V4,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V5,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V6,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V7,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V8,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V9,
#                            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V10)
# 
#         INPUT_VECTOR_breaks_01 = c( INPUT_VECTOR_breaks_01_V1,
#              INPUT_VECTOR_breaks_01_V2,
#              INPUT_VECTOR_breaks_01_V3,
#              INPUT_VECTOR_breaks_01_V4,
#              INPUT_VECTOR_breaks_01_V5,
#              INPUT_VECTOR_breaks_01_V6,
#              INPUT_VECTOR_breaks_01_V7,
#              INPUT_VECTOR_breaks_01_V8,
#              INPUT_VECTOR_breaks_01_V9,
#              INPUT_VECTOR_breaks_01_V10)
#         
#         INPUT_VECTOR_breaks_02 = 
#           c(INPUT_VECTOR_breaks_02_V1,
#             INPUT_VECTOR_breaks_02_V2,
#             INPUT_VECTOR_breaks_02_V3,
#             INPUT_VECTOR_breaks_02_V4,
#             INPUT_VECTOR_breaks_02_V5,
#             INPUT_VECTOR_breaks_02_V6,
#             INPUT_VECTOR_breaks_02_V7,
#             INPUT_VECTOR_breaks_02_V8,
#             INPUT_VECTOR_breaks_02_V9,
#             INPUT_VECTOR_breaks_02_V10)
#         
#         INPUT_VECTOR_breaks_03 = 
#           c( INPUT_VECTOR_breaks_03_V1,
#              INPUT_VECTOR_breaks_03_V2,
#              INPUT_VECTOR_breaks_03_V3,
#              INPUT_VECTOR_breaks_03_V4,
#              INPUT_VECTOR_breaks_03_V5,
#              INPUT_VECTOR_breaks_03_V6,
#              INPUT_VECTOR_breaks_03_V7,
#              INPUT_VECTOR_breaks_03_V8,
#              INPUT_VECTOR_breaks_03_V9,
#              INPUT_VECTOR_breaks_03_V10)
#-------------------------------------------------------------------------------------------------
}


#======================================================================================================
# GENERATE VECTORs of EVENT_BASED_BOUNDARY_CONDITIONS
#======================================================================================================
{
# INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
# INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
# INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
# INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS
#======================================================================================================

if(INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS != 0)
{
  my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = "INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS <- c("
  my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = "INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS <- c("
  my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = "INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS <- c("
  my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = "INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS <- c("
  
  for (i in 1:INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS)
  {
    if(i==INPUT_MY_NUMBER_OF_EVENT_BASED_CONDITIONS)
    {
      my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , ")" , sep="")
      my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , ")" , sep="")
      my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , ")" , sep="")
      my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , ")" , sep="")
    } else
    {
      my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , "," , sep="")
      my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , "," , sep="")
      my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , "," , sep="")
      my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = paste(my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS , "INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_", i , "," , sep="")
    }
  }
  eval(parse(text=my_for_eval_INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS))
  eval(parse(text=my_for_eval_INPUT_TYPE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS))
  eval(parse(text=my_for_eval_INPUT_LEFT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS))
  eval(parse(text=my_for_eval_INPUT_RIGHT_VALUE_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS))
}else
{ INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS = ""}

#--------- CONDITION-Vector-BACK UP in case of errors ---------------------------------------------
#INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS <- c(INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_1,
# INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS_2)
#---------------------------------------------------------------------------------------------------
}

#======================================================================================================
# GENERATE 1D_STATISTICS_TYPE_VECTOR
#======================================================================================================
{
# INPUT_1D_STATISTICS_TYPE_VECTOR
#======================================================================================================

my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR = "INPUT_1D_STATISTICS_TYPE_VECTOR = c("

for (i in 1:INPUT_Number_of_1D_STATISTICS_TYPEs)
{
  if(i==INPUT_Number_of_1D_STATISTICS_TYPEs)
  {
    my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR = paste(my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR , "INPUT_1D_STATISTICS_TYPE_", i , ")" , sep="")
  }else
  {
    my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR = paste(my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR , "INPUT_1D_STATISTICS_TYPE_", i , "," , sep="")
  }
}

if (INPUT_Number_of_1D_STATISTICS_TYPEs != 0)
{
  eval(parse(text=my_for_eval_INPUT_1D_STATISTICS_TYPE_VECTOR))
}else
{ INPUT_1D_STATISTICS_TYPE_VECTOR = ""}


#--------- INPUT_1D_STATISTICS_TYPE_VECTOR-BACK UP in case of errors ---------------------------------------------
# INPUT_1D_STATISTICS_TYPE_VECTOR = c( INPUT_1D_STATISTICS_TYPE_1,
#                                     INPUT_1D_STATISTICS_TYPE_2,
#                                     INPUT_1D_STATISTICS_TYPE_3,
#                                     INPUT_1D_STATISTICS_TYPE_4,
#                                     INPUT_1D_STATISTICS_TYPE_5,
#                                     INPUT_1D_STATISTICS_TYPE_6,
#                                     INPUT_1D_STATISTICS_TYPE_7,
#                                     INPUT_1D_STATISTICS_TYPE_8,
#                                     INPUT_1D_STATISTICS_TYPE_9,
#                                     INPUT_1D_STATISTICS_TYPE_10)

#======================================================================================================
# set remaining paths
#======================================================================================================

INPUT_VECTOR_1D_STATISTICS_path = paste(BASIC_Project_path , "1D_STATISTICS", "/", sep="")

INPUT_VECTOR_Time_Measurements_path = paste(BASIC_Project_path , "TIME_GRAPHS" , "/" , sep="")

INPUT_VECTOR_2D_STATISTICS_path = paste(BASIC_Project_path , "2D_STATISTICS", "/", sep="")

}

#======================================================================================================
#GENERATE INPUT_VECTOR_EBC
#======================================================================================================
{
  if (INPUT_NUMBER_OF_EBCs !=0)
  {
    my_for_eval_INPUT_VECTOR_EBC = "INPUT_VECTOR_EBC <- c("
    
    for (i in 1:INPUT_NUMBER_OF_EBCs)
    {
      
      if(i==INPUT_NUMBER_OF_EBCs)
      {
        my_for_eval_INPUT_VECTOR_EBC  = paste(my_for_eval_INPUT_VECTOR_EBC  , "EBC", i , ")" , sep="")
      }else
      {
        my_for_eval_INPUT_VECTOR_EBC  = paste(my_for_eval_INPUT_VECTOR_EBC  , "EBC", i , "," , sep="")
      }
    }
    
    eval(parse(text=my_for_eval_INPUT_VECTOR_EBC))
  }else
  {
    INPUT_VECTOR_EBC=""
    INPUT_LOGICAL_EBC_STRING=""
  }
}

##################################################################################################################
##################################################################################################################
##################################################################################################################
#
# CALLL function 
#
##################################################################################################################
##################################################################################################################
##################################################################################################################



# MY_PROJECT_DATA_XXX =  SALI_Analysis_of_Plant_Data(INPUT_Turbine_Data_Explorer_CSV_Path,
#                                                    INPUT_PROJECT_INPUT_TXT_FILE, INPUT_NUMBER_OF_LINES_SKIPPED,
#                                                    INPUT_VECTOR_1D_STATISTICS_path ,
#                                                    INPUT_VECTOR_Time_Measurements_path,
#                                                    INPUT_VECTOR_2D_STATISTICS_path, 
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
#                                                    INPUT_LOGICAL_BC_STRING,
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


}













