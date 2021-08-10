
#if (INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data == "reload_complete_data")
#{
# ====================================================================================
# Prepare Data for analysis / create directories etc.
# ====================================================================================
{
  
  #-------------------------------------------------------------------------------------
  # Intervalls
  # Titles
  # Xlabs
  # pdf_file_name_overall , jpeg_file_name_overall
  #-------------------------------------------------------------------------------------  
  {  
    My_VECTOR_Intervalls_02 <- (INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE -INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE )/ INPUT_VECTOR_breaks_02
    My_VECTOR_Intervalls_03 <- (INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE -INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE )/ INPUT_VECTOR_breaks_03
    #----------------------------------------------------------------------------------------------------------------------
    # pdf- and jpg-overall-Filenames for plots
    #----------------------------------------------------------------------------------------------------------------------
    {
      INPUT_VECTOR_histogram_pdf_file_name_overall = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, ".pdf", sep="")
      INPUT_VECTOR_histogram_jpeg_file_name_overall = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, ".jpeg", sep="")
      INPUT_VECTOR_histogram_pdf_file_name_02 = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, "_", INPUT_VECTOR_breaks_02, "_", "breaks", "_", ".pdf", sep="")
      INPUT_VECTOR_histogram_pdf_file_name_03 = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, "_", INPUT_VECTOR_breaks_03, "_", "breaks", "_", ".pdf", sep="")
    }
    #----------------------------------------------------------------------------------------------------------------------
    # txt-table-overall-Filenames for plots
    #----------------------------------------------------------------------------------------------------------------------
    {
      INPUT_VECTOR_histogram_txt_file_name_overall = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, ".txt", sep="")
      INPUT_VECTOR_histogram_txt_file_name_02 = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, "_", INPUT_VECTOR_breaks_02, "_", "breaks", "_", ".txt", sep="")
      INPUT_VECTOR_histogram_txt_file_name_03 = paste(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT, "_", INPUT_VECTOR_breaks_03, "_", "breaks", "_", ".txt", sep="")
    }
    
    #-----------------------------------------------------------------------------------------------------------------
    # titles 
    # xlabs
    #-----------------------------------------------------------------------------------------------------------------
    {  
      INPUT_VECTOR_main_01 = INPUT_Analysis_Title 
      INPUT_VECTOR_main_02 = INPUT_Analysis_Title
      INPUT_VECTOR_main_03 = INPUT_Analysis_Title
      
      #INPUT_VECTOR_xlab_01 = INPUT_VECTOR_VXX_Title_Type_Text
      #INPUT_VECTOR_xlab_02 = paste(INPUT_VECTOR_VXX_Title_Type_Text, " >= ", INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE , " ",  INPUT_VECTOR_UNIT , " and ", " <= ", INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE, " ", INPUT_VECTOR_UNIT , " , " , "breaks ", INPUT_VECTOR_breaks_02, sep="")
      #INPUT_VECTOR_xlab_03 = paste(INPUT_VECTOR_VXX_Title_Type_Text, " >= ", INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE , " ",  INPUT_VECTOR_UNIT , " and ", " <= ", INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE, " ", INPUT_VECTOR_UNIT , " , " , "breaks ", INPUT_VECTOR_breaks_03, sep="")
      
      INPUT_VECTOR_xlab_01 =
        paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT , 
              " <= " , 
              INPUT_VECTOR_VXX_Title_Type_Text,
              " <= " ,
              round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT ,
              sep="")
      
      
      INPUT_VECTOR_xlab_02 =
        paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT , 
              " <= " , 
              INPUT_VECTOR_VXX_Title_Type_Text,
              " <= " ,
              round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT ,
              " , " , "breaks ", INPUT_VECTOR_breaks_02,
              sep="")
      
      INPUT_VECTOR_xlab_03 =
        paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT , 
              " <= " , 
              INPUT_VECTOR_VXX_Title_Type_Text,
              " <= " ,
              round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,2), " ", INPUT_VECTOR_UNIT ,
              " , " , "breaks ", INPUT_VECTOR_breaks_03,
              sep="")
    }
  }
  
  #----------------------------------------------------------------------------------
  # directories .....
  #-----------------------------------------------------------------------------------
  { 
    
    mkdirs(BASIC_Project_path)
    
    INPUT_VECTOR_1D_STATISTICS_path = paste(BASIC_Project_path , "1D_STATISTICS", "/", sep="")
    mkdirs(INPUT_VECTOR_1D_STATISTICS_path)
    
    INPUT_VECTOR_Time_Measurements_path = paste(BASIC_Project_path , "TIME_GRAPHS" , "/" , sep="")
    mkdirs(INPUT_VECTOR_Time_Measurements_path)
    
    INPUT_VECTOR_2D_STATISTICS_path = paste(BASIC_Project_path , "2D_STATISTICS", "/", sep="")
    mkdirs(INPUT_VECTOR_2D_STATISTICS_path)
    
    INPUT_VECTOR_3D_STATISTICS_path = paste(BASIC_Project_path , "3D_STATISTICS", "/", sep="")
    mkdirs(INPUT_VECTOR_3D_STATISTICS_path)
    
    INPUT_VECTOR_1D_STATISTICS_TABLES_path = paste(BASIC_Project_path , "1D_STATISTICS_TABLES", "/", sep="")
    mkdirs(INPUT_VECTOR_1D_STATISTICS_TABLES_path)
    
    INPUT_VECTOR_Time_Measurements_TABLES_path = paste(BASIC_Project_path , "TIME_GRAPHS_TABLES" , "/" , sep="")
    mkdirs(INPUT_VECTOR_Time_Measurements_TABLES_path)
    
    INPUT_VECTOR_2D_STATISTICS_TABLES_path = paste(BASIC_Project_path , "2D_STATISTICS_TABLES", "/", sep="")
    mkdirs(INPUT_VECTOR_2D_STATISTICS_TABLES_path)
    
    INPUT_VECTOR_3D_STATISTICS_TABLES_path = paste(BASIC_Project_path , "3D_STATISTICS_TABLES", "/", sep="")
    mkdirs(INPUT_VECTOR_3D_STATISTICS_TABLES_path)
    
    Correlations_01_X_Y_GRAPHS_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "01_X_Y_GRAPHS/" , sep="")
    Correlations_02_X_Y_COUNT_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "02_X_Y_COUNT/" , sep="")
    Correlations_02_X_Y_PROBABILITY_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "02_X_Y_PROBABILITY/" , sep="")
    Correlations_02_X_Y_KERNEL_DENSITY_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "02_X_Y_KERNEL_DENSITY/" , sep="")
    Correlations_02_X_Y_ECDF_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "02_X_Y_ECDF/" , sep="")
    Correlations_02_X_Y_1_MINUS_ECDF_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "02_X_Y_1-ECDF/" , sep="")
    
    Correlations_01_X_Y_GRAPHS_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "01_X_Y_GRAPHS/" , sep="")
    Correlations_02_X_Y_COUNT_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "02_X_Y_COUNT/" , sep="")
    Correlations_02_X_Y_PROBABILITY_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "02_X_Y_PROBABILITY/" , sep="")
    Correlations_02_X_Y_KERNEL_DENSITY_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "02_X_Y_KERNEL_DENSITY/" , sep="")
    Correlations_02_X_Y_ECDF_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "02_X_Y_ECDF/" , sep="")
    Correlations_02_X_Y_1_MINUS_ECDF_TABLES_path = paste(INPUT_VECTOR_2D_STATISTICS_TABLES_path, "02_X_Y_1-ECDF/" , sep="")
    
    Correlations_03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS/" , sep="")
    Correlations_04_X_Y_COUNT_SENSITIVITY_ANALYSIS_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "04_X_Y_COUNT_SENSITIVITY_ANALYSIS/" , sep="")
    
    Correlations_05_P_GTGT_CUMULATIVE_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "05_P_GTGT_CUMULATIVE/" , sep="")
    
    Correlations_20_CUMULATIVE_CONTOURS_DISTRIBUTIONS_path = paste(INPUT_VECTOR_2D_STATISTICS_path, "20_CUMULATIVE_CONTOURS_DISTRIBUTIONS/" , sep="")
    
    #--- done later 
    #mkdirs(Correlations_01_X_Y_GRAPHS_path)
    #mkdirs(Correlations_02_X_Y_COUNT_path)
    #mkdirs(Correlations_02_X_Y_PROBABILITY_path)
    #mkdirs(Correlations_02_X_Y_KERNEL_DENSITY_path)
    #mkdirs(Correlations_02_X_Y_ECDF_path)
    #mkdirs(Correlations_02_X_Y_1_MINUS_ECDF_path)
    
    mkdirs(Correlations_03_X_Y_GRAPHS_SENSITIVITY_ANALYSIS_path)
    mkdirs(Correlations_04_X_Y_COUNT_SENSITIVITY_ANALYSIS_path)
    
    #mkdirs(Correlations_05_P_GTGT_CUMULATIVE_path)
    #mkdirs(Correlations_20_CUMULATIVE_CONTOURS_DISTRIBUTIONS_path)
  }
  
  #------------------------------
  # set colors for 2D
  #------------------------------
  {
    #bad
    #my_colors_for_2D_COUNT_X_Y <- rev(heat.colors(100))
    #my_colors_for_2D_COUNT_X_Y <- blue2red(100)
    #my_colors_for_2D_COUNT_X_Y <- blue2red(10)
    
    # nice but very uncomprehensible = bad
    #my_colors_for_2D_COUNT_X_Y <-  rev(rainbow(10, end = 4/6))
    
    #bad
    #my_colors_for_2D_COUNT_X_Y <- blue2red(10)
    
    # ok
    #pal <- colorRampPalette(c("white", "darkmagenta"))
    #my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    # ok 
    #pal <- colorRampPalette(c("lightyellow", "darkmagenta"))
    #my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    # ok
    #pal <- colorRampPalette(c("lightblue", "darkmagenta"))
    #my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    #bad
    #my_colors_for_2D_COUNT_X_Y <- rev(topo.colors(100))
    
    # medium
    #pal <- colorRampPalette(c("lightyellow","green", "darkblue"))
    #my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    # medium
    #pal <- colorRampPalette(c("lightyellow","lightgreen", "darkblue"))
    #my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    # very good
    # pal <- colorRampPalette(c("white", "magenta", "darkblue", "black"))
    # my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    # very good
    # pal <- colorRampPalette(c("lightyellow", "magenta", "darkblue", "black"))
    # my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    #-------------------------------------------------------------
    # very good
    pal <- colorRampPalette(rev(c("lightyellow", "magenta", "darkblue", "black")))
    my_colors_for_2D_COUNT_X_Y <- pal(100)
    
    my_colors_for_2D_PROBABILITY_X_Y = my_colors_for_2D_COUNT_X_Y
    my_colors_for_2D_KERNEL_DENSITY_X_Y = my_colors_for_2D_COUNT_X_Y
    
    my_colors_for_2D_COUNT_SA_X_Y = my_colors_for_2D_COUNT_X_Y
    
    #-------------------------------------------------------------
    #pal <- colorRampPalette(c("cyan", "darkblue"))
    #pal <- colorRampPalette(c("lightyellow", "magenta", "darkblue", "black"))
    pal <- colorRampPalette(c(rev(c("lightyellow", "magenta", "darkblue", "black"))))
    #pal <- colorRampPalette(c("darkblue", "cyan"))
    
    my_colors_for_2D_ECDF_X_Y <- pal(100)
    #--- IVANs colors
    my_colors_for_2D_ECDF_X_Y = terrain.colors
    
    #-------------------------------------------------------------
    #pal <- colorRampPalette(c("darkblue", "cyan"))
    pal <- colorRampPalette(c(rev(c("lightyellow", "magenta", "darkblue", "black"))))
    #pal <- colorRampPalette(c("lightyellow", "magenta", "darkblue", "black"))
    my_colors_for_2D_1_MINUS_ECDF_X_Y =  pal(100)
  }
  
  
  #------------------------------
  # set 2D geomterical parameters 
  # - binning numbers
  # - grid numbers
  # - INPUT_my_log_10_exp_plus_xValue
  # - my_na_color_value
  # - density_geom
  # - ONLY_SHOW_EXISTING_DATA_TUPEL
  #------------------------------
  {
    #--------set binning number---------------
    INPUT_ROUND_TO_NUMBER = 3
    
    #--------set grid numbers for density estimation---------------
    my_n_x_min = 2
    my_n_y_min = 2
    
    # my_n_x_min = 500
    # my_n_y_min = 500
    
    #my_n_x_max = 200
    #my_n_y_max = 200
    
    my_n_x_max = 500
    my_n_y_max = 500
    
    
    #===== increases number of considered values at density etsimation ============
    INPUT_my_log_10_exp_plus_xValue  = 2
    
    
    #=== not used but auomatically estimated h ======
    #my_hx_min = 0.1
    #my_hy_min = 0.1
    
    my_hx_min = 1000
    my_hy_min = 1000
    
    my_hx_max = 1
    my_hy_max = 1
    
    #----- my_na_color_value -----
    my_na_color_value = "white"
    
    #------ my_density_geom -----
    my_density_geom = "tile"
    my_density_geom = "density2d"
    
    #-------- MAX_NBR_OF_X/Y_GRIDS_FOR_ECDF ---------------------
    #   !!!  CRITICAL for calculation time   !!! 
    
    INPUT_MAX_NBR_OF_X_GRIDS_FOR_ECDF = 1000
    INPUT_MAX_NBR_OF_Y_GRIDS_FOR_ECDF = 1000
    
    #INPUT_MAX_NBR_OF_X_GRIDS_FOR_ECDF = 500
    #INPUT_MAX_NBR_OF_Y_GRIDS_FOR_ECDF = 500
    
    ONLY_SHOW_EXISTING_DATA_TUPEL=TRUE
    
    #---- set MY_EPSILON in case that my_PROJECT_DATA_VXX_i_step = 0 in 1D SENSITIVITY ANALYSIS
    # if(my_PROJECT_DATA_VXX_i_step == 0){ my_PROJECT_DATA_VXX_i_step = MY_EPSILON}
    MY_EPSILON = 0.001
  }
  
  
  #---- overwrite or skip if file exists
  # SKIP
  # OVERWRITE
  
  INPUT_OVERWRITE_or_SKIP_if_file_exists = "SKIP"
  
}