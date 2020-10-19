CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   F   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       FR GDAC    source        
Argo float     history       2018-11-23T04:02:54Z creation      
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6x   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                  @  6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                    7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                    9    STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                 �  ;    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��         <�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    <�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    <�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                    <�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                     =�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    =�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                    =�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                    >�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                    ?�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                     @�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution                   
_FillValue        A.�~       axis      T         @  A    JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A@   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution                   
_FillValue        A.�~          @  AH   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y         @  A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X         @  A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    B   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                  @  B   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    BP   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    BX   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    B`   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    Bh   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��         Jh   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z        �  J�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  SH   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z        �  Ux   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ^8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�       �  `h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  i(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  q�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  |�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �     PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                 �  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                 P  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                     �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                     �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                     �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                     �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                    �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  p  �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                     �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  �  �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar          ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar          ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�         �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  �  �(Argo profile    3.1 1.2 19500101000000  20050302205638  20181123040254  2900313 2900313 2900313 2900313 2900313 2900313 2900313 2900313 CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              CHINA ARGO PROJECT                                              JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     JIANPING XU                                                     PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL                                     AAAAAAAAHZHZHZHZHZHZHZHZ0019_23754_000                  0019_23754_001                  0019_23754_002                  0019_23754_003                  0019_23754_004                  0019_23754_005                  0019_23754_006                  0019_23754_007                  2C  2C  2C  2C  2C  2C  2C  2C  DDDDDDDDPROVOR                          PROVOR                          PROVOR                          PROVOR                          PROVOR                          PROVOR                          PROVOR                          PROVOR                          0327                            0327                            0327                            0327                            0327                            0327                            0327                            0327                                                                                                                                                                                                                                                                                            840 841 841 840 840 840 840 841 @��t�A@�"	J��@�$��)��@�'7��@@�)����@�,���@�.�����@�1�ʆB11111111@�����k@�"/7�I@�$�io@�'s|�@�)�ʶ͏@�,0��@�.�2@y]@�1N��@7<)    @7m�`   @7���   @8M�`   @8��   @8w�@   @8 A�   @7��    @_�n�   @_˅    @_�`@   @_ߍ@   @_�`   @_�   @_�`@   @_߾�   11111111ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   ARGOS   AAAAAAAAAAAAAAAAAAAAAAAAPrimary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                   Primary sampling: discrete []                                                                                                                                                                                                                                                           A��A���A�33B��BG��Bq33B�ffB���B�ffB�33B�33BC�fC�3C  C �C*�C4ffC>  CHffC[�3Cp33C�&fC���C�&fC���C��C�ٚC�33C��C��3C�&fC�  C��C�� D�D�D�fD�D  DfD 3D%3D*  D/�D4  D8��D>�DB�3DG��DN33DT��DZ� Da3DgY�Dm�fDs�3Dy�3D�@ D�s3D���D���D�@ D�p D���D�� D�	�Dԉ�D�  D� A��A�33A�33B"  BI33Bn��B�  B�  B�  BǙ�B���B�  C�CL�CffC 33C)�fC3� C>� CHL�C[33Co��C��3C�&fC��C�33C�L�C�ٚC���C���C�  C�33C��3C��C��3D��D��D�3DٚD� DfD �D%  D*3D/3D4  D9  D=�3DB�3DH&fDN@ DT�fDZ�3Da&fDgS3Dm�fDs��Dy�3D�FfD�|�D��fD�fD�I�D�|�D���D�|�D���D�s3D���D� AffA���A�  B!33BI��Bp��B�ffB�ffB�  B�  B���B�ffC�3C� C� C 33C*ffC3�3C>  CHL�C\  Cp  C�L�C�  C�ٚC�L�C�33C�@ C��3C��fC�ٚC��fC�  C��3C��fDٚD��D  D  D�3D  D 3D$��D)ٚD/�D433D8��D=ٚDC�DG� DNL�DT��DZ��D`� DgY�Dm� Ds�3Dz�D�0 D�vfD�� D�  D�C3D��fD�3D�vfD�  D�y�D�fD� A#33A�ffA홚B!33BH��Bp  B�  B�33B�33Bș�Bܙ�B�  C33C�3C33C � C)� C433C=�fCG�3C[��Co��C�&fC�L�C�� C�&fC��3C��C�  C��3C��fC�@ C��fC�33C�L�D,�D��D�D3D�D  D �D$��D)�fD.�3D4�D8��D>  DB��DH  DNffDT� DZ��Da�Dg@ Dmy�Ds�fDzfD�33D��fD���D�  D�I�D�y�D�	�D��fD��3D�vfD��fD�|�A&ffA�33A�  B��BH  BpffB���B�33B�33B�  B�ffB�ffC�fC�3CffC 33C*  C4ffC>33CG�fC\�Cp�C�L�C�&fC��fC��C��C�@ C��3C�@ C�ٚC�  C�� C��C���D� D�D��D�fD3D��D �D$�3D*  D/3D3��D93D=��DC  DH�DNFfDTy�DZ�fD`�fDg33Dms3Ds��Dy�3D�S3D�vfD���D���D�9�D���D��fD�s3D���D�y�D�fD�3AffA�33A�  B33BF  BpffB�ffB���B�33B���B�ffB�33C�fC33C� C �C)ffC3��C=��CH33C\L�Co�fC��3C�33C���C�&fC�L�C�  C��C�&fC�33C�L�C�� C��C�@ D�D�D�fD� D� DfD �D$�3D*fD.��D4,�D8� D=�fDC3DG�fDN&fDT��DZ��D`�fDgFfDms3DsٚDz3D�<�D��3D���D���D�L�D�vfD��D�|�D��3D�i�D��D�|�A(  A���A���B   BH  Bn��B���B���B���B�33B���B�33C��C� C�fC L�C*��C3ffC>�CG��C\  Co��C��C��3C��3C�&fC��C��3C�  C��C���C�ffC�� C�ٚC�33D�fD�D�DٚD��D�D &fD%3D*�D/  D4  D93D=�fDB��DH�DNFfDT�3DZ� D`��DgS3Dm� Ds� Dz3D�FfD��3D���D�fD�I�D��3D��D�vfD�  DԀ D��3D퉚AffA���A�33BffBFffBm33B�33B�  B���B�ffB���B���C�C  C� C 33C*�C4  C>L�CG�3C[��Co�fC��fC�  C�ffC�  C�&fC�� C�  C��C�@ C�  C�33C��3C�33D��D�fDfDfD  D��D fD$� D*  D/fD3�3D9fD=�3DB� DH�DNS3DT�fDZ��D`��DgS3Dmy�DsٚDy��D�C3D��fD�� D�  D�P D��3D���D��fD�	�Dԓ3D�fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A�33B��BG��Bq33B�ffB���B�ffB�33B�33BC�fC�3C  C �C*�C4ffC>  CHffC[�3Cp33C�&fC���C�&fC���C��C�ٚC�33C��C��3C�&fC�  C��C�� D�D�D�fD�D  DfD 3D%3D*  D/�D4  D8��D>�DB�3DG��DN33DT��DZ� Da3DgY�Dm�fDs�3Dy�3D�@ D�s3D���D���D�@ D�p D���D�� D�	�Dԉ�D�  D� A��A�33A�33B"  BI33Bn��B�  B�  B�  BǙ�B���B�  C�CL�CffC 33C)�fC3� C>� CHL�C[33Co��C��3C�&fC��C�33C�L�C�ٚC���C���C�  C�33C��3C��C��3D��D��D�3DٚD� DfD �D%  D*3D/3D4  D9  D=�3DB�3DH&fDN@ DT�fDZ�3Da&fDgS3Dm�fDs��Dy�3D�FfD�|�D��fD�fD�I�D�|�D���D�|�D���D�s3D���D� AffA���A�  B!33BI��Bp��B�ffB�ffB�  B�  B���B�ffC�3C� C� C 33C*ffC3�3C>  CHL�C\  Cp  C�L�C�  C�ٚC�L�C�33C�@ C��3C��fC�ٚC��fC�  C��3C��fDٚD��D  D  D�3D  D 3D$��D)ٚD/�D433D8��D=ٚDC�DG� DNL�DT��DZ��D`� DgY�Dm� Ds�3Dz�D�0 D�vfD�� D�  D�C3D��fD�3D�vfD�  D�y�D�fD� A#33A�ffA홚B!33BH��Bp  B�  B�33B�33Bș�Bܙ�B�  C33C�3C33C � C)� C433C=�fCG�3C[��Co��C�&fC�L�C�� C�&fC��3C��C�  C��3C��fC�@ C��fC�33C�L�D,�D��D�D3D�D  D �D$��D)�fD.�3D4�D8��D>  DB��DH  DNffDT� DZ��Da�Dg@ Dmy�Ds�fDzfD�33D��fD���D�  D�I�D�y�D�	�D��fD��3D�vfD��fD�|�A&ffA�33A�  B��BH  BpffB���B�33B�33B�  B�ffB�ffC�fC�3CffC 33C*  C4ffC>33CG�fC\�Cp�C�L�C�&fC��fC��C��C�@ C��3C�@ C�ٚC�  C�� C��C���D� D�D��D�fD3D��D �D$�3D*  D/3D3��D93D=��DC  DH�DNFfDTy�DZ�fD`�fDg33Dms3Ds��Dy�3D�S3D�vfD���D���D�9�D���D��fD�s3D���D�y�D�fD�3AffA�33A�  B33BF  BpffB�ffB���B�33B���B�ffB�33C�fC33C� C �C)ffC3��C=��CH33C\L�Co�fC��3C�33C���C�&fC�L�C�  C��C�&fC�33C�L�C�� C��C�@ D�D�D�fD� D� DfD �D$�3D*fD.��D4,�D8� D=�fDC3DG�fDN&fDT��DZ��D`�fDgFfDms3DsٚDz3D�<�D��3D���D���D�L�D�vfD��D�|�D��3D�i�D��D�|�A(  A���A���B   BH  Bn��B���B���B���B�33B���B�33C��C� C�fC L�C*��C3ffC>�CG��C\  Co��C��C��3C��3C�&fC��C��3C�  C��C���C�ffC�� C�ٚC�33D�fD�D�DٚD��D�D &fD%3D*�D/  D4  D93D=�fDB��DH�DNFfDT�3DZ� D`��DgS3Dm� Ds� Dz3D�FfD��3D���D�fD�I�D��3D��D�vfD�  DԀ D��3D퉚AffA���A�33BffBFffBm33B�33B�  B���B�ffB���B���C�C  C� C 33C*�C4  C>L�CG�3C[��Co�fC��fC�  C�ffC�  C�&fC�� C�  C��C�@ C�  C�33C��3C�33D��D�fDfDfD  D��D fD$� D*  D/fD3�3D9fD=�3DB� DH�DNS3DT�fDZ��D`��DgS3Dmy�DsٚDy��D�C3D��fD�� D�  D�P D��3D���D��fD�	�Dԓ3D�fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA��A��#A�33A�/AʾwA�^5A���A�  A���A�1A���A��uA�dZA���A��DA�v�A��FA�A�A�hsA��A���A�hsA�x�A��/A���A��A~�/At��An��AfVAX�!AK�AA�#A< �A0I�A'��AXA�/A�+A	l�@�C�@�9@�%@؛�@Ұ!@�l�@��@��@��y@�  @��@���@���@��`@��@��h@�hs@x��@pr�@h��@`��@[�F@U?}@@Ĝ@0r�@$1@��@��@
�!A�ZA�A�I�A�ZA㟾A�|�A�  A�7LAа!A���A̧�AƟ�A��A�ffA�/A�+A���A��#A�  A��FA���A�{A��FA�C�A�  A��A�$�A�;dAv��Aj�/A`��AWS�AN1'AF  A>�HA8��A/��A't�A�AbNA(�AV@�S�@�C�@�@���@�v�@�@���@�5?@��@�O�@�
=@��#@���@�
=@��+@�?}@K�@wl�@k33@g+@_�w@W|�@A�@2=q@%��@Z@C�@�/A�RA�+A�x�A앁A��A��A�ȴA�DA�VA�M�A�I�A�x�A�S�A�\)A�(�A��PA�`BA�?}A��
A�I�A�A�hsA�(�A���A���A�ƨA�{A~�+Awt�Alr�AcVAV��AG�-A@ZA9l�A.�DA&��A!�A��A$�Aff@�&�@�j@�;d@��@�-@�;d@��@���@�Z@�7L@���@��@��@���@�V@�+@�l�@�  @w�w@mO�@d1@]�@S�
@Cƨ@3�F@&V@�@o@z�A�n�Aڡ�A�|�A�&�A��A�C�A���Aӕ�Aω7A��#A�/A��+A�%A�E�A�-A��;A��A��^A���A�|�A���A��A�\)A�
=A��7A���A�r�A��Awx�An�Af=qA\1AP�ABn�A9dZA1��A(1AjA�mAbNA$�@�&�@�O�@� �@�5?@؋D@ЋD@��@���@�
=@��u@��9@���@�Q�@�1@��F@��y@�ff@�V@w+@l��@e�h@^�+@U�T@E�@0�`@$��@7L@�9@
~�A�jA���A���A�A�$�A�^5A��A�I�Aę�A�G�A�{A��A��A���A��\A��RA��\A���A���A���A��FA�+A��TA�v�A���A�VA��A��A���A�%Av��Ak��A`-AZ�jAQAH��AAXA?ƨA3%A(��A -A{A
�DAJ@��^@��y@��;@١�@�O�@�x�@�-@�X@��@���@��T@�?}@��@��@�t�@��u@q�^@`��@U@I&�@<��@.�@ �`@+@�^@9XA�+A�+A�(�A�1A�A�ĜA�1'A�I�A���A��TA���A�ĜA�VA�jA�\)A�bA��A��A�7LA��-A��wA�&�A�
=A�+A�XA��;A��A�(�A�1A���A�
=A{�-Au�ApjAd1AR �AL�!A@n�A6�RA,^5A%l�A9XA�A�@��w@���@�|�@��@�ff@�dZ@�ȴ@��y@��@��w@��D@���@��-@��`@��@��@|(�@m�@\�@Q��@=�-@/�P@#S�@��@v�@�FA��;A�A��A��A�ĜAϕ�A�ĜA�Q�A�x�A���A�\)A��hA��TA�A���A���A�bA��RA��A��PA���A��A�ȴA���A�VA�A�A�hsA�-A�33A���A��DA|��Aq��Ad��AZz�AP�AJ=qA>��A3�A)��A%�
A/A33A	�A�!@���@��@�t�@ְ!@��y@���@���@�Z@��H@��/@��#@���@�K�@��!@�j@~�+@i�@W�@Kƨ@7�P@)&�@!�@��@J@S�A��;A��TA��yA��`A��A��A��A��mA��TA�$�Aĩ�A��A��A�7LA��HA�z�A��A�l�A��A�A�A���A�O�A���A�dZA��uA�M�A��A�bA��^A�$�AzZAp�\Ag%A_ƨAL�`AA�wA;�A3�^A)�PA!dZA�DA-A�7A 5?@�O�@�l�@�Q�@�K�@�;d@���@ÍP@�$�@��@�@��H@���@��;@�v�@�9X@�%@oK�@`�u@Q%@G|�@7�@*n�@�!@��@��@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA��A��#A�33A�/AʾwA�^5A���A�  A���A�1A���A��uA�dZA���A��DA�v�A��FA�A�A�hsA��A���A�hsA�x�A��/A���A��A~�/At��An��AfVAX�!AK�AA�#A< �A0I�A'��AXA�/A�+A	l�@�C�@�9@�%@؛�@Ұ!@�l�@��@��@��y@�  @��@���@���@��`@��@��h@�hs@x��@pr�@h��@`��@[�F@U?}@@Ĝ@0r�@$1@��@��@
�!A�ZA�A�I�A�ZA㟾A�|�A�  A�7LAа!A���A̧�AƟ�A��A�ffA�/A�+A���A��#A�  A��FA���A�{A��FA�C�A�  A��A�$�A�;dAv��Aj�/A`��AWS�AN1'AF  A>�HA8��A/��A't�A�AbNA(�AV@�S�@�C�@�@���@�v�@�@���@�5?@��@�O�@�
=@��#@���@�
=@��+@�?}@K�@wl�@k33@g+@_�w@W|�@A�@2=q@%��@Z@C�@�/A�RA�+A�x�A앁A��A��A�ȴA�DA�VA�M�A�I�A�x�A�S�A�\)A�(�A��PA�`BA�?}A��
A�I�A�A�hsA�(�A���A���A�ƨA�{A~�+Awt�Alr�AcVAV��AG�-A@ZA9l�A.�DA&��A!�A��A$�Aff@�&�@�j@�;d@��@�-@�;d@��@���@�Z@�7L@���@��@��@���@�V@�+@�l�@�  @w�w@mO�@d1@]�@S�
@Cƨ@3�F@&V@�@o@z�A�n�Aڡ�A�|�A�&�A��A�C�A���Aӕ�Aω7A��#A�/A��+A�%A�E�A�-A��;A��A��^A���A�|�A���A��A�\)A�
=A��7A���A�r�A��Awx�An�Af=qA\1AP�ABn�A9dZA1��A(1AjA�mAbNA$�@�&�@�O�@� �@�5?@؋D@ЋD@��@���@�
=@��u@��9@���@�Q�@�1@��F@��y@�ff@�V@w+@l��@e�h@^�+@U�T@E�@0�`@$��@7L@�9@
~�A�jA���A���A�A�$�A�^5A��A�I�Aę�A�G�A�{A��A��A���A��\A��RA��\A���A���A���A��FA�+A��TA�v�A���A�VA��A��A���A�%Av��Ak��A`-AZ�jAQAH��AAXA?ƨA3%A(��A -A{A
�DAJ@��^@��y@��;@١�@�O�@�x�@�-@�X@��@���@��T@�?}@��@��@�t�@��u@q�^@`��@U@I&�@<��@.�@ �`@+@�^@9XA�+A�+A�(�A�1A�A�ĜA�1'A�I�A���A��TA���A�ĜA�VA�jA�\)A�bA��A��A�7LA��-A��wA�&�A�
=A�+A�XA��;A��A�(�A�1A���A�
=A{�-Au�ApjAd1AR �AL�!A@n�A6�RA,^5A%l�A9XA�A�@��w@���@�|�@��@�ff@�dZ@�ȴ@��y@��@��w@��D@���@��-@��`@��@��@|(�@m�@\�@Q��@=�-@/�P@#S�@��@v�@�FA��;A�A��A��A�ĜAϕ�A�ĜA�Q�A�x�A���A�\)A��hA��TA�A���A���A�bA��RA��A��PA���A��A�ȴA���A�VA�A�A�hsA�-A�33A���A��DA|��Aq��Ad��AZz�AP�AJ=qA>��A3�A)��A%�
A/A33A	�A�!@���@��@�t�@ְ!@��y@���@���@�Z@��H@��/@��#@���@�K�@��!@�j@~�+@i�@W�@Kƨ@7�P@)&�@!�@��@J@S�A��;A��TA��yA��`A��A��A��A��mA��TA�$�Aĩ�A��A��A�7LA��HA�z�A��A�l�A��A�A�A���A�O�A���A�dZA��uA�M�A��A�bA��^A�$�AzZAp�\Ag%A_ƨAL�`AA�wA;�A3�^A)�PA!dZA�DA-A�7A 5?@�O�@�l�@�Q�@�K�@�;d@���@ÍP@�$�@��@�@��H@���@��;@�v�@�9X@�%@oK�@`�u@Q%@G|�@7�@*n�@�!@��@��@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B	7B'�B��B1BuB�B�B �B�BB��B�B�5B�
B��B��B�jB��B�JBk�BP�B9XB'�BB
�TB
�wB
��B
r�B
VB
-B	�B	��B	�JB	|�B	@�B	�B��B�B�/B��B�}B�RB�?B�FBŢB��B�yB�B��B	hB	�B	)�B	0!B	@�B	T�B	dZB	v�B	�uB	�B	ɺB	�;B	�B
+B
�B
0!B
E�B
P�B
]/B
hsB
PB
:^B
z�B
ǮB33B��B�XB��B�)B�B�B��B��BJB��B��BĜB�B��B��B�hBv�BK�B)�B
��B
�B
ŢB
�B
w�B
B�B
hB	�B	�XB	��B	s�B	gmB	F�B	%�B��B�yB�
BɺB�jB�FB�FB�^BǮB�B�B��B�/B�B��B	VB	�B	-B	E�B	ZB	�B	��B	�^B	��B	�B
B
�B
'�B
;dB
K�B
XB
dZB
9XB
;dB
D�B
W
B
P�B
J�B
H�B{B�!B�B�;B�B��B�5B�BŢB�qB��B��B� BdZB@�B%�B�B
�B
�
B
�FB
��B
�B
P�B
,B	�B	��B	�B	cTB	9XB	�B	VB�B�BŢB�qB�RB�?B�RB��B��B��B��B�B�yB�B	B	bB	'�B	2-B	C�B	P�B	n�B	��B	�}B	�B	�B
1B
�B
(�B
9XB
G�B
XB
dZB
�B
�B
�B
��B
��BJB(�B@�B�JB�}B�B�B��B�B�qB�B��B�uB�Bw�BP�B&�B{B
��B
�B
�B
�jB
��B
�B
ZB
:^B

=B	��B	�7B	`BB	B�B	�B	B�TB�BȴB�qB�LB�LB�jB�}BƨB��B�B�B��B	1B	�B	!�B	(�B	:^B	Q�B	gmB	�%B	��B	�FB	��B	�`B	��B
uB
#�B
5?B
K�B
]/B
iyB
�wB
�dB
�dB
�^B
��B
��B,BH�Bo�B�uB��B�-B�RB�jB�?B�!B��B��B��B��B�oB~�BjBI�B9XB$�BB
�B
��B
�XB
�+B
N�B
�B
DB	�TB	�wB	��B	��B	bNB	6FB	�B�B�BŢB�!B��B��B�BÖBŢB��B�TB�B	:^B	I�B	K�B	p�B	��B	�'B	ǮB	�ZB	�B
B
JB
�B
)�B
<jB
P�B
[#B
ffB
��B
��B
��B
��B
��B
ǮBhBaHB~�B�7B��B�-B�LB�?B�-B�B��B��B��B��B��B��B�\B�BiyBZB?}B1B
��B
�TB
�^B
��B
�B
l�B
49B	�fB	��B	�{B	p�B	B�B	&�B��B�)BĜB�dB�3B�B�B�-B�9B�RB�XB�B�BB	B	D�B	e`B	n�B	�bB	��B	��B	�B	��B
1B
�B
%�B
:^B
I�B
ZB
gmB
�B
�?B
��B
��B
�#B+BL�B}�B��B�B�B�9B�!B�B��B��B��B��B��B��B�{B�JB{�Bl�BYBC�B49BoB
��B
�ZB
�qB
��B
o�B
8RB

=B	�/B	ÖB	�oB	_;B	:^B	+B	B�B��BƨB�FB�!B�B�B�!B�?B�}B��B��B	C�B	ZB	`BB	o�B	�uB	�LB	�/B	�B	��B
PB
�B
/B
9XB
J�B
\)B
iyB
��B
��B
��B
��B
��B
��B
��B
��B
��BbB\)Bz�B�oB��B�!B�B��B��B��B��B�oB�Bn�B^5BE�B$�B
��B
�fB
��B
�3B
�oB
gmB
9XB
�B	�wB	�B	hsB	O�B	2-B	�B	B�B��BŢB��B�qB�}B��B��B�B�TB�B	B	bB	�B	9XB	T�B	^5B	�B	�dB	�fB	��B
1B
hB
�B
1'B
F�B
VB
cTB
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBB"�B��BBPB�B�B�BoB��B��B�B�B��B��BǮB�LB��B�+BffBK�B49B"�B
��B
�5B
�XB
�{B
m�B
P�B
'�B	�B	��B	�+B	w�B	;dB	�B��B�B�B��B�^B�3B�!B�'B��B��B�ZB�`B�B	JB	�B	$�B	+B	;dB	O�B	_;B	q�B	�VB	��B	ĜB	�B	�B
B
�B
+B
@�B
K�B
XB
cTB
%B
33B
s�B
��B,B�bB�-BĜB��B�`B�B�B�BB��B��B�qB��B��B�{B�=Bo�BD�B"�B
�B
��B
�wB
��B
p�B
;dB

=B	�ZB	�-B	�bB	l�B	`BB	?}B	�B�B�NB��BB�?B�B�B�3B��B��B��B��B�B�sB�B	+B	{B	%�B	>wB	R�B	z�B	�{B	�3B	��B	�TB	��B
hB
 �B
49B
D�B
P�B
]/B
0!B
2-B
;dB
M�B
G�B
A�B
?}BDB��B{�B�B\B�B��B��B�jB�9B��B�JBv�B[#B7LB�BVB
�yB
��B
�B
��B
z�B
G�B
"�B	�ZB	��B	{�B	ZB	0!B	�B	B�NB��B�jB�9B�B�B�B�LBȴBĜB��B��B�BB�yB��B	+B	�B	(�B	:^B	G�B	e`B	��B	�FB	��B	�mB	��B
\B
�B
0!B
>wB
N�B
[#B
�BB
�HB
�sB
�B
�yBB�B5?B�B�9B�fBbB�B��B�-B��B��B�1By�Bl�BE�B�B	7B
�B
�TB
��B
�'B
��B
u�B
N�B
/B	��B	��B	}�B	T�B	7LB	uB��B�B��B�qB�-B�B�B�'B�9B�dBÖB��B�;B�B��B	PB	�B	�B	/B	F�B	\)B	z�B	��B	�B	��B	�B	�B
1B
�B
)�B
@�B
Q�B
^5B
�'B
�B
�B
�B
�3B
ƨB�B;dBbNB�%B��B��B�B�B��B��B��B�uB�\B�VB�Bq�B]/B<jB,B�B
��B
�;B
ǮB
�B
y�B
A�B
oB	��B	�B	�'B	�hB	�7B	T�B	(�B	JB�ZB��B�RB��B��B��B��B�FB�RB�wB�B�;B	-B	<jB	>wB	cTB	�=B	��B	�^B	�
B	�ZB	�B	��B
JB
�B
/B
C�B
M�B
YB
�dB
�jB
�qB
�wB
�qB
�RBBQ�Bo�By�B��B��B��B��B��B��B��B�uB�bB�VB�DB�1B� Bq�BZBJ�B0!B
��B
�sB
��B
�B
�7B
r�B
]/B
$�B	�
B	�dB	�B	aHB	33B	�B�B��B�?B�B��B��B��B��B��B��B��BȴB��B�B	5?B	VB	_;B	�B	�-B	ŢB	�HB	�B	��B
	7B
�B
+B
:^B
J�B
XB
��B
��B
�}B
ÖB
ɺB�B;dBl�B�B��B��B��B��B��B�{B�hB�VB�PB�DB�7B�Bz�BjB[#BG�B2-B"�BB
�B
��B
�B
�+B
^5B
&�B	��B	��B	�-B	�B	M�B	(�B	�B�B�BB�?B��B��B��B��B��B��B�B�qB�B	2-B	H�B	N�B	^5B	�B	��B	��B	�/B	�B	��B
PB
�B
'�B
9XB
J�B
XB
�B
�B
�B
�B
�%B
�%B
�1B
�DB
�DB
��BH�BgmB~�B�{B��B��B�{B�=B�B�+B~�Bp�B[#BJ�B2-BhB
�B
��B
�^B
��B
~�B
S�B
%�B
B	�B	q�B	T�B	<jB	�B	%B�B�
B��B�-B�B�B�B�B�XBÖB��B�;B�B��B	JB	%�B	A�B	J�B	q�B	��B	��B	�NB	��B	��B
DB
�B
33B
B�B
O�B
[#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
;��
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                               none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9999(+/-0.0001), vertically averaged dS =-0.005(+/-0.005)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.005)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.004)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.004)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9996(+/-0.0001), vertically averaged dS =-0.017(+/-0.004)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            WJO: r =0.9995(+/-0.0001), vertically averaged dS =-0.019(+/-0.004)                                                                                                                                                                                             The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               The quoted error is manufacturer specified accuracy at time of laboratory calibration.                                                                                                                                                                          The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected; WJO weighted least squares fit is adopted; PSAL_ADJ_ERR: max(WJO error, SBE sensor accuracy)                                                                                                                               200707260837592007072608375920070726083759200707260838012007072608380120070726083801200707260838022007072608380220070726083802200707260838042007072608380420070726083804200707260838052007072608380520070726083805200707260838072007072608380720070726083807200707260838082007072608380820070726083808200707260838102007072608381020070726083810