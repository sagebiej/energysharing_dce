######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Starting values of the mixed logit models.       ###
###               Each vector is the best solution of an            ###
###               apollo_searchStart() search over 100 candidate    ###
###               sets of starting values (ranges in                ###
###               6_MXL_searchStart_ranges.R), run in June 2026;    ###
###               for MXL_PS_Subsample_New_Target_Group in          ###
###               September 2026, after its specification changed.  ###
###               The mixed logit scripts estimate from these       ###
###               values when RUN_SEARCH = FALSE (0_Main_Script.R)  ###
###               and repeat the search when RUN_SEARCH = TRUE.     ###
###               A model without an entry runs the search anyway.  ###
######################################################################

MXL_START_VALUES <- list(
  # 6_MXL_PS.R
  MXL_base = c(
    asc           = -0.0441013585435375,
    borgcit       = 0.291946725965366,
    borgmun       = 0.245550593417256,
    bpartiinv     = 0.269176121386428,
    bpartimem     = 0.27460255268922,
    bgoalsoc      = 0.566280036287744,
    bgoaleco      = 0.819999221386204,
    bgoalboth     = 0.956478753055871,
    bconsplit     = -0.458615250272264,
    bprice        = -0.759391341642068,
    sig_borgcit   = 1.06925014852939,
    sig_borgmun   = 0.544065286336451,
    sig_bpartiinv = -0.357153199460195,
    sig_bpartimem = 0.060203986860731,
    sig_bgoalsoc  = -0.451894295918,
    sig_bgoaleco  = 0.123901940697493,
    sig_bgoalboth = 0.313828772943769,
    sig_bconsplit = 0.858743388697782,
    sig_bprice    = 1.19337248034018,
    sig_asc       = -4.02420681000374
  ),

  # 6_MXL_PS_all_sociodem.R
  mxl_PS_allcociodem_combined_mfh_tenant = c(
    asc                           = -0.063132477120599,
    borgcit                       = 0.305055374830206,
    borgmun                       = 0.271443123304191,
    bpartiinv                     = 0.300329397671326,
    bpartimem                     = 0.304799901030523,
    bgoalsoc                      = 0.565221864168811,
    bgoaleco                      = 0.797429604997518,
    bgoalboth                     = 0.906138759793843,
    bconsplit                     = -0.465673103661485,
    bprice                        = -0.802604001971821,
    sig_borgcit                   = -0.971226755907326,
    sig_borgmun                   = 0.57363398558041,
    sig_bpartiinv                 = -0.52691330407885,
    sig_bpartimem                 = -0.019952954506964,
    sig_bgoalsoc                  = -0.505127524329101,
    sig_bgoaleco                  = -0.0432633400743078,
    sig_bgoalboth                 = 0.215553680986267,
    sig_bconsplit                 = 0.768559738147231,
    sig_bprice                    = -1.16238638077164,
    sig_asc                       = -3.64386171963957,
    asc_env_awareness_score       = -0.227448031220711,
    bpartimem_env_awareness_score = -0.0219954947492462,
    asc_sex                       = 0.890459038678975,
    bpartimem_sex                 = -0.05088798119012,
    asc_age                       = 0.0507994392419698,
    bpartimem_age                 = -0.00265093524530131,
    asc_educ_years                = -0.179306757236682,
    bpartimem_educ_years          = 0.00868834573004074,
    asc_lowincome                 = 0.0391975529078723,
    bpartimem_lowincome           = -0.0993703847366794,
    asc_highincome                = -0.617186945011514,
    bpartimem_highincome          = -0.0917503178761589,
    asc_mfh_or_tenant             = 0.212280852944551,
    bpartimem_mfh_or_tenant       = -0.0942355506216843
  ),

  # 7_MXL_WTP.R
  MXL_wtp_base = c(
    asc           = -0.351791721908365,
    borgcit       = 0.826709620335032,
    borgmun       = 0.510381891105079,
    bpartiinv     = -0.108897015690893,
    bpartimem     = -0.0403382851950076,
    bgoalsoc      = 0.721375916792262,
    bgoaleco      = 1.44508075568362,
    bgoalboth     = 1.54436806305393,
    bconsplit     = -0.623326516817289,
    bprice        = -0.349631383265697,
    sig_borgcit   = 0.456986840878829,
    sig_borgmun   = 0.207864379760255,
    sig_bpartiinv = 0.0498860017026772,
    sig_bpartimem = -0.0128781576182558,
    sig_bgoalsoc  = -0.365861413966901,
    sig_bgoaleco  = -0.0132483970499899,
    sig_bgoalboth = 0.118287467642529,
    sig_bconsplit = -0.0494080897173129,
    sig_bprice    = -1.4833782709842,
    sig_asc       = 9.24442168887122
  ),

  # 7_MXL_WTP_all_sociodem.R
  mxl_WTP_allsociodem_combined_mfh_tenant = c(
    asc                           = -0.549210749761433,
    borgcit                       = 0.817875678120366,
    borgmun                       = 0.598765756521486,
    bpartiinv                     = 0.0432688168809486,
    bpartimem                     = 0.00763574452381666,
    bgoalsoc                      = 0.716422782384551,
    bgoaleco                      = 1.40996248278594,
    bgoalboth                     = 1.4634457371379,
    bconsplit                     = -0.666261850932232,
    bprice                        = -0.387502208205246,
    sig_borgcit                   = 0.32596485039842,
    sig_borgmun                   = 0.0415412467707452,
    sig_bpartiinv                 = -0.0938273336756028,
    sig_bpartimem                 = 0.10972832161176,
    sig_bgoalsoc                  = 0.0036799173135877,
    sig_bgoaleco                  = 0.00623769821546064,
    sig_bgoalboth                 = 0.0598281343101687,
    sig_bconsplit                 = 0.306831773786113,
    sig_bprice                    = 1.44602170790996,
    sig_asc                       = 8.5848582320927,
    asc_env_awareness_score       = -0.473496478475902,
    bpartimem_env_awareness_score = 0.0211738654569444,
    asc_sex                       = 2.21822292049996,
    bpartimem_sex                 = -0.0923292606298056,
    asc_age                       = 0.141860095688119,
    bpartimem_age                 = 0.00619645630194248,
    asc_educ_years                = -0.40407986981179,
    bpartimem_educ_years          = 0.0244274698144199,
    asc_lowincome                 = 1.13822216263483,
    bpartimem_lowincome           = -0.353377870785737,
    asc_highincome                = -1.32549723034852,
    bpartimem_highincome          = -0.180620947638135,
    asc_mfh_or_tenant             = 0.559091946823423,
    bpartimem_mfh_or_tenant       = -0.161476602958444
  ),

  # 7_MXL_WTP_New_Target_Group_all_sociodem.R
  mxl_WTP_New_Target_Group_Final = c(
    bgoalsoc                      = 0.809960556696788,
    bgoaleco                      = 1.31951057049894,
    bgoalboth                     = 1.42379997305012,
    bconsplit                     = -0.653200415107403,
    borgcit                       = 1.023023573187,
    borgmun                       = 0.674671190048789,
    bpartimem                     = -0.0127271563923239,
    bpartiinv                     = -0.0288886985945859,
    asc                           = -0.0501704454336075,
    bprice                        = -0.403502661638332,
    sig_borgcit                   = 0.150818853859791,
    sig_borgmun                   = -0.103777315776185,
    sig_bpartiinv                 = -0.111033024464964,
    sig_bpartimem                 = 0.14500495999836,
    sig_bgoalsoc                  = 0.301256131118086,
    sig_bgoaleco                  = -0.112831734681705,
    sig_bgoalboth                 = -0.1927026345255,
    sig_bconsplit                 = -0.17852502592267,
    sig_bprice                    = -1.29864218248857,
    sig_asc                       = 8.42710811482915,
    asc_env_awareness_score       = -0.398511168391093,
    bpartimem_env_awareness_score = 0.0476833559727065,
    asc_sex                       = 1.71333330985818,
    bpartimem_sex                 = -0.0112247498715593,
    asc_age                       = 0.124892976579969,
    bpartimem_age                 = 0.0102605808206903,
    asc_educ_years                = -0.390390652666685,
    bpartimem_educ_years          = 0.0152722265471545
  ),

  # 6_MXL_PS_New_Target_Group_allsociodem.R
  MXL_PS_Subsample_New_Target_Group = c(
    asc                           = 0.118416464272927,
    borgcit                       = 0.368034375522576,
    borgmun                       = 0.307764454475987,
    bpartiinv                     = 0.232498508795125,
    bpartimem                     = 0.297531568019033,
    bgoalsoc                      = 0.690662682734008,
    bgoaleco                      = 0.766555135586412,
    bgoalboth                     = 0.881868546526893,
    bconsplit                     = -0.430219134239337,
    bprice                        = -0.738722430881822,
    sig_borgcit                   = 0.92882026396844,
    sig_borgmun                   = -0.645938944317794,
    sig_bpartiinv                 = -0.399192675421567,
    sig_bpartimem                 = -0.020577855935946,
    sig_bgoalsoc                  = -0.285350976725357,
    sig_bgoaleco                  = -0.242534273649686,
    sig_bgoalboth                 = 0.137397352345915,
    sig_bconsplit                 = 0.743411276457165,
    sig_bprice                    = 1.08476278823894,
    sig_asc                       = 3.67360993981066,
    asc_env_awareness_score       = -0.193233008297175,
    bpartimem_env_awareness_score = -0.00566105652615794,
    asc_sex                       = 0.643327073565721,
    bpartimem_sex                 = -0.0773648935162776,
    asc_age                       = 0.0481907323701647,
    bpartimem_age                 = 0.000332420312287388,
    asc_educ_years                = -0.188251961957932,
    bpartimem_educ_years          = 0.0142140197779785
  )
)
