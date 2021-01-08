*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************

 INCLUDE LZSST_GTTTOP.                   " Global Declarations
 "Function Modules
INCLUDE LZSST_GTTD00.
* INCLUDE lzsst_gtt_v2d00.                      " Local interfaces definition - General + Transaction Processing
INCLUDE LZSST_GTTD01.
* INCLUDE lzsst_gtt_v2d01.                      " Local interfaces definition - Actual Events
INCLUDE LZSST_GTTD10.
* INCLUDE lzsst_gtt_v2d10.                      " Local class definition - General for Transaction Processing
INCLUDE LZSST_GTTD11.
* INCLUDE lzsst_gtt_v2d11.                      " Local class definition - General for Actual Events
INCLUDE LZSST_GTTD20.
* INCLUDE lzsst_gtt_v2d20.                      " Local class definition - Business Object Readers
INCLUDE LZSST_GTTD30.
* INCLUDE lzsst_gtt_v2d30.                      " Local class definition - Planned Events Fillers
INCLUDE LZSST_GTTD40.
* INCLUDE lzsst_gtt_v2d40.                      " Local class definition - Actual Events Fillers
INCLUDE LZSST_GTTD90.
* INCLUDE lzsst_gtt_v2d90.
 INCLUDE LZSST_GTTUXX.


*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LZSST_GTT_V2F...                   " Subroutines
* INCLUDE LZSST_GTT_V2O...                   " PBO-Modules
* INCLUDE LZSST_GTT_V2I...                   " PAI-Modules
* INCLUDE LZSST_GTT_V2E...                   " Events
* INCLUDE LZSST_GTT_V2P...                   " Local class implement.
* INCLUDE LZSST_GTT_V2T99.                   " ABAP Unit tests
INCLUDE zsst_gtt_ae_imp.
