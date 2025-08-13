*MIT License
*
*Copyright (c) 2025 BlueFunda
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.

REPORT zcurl_demo.

"*----------------------------------------------------------------------*
"* ZCurl Demo Program
"* 
"* This program demonstrates how to launch the ZCurl REST client
"* application built with abap2UI5.
"*
"* Features demonstrated:
"* - Basic application startup
"* - Enhanced application with advanced features
"* - Integration examples
"*----------------------------------------------------------------------*

PARAMETERS: p_app TYPE string DEFAULT 'ENHANCED' OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK demo WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(50) TEXT-002.
  SELECTION-SCREEN END OF LINE.
  
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(50) TEXT-003.
  SELECTION-SCREEN END OF LINE.
  
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(50) TEXT-004.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK demo.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE TEXT-005.
  PARAMETERS: r_basic TYPE abap_bool RADIOBUTTON GROUP app DEFAULT 'X',
              r_enhanc TYPE abap_bool RADIOBUTTON GROUP app.
SELECTION-SCREEN END OF BLOCK options.

START-OF-SELECTION.

  " Determine which app to launch
  DATA(lo_app) = VALUE REF TO z2ui5_if_app( ).
  
  IF r_basic = abap_true.
    " Launch basic ZCurl application
    lo_app = NEW zcl_curl_app( ).
    MESSAGE 'Launching ZCurl Basic Application...' TYPE 'I'.
    
  ELSEIF r_enhanc = abap_true.
    " Launch enhanced ZCurl application
    lo_app = NEW zcl_curl_app_enhanced( ).
    MESSAGE 'Launching ZCurl Enhanced Application...' TYPE 'I'.
    
  ENDIF.

  " Start the abap2UI5 application
  TRY.
      " This assumes you have abap2UI5 framework installed
      " The actual startup method may vary depending on your abap2UI5 version
      z2ui5_cl_http_handler=>main( lo_app ).
      
    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

"*----------------------------------------------------------------------*
"* Text Symbols
"*----------------------------------------------------------------------*
" TEXT-001: Demo Information
" TEXT-002: ZCurl is a Postman-like REST client for SAP systems
" TEXT-003: Built with abap2UI5 for modern UI5/Fiori experience  
" TEXT-004: Choose application version to launch below
" TEXT-005: Application Options
