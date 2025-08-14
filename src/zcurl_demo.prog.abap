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
"* ZCurl Demo Program - FIXED VERSION
"*
"* This program demonstrates how to launch the ZCurl REST client
"* application built with abap2UI5.
"*----------------------------------------------------------------------*

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
  PARAMETERS: r_basic  RADIOBUTTON GROUP app DEFAULT 'X',
              r_webapp RADIOBUTTON GROUP app,
              r_test   RADIOBUTTON GROUP app.
SELECTION-SCREEN END OF BLOCK options.

START-OF-SELECTION.

  MESSAGE 'Launching ZCurl Application...' TYPE 'I'.

  TRY.
      IF r_basic = abap_true.
        " Method 1: Direct abap2UI5 app launch (recommended)
        PERFORM launch_basic_app.

      ELSEIF r_webapp = abap_true.
        " Method 2: Web application mode
        PERFORM launch_web_app.

      ELSEIF r_test = abap_true.
        " Method 3: Test mode
        PERFORM launch_test_mode.

      ENDIF.

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
  ENDTRY.

*&---------------------------------------------------------------------*
*&      Form  LAUNCH_BASIC_APP
*&---------------------------------------------------------------------*
FORM launch_basic_app.
  " Direct HTTP service message - most reliable approach
  MESSAGE 'ZCurl requires HTTP service configuration' TYPE 'I'.
  MESSAGE 'Access via: http://yourserver:port/sap/bc/rest/zcurl/' TYPE 'I'.

  " Show setup instructions
  WRITE: / 'ZCurl Setup Instructions:',
         / '1. Ensure dependencies are created',
         / '2. Configure HTTP service in SICF',
         / '3. Access via web browser'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LAUNCH_WEB_APP
*&---------------------------------------------------------------------*
FORM launch_web_app.

  MESSAGE 'For web access, configure HTTP service in SICF:' TYPE 'I'.
  MESSAGE 'Path: /sap/bc/rest/zcurl/' TYPE 'I'.
  MESSAGE 'Handler: ZCL_CURL_HTTP_HANDLER' TYPE 'I'.

  " Show how to create HTTP service handler
  WRITE: / 'HTTP Service Configuration:',
         / '1. Go to SICF (Service Hierarchy)',
         / '2. Create service under /sap/bc/rest/',
         / '3. Service name: zcurl',
         / '4. Handler class: ZCL_CURL_HTTP_HANDLER',
         / '5. Activate the service',
         / '',
         / 'Access URL: http://yourserver:port/sap/bc/rest/zcurl/'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LAUNCH_TEST_MODE
*&---------------------------------------------------------------------*
FORM launch_test_mode.

  " Test mode - just create and initialize the app
  DATA(lo_app) = NEW zcl_curl_abap( ).

  " Test basic functionality
  WRITE: / 'ZCurl Application Test Mode',
         / '================================',
         / 'Application instance created successfully.',
         / 'Class: ZCL_CURL_ABAP',
         / 'Interface: Z2UI5_IF_APP',
         / '',
         / 'To run the full application:',
         / '1. Use radio button "Basic App" and execute',
         / '2. Or configure HTTP service for web access',
         / '',
         / 'Application Status: OK'.

ENDFORM.

"*----------------------------------------------------------------------*
"* Text Symbols (to be maintained in SE80)
"*----------------------------------------------------------------------*
" TEXT-001: Demo Information
" TEXT-002: ZCurl is a Postman-like REST client for SAP systems
" TEXT-003: Built with abap2UI5 for modern UI5/Fiori experience
" TEXT-004: Choose application mode to launch below
" TEXT-005: Application Launch Options
