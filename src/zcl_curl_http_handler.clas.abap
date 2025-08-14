*&---------------------------------------------------------------------*
*& Class ZCL_CURL_HTTP_HANDLER
*& HTTP Extension Handler for ZCurl Application
*&---------------------------------------------------------------------*
CLASS zcl_curl_http_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    METHODS:
      handle_zcurl_request
        IMPORTING
          io_server TYPE REF TO if_http_server,

      handle_cors
        IMPORTING
          io_server TYPE REF TO if_http_server,

      set_content_type
        IMPORTING
          io_response TYPE REF TO if_http_response
          iv_type     TYPE string DEFAULT 'text/html',

      handle_manual_request
        IMPORTING
          io_server TYPE REF TO if_http_server,

      log_request
        IMPORTING
          io_request TYPE REF TO if_http_request,

      handle_zcurl_manually
        IMPORTING
          io_server TYPE REF TO if_http_server,

      create_basic_interface
        IMPORTING
          io_server TYPE REF TO if_http_server,

      get_basic_html_interface
        RETURNING VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS zcl_curl_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    " Main entry point for HTTP requests

    TRY.
        " Log the incoming request
        log_request( server->request ).

        " Handle CORS preflight requests
        handle_cors( server ).

        " Handle the ZCurl application request
        handle_zcurl_request( server ).

      CATCH cx_root INTO DATA(lx_error).
        " Handle any errors gracefully
        server->response->set_status(
          code   = '500'
          reason = |Error: { lx_error->get_text( ) }|
        ).

        set_content_type(
          io_response = server->response
          iv_type     = 'application/json'
        ).

        DATA(lv_error_json) = |\{"error": "{ lx_error->get_text( ) }", "timestamp": "{ sy-datum }T{ sy-uzeit }"\}|.
        server->response->set_cdata( lv_error_json ).
    ENDTRY.

  ENDMETHOD.

  METHOD handle_zcurl_request.
    " Handle the ZCurl application using abap2UI5

    " Method 1: Use the correct abap2UI5 HTTP handler call
    " The z2ui5_cl_http_handler=>run() typically only takes server parameter
    z2ui5_cl_http_handler=>run( io_server ).



  ENDMETHOD.

  METHOD handle_cors.
    " Handle Cross-Origin Resource Sharing (CORS)

    " Set CORS headers for browser compatibility
    io_server->response->set_header_field(
      name  = 'Access-Control-Allow-Origin'
      value = '*'
    ).

    io_server->response->set_header_field(
      name  = 'Access-Control-Allow-Methods'
      value = 'GET, POST, PUT, DELETE, OPTIONS'
    ).

    io_server->response->set_header_field(
      name  = 'Access-Control-Allow-Headers'
      value = 'Content-Type, Authorization, X-Requested-With'
    ).

    " Handle OPTIONS preflight request
    IF io_server->request->get_method( ) = 'OPTIONS'.
      io_server->response->set_status(
        code   = '200'
        reason = 'OK'
      ).
    ENDIF.

  ENDMETHOD.

  METHOD set_content_type.
    " Set appropriate content type header

    io_response->set_header_field(
      name  = 'Content-Type'
      value = SWITCH #( iv_type
        WHEN 'json' THEN 'application/json; charset=utf-8'
        WHEN 'html' THEN 'text/html; charset=utf-8'
        WHEN 'css'  THEN 'text/css; charset=utf-8'
        WHEN 'js'   THEN 'application/javascript; charset=utf-8'
        ELSE iv_type
      )
    ).

  ENDMETHOD.

  METHOD log_request.
    " Log request details for debugging

    DATA(lv_method) = io_request->get_method( ).
    DATA(lv_path) = io_request->get_header_field( 'PATH_INFO' ).

    " Log to application log (optional)
    " You can implement your logging logic here

  ENDMETHOD.

  METHOD handle_manual_request.
    " Manual request handling when abap2UI5 framework methods are not available

    " Set basic HTML response
    set_content_type(
      io_response = io_server->response
      iv_type     = 'html'
    ).

    DATA(lv_html) = |<!DOCTYPE html>|
                 && |<html><head><title>ZCurl Application</title></head>|
                 && |<body>|
                 && |<h1>ZCurl REST Client</h1>|
                 && |<p>The application is running, but abap2UI5 framework integration needs to be configured.</p>|
                 && |<p>Please ensure:</p>|
                 && |<ul>|
                 && |<li>abap2UI5 framework is properly installed</li>|
                 && |<li>All required classes are available</li>|
                 && |<li>System has proper authorizations</li>|
                 && |</ul>|
                 && |<p>System Information:</p>|
                 && |<ul>|
                 && |<li>Date: { sy-datum }</li>|
                 && |<li>Time: { sy-uzeit }</li>|
                 && |<li>User: { sy-uname }</li>|
                 && |<li>Client: { sy-mandt }</li>|
                 && |</ul>|
                 && |</body></html>|.

    io_server->response->set_cdata( lv_html ).
    io_server->response->set_status(
      code   = '200'
      reason = 'OK'
    ).

  ENDMETHOD.

  METHOD handle_zcurl_manually.
    " Manual handling of ZCurl application when abap2UI5 is not available

    TRY.
        " Create ZCurl application instance
        DATA(lo_app) = NEW zcl_curl_abap( ).

        " Create a mock client for the application
        DATA(lo_client) = z2ui5_cl_util_http=>factory( io_server ).

        " Try to call the application main method
        " Note: This is a simplified approach and may not work perfectly
        " The proper way is through the abap2UI5 framework
        lo_app->z2ui5_if_app~main( CAST z2ui5_if_client( lo_client ) ).

      CATCH cx_root INTO DATA(lx_error).
        " If direct app execution fails, show a basic interface
        create_basic_interface( io_server ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_basic_interface.
    " Create a basic HTML interface as fallback

    set_content_type(
      io_response = io_server->response
      iv_type     = 'html'
    ).

    DATA(lv_html) = get_basic_html_interface( ).

    io_server->response->set_cdata( lv_html ).
    io_server->response->set_status(
      code   = '200'
      reason = 'OK'
    ).

  ENDMETHOD.

  METHOD get_basic_html_interface.
    " Generate basic HTML interface

    DATA(lv_html) =
      |<!DOCTYPE html>| &&
      |<html lang="en">| &&
      |<head>| &&
      |  <meta charset="UTF-8">| &&
      |  <meta name="viewport" content="width=device-width, initial-scale=1.0">| &&
      |  <title>ZCurl - REST Client</title>| &&
      |  <style>| &&
      |    body \{ font-family: Arial, sans-serif; margin: 20px; \}| &&
      |    .container \{ max-width: 800px; margin: 0 auto; \}| &&
      |    .form-group \{ margin-bottom: 15px; \}| &&
      |    label \{ display: block; margin-bottom: 5px; font-weight: bold; \}| &&
      |    input, select, textarea \{ width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; \}| &&
      |    button \{ background: #4CAF50; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; \}| &&
      |    button:hover \{ background: #45a049; \}| &&
      |    .response \{ background: #f9f9f9; padding: 15px; border-radius: 4px; margin-top: 20px; \}| &&
      |    .url-bar \{ display: flex; gap: 10px; align-items: center; \}| &&
      |    .method-select \{ width: 120px; flex-shrink: 0; \}| &&
      |  </style>| &&
      |</head>| &&
      |<body>| &&
      |  <div class="container">| &&
      |    <h1>ZCurl - REST Client</h1>| &&
      |    <p><strong>Note:</strong> This is a basic fallback interface. For full functionality, ensure abap2UI5 framework is properly configured.</p>| &&
      |    | &&
      |    <form method="POST" action="">| &&
      |      <div class="form-group">| &&
      |        <label>Request URL:</label>| &&
      |        <div class="url-bar">| &&
      |          <select name="method" class="method-select">| &&
      |            <option value="GET">GET</option>| &&
      |            <option value="POST">POST</option>| &&
      |            <option value="PUT">PUT</option>| &&
      |            <option value="DELETE">DELETE</option>| &&
      |          </select>| &&
      |          <input type="url" name="url" placeholder="https://api.example.com/endpoint" required>| &&
      |        </div>| &&
      |      </div>| &&
      |      | &&
      |      <div class="form-group">| &&
      |        <label>Headers (JSON format):</label>| &&
      |        <textarea name="headers" rows="3" placeholder='\{"Content-Type": "application/json", "Authorization": "Bearer token"\}'></textarea>| &&
      |      </div>| &&
      |      | &&
      |      <div class="form-group">| &&
      |        <label>Request Body:</label>| &&
      |        <textarea name="body" rows="6" placeholder='Enter request body here...'></textarea>| &&
      |      </div>| &&
      |      | &&
      |      <button type="submit">Send Request</button>| &&
      |    </form>| &&
      |    | &&
      |    <div class="response">| &&
      |      <h3>Setup Instructions:</h3>| &&
      |      <ol>| &&
      |        <li>Ensure abap2UI5 framework is installed</li>| &&
      |        <li>Check class Z2UI5_CL_HTTP_HANDLER exists</li>| &&
      |        <li>Verify HTTP service configuration in SICF</li>| &&
      |        <li>Test with proper abap2UI5 integration</li>| &&
      |      </ol>| &&
      |    </div>| &&
      |  </div>| &&
      |</body>| &&
      |</html>|.

    rv_result = lv_html.

  ENDMETHOD.

ENDCLASS.
