CLASS zcl_curl_http_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.

  PRIVATE SECTION.
    METHODS: handle_request
               IMPORTING
                 io_server TYPE REF TO if_http_server.

ENDCLASS.

CLASS zcl_curl_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    handle_request( server ).
  ENDMETHOD.

  METHOD handle_request.
    " Simplified HTTP handler to avoid binding issues

    TRY.
        " Set CORS headers for browser compatibility
        io_server->response->set_header_field(
          name  = 'Access-Control-Allow-Origin'
          value = '*' ).

        io_server->response->set_header_field(
          name  = 'Access-Control-Allow-Methods'
          value = 'GET, POST, PUT, DELETE, OPTIONS' ).

        io_server->response->set_header_field(
          name  = 'Access-Control-Allow-Headers'
          value = 'Content-Type, Authorization' ).

        " Handle OPTIONS preflight
        IF io_server->request->get_method( ) = 'OPTIONS'.
          io_server->response->set_status( code = 200 reason = 'OK' ).
          RETURN.
        ENDIF.

        " Try to use abap2UI5 framework
        z2ui5_cl_http_handler=>run( io_server ).

      CATCH cx_root INTO DATA(lx_error).
        " If abap2UI5 fails, provide a basic fallback
        io_server->response->set_header_field(
          name  = 'Content-Type'
          value = 'text/html; charset=utf-8' ).

        DATA(lv_html) =
          |<!DOCTYPE html>| &&
          |<html>| &&
          |<head><title>ZCurl Application Error</title></head>| &&
          |<body>| &&
          |<h1>ZCurl Application</h1>| &&
          |<p><strong>Error:</strong> { lx_error->get_text( ) }</p>| &&
          |<p>Please check:</p>| &&
          |<ul>| &&
          |<li>abap2UI5 framework is properly installed</li>| &&
          |<li>ZCL_CURL_ABAP class exists and is active</li>| &&
          |<li>All binding attributes are public in the class</li>| &&
          |</ul>| &&
          |<p>System: { sy-sysid } Client: { sy-mandt } User: { sy-uname }</p>| &&
          |</body>| &&
          |</html>|.

        io_server->response->set_cdata( lv_html ).
        io_server->response->set_status( code = 500 reason = 'Internal Server Error' ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
