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

CLASS zcl_curl_http_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_request_config,
             method      TYPE string,
             url         TYPE string,
             headers     TYPE tihttpnvp,
             body        TYPE string,
             timeout     TYPE i,
             auth_type   TYPE string,
             auth_user   TYPE string,
             auth_pass   TYPE string,
             auth_token  TYPE string,
           END OF ty_request_config.

    TYPES: BEGIN OF ty_response_data,
             status_code    TYPE i,
             status_text    TYPE string,
             headers        TYPE tihttpnvp,
             body           TYPE string,
             response_time  TYPE i,
             response_size  TYPE i,
             error_message  TYPE string,
           END OF ty_response_data.

    CLASS-METHODS: execute_request
                     IMPORTING iv_config       TYPE ty_request_config
                     RETURNING VALUE(result)   TYPE ty_response_data
                     RAISING   zcx_rest_client,
                   
                   validate_url
                     IMPORTING iv_url          TYPE string
                     RETURNING VALUE(result)   TYPE abap_bool,
                   
                   format_response_body
                     IMPORTING iv_body         TYPE string
                               iv_content_type TYPE string
                     RETURNING VALUE(result)   TYPE string,
                   
                   parse_content_type
                     IMPORTING iv_content_type TYPE string
                     RETURNING VALUE(result)   TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS: get_rest_client
                     RETURNING VALUE(result) TYPE REF TO zif_rest_client,
                   
                   add_authentication
                     IMPORTING iv_config TYPE ty_request_config
                     CHANGING  ct_headers TYPE tihttpnvp,
                   
                   calculate_response_size
                     IMPORTING iv_body TYPE string
                     RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS zcl_curl_http_service IMPLEMENTATION.

  METHOD execute_request.
    DATA: lo_client TYPE REF TO zif_rest_client,
          lv_start_time TYPE timestampl,
          lv_end_time   TYPE timestampl.

    " Get timestamp before request
    GET TIME STAMP FIELD lv_start_time.

    TRY.
        " Validate URL
        IF validate_url( iv_config-url ) = abap_false.
          result-error_message = 'Invalid URL format'.
          RETURN.
        ENDIF.

        " Get REST client instance
        lo_client = get_rest_client( ).

        " Prepare headers with authentication
        DATA(lt_headers) = iv_config-headers.
        add_authentication( EXPORTING iv_config = iv_config CHANGING ct_headers = lt_headers ).

        " Execute request based on method
        DATA(lo_response) = VALUE REF TO zif_rest_client=>ty_response( ).
        
        CASE to_upper( iv_config-method ).
          WHEN 'GET'.
            lo_response = lo_client->get( 
              url = iv_config-url 
              header = lt_headers 
              timeout = iv_config-timeout ).
              
          WHEN 'POST'.
            lo_response = lo_client->post( 
              url = iv_config-url 
              header = lt_headers 
              body = iv_config-body 
              timeout = iv_config-timeout ).
              
          WHEN 'PUT'.
            lo_response = lo_client->put( 
              url = iv_config-url 
              header = lt_headers 
              body = iv_config-body 
              timeout = iv_config-timeout ).
              
          WHEN 'DELETE'.
            lo_response = lo_client->delete( 
              url = iv_config-url 
              header = lt_headers 
              timeout = iv_config-timeout ).
              
          WHEN 'PATCH'.
            " Note: If PATCH is not supported, we could use POST with X-HTTP-Method-Override header
            APPEND VALUE #( name = 'X-HTTP-Method-Override' value = 'PATCH' ) TO lt_headers.
            lo_response = lo_client->post( 
              url = iv_config-url 
              header = lt_headers 
              body = iv_config-body 
              timeout = iv_config-timeout ).
              
          WHEN OTHERS.
            result-error_message = |Unsupported HTTP method: { iv_config-method }|.
            RETURN.
        ENDCASE.

        " Get timestamp after request
        GET TIME STAMP FIELD lv_end_time.

        " Calculate response time in milliseconds
        result-response_time = ( lv_end_time - lv_start_time ) * 1000.

        " Fill response data
        result-status_code = lo_response->code.
        result-status_text = lo_response->reason.
        result-headers = lo_response->header.
        
        " Format response body based on content type
        READ TABLE lo_response->header INTO DATA(content_type_header) WITH KEY name = 'content-type'.
        IF sy-subrc = 0.
          result-body = format_response_body( 
            iv_body = lo_response->body 
            iv_content_type = content_type_header-value ).
        ELSE.
          result-body = lo_response->body.
        ENDIF.
        
        result-response_size = calculate_response_size( result-body ).

      CATCH zcx_rest_client INTO DATA(lx_exception).
        result-error_message = lx_exception->get_text( ).
        result-status_code = 0.
        result-status_text = 'Error'.
        
        " Get timestamp for error case
        GET TIME STAMP FIELD lv_end_time.
        result-response_time = ( lv_end_time - lv_start_time ) * 1000.
        
    ENDTRY.
  ENDMETHOD.

  METHOD get_rest_client.
    " Use the existing REST client from abap-odata-test
    result = zcl_rest_client=>construct( ).
  ENDMETHOD.

  METHOD validate_url.
    " Basic URL validation
    IF iv_url IS INITIAL.
      result = abap_false.
      RETURN.
    ENDIF.

    " Check for http/https protocol
    IF iv_url CS 'http://' OR iv_url CS 'https://'.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD add_authentication.
    CASE iv_config-auth_type.
      WHEN 'Bearer'.
        IF iv_config-auth_token IS NOT INITIAL.
          APPEND VALUE #( name = 'Authorization' value = |Bearer { iv_config-auth_token }| ) TO ct_headers.
        ENDIF.
        
      WHEN 'Basic'.
        IF iv_config-auth_user IS NOT INITIAL AND iv_config-auth_pass IS NOT INITIAL.
          DATA(lv_credentials) = |{ iv_config-auth_user }:{ iv_config-auth_pass }|.
          " In a real implementation, you would base64 encode the credentials
          " For now, we'll just add them as is (this needs proper base64 encoding)
          APPEND VALUE #( name = 'Authorization' value = |Basic { lv_credentials }| ) TO ct_headers.
        ENDIF.
        
      WHEN 'API-Key'.
        IF iv_config-auth_token IS NOT INITIAL.
          APPEND VALUE #( name = 'X-API-Key' value = iv_config-auth_token ) TO ct_headers.
        ENDIF.
        
      WHEN OTHERS.
        " No authentication or custom authentication handled in headers
    ENDCASE.
  ENDMETHOD.

  METHOD format_response_body.
    DATA(lv_content_type) = parse_content_type( iv_content_type ).
    
    CASE lv_content_type.
      WHEN 'application/json'.
        " Format JSON for better readability
        result = iv_body.
        " Basic JSON formatting (in real implementation, use proper JSON library)
        REPLACE ALL OCCURRENCES OF ',' IN result WITH ',\n  '.
        REPLACE ALL OCCURRENCES OF '{' IN result WITH '{\n  '.
        REPLACE ALL OCCURRENCES OF '}' IN result WITH '\n}'.
        REPLACE ALL OCCURRENCES OF '[' IN result WITH '[\n  '.
        REPLACE ALL OCCURRENCES OF ']' IN result WITH '\n]'.
        
      WHEN 'application/xml' OR 'text/xml'.
        " Format XML for better readability
        result = iv_body.
        " Basic XML formatting
        REPLACE ALL OCCURRENCES OF '><' IN result WITH '>\n<'.
        
      WHEN OTHERS.
        " Return as is for other content types
        result = iv_body.
    ENDCASE.
  ENDMETHOD.

  METHOD parse_content_type.
    " Extract main content type from header (e.g., "application/json; charset=utf-8" -> "application/json")
    SPLIT iv_content_type AT ';' INTO result DATA(dummy).
    result = to_lower( condense( result ) ).
  ENDMETHOD.

  METHOD calculate_response_size.
    " Calculate size in bytes
    result = strlen( iv_body ).
    " In ABAP, each character is typically 2 bytes for Unicode
    result = result * cl_abap_conv_codepage=>sap_codepage( )->utf8_bandwidth.
  ENDMETHOD.

ENDCLASS.
