CLASS zcl_curl_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    " Simple public attributes for binding
    DATA: mv_url             TYPE string,
          mv_method          TYPE string VALUE 'GET',
          mv_request_body    TYPE string,
          mv_response_body   TYPE string,
          mv_response_status TYPE string,
          mv_response_time   TYPE string,
          mv_response_size   TYPE string.

  PRIVATE SECTION.
    DATA: mo_client TYPE REF TO z2ui5_if_client.

    METHODS: handle_send_request,
      build_view
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view.

ENDCLASS.

CLASS zcl_curl_abap IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    mo_client = client.

    " Handle events
    CASE client->get( )-event.
      WHEN 'SEND_REQUEST'.
        handle_send_request( ).
      WHEN 'METHOD_GET'.
        mv_method = 'GET'.
      WHEN 'METHOD_POST'.
        mv_method = 'POST'.
      WHEN 'METHOD_PUT'.
        mv_method = 'PUT'.
      WHEN 'METHOD_DELETE'.
        mv_method = 'DELETE'.
      WHEN OTHERS.
        " Initialize on first load
        IF mv_url IS INITIAL.
          mv_url = 'https://jsonplaceholder.typicode.com/posts/1'.
          mv_method = 'GET'.
          mv_response_status = 'Ready to send request'.
        ENDIF.
    ENDCASE.

    " Build and display view
    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    build_view( view ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD build_view.
    " Ultra-simple UI to avoid any binding issues
    DATA(page) = io_view->shell( )->page(
      title = 'ZCurl - REST Client'
      shownavbutton = abap_false
      class = 'sapUiContentPadding' ).

    " Main content panel
    DATA(panel) = page->content( )->panel(
      headertext = 'Request Configuration'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    " Use VBox for simple layout
    DATA(vbox) = panel->content( )->vbox( class = 'sapUiMediumMargin' ).

    " HTTP Method selection - using buttons instead of combobox
    vbox->label( text = 'HTTP Method:' class = 'sapUiMediumMarginBottom' ).

    DATA(method_buttons) = vbox->hbox( class = 'sapUiTinyMarginBottom' ).

    method_buttons->button(
      text = 'GET'
      type = COND #( WHEN mv_method = 'GET' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_GET' )
      class = 'sapUiTinyMarginEnd' ).

    method_buttons->button(
      text = 'POST'
      type = COND #( WHEN mv_method = 'POST' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_POST' )
      class = 'sapUiTinyMarginEnd' ).

    method_buttons->button(
      text = 'PUT'
      type = COND #( WHEN mv_method = 'PUT' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_PUT' )
      class = 'sapUiTinyMarginEnd' ).

    method_buttons->button(
      text = 'DELETE'
      type = COND #( WHEN mv_method = 'DELETE' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_DELETE' ) ).

    " Current method display
    vbox->text(
      text = |Selected Method: { mv_method }|
      class = 'sapUiMediumMarginBottom' ).

    " URL input
    vbox->label( text = 'Request URL:' class = 'sapUiSmallMarginBottom' ).
    vbox->input(
      value = mo_client->_bind_edit( mv_url )
      placeholder = 'Enter URL (e.g., https://api.example.com/data)'
      width = '100%'
      class = 'sapUiMediumMarginBottom' ).

    " Request body (shown only for POST/PUT)
    IF mv_method = 'POST' OR mv_method = 'PUT'.
      vbox->label( text = 'Request Body (JSON):' class = 'sapUiSmallMarginBottom' ).
      vbox->text_area(
        value = mo_client->_bind_edit( mv_request_body )
        placeholder = 'Enter JSON body...'
        rows = '6'
        width = '100%'
        class = 'sapUiMediumMarginBottom' ).
    ENDIF.

    " Send button
    vbox->button(
      text = |Send { mv_method } Request|
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = mo_client->_event( 'SEND_REQUEST' )
      class = 'sapUiMediumMarginBottom' ).

    " Response section
    DATA(response_panel) = page->content( )->panel(
      headertext = |Response Status: { mv_response_status }|
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(response_vbox) = response_panel->content( )->vbox( ).

    " Response info
    IF mv_response_time IS NOT INITIAL.
      response_vbox->text(
        text = |Response Time: { mv_response_time } Size: { mv_response_size }|
        class = 'sapUiSmallMarginBottom' ).
    ENDIF.

    " Response body
    response_vbox->text_area(
      value = mo_client->_bind( mv_response_body )
      editable = abap_false
      rows = '12'
      width = '100%'
      placeholder = 'Response will appear here after sending request...' ).

  ENDMETHOD.

  METHOD handle_send_request.
    " HTTP request implementation using standard SAP classes
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_start_time  TYPE timestampl,
          lv_end_time    TYPE timestampl.

    " Clear previous response
    CLEAR: mv_response_body, mv_response_time, mv_response_size.
    mv_response_status = 'Sending request...'.

    TRY.
        " Record start time
        GET TIME STAMP FIELD lv_start_time.

        " Validate URL
        IF mv_url IS INITIAL.
          mv_response_status = 'Error: URL is required'.
          RETURN.
        ENDIF.

        " Create HTTP client
        cl_http_client=>create_by_url(
          EXPORTING
            url    = mv_url
          IMPORTING
            client = lo_http_client ).

        " Set HTTP method
        lo_http_client->request->set_method( mv_method ).

        " Set request body for POST/PUT
        IF ( mv_method = 'POST' OR mv_method = 'PUT' ) AND mv_request_body IS NOT INITIAL.
          lo_http_client->request->set_cdata( mv_request_body ).
          lo_http_client->request->set_header_field(
            name  = 'Content-Type'
            value = 'application/json' ).
        ENDIF.

        " Set common headers
        lo_http_client->request->set_header_field(
          name  = 'Accept'
          value = 'application/json' ).

        lo_http_client->request->set_header_field(
          name  = 'User-Agent'
          value = 'ZCurl-ABAP-Client/1.0' ).

        " Send request and receive response
        lo_http_client->send( ).
        lo_http_client->receive( ).

        " Get response data
        TYPES: BEGIN OF ty_status,
                 code   TYPE i,
                 reason TYPE string,
               END OF ty_status.
        DATA ls_status TYPE ty_status.
        lo_http_client->response->get_status( ).
        mv_response_status = |{ ls_status-code } { ls_status-reason }|.
        mv_response_body = lo_http_client->response->get_cdata( ).

        " Calculate response time
        GET TIME STAMP FIELD lv_end_time.
        DATA(lv_duration) = lv_end_time - lv_start_time.
        mv_response_time = |{ lv_duration * 1000 DECIMALS = 0 } ms|.

        " Calculate response size
        mv_response_size = |{ strlen( mv_response_body ) } bytes|.

        " Basic JSON formatting for readability
        IF mv_response_body CS '{' OR mv_response_body CS '['.
          " Simple JSON pretty printing
          REPLACE ALL OCCURRENCES OF ',' IN mv_response_body WITH ',\n'.
          REPLACE ALL OCCURRENCES OF '{' IN mv_response_body WITH '{\n  '.
          REPLACE ALL OCCURRENCES OF '}' IN mv_response_body WITH '\n}'.
          REPLACE ALL OCCURRENCES OF '[' IN mv_response_body WITH '[\n  '.
          REPLACE ALL OCCURRENCES OF ']' IN mv_response_body WITH '\n]'.
        ENDIF.

        " Close HTTP client
        lo_http_client->close( ).

      CATCH cx_root INTO DATA(lx_error).
        " Handle errors
        mv_response_status = |Error: { lx_error->get_text( ) }|.
        mv_response_body = |Request failed with error:\n{ lx_error->get_text( ) }\n\nPlease check:\n- URL is correct and accessible\n- Network connectivity\n- Firewall settings|.
        mv_response_time = '0 ms'.
        mv_response_size = '0 bytes'.

        " Close client if bound
        IF lo_http_client IS BOUND.
          TRY.
              lo_http_client->close( ).
            CATCH cx_root ##NO_HANDLER.
              " Ignore close errors
          ENDTRY.
        ENDIF.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
