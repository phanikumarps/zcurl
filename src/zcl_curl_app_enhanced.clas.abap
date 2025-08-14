CLASS zcl_curl_app_enhanced DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES: BEGIN OF ty_header_row,
             key     TYPE string,
             value   TYPE string,
             enabled TYPE abap_bool,
           END OF ty_header_row.

    TYPES: BEGIN OF ty_param_row,
             key     TYPE string,
             value   TYPE string,
             enabled TYPE abap_bool,
           END OF ty_param_row.

    TYPES: BEGIN OF ty_auth_config,
             type  TYPE string,
             user  TYPE string,
             pass  TYPE string,
             token TYPE string,
           END OF ty_auth_config.

    TYPES: BEGIN OF ty_response_header,
             name  TYPE string,
             value TYPE string,
           END OF ty_response_header.

    TYPES: BEGIN OF ty_environment,
             name  TYPE string,
             value TYPE string,
           END OF ty_environment.

    TYPES: BEGIN OF ty_collection_item,
             id          TYPE string,
             name        TYPE string,
             method      TYPE string,
             url         TYPE string,
             description TYPE string,
           END OF ty_collection_item.

    TYPES: tt_headers          TYPE TABLE OF ty_header_row,
           tt_params           TYPE TABLE OF ty_param_row,
           tt_response_headers TYPE TABLE OF ty_response_header,
           tt_environments     TYPE TABLE OF ty_environment,
           tt_collections      TYPE TABLE OF ty_collection_item.

    " ALL BINDING DATA MUST BE PUBLIC
    DATA: mv_url              TYPE string,
          mv_method           TYPE string VALUE 'GET',
          mv_request_body     TYPE string,
          mv_response_body    TYPE string,
          mv_response_status  TYPE string,
          mv_response_time    TYPE string,
          mv_response_size    TYPE string,
          mv_selected_tab     TYPE string VALUE 'request',
          mv_request_tab      TYPE string VALUE 'params',
          mv_response_tab     TYPE string VALUE 'body',
          mv_auth_tab         TYPE string VALUE 'none',
          mv_collection_name  TYPE string,
          mv_request_name     TYPE string,
          mv_request_desc     TYPE string,
          mv_environment      TYPE string VALUE 'Development',
          mv_timeout          TYPE i VALUE 30,
          " Simple header fields to avoid complex table binding
          mv_content_type     TYPE string VALUE 'application/json',
          mv_authorization    TYPE string,
          mv_custom_header1   TYPE string,
          mv_custom_header2   TYPE string,
          mt_headers          TYPE tt_headers,
          mt_params           TYPE tt_params,
          mt_response_headers TYPE tt_response_headers,
          mt_collections      TYPE tt_collections,
          mt_environments     TYPE tt_environments,
          ms_auth_config      TYPE ty_auth_config,
          mv_show_save_dialog TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mo_client TYPE REF TO z2ui5_if_client.

    METHODS: initialize_app,
      handle_send_request,
      handle_save_request,
      handle_load_request
        IMPORTING iv_request_id TYPE string,
      handle_auth_change,
      build_main_view
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_url_bar_inline
        IMPORTING io_vbox TYPE REF TO z2ui5_cl_xml_view,
      build_request_panel_inline
        IMPORTING io_vbox TYPE REF TO z2ui5_cl_xml_view,
      build_response_panel_inline
        IMPORTING io_vbox TYPE REF TO z2ui5_cl_xml_view,
      build_request_panel
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_response_panel
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_sidebar
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_url_bar
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      process_environment_vars
        IMPORTING iv_text       TYPE string
        RETURNING VALUE(result) TYPE string,
      add_empty_header_row,
      add_empty_param_row.

ENDCLASS.

CLASS zcl_curl_app_enhanced IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    mo_client = client.
    DATA(lo_client) = client->get( ).
    CASE client->get( )-event.
      WHEN 'SEND_REQUEST'.
        handle_send_request( ).
      WHEN 'SAVE_REQUEST'.
        handle_save_request( ).
      WHEN 'LOAD_REQUEST'.
        IF lines( client->get( )-t_event_arg ) > 0.
          handle_load_request( lo_client-t_event_arg[ 1 ] ).
        ENDIF.
      WHEN 'ADD_HEADER'.
        add_empty_header_row( ).
      WHEN 'ADD_PARAM'.
        add_empty_param_row( ).
      WHEN 'TAB_SELECT'.
        IF lines( client->get( )-t_event_arg ) > 0.
          mv_selected_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'REQUEST_TAB_SELECT'.
        IF lines( client->get( )-t_event_arg ) > 0.
          mv_request_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'RESPONSE_TAB_SELECT'.
        IF lines( client->get( )-t_event_arg ) > 0.
          mv_response_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'AUTH_CHANGE'.
        handle_auth_change( ).
      WHEN 'METHOD_GET'.
        mv_method = 'GET'.
      WHEN 'METHOD_POST'.
        mv_method = 'POST'.
      WHEN 'METHOD_PUT'.
        mv_method = 'PUT'.
      WHEN 'METHOD_DELETE'.
        mv_method = 'DELETE'.
      WHEN 'METHOD_PATCH'.
        mv_method = 'PATCH'.
      WHEN OTHERS.
        initialize_app( ).
    ENDCASE.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    build_main_view( view ).
    client->view_display( view->stringify( ) ).
  ENDMETHOD.

  METHOD initialize_app.
    " Initialize without external dependencies
    IF mv_url IS INITIAL.
      mv_url = 'https://jsonplaceholder.typicode.com/posts/1'.
      mv_response_status = 'Ready to send request'.
    ENDIF.

    " Initialize default headers if empty
    IF lines( mt_headers ) = 0.
      APPEND VALUE #( key = 'Content-Type' value = 'application/json' enabled = abap_true ) TO mt_headers.
      APPEND VALUE #( key = 'Accept' value = 'application/json' enabled = abap_true ) TO mt_headers.
      add_empty_header_row( ).
    ENDIF.

    " Initialize default params if empty
    IF lines( mt_params ) = 0.
      add_empty_param_row( ).
    ENDIF.

    " Initialize sample environments
    IF lines( mt_environments ) = 0.
      APPEND VALUE #( name = 'Development' value = 'https://api-dev.example.com' ) TO mt_environments.
      APPEND VALUE #( name = 'Staging' value = 'https://api-staging.example.com' ) TO mt_environments.
      APPEND VALUE #( name = 'Production' value = 'https://api.example.com' ) TO mt_environments.
    ENDIF.

    " Initialize sample collections
    IF lines( mt_collections ) = 0.
      APPEND VALUE #( id = '1' name = 'Get Posts' method = 'GET'
                     url = 'https://jsonplaceholder.typicode.com/posts'
                     description = 'Get all posts' ) TO mt_collections.
      APPEND VALUE #( id = '2' name = 'Get User' method = 'GET'
                     url = 'https://jsonplaceholder.typicode.com/users/1'
                     description = 'Get user by ID' ) TO mt_collections.
    ENDIF.
  ENDMETHOD.

  METHOD build_main_view.
    " Simplified main page layout using proper UI5 controls
    DATA(page) = io_view->shell( )->page(
      title = 'ZCurl - Enhanced REST Client'
      shownavbutton = abap_false
      class = 'sapUiContentPadding' ).

    " Add custom styles
    page->_generic( name = 'style' ns = 'html' )->_cc_plain_xml(
      |.curl-method-get \{ color: #61affe; font-weight: bold; \}| &&
      |.curl-method-post \{ color: #49cc90; font-weight: bold; \}| &&
      |.curl-method-put \{ color: #fca130; font-weight: bold; \}| &&
      |.curl-method-delete \{ color: #f93e3e; font-weight: bold; \}| &&
      |.curl-method-patch \{ color: #50e3c2; font-weight: bold; \}| &&
      |.curl-status-success \{ color: #49cc90; \}| &&
      |.curl-status-error \{ color: #f93e3e; \}| &&
      |.curl-status-warning \{ color: #fca130; \}| ).

    " Use proper UI5 VBox layout
    DATA(main_vbox) = page->vbox( class = 'sapUiMediumMargin' ).

    " Collections section (simplified sidebar)
    DATA(collections_panel) = main_vbox->panel(
      headertext = 'Saved Requests'
      expanded = abap_false
      class = 'sapUiResponsiveMargin' ).

    collections_panel->list(
      items = mo_client->_bind( mt_collections )
      mode = 'SingleSelectMaster'
      itempress = mo_client->_event(
        val = 'LOAD_REQUEST'
        t_arg = VALUE #( ( `${id}` ) ) )
    )->standard_list_item(
      title = '{name}'
      description = '{description}'
      info = '{method}'
      type = 'Active' ).

    " URL bar section
    build_url_bar_inline( main_vbox ).

    " Request configuration section
    build_request_panel_inline( main_vbox ).

    " Response section
    build_response_panel_inline( main_vbox ).
  ENDMETHOD.

  METHOD build_url_bar.
    " URL input section with method buttons
    DATA(url_panel) = io_view->content( )->panel(
      headertext = 'Request URL'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(url_vbox) = url_panel->content( )->vbox( ).

    " Method selection buttons
    url_vbox->label( text = 'HTTP Method:' class = 'sapUiSmallMarginBottom' ).

    DATA(method_buttons) = url_vbox->hbox( class = 'sapUiTinyMarginBottom' ).

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
      press = mo_client->_event( 'METHOD_DELETE' )
      class = 'sapUiTinyMarginEnd' ).

    method_buttons->button(
      text = 'PATCH'
      type = COND #( WHEN mv_method = 'PATCH' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_PATCH' ) ).

    " URL input
    url_vbox->label( text = 'URL:' class = 'sapUiSmallMarginTop sapUiSmallMarginBottom' ).

    DATA(url_hbox) = url_vbox->hbox( ).
    url_hbox->input(
      value = mo_client->_bind_edit( mv_url )
      placeholder = 'Enter request URL...'
      width = '70%'
      class = 'sapUiTinyMarginEnd' ).

    url_hbox->button(
      text = |Send { mv_method }|
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = mo_client->_event( 'SEND_REQUEST' ) ).

    url_hbox->button(
      text = 'Save'
      type = 'Default'
      icon = 'sap-icon://save'
      press = mo_client->_event( 'SAVE_REQUEST' )
      class = 'sapUiTinyMarginBegin' ).
  ENDMETHOD.

  METHOD build_request_panel.
    " Request configuration panel
    DATA(request_panel) = io_view->content( )->panel(
      headertext = 'Request Configuration'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(request_vbox) = request_panel->content( )->vbox( ).

    " Request body section (only for POST/PUT/PATCH)
    IF mv_method = 'POST' OR mv_method = 'PUT' OR mv_method = 'PATCH'.
      request_vbox->label( text = 'Request Body (JSON):' class = 'sapUiMediumMarginBottom' ).
      request_vbox->text_area(
        value = mo_client->_bind_edit( mv_request_body )
        placeholder = 'Enter JSON request body...'
        rows = '8'
        width = '100%'
        class = 'sapUiMediumMarginBottom' ).
    ENDIF.

    " Headers section (simplified)
    request_vbox->label( text = 'Request Headers:' class = 'sapUiMediumMarginBottom' ).

    " Simple headers table
    DATA(headers_table) = request_vbox->table(
      items = mo_client->_bind( mt_headers )
      growing = abap_true
      growingthreshold = '5'
      class = 'sapUiMediumMarginBottom' ).

    headers_table->columns(
      )->column( width = '30%' )->text( text = 'Header Name'
      )->column( width = '50%' )->text( text = 'Header Value'
      )->column( width = '10%' )->text( text = 'Enabled'
      )->column( width = '10%' )->text( text = 'Action' ).

    headers_table->items( )->column_list_item(
      )->cells(
        )->input(
          value = mo_client->_bind_edit( 'key' )
          placeholder = 'Header name'
        )->input(
          value = mo_client->_bind_edit( 'value' )
          placeholder = 'Header value'
        )->checkbox(
          selected = mo_client->_bind_edit( 'enabled' )
        )->button(
          icon = 'sap-icon://add'
          press = mo_client->_event( 'ADD_HEADER' )
          type = 'Transparent' ).

    " Authentication section (simplified)
    DATA(auth_panel) = request_vbox->panel(
      headertext = 'Authentication'
      expanded = abap_false
      class = 'sapUiMediumMarginTop' ).

    DATA(auth_vbox) = auth_panel->content( )->vbox( ).

    auth_vbox->text(
      text = 'Authentication support will be added in future versions'
      class = 'sapUiMediumMargin' ).
  ENDMETHOD.

  METHOD build_response_panel.
    " Response panel
    DATA(response_panel) = io_view->content( )->panel(
      headertext = |Response: { mv_response_status }|
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(response_vbox) = response_panel->content( )->vbox( ).

    " Response info
    IF mv_response_time IS NOT INITIAL.
      response_vbox->text(
        text = |Time: { mv_response_time } Size: { mv_response_size }|
        class = 'sapUiSmallMarginBottom' ).
    ENDIF.

    " Response body
    response_vbox->text_area(
      value = mo_client->_bind( mv_response_body )
      editable = abap_false
      rows = '15'
      width = '100%'
      placeholder = 'Response will appear here...' ).
  ENDMETHOD.

  METHOD build_sidebar.
    DATA(sidebar_page) = io_view->page( title = 'Collections & Environment' ).
    DATA(content) = sidebar_page->content( ).

    " Environment selector
    content->panel(
      headertext = 'Environment'
      expanded = abap_true
      class = 'sapUiResponsiveMargin'
    )->content( )->vbox( )->items(
      )->text( text = |Current: { mv_environment }| class = 'sapUiMediumMargin' ).

    " Collections panel
    DATA(collections_panel) = content->panel(
      headertext = 'Saved Requests'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    collections_panel->content( )->list(
      items = mo_client->_bind( mt_collections )
      mode = 'SingleSelectMaster'
      itempress = mo_client->_event(
        val = 'LOAD_REQUEST'
        t_arg = VALUE #( ( `${id}` ) ) )
    )->standard_list_item(
      title = '{name}'
      description = '{description}'
      info = '{method}'
      type = 'Active' ).
  ENDMETHOD.

  METHOD add_empty_header_row.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_headers.
  ENDMETHOD.

  METHOD add_empty_param_row.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_params.
  ENDMETHOD.

  METHOD process_environment_vars.
    result = iv_text.
    " Basic environment variable processing
    REPLACE ALL OCCURRENCES OF '{{base_url}}' IN result WITH 'https://jsonplaceholder.typicode.com'.
    REPLACE ALL OCCURRENCES OF '{{api_key}}' IN result WITH 'demo_api_key'.
  ENDMETHOD.

  METHOD handle_send_request.
    " Same HTTP request logic as basic version with enhanced headers
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_start_time  TYPE timestampl,
          lv_end_time    TYPE timestampl.

    CLEAR: mv_response_body, mv_response_time, mv_response_size.
    mv_response_status = 'Sending request...'.

    TRY.
        GET TIME STAMP FIELD lv_start_time.

        IF mv_url IS INITIAL.
          mv_response_status = 'Error: URL is required'.
          RETURN.
        ENDIF.

        " Process environment variables
        DATA(processed_url) = process_environment_vars( mv_url ).

        cl_http_client=>create_by_url(
          EXPORTING url = processed_url
          IMPORTING client = lo_http_client ).

        lo_http_client->request->set_method( mv_method ).

        " Add headers from simple fields
        IF mv_content_type IS NOT INITIAL.
          lo_http_client->request->set_header_field( name = 'Content-Type' value = mv_content_type ).
        ENDIF.

        IF mv_authorization IS NOT INITIAL.
          lo_http_client->request->set_header_field( name = 'Authorization' value = mv_authorization ).
        ENDIF.

        " Parse and add custom headers
        IF mv_custom_header1 IS NOT INITIAL AND mv_custom_header1 CS ':'.
          SPLIT mv_custom_header1 AT ':' INTO DATA(header1_name) DATA(header1_value).
          lo_http_client->request->set_header_field(
            name = condense( header1_name )
            value = condense( header1_value ) ).
        ENDIF.

        IF mv_custom_header2 IS NOT INITIAL AND mv_custom_header2 CS ':'.
          SPLIT mv_custom_header2 AT ':' INTO DATA(header2_name) DATA(header2_value).
          lo_http_client->request->set_header_field(
            name = condense( header2_name )
            value = condense( header2_value ) ).
        ENDIF.

        " Set default headers
        lo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
        lo_http_client->request->set_header_field( name = 'User-Agent' value = 'ZCurl-Enhanced/1.0' ).

        " Set request body for POST/PUT/PATCH
        IF ( mv_method = 'POST' OR mv_method = 'PUT' OR mv_method = 'PATCH' )
           AND mv_request_body IS NOT INITIAL.
          lo_http_client->request->set_cdata( mv_request_body ).
          " Set Content-Type if not already set
          IF mv_content_type IS INITIAL.
            lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
          ENDIF.
        ENDIF.

        lo_http_client->send( ).
        lo_http_client->receive( ).

        " Get response
        DATA: BEGIN OF ls_status,
                code   TYPE i,
                reason TYPE string,
              END OF ls_status.
        lo_http_client->response->get_status( IMPORTING code = ls_status-code reason = ls_status-reason ).
        mv_response_status = |{ ls_status-code } { ls_status-reason }|.
        mv_response_body = lo_http_client->response->get_cdata( ).

        GET TIME STAMP FIELD lv_end_time.
        DATA(lv_duration) = lv_end_time - lv_start_time.
        mv_response_time = |{ lv_duration * 1000 DECIMALS = 0 } ms|.
        mv_response_size = |{ strlen( mv_response_body ) } bytes|.

        " Basic JSON formatting
        IF mv_response_body CS '{' OR mv_response_body CS '['.
          REPLACE ALL OCCURRENCES OF ',' IN mv_response_body WITH ',\n'.
          REPLACE ALL OCCURRENCES OF '{' IN mv_response_body WITH '{\n  '.
          REPLACE ALL OCCURRENCES OF '}' IN mv_response_body WITH '\n}'.
        ENDIF.

        " Store response headers
        CLEAR mt_response_headers.
        DATA(lt_header_fields) = VALUE tihttpnvp( ).
        lo_http_client->response->get_header_fields( CHANGING fields = lt_header_fields ).
        LOOP AT lt_header_fields INTO DATA(response_header).
          APPEND VALUE #( name = response_header-name value = response_header-value ) TO mt_response_headers.
        ENDLOOP.

        lo_http_client->close( ).

      CATCH cx_root INTO DATA(lx_error).
        mv_response_status = |Error: { lx_error->get_text( ) }|.
        mv_response_body = |Request failed:\n{ lx_error->get_text( ) }|.
        mv_response_time = '0 ms'.
        mv_response_size = '0 bytes'.

        IF lo_http_client IS BOUND.
          TRY.
              lo_http_client->close( ).
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_save_request.
    " Simple save functionality
    DATA(new_id) = |{ lines( mt_collections ) + 1 }|.

    APPEND VALUE #(
      id = new_id
      name = COND #( WHEN mv_request_name IS NOT INITIAL THEN mv_request_name
                     ELSE |{ mv_method } Request| )
      method = mv_method
      url = mv_url
      description = COND #( WHEN mv_request_desc IS NOT INITIAL THEN mv_request_desc
                           ELSE |Saved at { sy-datum } { sy-uzeit }| )
    ) TO mt_collections.

    CLEAR: mv_request_name, mv_request_desc.
    mv_response_status = 'Request saved successfully'.
  ENDMETHOD.

  METHOD handle_load_request.
    READ TABLE mt_collections INTO DATA(collection) WITH KEY id = iv_request_id.
    IF sy-subrc = 0.
      mv_url = collection-url.
      mv_method = collection-method.
      mv_response_status = |Loaded: { collection-name }|.
    ENDIF.
  ENDMETHOD.

  METHOD handle_auth_change.
    " Authentication change handling
    ms_auth_config-type = mv_auth_tab.
  ENDMETHOD.

  METHOD build_url_bar_inline.
    " URL input section with method buttons using proper UI5 controls
    DATA(url_panel) = io_vbox->panel(
      headertext = 'Request URL'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(url_vbox) = url_panel->vbox( class = 'sapUiMediumMargin' ).

    " Method selection buttons
    url_vbox->label( text = 'HTTP Method:' class = 'sapUiSmallMarginBottom' ).

    DATA(method_buttons) = url_vbox->hbox( class = 'sapUiTinyMarginBottom' ).

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
      press = mo_client->_event( 'METHOD_DELETE' )
      class = 'sapUiTinyMarginEnd' ).

    method_buttons->button(
      text = 'PATCH'
      type = COND #( WHEN mv_method = 'PATCH' THEN 'Emphasized' ELSE 'Default' )
      press = mo_client->_event( 'METHOD_PATCH' ) ).

    " URL input
    url_vbox->label( text = 'URL:' class = 'sapUiSmallMarginTop sapUiSmallMarginBottom' ).

    DATA(url_hbox) = url_vbox->hbox( ).
    url_hbox->input(
      value = mo_client->_bind_edit( mv_url )
      placeholder = 'Enter request URL...'
      width = '70%'
      class = 'sapUiTinyMarginEnd' ).

    url_hbox->button(
      text = |Send { mv_method }|
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = mo_client->_event( 'SEND_REQUEST' ) ).

    url_hbox->button(
      text = 'Save'
      type = 'Default'
      icon = 'sap-icon://save'
      press = mo_client->_event( 'SAVE_REQUEST' )
      class = 'sapUiTinyMarginBegin' ).
  ENDMETHOD.

  METHOD build_request_panel_inline.
    " Request configuration panel using proper UI5 controls
    DATA(request_panel) = io_vbox->panel(
      headertext = 'Request Configuration'
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(request_vbox) = request_panel->vbox( class = 'sapUiMediumMargin' ).

    " Request body section (only for POST/PUT/PATCH)
    IF mv_method = 'POST' OR mv_method = 'PUT' OR mv_method = 'PATCH'.
      request_vbox->label( text = 'Request Body (JSON):' class = 'sapUiMediumMarginBottom' ).
      request_vbox->text_area(
        value = mo_client->_bind_edit( mv_request_body )
        placeholder = 'Enter JSON request body...'
        rows = '8'
        width = '100%'
        class = 'sapUiMediumMarginBottom' ).
    ENDIF.

    " Headers section (simplified without complex table binding)
    request_vbox->label( text = 'Request Headers:' class = 'sapUiMediumMarginBottom' ).

    " Simple form for headers
    DATA(headers_form) = request_vbox->simple_form(
      editable = abap_true
      layout = 'ResponsiveGridLayout'
      title = 'Common Headers'
      class = 'sapUiMediumMarginBottom' ).

    " Content-Type header
    headers_form->label( text = 'Content-Type:' ).
    headers_form->input(
      value = mo_client->_bind_edit( mv_content_type )
      placeholder = 'application/json' ).

    " Authorization header
    headers_form->label( text = 'Authorization:' ).
    headers_form->input(
      value = mo_client->_bind_edit( mv_authorization )
      placeholder = 'Bearer your-token-here'
      type = 'Password' ).

    " Custom header 1
    headers_form->label( text = 'Custom Header 1:' ).
    headers_form->input(
      value = mo_client->_bind_edit( mv_custom_header1 )
      placeholder = 'Header-Name: Header-Value' ).

    " Custom header 2
    headers_form->label( text = 'Custom Header 2:' ).
    headers_form->input(
      value = mo_client->_bind_edit( mv_custom_header2 )
      placeholder = 'Header-Name: Header-Value' ).

    " Note about headers
    request_vbox->text(
      text = 'Headers will be automatically added. Use format "Header-Name: Header-Value" for custom headers.'
      class = 'sapUiSmallMarginTop' ).
  ENDMETHOD.

  METHOD build_response_panel_inline.
    " Response panel using proper UI5 controls
    DATA(response_panel) = io_vbox->panel(
      headertext = |Response: { mv_response_status }|
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    DATA(response_vbox) = response_panel->vbox( class = 'sapUiMediumMargin' ).

    " Response info
    IF mv_response_time IS NOT INITIAL.
      response_vbox->text(
        text = |Time: { mv_response_time } Size: { mv_response_size }|
        class = 'sapUiSmallMarginBottom' ).
    ENDIF.

    " Response body
    response_vbox->text_area(
      value = mo_client->_bind( mv_response_body )
      editable = abap_false
      rows = '15'
      width = '100%'
      placeholder = 'Response will appear here...' ).
  ENDMETHOD.

ENDCLASS.
