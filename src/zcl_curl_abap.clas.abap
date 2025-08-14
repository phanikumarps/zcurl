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

CLASS zcl_curl_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES: BEGIN OF ty_request_header,
             key     TYPE string,
             value   TYPE string,
             enabled TYPE abap_bool,
           END OF ty_request_header.

    TYPES: BEGIN OF ty_request_param,
             key     TYPE string,
             value   TYPE string,
             enabled TYPE abap_bool,
           END OF ty_request_param.

    TYPES: BEGIN OF ty_response_header,
             name  TYPE string,
             value TYPE string,
           END OF ty_response_header.

    TYPES: BEGIN OF ty_request_collection,
             id          TYPE string,
             name        TYPE string,
             method      TYPE string,
             url         TYPE string,
             description TYPE string,
             created_at  TYPE string,
           END OF ty_request_collection.

    TYPES: BEGIN OF ty_environment_var,
             key     TYPE string,
             value   TYPE string,
             enabled TYPE abap_bool,
           END OF ty_environment_var.

    TYPES: tt_request_headers    TYPE TABLE OF ty_request_header,
           tt_request_params     TYPE TABLE OF ty_request_param,
           tt_response_headers   TYPE TABLE OF ty_response_header,
           tt_request_collection TYPE TABLE OF ty_request_collection,
           tt_environment_vars   TYPE TABLE OF ty_environment_var.

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
          mv_collection_name  TYPE string,
          mv_request_name     TYPE string,
          mv_request_desc     TYPE string,
          mv_environment      TYPE string VALUE 'Development',
          mt_headers          TYPE tt_request_headers,
          mt_params           TYPE tt_request_params,
          mt_response_headers TYPE tt_response_headers,
          mt_collections      TYPE tt_request_collection,
          mt_environments     TYPE tt_environment_vars.

  PRIVATE SECTION.
    DATA: mo_rest_client TYPE REF TO zif_rest_client,
          mo_client      TYPE REF TO z2ui5_if_client.

    METHODS: send_request
               RETURNING VALUE(result) TYPE string
               RAISING   zcx_rest_client,
             initialize_data,
             build_main_view
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_request_section
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_response_section
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_sidebar
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             handle_send_request,
             handle_save_request,
             handle_load_request
               IMPORTING iv_request_id TYPE string,
             handle_add_header,
             handle_add_param,
             handle_add_env_var,
             process_environment_variables
               IMPORTING iv_text       TYPE string
               RETURNING VALUE(result) TYPE string,
             format_json
               IMPORTING iv_json       TYPE string
               RETURNING VALUE(result) TYPE string,
             get_method_icon
               IMPORTING iv_method     TYPE string
               RETURNING VALUE(result) TYPE string,
             get_status_color
               IMPORTING iv_status     TYPE string
               RETURNING VALUE(result) TYPE string,
             build_sidebar_content
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_url_bar
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_request_panel
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_response_panel
               IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
             build_params_tab
               IMPORTING io_tabs TYPE REF TO z2ui5_cl_xml_view,
             build_headers_tab
               IMPORTING io_tabs TYPE REF TO z2ui5_cl_xml_view,
             build_body_tab
               IMPORTING io_tabs TYPE REF TO z2ui5_cl_xml_view,
             build_auth_tab
               IMPORTING io_tabs TYPE REF TO z2ui5_cl_xml_view.

ENDCLASS.

CLASS zcl_curl_abap IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    " Store client reference for use in other methods
    mo_client = client.

    DATA(lo_client_data) = client->get( ).

    CASE lo_client_data-event.
      WHEN 'SEND_REQUEST'.
        handle_send_request( ).

      WHEN 'SAVE_REQUEST'.
        handle_save_request( ).

      WHEN 'LOAD_REQUEST'.
        IF lines( lo_client_data-t_event_arg ) > 0.
          handle_load_request( lo_client_data-t_event_arg[ 1 ] ).
        ENDIF.

      WHEN 'TAB_SELECT'.
        IF lines( lo_client_data-t_event_arg ) > 0.
          mv_selected_tab = lo_client_data-t_event_arg[ 1 ].
        ENDIF.

      WHEN 'REQUEST_TAB_SELECT'.
        IF lines( lo_client_data-t_event_arg ) > 0.
          mv_request_tab = lo_client_data-t_event_arg[ 1 ].
        ENDIF.

      WHEN 'RESPONSE_TAB_SELECT'.
        IF lines( lo_client_data-t_event_arg ) > 0.
          mv_response_tab = lo_client_data-t_event_arg[ 1 ].
        ENDIF.

      WHEN 'ADD_HEADER'.
        handle_add_header( ).

      WHEN 'ADD_PARAM'.
        handle_add_param( ).

      WHEN 'ADD_ENV_VAR'.
        handle_add_env_var( ).

      WHEN OTHERS.
        initialize_data( ).
    ENDCASE.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    build_main_view( view ).
    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD initialize_data.
    " Initialize REST client
    mo_rest_client = zcl_rest_client=>construct( ).

    " Add default headers
    IF lines( mt_headers ) = 0.
      APPEND VALUE #( key = 'Content-Type' value = 'application/json' enabled = abap_true ) TO mt_headers.
      APPEND VALUE #( key = 'Accept' value = 'application/json' enabled = abap_true ) TO mt_headers.
      APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_headers.
    ENDIF.

    " Add default params
    IF lines( mt_params ) = 0.
      APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_params.
    ENDIF.

    " Add default environment variables
    IF lines( mt_environments ) = 0.
      APPEND VALUE #( key = 'base_url' value = 'https://api.example.com' enabled = abap_true ) TO mt_environments.
      APPEND VALUE #( key = 'api_key' value = 'your_api_key_here' enabled = abap_true ) TO mt_environments.
      APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_environments.
    ENDIF.

    " Initialize sample collections
    IF lines( mt_collections ) = 0.
      APPEND VALUE #( id = '1' name = 'Get Users' method = 'GET'
                     url = '{{base_url}}/users' description = 'Retrieve all users'
                     created_at = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }| ) TO mt_collections.
      APPEND VALUE #( id = '2' name = 'Create User' method = 'POST'
                     url = '{{base_url}}/users' description = 'Create a new user'
                     created_at = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }| ) TO mt_collections.
    ENDIF.
  ENDMETHOD.

  METHOD build_main_view.
    " Main page
    DATA(page) = io_view->shell( )->page(
      title = 'ZCurl - Advanced REST Client'
      shownavbutton = abap_false
      class = 'sapUiContentPadding' ).

    " Add custom CSS
    page->_generic( name = 'style' ns = 'html' )->_cc_plain_xml(
      |.curl-method-get \{ color: #61affe; font-weight: bold; \}| &&
      |.curl-method-post \{ color: #49cc90; font-weight: bold; \}| &&
      |.curl-method-put \{ color: #fca130; font-weight: bold; \}| &&
      |.curl-method-delete \{ color: #f93e3e; font-weight: bold; \}| &&
      |.curl-method-patch \{ color: #50e3c2; font-weight: bold; \}| &&
      |.curl-status-success \{ color: #49cc90; \}| &&
      |.curl-status-error \{ color: #f93e3e; \}| &&
      |.curl-status-warning \{ color: #fca130; \}| ).

    " Split container layout
    DATA(split_container) = page->split_container( ).

    " Sidebar (master)
    DATA(master_page) = split_container->master_pages( )->page(
      title = 'Collections'
      class = 'sapUiContentPadding' ).
    build_sidebar_content( master_page ).

    " Main content (detail)
    DATA(detail_page) = split_container->detail_pages( )->page(
      title = 'Request Builder'
      class = 'sapUiContentPadding' ).

    " URL bar
    build_url_bar( detail_page ).

    " Request/Response tabs using icon_tab_bar
    DATA(tab_bar) = detail_page->content( )->icon_tab_bar(
      selectedkey = mo_client->_bind_edit( mv_selected_tab )
      select = mo_client->_event(
        val = 'TAB_SELECT'
        t_arg = VALUE #( ( `${$source>/key}` ) )
      )
      class = 'sapUiResponsiveMargin' ).

    " Request tab
    DATA(request_tab) = tab_bar->items( )->icon_tab_filter(
      key = 'request'
      text = 'Request'
      icon = 'sap-icon://upload' ).
    build_request_panel( request_tab ).

    " Response tab
    DATA(response_tab) = tab_bar->items( )->icon_tab_filter(
      key = 'response'
      text = 'Response'
      icon = 'sap-icon://download' ).
    build_response_panel( response_tab ).

  ENDMETHOD.

  METHOD build_url_bar.
    " URL input bar with method selector and send button
    DATA(toolbar) = io_view->content( )->toolbar( design = 'Solid' ).

    toolbar->combobox(
      selectedkey = mo_client->_bind_edit( mv_method )
      width = '120px'
      items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
        ( n = 'GET' v = 'GET' )
        ( n = 'POST' v = 'POST' )
        ( n = 'PUT' v = 'PUT' )
        ( n = 'DELETE' v = 'DELETE' )
        ( n = 'PATCH' v = 'PATCH' ) ) ) ).

    toolbar->input(
      value = mo_client->_bind_edit( mv_url )
      placeholder = 'Enter request URL (e.g., https://api.example.com/users)'
      width = 'auto'
      class = 'sapUiMediumMarginBegin' ).

    toolbar->toolbar_spacer( ).

    toolbar->button(
      text = 'Send'
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = mo_client->_event( 'SEND_REQUEST' )
      class = 'sapUiMediumMarginBegin' ).

    toolbar->button(
      text = 'Save'
      type = 'Default'
      icon = 'sap-icon://save'
      press = mo_client->_event( 'SAVE_REQUEST' )
      class = 'sapUiTinyMarginBegin' ).

  ENDMETHOD.

  METHOD build_request_panel.
    " Request configuration tabs
    DATA(request_tabs) = io_view->icon_tab_bar(
      selectedkey = mo_client->_bind_edit( mv_request_tab )
      select = mo_client->_event(
        val = 'REQUEST_TAB_SELECT'
        t_arg = VALUE #( ( `${$source>/key}` ) )
      )
      expanded = abap_true
      class = 'sapUiResponsiveMargin' ).

    " Parameters tab
    build_params_tab( request_tabs ).

    " Headers tab
    build_headers_tab( request_tabs ).

    " Body tab
    build_body_tab( request_tabs ).

    " Authorization tab
    build_auth_tab( request_tabs ).

  ENDMETHOD.

  METHOD build_params_tab.
    DATA(params_tab) = io_tabs->items( )->icon_tab_filter(
      key = 'params'
      text = 'Query Params'
      icon = 'sap-icon://add-filter' ).

    DATA(params_table) = params_tab->table(
      items = mo_client->_bind( mt_params )
      growing = abap_true
      growingthreshold = '10'
      class = 'sapUiResponsiveMargin' ).

    params_table->columns(
      )->column( )->text( text = 'Key'
      )->column( )->text( text = 'Value'
      )->column( )->text( text = 'Enabled'
      )->column( ).

    params_table->items( )->column_list_item(
      )->cells(
        )->input( value = mo_client->_bind_edit( 'key' ) placeholder = 'Parameter key'
        )->input( value = mo_client->_bind_edit( 'value' ) placeholder = 'Parameter value'
        )->checkbox( selected = mo_client->_bind_edit( 'enabled' )
        )->button( icon = 'sap-icon://add' press = mo_client->_event( 'ADD_PARAM' ) type = 'Transparent'  ).
  ENDMETHOD.

  METHOD build_headers_tab.
    DATA(headers_tab) = io_tabs->items( )->icon_tab_filter(
      key = 'headers'
      text = 'Headers'
      icon = 'sap-icon://header' ).

    DATA(headers_table) = headers_tab->table(
      items = mo_client->_bind( mt_headers )
      growing = abap_true
      class = 'sapUiResponsiveMargin' ).

    headers_table->columns(
      )->column( )->text( text = 'Key'
      )->column( )->text( text = 'Value'
      )->column( )->text( text = 'Enabled'
      )->column( ).

    headers_table->items( )->column_list_item(
      )->cells(
        )->input( value = mo_client->_bind_edit( 'key' ) placeholder = 'Header key'
        )->input( value = mo_client->_bind_edit( 'value' ) placeholder = 'Header value'
        )->checkbox( selected = mo_client->_bind_edit( 'enabled'  )
        )->button( icon = 'sap-icon://add' press = mo_client->_event( 'ADD_HEADER' ) type = 'Transparent'  ).
  ENDMETHOD.

  METHOD build_body_tab.
    DATA(body_tab) = io_tabs->items( )->icon_tab_filter(
      key = 'body'
      text = 'Request Body'
      icon = 'sap-icon://document-text' ).

    body_tab->text_area(
      value = mo_client->_bind_edit( mv_request_body )
      placeholder = 'Enter request body (JSON, XML, etc.)...'
      rows = '15'
      width = '100%'
      class = 'sapUiResponsiveMargin' ).
  ENDMETHOD.

  METHOD build_auth_tab.
  DATA(auth_tab) = io_tabs->items( )->icon_tab_filter(
    key = 'auth'
    text = 'Authorization'
    icon = 'sap-icon://key' ).

  " Simple form-like layout using VBox
  DATA(auth_container) = auth_tab->vbox( class = 'sapUiResponsiveMargin' ).

  " Authentication type selection
  auth_container->label(
    text = 'Authentication Type'
    class = 'sapUiMediumMarginBottom' ).

  auth_container->combobox(
    selectedkey = mo_client->_bind_edit( mv_environment )
    items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
      ( n = 'No Auth' v = 'none' )
      ( n = 'Bearer Token' v = 'bearer' )
      ( n = 'Basic Auth' v = 'basic' )
      ( n = 'API Key' v = 'apikey' ) ) )
    width = '300px'
    class = 'sapUiLargeMarginBottom' ).

  " Credentials section
  auth_container->label(
    text = 'Credentials'
    class = 'sapUiMediumMarginBottom' ).

  auth_container->input(
    value = mo_client->_bind_edit( mv_request_name )
    type = 'Password'
    placeholder = 'Enter authentication token or API key'
    width = '300px'
    class = 'sapUiMediumMarginBottom' ).

  " Help text
  auth_container->text(
    text = 'Enter your authentication credentials based on the selected type.'
    class = 'sapUiTinyMarginTop' ).

ENDMETHOD.

  METHOD build_response_panel.
    " Response status bar
    DATA(status_toolbar) = io_view->toolbar( ).
    status_toolbar->text(
      text = |Status: { mv_response_status }|
      class = get_status_color( mv_response_status ) ).
    status_toolbar->toolbar_spacer( ).
    status_toolbar->text( text = |Time: { mv_response_time }| ).
    status_toolbar->toolbar_spacer( ).
    status_toolbar->text( text = |Size: { mv_response_size }| ).

    " Response tabs using icon_tab_bar
    DATA(response_tabs) = io_view->icon_tab_bar(
      selectedkey = mo_client->_bind_edit( mv_response_tab )
      select = mo_client->_event(
        val = 'RESPONSE_TAB_SELECT'
        t_arg = VALUE #( ( `${$source>/key}` ) )
      )
      class = 'sapUiResponsiveMargin' ).

    " Response body tab
    DATA(body_tab) = response_tabs->items( )->icon_tab_filter(
      key = 'body'
      text = 'Response Body'
      icon = 'sap-icon://document-text' ).
    body_tab->text_area(
      value = mo_client->_bind( mv_response_body )
      editable = abap_false
      rows = '20'
      width = '100%'
      class = 'sapUiResponsiveMargin' ).

    " Response headers tab
    DATA(headers_tab) = response_tabs->items( )->icon_tab_filter(
      key = 'headers'
      text = 'Headers'
      icon = 'sap-icon://header' ).
    headers_tab->table(
      items = mo_client->_bind( mt_response_headers )
      class = 'sapUiResponsiveMargin'
    )->columns(
      )->column( )->text( text = 'Name'
      )->column( )->text( text = 'Value'
    )->items( )->column_list_item(
      )->cells(
        )->text( text = '{name}'
        )->text( text = '{value}' ).

  ENDMETHOD.

  METHOD build_sidebar_content.
    " Environment selector
    io_view->content( )->panel(
      headertext = 'Environment'
      expanded = abap_true
      class = 'sapUiResponsiveMargin'
    )->content( )->combobox(
      selectedkey = mo_client->_bind_edit( mv_environment )
      items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
        ( n = 'Development' v = 'Development' )
        ( n = 'Staging' v = 'Staging' )
        ( n = 'Production' v = 'Production' ) ) )
      width = '100%' ).

    " Collections list
    io_view->content( )->panel(
      headertext = 'Saved Requests'
      expanded = abap_true
      class = 'sapUiResponsiveMargin'
    )->content( )->list(
      items = mo_client->_bind( mt_collections )
      mode = 'SingleSelectMaster'
      itempress = mo_client->_event(
        val = 'LOAD_REQUEST'
        t_arg = VALUE #( ( `${id}` ) )
      )
    )->standard_list_item(
      title = '{name}'
      description = '{description}'
      info = '{method}'
      type = 'Active' ).

    " Save button
    io_view->content( )->button(
      text = 'Save Current Request'
      type = 'Emphasized'
      press = mo_client->_event( 'SAVE_REQUEST' )
      width = '100%'
      class = 'sapUiMediumMarginTop' ).
  ENDMETHOD.

  METHOD build_sidebar.
    DATA(sidebar_page) = io_view->page( title = 'Collections & Environment' ).
    build_sidebar_content( sidebar_page ).
  ENDMETHOD.

  METHOD build_request_section.
    build_request_panel( io_view ).
  ENDMETHOD.

  METHOD build_response_section.
    build_response_panel( io_view ).
  ENDMETHOD.

  METHOD handle_send_request.
    TRY.
        GET TIME STAMP FIELD DATA(start_time).

        " Process environment variables in URL
        DATA(processed_url) = process_environment_variables( mv_url ).

        " Build headers from table
        DATA(headers) = VALUE tihttpnvp( ).
        LOOP AT mt_headers INTO DATA(header) WHERE enabled = abap_true AND key IS NOT INITIAL.
          APPEND VALUE #( name = header-key value = process_environment_variables( header-value ) ) TO headers.
        ENDLOOP.

        " Execute request based on method
        DATA(response) = NEW zif_rest_client=>ty_response( ).
        CASE mv_method.
          WHEN 'GET'.
            response = mo_rest_client->get( url = processed_url header = headers ).
          WHEN 'POST'.
            response = mo_rest_client->post( url = processed_url header = headers body = mv_request_body ).
          WHEN 'PUT'.
            response = mo_rest_client->put( url = processed_url header = headers body = mv_request_body ).
          WHEN 'DELETE'.
            response = mo_rest_client->delete( url = processed_url header = headers ).
          WHEN 'PATCH'.
            " Note: PATCH might need to be implemented in the REST client
            response = mo_rest_client->post( url = processed_url header = headers body = mv_request_body ).
        ENDCASE.

        " Calculate response time
        GET TIME STAMP FIELD DATA(end_time).
        DATA(duration) = cl_abap_tstmp=>subtract( tstmp1 = end_time tstmp2 = start_time ).
        mv_response_time = |{ duration * 1000 } ms|.

        " Set response data
        mv_response_status = |{ response->code } { response->reason }|.
        mv_response_body = format_json( response->body ).
        mv_response_size = |{ strlen( response->body ) } bytes|.

        " Set response headers
        CLEAR mt_response_headers.
        LOOP AT response->header INTO DATA(resp_header).
          APPEND VALUE #( name = resp_header-name value = resp_header-value ) TO mt_response_headers.
        ENDLOOP.

        " Switch to response tab
        mv_selected_tab = 'response'.

      CATCH zcx_rest_client INTO DATA(exception).
        mv_response_status = 'Error'.
        mv_response_body = exception->get_text( ).
        mv_response_time = '0 ms'.
        mv_response_size = '0 bytes'.
        mv_selected_tab = 'response'.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_save_request.
    " Generate new ID
    DATA(new_id) = |{ lines( mt_collections ) + 1 }|.

    " Add to collections
    APPEND VALUE #(
      id = new_id
      name = COND #( WHEN mv_request_name IS NOT INITIAL THEN mv_request_name
                     ELSE |{ mv_method } Request| )
      method = mv_method
      url = mv_url
      description = COND #( WHEN mv_request_desc IS NOT INITIAL THEN mv_request_desc
                           ELSE |Saved on { sy-datum DATE = USER }| )
      created_at = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }|
    ) TO mt_collections.

    " Clear input fields
    CLEAR: mv_request_name, mv_request_desc.
  ENDMETHOD.

  METHOD handle_load_request.
    READ TABLE mt_collections INTO DATA(collection) WITH KEY id = iv_request_id.
    IF sy-subrc = 0.
      mv_url = collection-url.
      mv_method = collection-method.
      mv_selected_tab = 'request'.
      mv_request_tab = 'params'.
    ENDIF.
  ENDMETHOD.

  METHOD handle_add_header.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_headers.
  ENDMETHOD.

  METHOD handle_add_param.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_params.
  ENDMETHOD.

  METHOD handle_add_env_var.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_environments.
  ENDMETHOD.

  METHOD process_environment_variables.
    result = iv_text.

    " Replace environment variables in format {{variable_name}}
    LOOP AT mt_environments INTO DATA(env_var) WHERE enabled = abap_true AND key IS NOT INITIAL.
      DATA(pattern) = |\{\{{ env_var-key }\}\}|.
      REPLACE ALL OCCURRENCES OF pattern IN result WITH env_var-value.
    ENDLOOP.
  ENDMETHOD.

  METHOD format_json.
    " Basic JSON formatting - in real implementation, use proper JSON formatter
    result = iv_json.
    " Add basic formatting for better readability
    REPLACE ALL OCCURRENCES OF ',' IN result WITH ',\n'.
    REPLACE ALL OCCURRENCES OF '{' IN result WITH '{\n'.
    REPLACE ALL OCCURRENCES OF '}' IN result WITH '\n}'.
  ENDMETHOD.

  METHOD get_method_icon.
    CASE iv_method.
      WHEN 'GET'.
        result = 'sap-icon://download'.
      WHEN 'POST'.
        result = 'sap-icon://add'.
      WHEN 'PUT'.
        result = 'sap-icon://edit'.
      WHEN 'DELETE'.
        result = 'sap-icon://delete'.
      WHEN 'PATCH'.
        result = 'sap-icon://edit'.
      WHEN OTHERS.
        result = 'sap-icon://request'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_status_color.
    DATA(status_code) = substring_before( val = iv_status sub = ' ' ).
    CASE status_code(1).
      WHEN '2'.
        result = 'curl-status-success'.
      WHEN '4'.
        result = 'curl-status-error'.
      WHEN '5'.
        result = 'curl-status-error'.
      WHEN OTHERS.
        result = ''.
    ENDCASE.
  ENDMETHOD.

  METHOD send_request.
    " Delegate to handle_send_request
    handle_send_request( ).
  ENDMETHOD.

ENDCLASS.
