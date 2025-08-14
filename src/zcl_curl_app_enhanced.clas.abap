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

    TYPES: tt_headers          TYPE TABLE OF ty_header_row,
           tt_params           TYPE TABLE OF ty_param_row,
           tt_response_headers TYPE TABLE OF ty_response_header,
           tt_environments     TYPE TABLE OF ty_environment.

  PRIVATE SECTION.
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
          mt_headers          TYPE tt_headers,
          mt_params           TYPE tt_params,
          mt_response_headers TYPE tt_response_headers,
          mt_collections      TYPE zcl_curl_data_manager=>tt_request_items,
          mt_environments     TYPE tt_environments,
          ms_auth_config      TYPE ty_auth_config,
          mv_show_save_dialog TYPE abap_bool,
          mo_client           TYPE REF TO z2ui5_if_client.

    METHODS: initialize_app,
      handle_send_request,
      handle_save_request,
      handle_load_request
        IMPORTING iv_request_id TYPE string,
      handle_auth_change,
      build_main_view
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_request_panel
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_response_panel
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_sidebar
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_url_bar
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      build_save_dialog
        IMPORTING io_view TYPE REF TO z2ui5_cl_xml_view,
      get_method_variant
        IMPORTING iv_method     TYPE string
        RETURNING VALUE(result) TYPE string,
      get_status_state
        IMPORTING iv_status     TYPE string
        RETURNING VALUE(result) TYPE string,
      process_environment_vars
        IMPORTING iv_text       TYPE string
        RETURNING VALUE(result) TYPE string,
      build_headers_from_table
        RETURNING VALUE(result) TYPE tihttpnvp,
      add_empty_header_row,
      add_empty_param_row,
      show_message
        IMPORTING iv_type TYPE string
                  iv_text TYPE string.

ENDCLASS.



CLASS ZCL_CURL_APP_ENHANCED IMPLEMENTATION.


  METHOD add_empty_header_row.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_headers.
  ENDMETHOD.


  METHOD add_empty_param_row.
    APPEND VALUE #( key = '' value = '' enabled = abap_false ) TO mt_params.
  ENDMETHOD.


  METHOD build_headers_from_table.
    LOOP AT mt_headers INTO DATA(header) WHERE enabled = abap_true AND key IS NOT INITIAL.
      APPEND VALUE #( name = header-key value = process_environment_vars( header-value ) ) TO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_main_view.
    " Main page with split container layout
    DATA(page) = io_view->shell( )->page(
      title = 'ZCurl - Advanced REST Client'
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

    " Use split_container with master/detail
    DATA(split_container) = page->split_container( ).

    " Sidebar (master page)
    build_sidebar( split_container->master_pages( ) ).

    " Main content area (detail page)
    DATA(detail_page) = split_container->detail_pages( )->page(
      title = 'Request Builder' ).

    build_url_bar( detail_page ).
    build_request_panel( detail_page ).
    build_response_panel( detail_page ).

  ENDMETHOD.


  METHOD build_request_panel.
    " Request panel content
    DATA(request_panel) = io_view->content( )->panel(
      headertext = 'Request Configuration'
      expanded = abap_true ).

    " Parameters section
    request_panel->content( )->panel(
      headertext = 'Parameters'
      expanded = abap_true
    )->content( )->table(
      items = mo_client->_bind( mt_params )
      growing = abap_true
      growingthreshold = '10'
    )->columns(
      )->column( )->text( text = 'Key'
      )->column( )->text( text = 'Value'
      )->column( )->text( text = 'Enabled'
      )->column(
    )->items( )->column_list_item(
      )->cells(
        )->input( value = mo_client->_bind_edit( 'key' ) placeholder = 'Parameter key'
        )->input( value = mo_client->_bind_edit( 'value' ) placeholder = 'Parameter value'
        )->checkbox( selected = mo_client->_bind_edit( 'enabled' )
        )->button( icon = 'sap-icon://add' press = mo_client->_event( 'ADD_PARAM' ) type = 'Transparent'  ).

    " Headers section
   request_panel->content( )->panel(
      headertext = 'Headers'
      expanded = abap_true
    )->content( )->table(
      items = mo_client->_bind( mt_headers )
      growing = abap_true
    )->columns(
      )->column( )->text( text = 'Key'
      )->column( )->text( text = 'Value'
      )->column( )->text( text = 'Enabled'
      )->column(
    )->items( )->column_list_item(
      )->cells(
        )->input( value = mo_client->_bind_edit( 'key' ) placeholder = 'Header key'
        )->input( value = mo_client->_bind_edit( 'value' ) placeholder = 'Header value'
        )->checkbox( selected = mo_client->_bind_edit( 'enabled' )
        )->button( icon = 'sap-icon://add' press = mo_client->_event( 'ADD_HEADER' ) type = 'Transparent' ).

    " Body section
    request_panel->content( )->panel(
      headertext = 'Request Body'
      expanded = abap_true
    )->content( )->text_area(
      value = mo_client->_bind_edit( mv_request_body )
      placeholder = 'Enter request body (JSON, XML, etc.)...'
      rows = '15'
      width = '100%' ).

    " Auth section
    request_panel->content( )->panel(
      headertext = 'Authentication'
      expanded = abap_true
    )->content( )->vbox( )->items(
      )->combobox(
        selectedkey = mo_client->_bind_edit( mv_auth_tab )
        selectionchange = mo_client->_event( 'AUTH_CHANGE' )
        items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
          ( n = 'No Auth' v = 'none' )
          ( n = 'Bearer Token' v = 'bearer' )
          ( n = 'Basic Auth' v = 'basic' )
          ( n = 'API Key' v = 'apikey' ) ) ) ).

    " Conditional auth fields based on type
    CASE ms_auth_config-type.
      WHEN 'bearer'.
        request_panel->content( )->panel(
          headertext = 'Bearer Token'
          expanded = abap_true
        )->content( )->input(
          value = mo_client->_bind_edit( ms_auth_config-token )
          type = 'Password'
          placeholder = 'Enter bearer token' ).
      WHEN 'basic'.
        request_panel->content( )->panel(
          headertext = 'Basic Authentication'
          expanded = abap_true
        )->content( )->vbox( )->items(
          )->input(
            value = mo_client->_bind_edit( ms_auth_config-user )
            placeholder = 'Username'
          )->input(
            value = mo_client->_bind_edit( ms_auth_config-pass )
            type = 'Password'
            placeholder = 'Password' ).
      WHEN 'apikey'.
        request_panel->content( )->panel(
          headertext = 'API Key'
          expanded = abap_true
        )->content( )->input(
          value = mo_client->_bind_edit( ms_auth_config-token )
          type = 'Password'
          placeholder = 'Enter API key' ).
    ENDCASE.

  ENDMETHOD.


  METHOD build_response_panel.
    " Response panel content
    DATA(response_panel) = io_view->content( )->panel(
      headertext = 'Response'
      expanded = abap_true ).

    " Response status bar
    response_panel->content( )->toolbar( )->text(
      text = |Status: { mv_response_status }|
      class = get_status_state( mv_response_status )
    )->toolbar_spacer( )->text(
      text = |Time: { mv_response_time }|
    )->toolbar_spacer( )->text(
      text = |Size: { mv_response_size }| ).

    " Response body
    response_panel->content( )->panel(
      headertext = 'Response Body'
      expanded = abap_true
    )->content( )->text_area(
      value = mo_client->_bind( mv_response_body )
      editable = abap_false
      rows = '20'
      width = '100%' ).

    " Response headers
    response_panel->content( )->panel(
      headertext = 'Response Headers'
      expanded = abap_true
    )->content( )->table(
      items = mo_client->_bind( mt_response_headers )
    )->columns(
      )->column( )->text( text = 'Name'
      )->column( )->text( text = 'Value'
    )->items( )->column_list_item(
      )->cells(
        )->text( text = '{name}'
        )->text( text = '{value}'  ).

  ENDMETHOD.


  METHOD build_save_dialog.
    DATA(dialog) = io_view->dialog(
      title = 'Save Request'
      contentwidth = '400px'
      contentheight = '300px' ).

    " Use simple_form for the dialog content
    DATA(form_container) = dialog->content( )->simple_form(
      editable = abap_true
      layout = 'ResponsiveGridLayout' ).

    form_container->label( text = 'Request Name' ).
    form_container->input(
      value = mo_client->_bind_edit( mv_request_name )
      required = abap_true ).

    form_container->label( text = 'Description' ).
    form_container->text_area(
      value = mo_client->_bind_edit( mv_request_desc )
      rows = '3' ).

    dialog->begin_button( )->button(
      text = 'Save'
      type = 'Emphasized'
      press = mo_client->_event( 'SAVE_REQUEST_CONFIRM' ) ).

    dialog->end_button( )->button(
      text = 'Cancel'
      press = mo_client->_event( 'SAVE_REQUEST_CANCEL' ) ).

  ENDMETHOD.


  METHOD build_sidebar.
    DATA(sidebar_page) = io_view->page( title = 'Collections & Environment' ).
    DATA(content) = sidebar_page->content( ).

    " Environment selector
    content->panel( headertext = 'Environment' expanded = abap_true )->content(
      )->vbox( )->items(
        )->combobox( selectedkey = mo_client->_bind_edit( mv_environment )
                    items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
                      ( n = 'Development' v = 'Development' )
                      ( n = 'Staging' v = 'Staging' )
                      ( n = 'Production' v = 'Production' ) ) ) ) .

    " Collections panel
    DATA(collections_panel) = content->panel( headertext = 'Request Collections' expanded = abap_true ).

    collections_panel->content( )->list(
      items = mo_client->_bind( mt_collections )
      mode = 'SingleSelectMaster'
      itempress = mo_client->_event( val = 'LOAD_REQUEST' t_arg = VALUE #( ( `${id}` ) ) )
    )->standard_list_item(
      title = '{name}'
      description = '{description}'
      info = '{method}'
      type = 'Active' ).

    " Add new request button
    collections_panel->content( )->button(
      text = 'Save Current Request'
      type = 'Emphasized'
      press = mo_client->_event( 'SAVE_REQUEST' )
      width = '100%' ).

  ENDMETHOD.


  METHOD build_url_bar.
    " URL input section (based on reference implementation)
    DATA(url_toolbar) = io_view->content( )->toolbar( ).
    url_toolbar->toolbar_spacer( ).
    url_toolbar->combobox(
      selectedkey = mo_client->_bind_edit( mv_method )
      width = '100px'
      items = mo_client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value(
        ( n = 'GET' v = 'GET' )
        ( n = 'POST' v = 'POST' )
        ( n = 'PUT' v = 'PUT' )
        ( n = 'DELETE' v = 'DELETE' )
        ( n = 'PATCH' v = 'PATCH' ) ) ) ).

    url_toolbar->input(
      value = mo_client->_bind_edit( mv_url )
      placeholder = 'Enter request URL...'
      width = '70%' ).

    url_toolbar->button(
      text = 'Send'
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = mo_client->_event( 'SEND_REQUEST' ) ).

    url_toolbar->button(
      text = 'Save'
      type = 'Default'
      icon = 'sap-icon://save'
      press = mo_client->_event( 'SAVE_REQUEST' ) ).
    url_toolbar->toolbar_spacer( ).

  ENDMETHOD.


  METHOD get_method_variant.
    CASE to_upper( iv_method ).
      WHEN 'GET'.
        result = 'curl-method-get'.
      WHEN 'POST'.
        result = 'curl-method-post'.
      WHEN 'PUT'.
        result = 'curl-method-put'.
      WHEN 'DELETE'.
        result = 'curl-method-delete'.
      WHEN 'PATCH'.
        result = 'curl-method-patch'.
      WHEN OTHERS.
        result = ''.
    ENDCASE.
  ENDMETHOD.


  METHOD get_status_state.
    IF iv_status CS '2'.
      result = 'curl-status-success'.
    ELSEIF iv_status CS '4' OR iv_status CS '5'.
      result = 'curl-status-error'.
    ELSEIF iv_status CS '3'.
      result = 'curl-status-warning'.
    ELSE.
      result = ''.
    ENDIF.
  ENDMETHOD.


  METHOD handle_auth_change.
    " Handle authentication type changes
    " Clear sensitive data when changing auth type
    IF ms_auth_config-type <> mv_auth_tab.
      CLEAR: ms_auth_config-user, ms_auth_config-pass, ms_auth_config-token.
    ENDIF.
    ms_auth_config-type = mv_auth_tab.
  ENDMETHOD.


  METHOD handle_load_request.
    TRY.
        DATA(ls_request) = zcl_curl_data_manager=>load_request( iv_request_id ).

        " Load request data into form
        mv_url = ls_request-url.
        mv_method = ls_request-method.
        mv_request_body = ls_request-body.

        " Parse headers JSON (simplified parsing)
        " In real implementation, use proper JSON parser
        CLEAR mt_headers.
        " For demo, set default headers
        APPEND VALUE #( key = 'Content-Type' value = 'application/json' enabled = abap_true ) TO mt_headers.
        add_empty_header_row( ).

        " Switch to request tab
        mv_selected_tab = 'request'.

        show_message( iv_type = 'Success' iv_text = 'Request loaded successfully' ).

      CATCH zcx_curl_data INTO DATA(lx_data).
        show_message( iv_type = 'Error' iv_text = lx_data->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_save_request.
    TRY.
        " Convert headers to JSON string
        DATA(lv_headers_json) = '['.
        LOOP AT mt_headers INTO DATA(header) WHERE key IS NOT INITIAL.
          IF sy-tabix > 1.
            lv_headers_json = |{ lv_headers_json },|.
          ENDIF.
          lv_headers_json = |{ lv_headers_json }\{"key":"{ header-key }","value":"{ header-value }","enabled":{ COND #( WHEN header-enabled = abap_true THEN 'true' ELSE 'false' ) }\}|.
        ENDLOOP.
        lv_headers_json = |{ lv_headers_json }]|.

        " Create request item
        DATA(ls_request) = VALUE zcl_curl_data_manager=>ty_request_item(
          name = COND #( WHEN mv_request_name IS NOT INITIAL THEN mv_request_name ELSE |{ mv_method } { mv_url }| )
          method = mv_method
          url = mv_url
          headers = lv_headers_json
          body = mv_request_body
          description = mv_request_desc
        ).

        " Save request
        DATA(lv_id) = zcl_curl_data_manager=>save_request( ls_request ).

        " Refresh collections
        mt_collections = zcl_curl_data_manager=>get_all_requests( ).

        " Clear form
        CLEAR: mv_request_name, mv_request_desc.

        show_message( iv_type = 'Success' iv_text = 'Request saved successfully' ).

      CATCH zcx_curl_data INTO DATA(lx_data).
        show_message( iv_type = 'Error' iv_text = lx_data->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_send_request.
    TRY.
        " Prepare request configuration
        DATA(ls_config) = VALUE zcl_curl_http_service=>ty_request_config(
          method = mv_method
          url = process_environment_vars( mv_url )
          headers = build_headers_from_table( )
          body = mv_request_body
          timeout = mv_timeout
          auth_type = ms_auth_config-type
          auth_user = ms_auth_config-user
          auth_pass = ms_auth_config-pass
          auth_token = ms_auth_config-token
        ).

        " Execute request
        DATA(ls_response) = zcl_curl_http_service=>execute_request( ls_config ).

        " Update UI with response
        IF ls_response-error_message IS NOT INITIAL.
          mv_response_status = |Error: { ls_response-error_message }|.
          mv_response_body = ls_response-error_message.
        ELSE.
          mv_response_status = |{ ls_response-status_code } { ls_response-status_text }|.
          mv_response_body = ls_response-body.
        ENDIF.

        mv_response_time = |{ ls_response-response_time } ms|.
        mv_response_size = |{ ls_response-response_size } bytes|.

        " Convert response headers
        CLEAR mt_response_headers.
        LOOP AT ls_response-headers INTO DATA(header).
          APPEND VALUE #( name = header-name value = header-value ) TO mt_response_headers.
        ENDLOOP.

        " Switch to response tab
        mv_selected_tab = 'response'.

        " Show success message
        show_message( iv_type = 'Success' iv_text = 'Request sent successfully' ).

      CATCH zcx_rest_client INTO DATA(lx_rest).
        mv_response_status = 'Request Failed'.
        mv_response_body = lx_rest->get_text( ).
        mv_response_time = '0 ms'.
        mv_response_size = '0 bytes'.
        show_message( iv_type = 'Error' iv_text = lx_rest->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD initialize_app.
    TRY.
        " Load collections and environments
        mt_collections = zcl_curl_data_manager=>get_all_requests( ).

        " Initialize sample environments
        IF lines( mt_environments ) = 0.
          APPEND VALUE #( name = 'Development' value = 'Development' ) TO mt_environments.
          APPEND VALUE #( name = 'Staging' value = 'Staging' ) TO mt_environments.
          APPEND VALUE #( name = 'Production' value = 'Production' ) TO mt_environments.
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

      CATCH zcx_curl_data INTO DATA(lx_data).
        show_message( iv_type = 'Error' iv_text = lx_data->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD process_environment_vars.
    result = iv_text.
    " Process environment variables - simplified implementation
    " In real implementation, load from mt_environments and replace {{var}} patterns
    REPLACE ALL OCCURRENCES OF '{{base_url}}' IN result WITH 'https://api.example.com'.
    REPLACE ALL OCCURRENCES OF '{{api_key}}' IN result WITH 'demo_api_key'.
  ENDMETHOD.


  METHOD show_message.
    " In real implementation, show message toast or message strip
    " For now, just a placeholder
    " MESSAGE iv_text TYPE iv_type.
  ENDMETHOD.


  METHOD z2ui5_if_app~main.
    " Store client reference
    mo_client = client.
    DATA(lo_client) = client->get( ).

    CASE client->get( )-event.
      WHEN 'SEND_REQUEST'.
        handle_send_request( ).
      WHEN 'SAVE_REQUEST'.
        mv_show_save_dialog = abap_true.
      WHEN 'SAVE_REQUEST_CONFIRM'.
        handle_save_request( ).
        mv_show_save_dialog = abap_false.
      WHEN 'SAVE_REQUEST_CANCEL'.
        mv_show_save_dialog = abap_false.
      WHEN 'LOAD_REQUEST'.
        IF lines( lo_client-t_event_arg ) > 0.
          handle_load_request( lo_client-t_event_arg[ 1 ] ).
        ENDIF.
      WHEN 'ADD_HEADER'.
        add_empty_header_row( ).
      WHEN 'ADD_PARAM'.
        add_empty_param_row( ).
      WHEN 'TAB_SELECT'.
        IF lines( lo_client-t_event_arg ) > 0.
          mv_selected_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'REQUEST_TAB_SELECT'.
        IF lines( lo_client-t_event_arg ) > 0.
          mv_request_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'RESPONSE_TAB_SELECT'.
        IF lines( lo_client-t_event_arg ) > 0.
          mv_response_tab = lo_client-t_event_arg[ 1 ].
        ENDIF.
      WHEN 'AUTH_CHANGE'.
        handle_auth_change( ).
      WHEN OTHERS.
        initialize_app( ).
    ENDCASE.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).
    build_main_view( view ).

    IF mv_show_save_dialog = abap_true.
      build_save_dialog( view ).
    ENDIF.

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
