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

CLASS zcl_curl_app DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES: BEGIN OF ty_request_header,
             key   TYPE string,
             value TYPE string,
             enabled TYPE abap_bool,
           END OF ty_request_header.

    TYPES: BEGIN OF ty_request_param,
             key   TYPE string,
             value TYPE string,
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
             key   TYPE string,
             value TYPE string,
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
    DATA: mo_rest_client TYPE REF TO zif_rest_client.

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
               IMPORTING iv_text TYPE string
               RETURNING VALUE(result) TYPE string,
             format_json
               IMPORTING iv_json TYPE string
               RETURNING VALUE(result) TYPE string,
             get_method_icon
               IMPORTING iv_method TYPE string
               RETURNING VALUE(result) TYPE string,
             get_status_color
               IMPORTING iv_status TYPE string
               RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS zcl_curl_app IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    CASE client->get( )-event.
      WHEN 'SEND_REQUEST'.
        handle_send_request( ).
      WHEN 'SAVE_REQUEST'.
        handle_save_request( ).
      WHEN 'ADD_HEADER'.
        handle_add_header( ).
      WHEN 'ADD_PARAM'.
        handle_add_param( ).
      WHEN 'ADD_ENV_VAR'.
        handle_add_env_var( ).
      WHEN 'LOAD_REQUEST'.
        handle_load_request( client->get( )-t_event_arg[ 1 ]-v ).
      WHEN 'TAB_SELECT'.
        mv_selected_tab = client->get( )-t_event_arg[ 1 ]-v.
      WHEN 'REQUEST_TAB_SELECT'.
        mv_request_tab = client->get( )-t_event_arg[ 1 ]-v.
      WHEN 'RESPONSE_TAB_SELECT'.
        mv_response_tab = client->get( )-t_event_arg[ 1 ]-v.
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
    DATA(page) = io_view->shell( )->page( 
      title = 'ZCurl - REST API Client'
      navbuttontap = client->_event( 'BACK' )
      shownavbutton = abap_true ).

    " Add custom CSS for better styling
    page->_generic( name = 'style' ns = 'html' )->_cc_plain_xml( 
      |.sapMSplitContainer .sapMSplitContainerSidePane \{ background-color: #f7f7f7; \}| &&
      |.curl-method-badge \{ display: inline-block; padding: 4px 8px; border-radius: 3px; | &&
      |color: white; font-weight: bold; font-size: 0.8em; \}| &&
      |.curl-get \{ background-color: #61affe; \}| &&
      |.curl-post \{ background-color: #49cc90; \}| &&
      |.curl-put \{ background-color: #fca130; \}| &&
      |.curl-delete \{ background-color: #f93e3e; \}| &&
      |.curl-patch \{ background-color: #50e3c2; \}| &&
      |.curl-status-200 \{ color: #49cc90; \}| &&
      |.curl-status-400 \{ color: #f93e3e; \}| &&
      |.curl-status-500 \{ color: #f93e3e; \}| ).

    " Main split container
    DATA(split_container) = page->split_container( orientation = 'Horizontal' ).
    
    " Sidebar
    build_sidebar( split_container->master_pages( ) ).
    
    " Main content area
    DATA(detail_page) = split_container->detail_pages( )->page( title = 'Request Builder' ).
    
    " Tab container for main content
    DATA(tab_container) = detail_page->content( )->tab_container( 
      selectedkey = client->_bind_edit( mv_selected_tab )
      itemselect = client->_event( val = 'TAB_SELECT' t_arg = VALUE #( ( `${$source>/key}` ) ) ) ).

    " Request tab
    DATA(request_tab) = tab_container->items( )->tab_container_item( key = 'request' text = 'Request' ).
    build_request_section( request_tab ).

    " Response tab  
    DATA(response_tab) = tab_container->items( )->tab_container_item( key = 'response' text = 'Response' ).
    build_response_section( response_tab ).

  ENDMETHOD.

  METHOD build_sidebar.
    DATA(sidebar_page) = io_view->page( title = 'Collections & Environment' ).
    DATA(content) = sidebar_page->content( ).

    " Environment selector
    content->panel( headertext = 'Environment' expanded = abap_true )->content( 
      )->vbox( )->items( 
        )->combobox( selectedkey = client->_bind_edit( mv_environment )
                    items = client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value( 
                      ( n = 'Development' v = 'Development' )
                      ( n = 'Staging' v = 'Staging' )
                      ( n = 'Production' v = 'Production' ) ) ) ).

    " Collections panel
    DATA(collections_panel) = content->panel( headertext = 'Request Collections' expanded = abap_true ).
    
    collections_panel->content( )->list( 
      items = client->_bind( mt_collections )
      mode = 'SingleSelectMaster'
      itempress = client->_event( val = 'LOAD_REQUEST' t_arg = VALUE #( ( `${id}` ) ) )
    )->standard_list_item( 
      title = '{name}'
      description = '{description}'
      info = '{method}'
      icon = |{ get_method_icon( '{method}' ) }|
      type = 'Active'
    )->custom_data( )->core_custom_data( key = 'method' value = '{method}' ).

    " Add new request button
    collections_panel->content( )->button( 
      text = 'Save Current Request'
      type = 'Emphasized'
      press = client->_event( 'SAVE_REQUEST' )
      width = '100%' ).

  ENDMETHOD.

  METHOD build_request_section.
    DATA(content) = io_view->content( ).

    " URL input section
    DATA(url_toolbar) = content->toolbar( ).
    url_toolbar->toolbar_spacer( ).
    url_toolbar->combobox( 
      selectedkey = client->_bind_edit( mv_method )
      width = '100px'
      items = client->_bind( VALUE z2ui5_cl_util=>ty_t_name_value( 
        ( n = 'GET' v = 'GET' )
        ( n = 'POST' v = 'POST' )
        ( n = 'PUT' v = 'PUT' )
        ( n = 'DELETE' v = 'DELETE' )
        ( n = 'PATCH' v = 'PATCH' ) ) ) ).
    
    url_toolbar->input( 
      value = client->_bind_edit( mv_url )
      placeholder = 'Enter request URL...'
      width = '70%' ).
    
    url_toolbar->button( 
      text = 'Send'
      type = 'Emphasized'
      icon = 'sap-icon://paper-plane'
      press = client->_event( 'SEND_REQUEST' ) ).
    url_toolbar->toolbar_spacer( ).

    " Request details tab container
    DATA(request_tabs) = content->tab_container( 
      selectedkey = client->_bind_edit( mv_request_tab )
      itemselect = client->_event( val = 'REQUEST_TAB_SELECT' t_arg = VALUE #( ( `${$source>/key}` ) ) ) ).

    " Parameters tab
    DATA(params_tab) = request_tabs->items( )->tab_container_item( key = 'params' text = 'Params' ).
    DATA(params_table) = params_tab->table( 
      items = client->_bind( mt_params )
      growing = abap_true
      growingthreshold = 10 ).
    
    params_table->columns( 
      )->column( )->text( text = 'Key' )
      )->column( )->text( text = 'Value' )
      )->column( )->text( text = 'Enabled' )
      )->column( ).

    params_table->items( )->column_list_item( 
      )->cells( 
        )->input( value = client->_bind_edit( 'key' ) placeholder = 'Parameter key'
        )->input( value = client->_bind_edit( 'value' ) placeholder = 'Parameter value'
        )->checkbox( selected = client->_bind_edit( 'enabled' )
        )->button( icon = 'sap-icon://add' press = client->_event( 'ADD_PARAM' ) type = 'Transparent' ).

    " Headers tab
    DATA(headers_tab) = request_tabs->items( )->tab_container_item( key = 'headers' text = 'Headers' ).
    DATA(headers_table) = headers_tab->table( 
      items = client->_bind( mt_headers )
      growing = abap_true ).
    
    headers_table->columns( 
      )->column( )->text( text = 'Key' )
      )->column( )->text( text = 'Value' )
      )->column( )->text( text = 'Enabled' )
      )->column( ).

    headers_table->items( )->column_list_item( 
      )->cells( 
        )->input( value = client->_bind_edit( 'key' ) placeholder = 'Header key'
        )->input( value = client->_bind_edit( 'value' ) placeholder = 'Header value'
        )->checkbox( selected = client->_bind_edit( 'enabled' )
        )->button( icon = 'sap-icon://add' press = client->_event( 'ADD_HEADER' ) type = 'Transparent' ).

    " Body tab
    DATA(body_tab) = request_tabs->items( )->tab_container_item( key = 'body' text = 'Body' ).
    body_tab->text_area( 
      value = client->_bind_edit( mv_request_body )
      placeholder = 'Enter request body (JSON, XML, etc.)...'
      rows = '15'
      width = '100%' ).

  ENDMETHOD.

  METHOD build_response_section.
    DATA(content) = io_view->content( ).

    " Response status bar
    DATA(status_bar) = content->toolbar( ).
    status_bar->text( text = |Status: { mv_response_status }| class = get_status_color( mv_response_status ) ).
    status_bar->toolbar_spacer( ).
    status_bar->text( text = |Time: { mv_response_time }| ).
    status_bar->toolbar_spacer( ).
    status_bar->text( text = |Size: { mv_response_size }| ).

    " Response tabs
    DATA(response_tabs) = content->tab_container( 
      selectedkey = client->_bind_edit( mv_response_tab )
      itemselect = client->_event( val = 'RESPONSE_TAB_SELECT' t_arg = VALUE #( ( `${$source>/key}` ) ) ) ).

    " Response body tab
    DATA(body_tab) = response_tabs->items( )->tab_container_item( key = 'body' text = 'Response Body' ).
    body_tab->text_area( 
      value = client->_bind( mv_response_body )
      editable = abap_false
      rows = '20'
      width = '100%' ).

    " Response headers tab
    DATA(headers_tab) = response_tabs->items( )->tab_container_item( key = 'headers' text = 'Headers' ).
    headers_tab->table( 
      items = client->_bind( mt_response_headers )
    )->columns( 
      )->column( )->text( text = 'Name' )
      )->column( )->text( text = 'Value' )
    )->items( )->column_list_item( 
      )->cells( 
        )->text( text = '{name}'
        )->text( text = '{value}' ).

  ENDMETHOD.

  METHOD handle_send_request.
    TRY.
        DATA(start_time) = cl_abap_tstmp=>systimestamp( ).
        
        " Process environment variables in URL
        DATA(processed_url) = process_environment_variables( mv_url ).
        
        " Build headers from table
        DATA(headers) = VALUE tihttpnvp( ).
        LOOP AT mt_headers INTO DATA(header) WHERE enabled = abap_true AND key IS NOT INITIAL.
          APPEND VALUE #( name = header-key value = process_environment_variables( header-value ) ) TO headers.
        ENDLOOP.

        " Execute request based on method
        DATA(response) = VALUE REF TO zif_rest_client=>ty_response( ).
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
        DATA(end_time) = cl_abap_tstmp=>systimestamp( ).
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
      DATA(pattern) = |\\{\\{{ env_var-key }\\}\\}|.
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
        result = 'curl-status-200'.
      WHEN '4'.
        result = 'curl-status-400'.
      WHEN '5'.
        result = 'curl-status-500'.
      WHEN OTHERS.
        result = ''.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
