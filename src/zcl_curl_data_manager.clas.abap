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

CLASS zcl_curl_data_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_request_item,
             id          TYPE string,
             name        TYPE string,
             method      TYPE string,
             url         TYPE string,
             headers     TYPE string, " JSON string of headers
             body        TYPE string,
             description TYPE string,
             created_by  TYPE sy-uname,
             created_at  TYPE timestampl,
             updated_at  TYPE timestampl,
           END OF ty_request_item.

    TYPES: BEGIN OF ty_collection,
             id          TYPE string,
             name        TYPE string,
             description TYPE string,
             created_by  TYPE sy-uname,
             created_at  TYPE timestampl,
             updated_at  TYPE timestampl,
           END OF ty_collection.

    TYPES: BEGIN OF ty_environment,
             id          TYPE string,
             name        TYPE string,
             variables   TYPE string, " JSON string of variables
             created_by  TYPE sy-uname,
             created_at  TYPE timestampl,
             updated_at  TYPE timestampl,
           END OF ty_environment.

    TYPES: tt_request_items TYPE TABLE OF ty_request_item,
           tt_collections   TYPE TABLE OF ty_collection,
           tt_environments  TYPE TABLE OF ty_environment.

    CLASS-METHODS: save_request
                     IMPORTING iv_request    TYPE ty_request_item
                     RETURNING VALUE(result) TYPE string " Returns ID
                     RAISING   zcx_curl_data,
                   
                   load_request
                     IMPORTING iv_id         TYPE string
                     RETURNING VALUE(result) TYPE ty_request_item
                     RAISING   zcx_curl_data,
                   
                   delete_request
                     IMPORTING iv_id TYPE string
                     RAISING   zcx_curl_data,
                   
                   get_all_requests
                     IMPORTING iv_collection_id TYPE string OPTIONAL
                     RETURNING VALUE(result)    TYPE tt_request_items
                     RAISING   zcx_curl_data,
                   
                   save_collection
                     IMPORTING iv_collection TYPE ty_collection
                     RETURNING VALUE(result) TYPE string " Returns ID
                     RAISING   zcx_curl_data,
                   
                   load_collection
                     IMPORTING iv_id         TYPE string
                     RETURNING VALUE(result) TYPE ty_collection
                     RAISING   zcx_curl_data,
                   
                   get_all_collections
                     RETURNING VALUE(result) TYPE tt_collections
                     RAISING   zcx_curl_data,
                   
                   save_environment
                     IMPORTING iv_environment TYPE ty_environment
                     RETURNING VALUE(result)  TYPE string " Returns ID
                     RAISING   zcx_curl_data,
                   
                   load_environment
                     IMPORTING iv_id         TYPE string
                     RETURNING VALUE(result) TYPE ty_environment
                     RAISING   zcx_curl_data,
                   
                   get_all_environments
                     RETURNING VALUE(result) TYPE tt_environments
                     RAISING   zcx_curl_data,
                   
                   generate_id
                     RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    " In a real implementation, these would be database tables
    " For this demo, we'll use memory storage with class attributes
    CLASS-DATA: gt_requests     TYPE tt_request_items,
                gt_collections TYPE tt_collections,
                gt_environments TYPE tt_environments.

    CLASS-METHODS: initialize_demo_data.

ENDCLASS.

CLASS zcl_curl_data_manager IMPLEMENTATION.

  METHOD save_request.
    " Generate ID if not provided
    IF iv_request-id IS INITIAL.
      result = generate_id( ).
    ELSE.
      result = iv_request-id.
    ENDIF.

    " Prepare request data
    DATA(ls_request) = iv_request.
    ls_request-id = result.
    ls_request-created_by = sy-uname.
    GET TIME STAMP FIELD ls_request-updated_at.
    
    IF ls_request-created_at IS INITIAL.
      ls_request-created_at = ls_request-updated_at.
    ENDIF.

    " Check if request already exists (update) or new (insert)
    READ TABLE gt_requests INTO DATA(existing_request) WITH KEY id = result.
    IF sy-subrc = 0.
      " Update existing
      MODIFY gt_requests FROM ls_request TRANSPORTING ALL FIELDS WHERE id = result.
    ELSE.
      " Insert new
      APPEND ls_request TO gt_requests.
    ENDIF.

    " In real implementation: INSERT/UPDATE database table
    " INSERT zcurl_requests FROM ls_request.
  ENDMETHOD.

  METHOD load_request.
    READ TABLE gt_requests INTO result WITH KEY id = iv_id.
    IF sy-subrc <> 0.
      " In real implementation: SELECT from database
      RAISE EXCEPTION TYPE zcx_curl_data
        MESSAGE e001(zcurl) WITH 'Request not found' iv_id.
    ENDIF.
  ENDMETHOD.

  METHOD delete_request.
    DELETE gt_requests WHERE id = iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_curl_data
        MESSAGE e001(zcurl) WITH 'Request not found for deletion' iv_id.
    ENDIF.

    " In real implementation: DELETE from database
    " DELETE FROM zcurl_requests WHERE id = iv_id.
  ENDMETHOD.

  METHOD get_all_requests.
    " Initialize demo data if empty
    IF lines( gt_requests ) = 0.
      initialize_demo_data( ).
    ENDIF.

    " If collection ID is provided, filter by collection
    IF iv_collection_id IS NOT INITIAL.
      " In real implementation, there would be a relationship table
      " For now, return all requests
      result = gt_requests.
    ELSE.
      result = gt_requests.
    ENDIF.

    " In real implementation: SELECT from database
    " SELECT * FROM zcurl_requests INTO TABLE result WHERE collection_id = iv_collection_id.
  ENDMETHOD.

  METHOD save_collection.
    " Generate ID if not provided
    IF iv_collection-id IS INITIAL.
      result = generate_id( ).
    ELSE.
      result = iv_collection-id.
    ENDIF.

    " Prepare collection data
    DATA(ls_collection) = iv_collection.
    ls_collection-id = result.
    ls_collection-created_by = sy-uname.
    GET TIME STAMP FIELD ls_collection-updated_at.
    
    IF ls_collection-created_at IS INITIAL.
      ls_collection-created_at = ls_collection-updated_at.
    ENDIF.

    " Check if collection already exists
    READ TABLE gt_collections INTO DATA(existing_collection) WITH KEY id = result.
    IF sy-subrc = 0.
      " Update existing
      MODIFY gt_collections FROM ls_collection TRANSPORTING ALL FIELDS WHERE id = result.
    ELSE.
      " Insert new
      APPEND ls_collection TO gt_collections.
    ENDIF.
  ENDMETHOD.

  METHOD load_collection.
    READ TABLE gt_collections INTO result WITH KEY id = iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_curl_data
        MESSAGE e001(zcurl) WITH 'Collection not found' iv_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_all_collections.
    " Initialize demo data if empty
    IF lines( gt_collections ) = 0.
      initialize_demo_data( ).
    ENDIF.

    result = gt_collections.
  ENDMETHOD.

  METHOD save_environment.
    " Generate ID if not provided
    IF iv_environment-id IS INITIAL.
      result = generate_id( ).
    ELSE.
      result = iv_environment-id.
    ENDIF.

    " Prepare environment data
    DATA(ls_environment) = iv_environment.
    ls_environment-id = result.
    ls_environment-created_by = sy-uname.
    GET TIME STAMP FIELD ls_environment-updated_at.
    
    IF ls_environment-created_at IS INITIAL.
      ls_environment-created_at = ls_environment-updated_at.
    ENDIF.

    " Check if environment already exists
    READ TABLE gt_environments INTO DATA(existing_env) WITH KEY id = result.
    IF sy-subrc = 0.
      " Update existing
      MODIFY gt_environments FROM ls_environment TRANSPORTING ALL FIELDS WHERE id = result.
    ELSE.
      " Insert new
      APPEND ls_environment TO gt_environments.
    ENDIF.
  ENDMETHOD.

  METHOD load_environment.
    READ TABLE gt_environments INTO result WITH KEY id = iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_curl_data
        MESSAGE e001(zcurl) WITH 'Environment not found' iv_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_all_environments.
    " Initialize demo data if empty
    IF lines( gt_environments ) = 0.
      initialize_demo_data( ).
    ENDIF.

    result = gt_environments.
  ENDMETHOD.

  METHOD generate_id.
    " Generate a unique ID using timestamp and random number
    DATA: lv_timestamp TYPE timestampl,
          lv_random    TYPE i.

    GET TIME STAMP FIELD lv_timestamp.
    CALL FUNCTION 'RANDOM_INT'
      EXPORTING
        min = 1000
        max = 9999
      IMPORTING
        value = lv_random.

    result = |{ lv_timestamp }{ lv_random }|.
  ENDMETHOD.

  METHOD initialize_demo_data.
    " Initialize some demo collections
    GET TIME STAMP FIELD DATA(current_time).
    
    " Demo Collections
    APPEND VALUE #( 
      id = 'COL001'
      name = 'User Management APIs'
      description = 'Collection of user-related API endpoints'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_collections.
    
    APPEND VALUE #( 
      id = 'COL002'
      name = 'Product APIs'
      description = 'Collection of product-related API endpoints'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_collections.

    " Demo Environments
    APPEND VALUE #( 
      id = 'ENV001'
      name = 'Development'
      variables = '{"base_url":"https://api-dev.example.com","api_key":"dev_key_123"}'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_environments.
    
    APPEND VALUE #( 
      id = 'ENV002'
      name = 'Production'
      variables = '{"base_url":"https://api.example.com","api_key":"prod_key_456"}'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_environments.

    " Demo Requests
    APPEND VALUE #( 
      id = 'REQ001'
      name = 'Get All Users'
      method = 'GET'
      url = '{{base_url}}/users'
      headers = '[{"key":"Accept","value":"application/json","enabled":true}]'
      body = ''
      description = 'Retrieve all users from the system'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_requests.
    
    APPEND VALUE #( 
      id = 'REQ002'
      name = 'Create User'
      method = 'POST'
      url = '{{base_url}}/users'
      headers = '[{"key":"Content-Type","value":"application/json","enabled":true}]'
      body = '{"name":"John Doe","email":"john@example.com","role":"user"}'
      description = 'Create a new user in the system'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_requests.
    
    APPEND VALUE #( 
      id = 'REQ003'
      name = 'Get User by ID'
      method = 'GET'
      url = '{{base_url}}/users/123'
      headers = '[{"key":"Accept","value":"application/json","enabled":true}]'
      body = ''
      description = 'Retrieve a specific user by ID'
      created_by = sy-uname
      created_at = current_time
      updated_at = current_time
    ) TO gt_requests.

  ENDMETHOD.

ENDCLASS.
