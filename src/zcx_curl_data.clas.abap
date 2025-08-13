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

CLASS zcx_curl_data DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS: BEGIN OF zcx_curl_data,
                 msgid TYPE symsgid VALUE 'ZCURL',
                 msgno TYPE symsgno VALUE '001',
                 attr1 TYPE scx_attrname VALUE 'MV_ATTR1',
                 attr2 TYPE scx_attrname VALUE 'MV_ATTR2',
                 attr3 TYPE scx_attrname VALUE 'MV_ATTR3',
                 attr4 TYPE scx_attrname VALUE 'MV_ATTR4',
               END OF zcx_curl_data .

    DATA: mv_attr1 TYPE string,
          mv_attr2 TYPE string,
          mv_attr3 TYPE string,
          mv_attr4 TYPE string.

    METHODS: constructor
               IMPORTING !textid    LIKE textid OPTIONAL
                        !previous   LIKE previous OPTIONAL
                        !mv_attr1   TYPE string OPTIONAL
                        !mv_attr2   TYPE string OPTIONAL
                        !mv_attr3   TYPE string OPTIONAL
                        !mv_attr4   TYPE string OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcx_curl_data IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->mv_attr1 = mv_attr1.
    me->mv_attr2 = mv_attr2.
    me->mv_attr3 = mv_attr3.
    me->mv_attr4 = mv_attr4.
  ENDMETHOD.

ENDCLASS.
