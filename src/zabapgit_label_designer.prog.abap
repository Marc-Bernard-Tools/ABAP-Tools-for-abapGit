REPORT zabapgit_label_designer.

********************************************************************************
* abapGit Label Designer
*
* A tool for designing labels for your personal abapGit settings
* Colors are based on the standard CSS colors supported by browsers
* https://www.w3schools.com/cssref/css_colors.php
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
* https://marcbernardtools.com/
********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2023 Marc Bernard
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

CLASS lcl_gui DEFINITION.

  PUBLIC SECTION.
    METHODS on_event
      FOR EVENT sapevent OF zif_abapgit_html_viewer
      IMPORTING
        !action
        !frame
        !getdata
        !postdata
        !query_table.

    METHODS startup
      RAISING
        zcx_abapgit_exception.

    METHODS cache_html
      IMPORTING
        iv_text       TYPE string
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS show_url
      IMPORTING
        iv_url TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS render_page_1
      RAISING
        zcx_abapgit_exception.

    METHODS render_page_2
      IMPORTING
        ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception.

    METHODS handle_action
      IMPORTING
        !iv_action   TYPE c
        !iv_getdata  TYPE c OPTIONAL
        !it_postdata TYPE cnht_post_data_tab OPTIONAL.

    METHODS save
      IMPORTING
        ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception.

    METHODS back
      RETURNING
        VALUE(rv_exit) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS free
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    CONSTANTS c_cols TYPE i VALUE 6.

    TYPES:
      BEGIN OF ty_color,
        id     TYPE string,
        name   TYPE string,
        mode   TYPE string,
        fg     TYPE string,
        bg     TYPE string,
        border TYPE string,
      END OF ty_color,
      ty_colors TYPE STANDARD TABLE OF ty_color WITH KEY id,
      BEGIN OF ty_label,
        id    TYPE string,
        text  TYPE string,
        color TYPE ty_color,
      END OF ty_label,
      ty_labels TYPE SORTED TABLE OF ty_label WITH UNIQUE KEY id.

    DATA:
      mx_error    TYPE REF TO zcx_abapgit_exception,
      mo_settings TYPE REF TO zcl_abapgit_settings,
      ms_settings TYPE zif_abapgit_definitions=>ty_s_user_settings,
      mo_colors   TYPE REF TO zcl_abapgit_string_map,
      mt_colors   TYPE ty_colors,
      mv_header   TYPE string,
      mv_styles   TYPE string,
      mt_labels   TYPE ty_labels,
      mi_viewer   TYPE REF TO zif_abapgit_html_viewer,
      mv_page     TYPE i.

    METHODS _get_styles.

    METHODS _get_header.

    METHODS _get_colors.

    METHODS _load_settings.

    METHODS _save_settings
      IMPORTING
        iv_label_colors TYPE string.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD startup.

    DATA:
      lt_events TYPE cntl_simple_events,
      ls_event  LIKE LINE OF lt_events.

    mi_viewer = zcl_abapgit_ui_factory=>get_html_viewer( ).

    ls_event-eventid    = mi_viewer->c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mi_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mi_viewer.

    _get_styles( ).
    _get_header( ).
    _get_colors( ).

    _load_settings( ).

  ENDMETHOD.

  METHOD cache_html.

    DATA:
      lv_size TYPE i,
      lt_html TYPE w3htmltab.

    zcl_abapgit_convert=>string_to_tab(
      EXPORTING
        iv_str  = iv_text
      IMPORTING
        ev_size = lv_size
        et_tab  = lt_html ).

    mi_viewer->load_data(
      EXPORTING
        iv_type         = 'text'
        iv_subtype      = 'html'
        iv_size         = lv_size
      IMPORTING
        ev_assigned_url = rv_url
      CHANGING
        ct_data_table   = lt_html ).

  ENDMETHOD.

  METHOD show_url.
    mi_viewer->show_url( iv_url ).
  ENDMETHOD.

  METHOD render_page_1.

    DATA:
      lv_url   TYPE string,
      lv_html  TYPE string,
      lv_tabix TYPE sy-tabix,
      ls_color TYPE ty_color,
      ls_label TYPE ty_label.

    mv_page = 1.

    lv_html = mv_header &&
      |<form method="post" id="form" action="sapevent:preview" class="form">\n| &&
      |<p class="pad">Enter a label text into the labels you want to use in abapGit and select "Preview" at the bottom of the page.</p>\n| &&
      |<table>\n|.

    LOOP AT mt_colors INTO ls_color.
      lv_tabix = sy-tabix.

      IF lv_tabix MOD c_cols = 1.
        lv_html = lv_html && |<tr>\n|.
      ENDIF.

      READ TABLE mt_labels INTO ls_label WITH TABLE KEY id = ls_color-id.
      IF sy-subrc <> 0.
        CLEAR ls_label.
      ENDIF.

      lv_html = lv_html &&
        |<td style="padding:10px">\n| &&
        |<div style="| &&
        |width:180px;height:50px;| &&
        |border-radius:6px;border:solid 1px;| &&
        |padding:5px 5px 5px 5px;text-align:center;| &&
        |color:#{ ls_color-fg };| &&
        |background-color:#{ ls_color-bg };| &&
        |border-color:{ ls_color-border };">| &&
        |{ ls_color-name }<br>\n| &&
        |<input type="text" value="{ ls_label-text }" width="15" name="{ ls_color-id }">\n| &&
        |</div>\n| &&
        |</td>\n|.

      IF lv_tabix MOD c_cols = 0.
        lv_html = lv_html && |</tr>\n|.
      ENDIF.
    ENDLOOP.

    lv_html = lv_html &&
      |</table>\n| &&
      |<div class="pad">| &&
      |<input type="submit" value="Preview Selected Labels">\n| &&
      |</div>| &&
      |</form>\n| &&
      |</body></html>\n|.

    lv_url = cache_html( lv_html ).

    show_url( lv_url ).

  ENDMETHOD.

  METHOD render_page_2.

    DATA:
      lv_url    TYPE string,
      lv_html   TYPE string,
      lv_label  TYPE string,
      ls_color  TYPE ty_color,
      ls_label  TYPE ty_label,
      lt_labels TYPE string_table,
      lo_colors TYPE REF TO zcl_abapgit_string_map.

    mv_page = 2.

    lv_html = mv_header &&
      |<form method="post" id="form" action="sapevent:save" class="form">\n| &&
      |<p class="pad">Here's a preview of what your labels will look like. Select "Save" at the bottom of the page.</p>\n|.

    CLEAR mt_labels.

    CREATE OBJECT lo_colors.

    LOOP AT mt_colors INTO ls_color.
      lv_label = ii_event->form_data( )->get( ls_color-id ).

      IF lv_label IS NOT INITIAL.
        TRY.
            zcl_abapgit_repo_labels=>validate( lv_label ).
          CATCH zcx_abapgit_exception INTO mx_error.
            lv_html = lv_html &&
              |<p style="color:red">| &&
              |Label "{ escape( val = lv_label format = cl_abap_format=>e_html_text ) }" contains disallowed characters| &&
              |</p>|.
            CONTINUE.
        ENDTRY.

        READ TABLE mt_labels INTO ls_label WITH KEY text = lv_label.
        IF sy-subrc = 0.
          lv_html = lv_html &&
            |<p style="color:red">| &&
            |Label "{ escape( val = lv_label format = cl_abap_format=>e_html_text ) }" | &&
            |set for "{ ls_label-color-name }{ ls_label-color-mode }" and "{ ls_color-name }{ ls_color-mode }"| &&
            |</p>|.
          CONTINUE.
        ENDIF.

        CLEAR ls_label.
        ls_label-id    = ls_color-id.
        ls_label-text  = lv_label.
        ls_label-color = ls_color.
        INSERT ls_label INTO TABLE mt_labels.

        INSERT lv_label INTO TABLE lt_labels.
        lo_colors->set(
          iv_key = lv_label
          iv_val = |#{ ls_color-fg }/{ ls_color-bg }/{ ls_color-border }| ).
      ENDIF.
    ENDLOOP.

    SORT lt_labels.

    lv_html = lv_html &&
      |<div class="repo-label-catalog">\n| &&
      zcl_abapgit_gui_chunk_lib=>render_label_list(
        it_labels       = lt_labels
        io_label_colors = lo_colors ) &&
      |</div>|.

    lv_html = lv_html &&
      |<div class="pad">| &&
      |<input type="submit" value="Save Label Designs to Personal Settings" formaction="sapevent:save">&nbsp;&nbsp;| &&
      |<input type="submit" value="Back" formaction="sapevent:back">\n| &&
      |</div>| &&
      |</form>\n| &&
      |</body></html>\n|.

    lv_url = cache_html( lv_html ).

    show_url( lv_url ).

  ENDMETHOD.

  METHOD on_event.

    handle_action(
      iv_action   = action
      iv_getdata  = getdata
      it_postdata = postdata ).

  ENDMETHOD.

  METHOD handle_action.

    DATA li_event TYPE REF TO zif_abapgit_gui_event.

    CREATE OBJECT li_event TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action   = iv_action
        iv_getdata  = iv_getdata
        it_postdata = it_postdata.

    TRY.
        IF iv_action = 'preview'.
          render_page_2( li_event ).
        ELSEIF iv_action = 'save'.
          save( li_event ).
        ELSEIF iv_action = 'back'.
          back( ).
        ELSE.
          BREAK-POINT.
        ENDIF.
      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD.

  METHOD save.

    DATA:
      ls_label  TYPE ty_label,
      lv_color  TYPE string,
      lv_labels TYPE string.

    LOOP AT mt_labels INTO ls_label.
      lv_labels = lv_labels && |{ ls_label-text }:| &&
        |#{ ls_label-color-fg }/{ ls_label-color-bg }/{ ls_label-color-border }|.
      IF sy-tabix < lines( mt_labels ).
        lv_labels = lv_labels && ','.
      ENDIF.
    ENDLOOP.

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( lv_labels ).

        _save_settings( lv_labels ).
      CATCH zcx_abapgit_exception INTO mx_error.
        MESSAGE mx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD back.

    IF mv_page = 2.
      render_page_1( ).
    ELSE.
      rv_exit = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD free.

    SET HANDLER on_event FOR mi_viewer ACTIVATION space.
    mi_viewer->close_document( ).
    mi_viewer->free( ).
    FREE mi_viewer.

  ENDMETHOD.

  METHOD _load_settings.

    TRY.
        mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
        ms_settings = mo_settings->get_user_settings( ).
        mo_colors   = zcl_abapgit_repo_labels=>split_colors_into_map( ms_settings-label_colors ).
      CATCH zcx_abapgit_exception INTO mx_error.
        MESSAGE mx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _save_settings.

    TRY.
        ms_settings-label_colors = iv_label_colors.
        mo_settings->set_user_settings( ms_settings ).
        zcl_abapgit_persist_factory=>get_settings( )->modify( mo_settings ).
      CATCH zcx_abapgit_exception INTO mx_error.
        MESSAGE mx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD _get_colors.

    DATA:
      lv_colors TYPE string,
      lt_colors TYPE string_table,
      ls_color  TYPE ty_color.

    lv_colors =
      'aliceblue,f0f8ff,1f2d3d;' &&
      'antiquewhite,faebd7,1f2d3d;' &&
      'aqua,00ffff,1f2d3d;' &&
      'aquamarine,7fffd4,1f2d3d;' &&
      'azure,f0ffff,1f2d3d;' &&
      'beige,f5f5dc,1f2d3d;' &&
      'bisque,ffe4c4,1f2d3d;' &&
      'black,000000,fff;' &&
      'blanchedalmond,ffebcd,1f2d3d;' &&
      'blue,0000ff,fff;' &&
      'blueviolet,8a2be2,fff;' &&
      'brown,a52a2a,fff;' &&
      'burlywood,deb887,1f2d3d;' &&
      'cadetblue,5f9ea0,fff;' &&
      'chartreuse,7fff00,1f2d3d;' &&
      'chocolate,d2691e,fff;' &&
      'coral,ff7f50,1f2d3d;' &&
      'cornflowerblue,6495ed,fff;' &&
      'cornsilk,fff8dc,1f2d3d;' &&
      'crimson,dc143c,fff;' &&
      'cyan,00ffff,1f2d3d;' &&
      'darkblue,00008b,fff;' &&
      'darkcyan,008b8b,fff;' &&
      'darkgoldenrod,b8860b,fff;' &&
      'darkgray,a9a9a9,1f2d3d;' &&
      'darkgreen,006400,fff;' &&
      'darkgrey,a9a9a9,1f2d3d;' &&
      'darkkhaki,bdb76b,1f2d3d;' &&
      'darkmagenta,8b008b,fff;' &&
      'darkolivegreen,556b2f,fff;' &&
      'darkorange,ff8c00,1f2d3d;' &&
      'darkorchid,9932cc,fff;' &&
      'darkred,8b0000,fff;' &&
      'darksalmon,e9967a,1f2d3d;' &&
      'darkseagreen,8fbc8f,1f2d3d;' &&
      'darkslateblue,483d8b,fff;' &&
      'darkslategray,2f4f4f,fff;' &&
      'darkslategrey,2f4f4f,fff;' &&
      'darkturquoise,00ced1,fff;' &&
      'darkviolet,9400d3,fff;' &&
      'deeppink,ff1493,fff;' &&
      'deepskyblue,00bfff,1f2d3d;' &&
      'dimgray,696969,fff;' &&
      'dimgrey,696969,fff;' &&
      'dodgerblue,1e90ff,fff;' &&
      'firebrick,b22222,fff;' &&
      'floralwhite,fffaf0,1f2d3d;' &&
      'forestgreen,228b22,fff;' &&
      'fuchsia,ff00ff,fff;' &&
      'gainsboro,dcdcdc,1f2d3d;' &&
      'ghostwhite,f8f8ff,1f2d3d;' &&
      'gold,ffd700,1f2d3d;' &&
      'goldenrod,daa520,1f2d3d;' &&
      'gray,808080,fff;' &&
      'green,008000,fff;' &&
      'greenyellow,adff2f,1f2d3d;' &&
      'grey,808080,fff;' &&
      'honeydew,f0fff0,1f2d3d;' &&
      'hotpink,ff69b4,1f2d3d;' &&
      'indianred,cd5c5c,fff;' &&
      'indigo,4b0082,fff;' &&
      'ivory,fffff0,1f2d3d;' &&
      'khaki,f0e68c,1f2d3d;' &&
      'lavender,e6e6fa,1f2d3d;' &&
      'lavenderblush,fff0f5,1f2d3d;' &&
      'lawngreen,7cfc00,1f2d3d;' &&
      'lemonchiffon,fffacd,1f2d3d;' &&
      'lightblue,add8e6,1f2d3d;' &&
      'lightcoral,f08080,1f2d3d;' &&
      'lightcyan,e0ffff,1f2d3d;' &&
      'lightgoldenrodyellow,fafad2,1f2d3d;' &&
      'lightgray,d3d3d3,1f2d3d;' &&
      'lightgreen,90ee90,1f2d3d;' &&
      'lightgrey,d3d3d3,1f2d3d;' &&
      'lightpink,ffb6c1,1f2d3d;' &&
      'lightsalmon,ffa07a,1f2d3d;' &&
      'lightseagreen,20b2aa,fff;' &&
      'lightskyblue,87cefa,1f2d3d;' &&
      'lightslategray,778899,fff;' &&
      'lightslategrey,778899,fff;' &&
      'lightsteelblue,b0c4de,1f2d3d;' &&
      'lightyellow,ffffe0,1f2d3d;' &&
      'lime,00ff00,fff;' &&
      'limegreen,32cd32,fff;' &&
      'linen,faf0e6,1f2d3d;' &&
      'magenta,ff00ff,fff;' &&
      'maroon,800000,fff;' &&
      'mediumaquamarine,66cdaa,1f2d3d;' &&
      'mediumblue,0000cd,fff;' &&
      'mediumorchid,ba55d3,fff;' &&
      'mediumpurple,9370db,fff;' &&
      'mediumseagreen,3cb371,fff;' &&
      'mediumslateblue,7b68ee,fff;' &&
      'mediumspringgreen,00fa9a,1f2d3d;' &&
      'mediumturquoise,48d1cc,1f2d3d;' &&
      'mediumvioletred,c71585,fff;' &&
      'midnightblue,191970,fff;' &&
      'mintcream,f5fffa,1f2d3d;' &&
      'mistyrose,ffe4e1,1f2d3d;' &&
      'moccasin,ffe4b5,1f2d3d;' &&
      'navajowhite,ffdead,1f2d3d;' &&
      'navy,000080,fff;' &&
      'oldlace,fdf5e6,1f2d3d;' &&
      'olive,808000,fff;' &&
      'olivedrab,6b8e23,fff;' &&
      'orange,ffa500,1f2d3d;' &&
      'orangered,ff4500,fff;' &&
      'orchid,da70d6,1f2d3d;' &&
      'palegoldenrod,eee8aa,1f2d3d;' &&
      'palegreen,98fb98,1f2d3d;' &&
      'paleturquoise,afeeee,1f2d3d;' &&
      'palevioletred,db7093,fff;' &&
      'papayawhip,ffefd5,1f2d3d;' &&
      'peachpuff,ffdab9,1f2d3d;' &&
      'peru,cd853f,fff;' &&
      'pink,ffc0cb,1f2d3d;' &&
      'plum,dda0dd,1f2d3d;' &&
      'powderblue,b0e0e6,1f2d3d;' &&
      'purple,800080,fff;' &&
      'rebeccapurple,663399,fff;' &&
      'red,ff0000,fff;' &&
      'rosybrown,bc8f8f,1f2d3d;' &&
      'royalblue,4169e1,fff;' &&
      'saddlebrown,8b4513,fff;' &&
      'salmon,fa8072,1f2d3d;' &&
      'sandybrown,f4a460,1f2d3d;' &&
      'seagreen,2e8b57,fff;' &&
      'seashell,fff5ee,1f2d3d;' &&
      'sienna,a0522d,fff;' &&
      'silver,c0c0c0,1f2d3d;' &&
      'skyblue,87ceeb,1f2d3d;' &&
      'slateblue,6a5acd,fff;' &&
      'slategray,708090,fff;' &&
      'slategrey,708090,fff;' &&
      'snow,fffafa,1f2d3d;' &&
      'springgreen,00ff7f,1f2d3d;' &&
      'steelblue,4682b4,fff;' &&
      'tan,d2b48c,1f2d3d;' &&
      'teal,008080,fff;' &&
      'thistle,d8bfd8,1f2d3d;' &&
      'tomato,ff6347,fff;' &&
      'turquoise,40e0d0,1f2d3d;' &&
      'violet,ee82ee,1f2d3d;' &&
      'wheat,f5deb3,1f2d3d;' &&
      'white,fff,1f2d3d;' &&
      'whitesmoke,f5f5f5,1f2d3d;' &&
      'yellow,ffff00,1f2d3d;' &&
      'yellowgreen,9acd32,1f2d3d'.

    SPLIT lv_colors AT ';' INTO TABLE lt_colors.
    LOOP AT lt_colors INTO lv_colors.
      DO 3 TIMES.
        CLEAR ls_color.
        SPLIT lv_colors AT ',' INTO ls_color-name ls_color-bg ls_color-fg.
        CASE sy-index.
          WHEN 1.
            ls_color-id     = |col-{ sy-tabix }|.
            ls_color-mode   = ''.
            ls_color-border = ls_color-bg.
          WHEN 2.
            ls_color-id     = |wht-{ sy-tabix }|.
            ls_color-mode   = ' on white'.
            ls_color-fg     = ls_color-bg.
            ls_color-border = ls_color-bg.
            ls_color-bg     = 'fff'.
          WHEN 3.
            ls_color-id     = |blk-{ sy-tabix }|.
            ls_color-mode   = ' on black'.
            ls_color-fg     = ls_color-bg.
            ls_color-border = '000'.
            ls_color-bg     = '000'.
        ENDCASE.
        INSERT ls_color INTO TABLE mt_colors.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD _get_styles.

    mv_styles =
      '.main {' &&
      '  font-family:Arial;' &&
      '  font-size:16px;' &&
      '  background-color:#e8e8e8;' &&
      '}' &&
      'h1 {' &&
      '  font-size:28px;' &&
      '  color:darkgray;' &&
      '}' &&
      '.pad {' &&
      '  padding:20px 10px 0px;' &&
      '}' &&
      '.form {' &&
      '  background-color:#f2f2f2;' &&
      '  padding-bottom:20px;' &&
      '}' &&
      '.repo-label-catalog {' &&
      '  padding: 1em 1em;' &&
      '  margin-top: -1em;' &&
      '}' &&
      '.repo-label-catalog label {' &&
      '  margin-right: 0.5em;' &&
      '}' &&
      'ul.repo-labels {' &&
      '  display: inline-block;' &&
      '  list-style-type: none;' &&
      '  padding-inline-start: 0px;' &&
      '  padding-left: 0px;' &&
      '  margin-block-start: 0px;' &&
      '  margin-block-end: 0px;' &&
      '  margin-top: 0px;' &&
      '  margin-bottom: 0px;' &&
      '}' &&
      'ul.repo-labels li {' &&
      '  display: inline-block;' &&
      '  padding: 3px 5px;' &&
      '  border-radius: 3px;' &&
      '  border-style: solid;' &&
      '  border-width: 1px;' &&
      '  margin-bottom: 2px;' &&
      '}' &&
      'ul.repo-labels li a {' &&
      '  color: inherit;' &&
      '}' &&
      'ul.repo-labels li:not(:last-child) { margin-right: 0.3em; }' &&
      'table ul.repo-labels li {' &&
      '  font-size: 90%;' &&
      '  padding: 2px 4px;' &&
      '}'.

  ENDMETHOD.

  METHOD _get_header.

    mv_header =
      |<html><head>\n| &&
      |<style>{ mv_styles }</style>| &&
      |</head>\n| &&
      |<body class="main">\n| &&
      |<h1>| &&
      |<img src="https://abapgit.org/img/logo.svg" width="110px" style="vertical-align:middle;">| &&
      |<span style="vertical-align:middle;padding:10px;">Label Designer</span>| &&
      |</h1>\n|.

  ENDMETHOD.

ENDCLASS.

DATA go_gui TYPE REF TO lcl_gui.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  CASE sy-ucomm.
    WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
      IF go_gui->back( ) = abap_true.
        go_gui->free( ).
      ELSE.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.

START-OF-SELECTION.

  CREATE OBJECT go_gui.

  go_gui->startup( ).

  go_gui->render_page_1( ).

  CALL SELECTION-SCREEN 1001.
