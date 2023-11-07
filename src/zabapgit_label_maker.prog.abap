REPORT zabapgit_label_maker.

********************************************************************************
* abapGit Label Maker
*
* A tool for doing defining labels for your personal abapGit settings
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
      RAISING
        zcx_abapgit_exception.

    METHODS handle_action
      IMPORTING
        !iv_action   TYPE c
        !iv_getdata  TYPE c OPTIONAL
        !it_postdata TYPE cnht_post_data_tab OPTIONAL.

    METHODS back
      RETURNING
        VALUE(rv_exit) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS free
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_color,
        name       TYPE string,
        background TYPE string,
        foreground TYPE string,
      END OF ty_color.

    DATA mt_colors TYPE SORTED TABLE OF ty_color WITH UNIQUE KEY name.
    DATA mi_html_viewer TYPE REF TO zif_abapgit_html_viewer.
    DATA mv_page TYPE i.

    METHODS get_colors.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD startup.

    DATA:
      lt_events TYPE cntl_simple_events,
      ls_event  LIKE LINE OF lt_events.

    mi_html_viewer = zcl_abapgit_ui_factory=>get_html_viewer( ).

    ls_event-eventid    = mi_html_viewer->c_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mi_html_viewer->set_registered_events( lt_events ).
    SET HANDLER on_event FOR mi_html_viewer.

    get_colors( ).

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

    mi_html_viewer->load_data(
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
    mi_html_viewer->show_url( iv_url ).
  ENDMETHOD.

  METHOD render_page_1.

    DATA:
      lv_url   TYPE string,
      lv_html  TYPE string,
      ls_color TYPE ty_color.

    mv_page = 1.

    lv_html =
      |<html><head></head><body style="font-family:Arial;font-size:16px;background-color:#eeeeee">\n| &&
      |<h1><img src="https://abapgit.org/img/logo.svg" width="200px"></h1>\n| &&
      |<p>Enter a label text into the labels you want to use in abapGit and select "Preview" at the bottom of the page.</p>\n|.

    lv_html = lv_html &&
      |<form method="post" id="form" action="sapevent:preview">\n| &&
      |<table>\n|.

    LOOP AT mt_colors INTO ls_color.
      lv_html = lv_html &&
        |<tr>\n| &&
        |<td style="padding:10px">\n| &&
        |<div style="\n| &&
        |background-color:#{ ls_color-background };color:#{ ls_color-foreground };\n| &&
        |width:180px;height:60px;\n| &&
        |border-radius:5px;border:solid 1px;border-color:{ ls_color-background };\n| &&
        |padding:12px 5px 5px 5px;text-align:center\n| &&
        |">{ ls_color-name }<br>\n| &&
        |<input type="text" value="" width="15" id="col-{ sy-tabix }">\n| &&
        |</div>\n| &&
        |</td>\n| &&
        |<td style="padding:10px">\n| &&
        |<div style="\n| &&
        |background-color:#ffffff;color:#{ ls_color-background };\n| &&
        |width:180px;height:60px;\n| &&
        |border-radius:5px;border:solid 1px;border-color:#{ ls_color-background };\n| &&
        |padding:12px 5px 5px 5px;text-align:center\n| &&
        |">{ ls_color-name }<br>\n| &&
        |<input type="text" value="" width="15" id="wht-{ sy-tabix }">\n| &&
        |</div>\n| &&
        |</td>\n| &&
        |<td style="padding:10px">\n| &&
        |<div style="\n| &&
        |background-color:#000000;color:#{ ls_color-background };\n| &&
        |width:180px;height:60px;\n| &&
        |border-radius:5px;border:solid 1px;border-color:#000000;\n| &&
        |padding:12px 5px 5px 5px;text-align:center\n| &&
        |">{ ls_color-name }<br>\n| &&
        |<input type="text" value="" width="15" id="blk-{ sy-tabix }">\n| &&
        |</div>\n| &&
        |</td>\n| &&
        |</tr>\n|.
    ENDLOOP.

    lv_html = lv_html &&
      |</table>\n| &&
      |<input type="submit" value="Preview Selected Labels">\n| &&
      |</form>\n| &&
      |</body></html>\n|.

    lv_url = cache_html( lv_html ).

    show_url( lv_url ).

  ENDMETHOD.

  METHOD render_page_2.

    DATA:
      lv_url  TYPE string,
      lv_html TYPE string.

    mv_page = 2.

    lv_html =
      |<html><head></head><body style="font-family:Arial;font-size:16px;background-color:#eeeeee">\n| &&
      |<h1><img src="https://abapgit.org/img/logo.svg" width="200px"></h1>\n| &&
      |<p>Here's a preview of what your labels will look like. Select "Save" at the bottom of the page.</p>\n|.

    lv_html = lv_html &&
      |<form method="post" id="form">\n| &&
      |<div style="background-color:teal;color:white">label1</div>\n| &&
      |<input type="submit" value="Save Labels to Personal Settings" formaction="sapevent:save">&nbsp;&nbsp;| &&
      |<input type="submit" value="Back" formaction="sapevent:back">\n| &&
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

    TRY.
        IF iv_action = 'preview'.
          render_page_2( ).
        ELSEIF iv_action = 'save'.
          MESSAGE 'to-do' TYPE 'I'.
        ELSEIF iv_action = 'back'.
          back( ).
        ELSE.
          BREAK-POINT.
        ENDIF.
      CATCH cx_root.
        BREAK-POINT.
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

    SET HANDLER on_event FOR mi_html_viewer ACTIVATION space.
    mi_html_viewer->close_document( ).
    mi_html_viewer->free( ).
    FREE mi_html_viewer.

  ENDMETHOD.

  METHOD get_colors.

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
      'black,000000,ffffff;' &&
      'blanchedalmond,ffebcd,1f2d3d;' &&
      'blue,0000ff,ffffff;' &&
      'blueviolet,8a2be2,ffffff;' &&
      'brown,a52a2a,ffffff;' &&
      'burlywood,deb887,1f2d3d;' &&
      'cadetblue,5f9ea0,ffffff;' &&
      'chartreuse,7fff00,1f2d3d;' &&
      'chocolate,d2691e,ffffff;' &&
      'coral,ff7f50,1f2d3d;' &&
      'cornflowerblue,6495ed,ffffff;' &&
      'cornsilk,fff8dc,1f2d3d;' &&
      'crimson,dc143c,ffffff;' &&
      'cyan,00ffff,1f2d3d;' &&
      'darkblue,00008b,ffffff;' &&
      'darkcyan,008b8b,ffffff;' &&
      'darkgoldenrod,b8860b,ffffff;' &&
      'darkgray,a9a9a9,1f2d3d;' &&
      'darkgreen,006400,ffffff;' &&
      'darkgrey,a9a9a9,1f2d3d;' &&
      'darkkhaki,bdb76b,1f2d3d;' &&
      'darkmagenta,8b008b,ffffff;' &&
      'darkolivegreen,556b2f,ffffff;' &&
      'darkorange,ff8c00,1f2d3d;' &&
      'darkorchid,9932cc,ffffff;' &&
      'darkred,8b0000,ffffff;' &&
      'darksalmon,e9967a,1f2d3d;' &&
      'darkseagreen,8fbc8f,1f2d3d;' &&
      'darkslateblue,483d8b,ffffff;' &&
      'darkslategray,2f4f4f,ffffff;' &&
      'darkslategrey,2f4f4f,ffffff;' &&
      'darkturquoise,00ced1,ffffff;' &&
      'darkviolet,9400d3,ffffff;' &&
      'deeppink,ff1493,ffffff;' &&
      'deepskyblue,00bfff,1f2d3d;' &&
      'dimgray,696969,ffffff;' &&
      'dimgrey,696969,ffffff;' &&
      'dodgerblue,1e90ff,ffffff;' &&
      'firebrick,b22222,ffffff;' &&
      'floralwhite,fffaf0,1f2d3d;' &&
      'forestgreen,228b22,ffffff;' &&
      'fuchsia,ff00ff,ffffff;' &&
      'gainsboro,dcdcdc,1f2d3d;' &&
      'ghostwhite,f8f8ff,1f2d3d;' &&
      'gold,ffd700,1f2d3d;' &&
      'goldenrod,daa520,1f2d3d;' &&
      'gray,808080,ffffff;' &&
      'green,008000,ffffff;' &&
      'greenyellow,adff2f,1f2d3d;' &&
      'grey,808080,ffffff;' &&
      'honeydew,f0fff0,1f2d3d;' &&
      'hotpink,ff69b4,1f2d3d;' &&
      'indianred,cd5c5c,ffffff;' &&
      'indigo,4b0082,ffffff;' &&
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
      'lightseagreen,20b2aa,ffffff;' &&
      'lightskyblue,87cefa,1f2d3d;' &&
      'lightslategray,778899,ffffff;' &&
      'lightslategrey,778899,ffffff;' &&
      'lightsteelblue,b0c4de,1f2d3d;' &&
      'lightyellow,ffffe0,1f2d3d;' &&
      'lime,00ff00,ffffff;' &&
      'limegreen,32cd32,ffffff;' &&
      'linen,faf0e6,1f2d3d;' &&
      'magenta,ff00ff,ffffff;' &&
      'maroon,800000,ffffff;' &&
      'mediumaquamarine,66cdaa,1f2d3d;' &&
      'mediumblue,0000cd,ffffff;' &&
      'mediumorchid,ba55d3,ffffff;' &&
      'mediumpurple,9370db,ffffff;' &&
      'mediumseagreen,3cb371,ffffff;' &&
      'mediumslateblue,7b68ee,ffffff;' &&
      'mediumspringgreen,00fa9a,1f2d3d;' &&
      'mediumturquoise,48d1cc,1f2d3d;' &&
      'mediumvioletred,c71585,ffffff;' &&
      'midnightblue,191970,ffffff;' &&
      'mintcream,f5fffa,1f2d3d;' &&
      'mistyrose,ffe4e1,1f2d3d;' &&
      'moccasin,ffe4b5,1f2d3d;' &&
      'navajowhite,ffdead,1f2d3d;' &&
      'navy,000080,ffffff;' &&
      'oldlace,fdf5e6,1f2d3d;' &&
      'olive,808000,ffffff;' &&
      'olivedrab,6b8e23,ffffff;' &&
      'orange,ffa500,1f2d3d;' &&
      'orangered,ff4500,ffffff;' &&
      'orchid,da70d6,1f2d3d;' &&
      'palegoldenrod,eee8aa,1f2d3d;' &&
      'palegreen,98fb98,1f2d3d;' &&
      'paleturquoise,afeeee,1f2d3d;' &&
      'palevioletred,db7093,ffffff;' &&
      'papayawhip,ffefd5,1f2d3d;' &&
      'peachpuff,ffdab9,1f2d3d;' &&
      'peru,cd853f,ffffff;' &&
      'pink,ffc0cb,1f2d3d;' &&
      'plum,dda0dd,1f2d3d;' &&
      'powderblue,b0e0e6,1f2d3d;' &&
      'purple,800080,ffffff;' &&
      'rebeccapurple,663399,ffffff;' &&
      'red,ff0000,ffffff;' &&
      'rosybrown,bc8f8f,1f2d3d;' &&
      'royalblue,4169e1,ffffff;' &&
      'saddlebrown,8b4513,ffffff;' &&
      'salmon,fa8072,1f2d3d;' &&
      'sandybrown,f4a460,1f2d3d;' &&
      'seagreen,2e8b57,ffffff;' &&
      'seashell,fff5ee,1f2d3d;' &&
      'sienna,a0522d,ffffff;' &&
      'silver,c0c0c0,1f2d3d;' &&
      'skyblue,87ceeb,1f2d3d;' &&
      'slateblue,6a5acd,ffffff;' &&
      'slategray,708090,ffffff;' &&
      'slategrey,708090,ffffff;' &&
      'snow,fffafa,1f2d3d;' &&
      'springgreen,00ff7f,1f2d3d;' &&
      'steelblue,4682b4,ffffff;' &&
      'tan,d2b48c,1f2d3d;' &&
      'teal,008080,ffffff;' &&
      'thistle,d8bfd8,1f2d3d;' &&
      'tomato,ff6347,ffffff;' &&
      'turquoise,40e0d0,1f2d3d;' &&
      'violet,ee82ee,1f2d3d;' &&
      'wheat,f5deb3,1f2d3d;' &&
      'white,ffffff,1f2d3d;' &&
      'whitesmoke,f5f5f5,1f2d3d;' &&
      'yellow,ffff00,1f2d3d;' &&
      'yellowgreen,9acd32,1f2d3d'.

    SPLIT lv_colors AT ';' INTO TABLE lt_colors.
    LOOP AT lt_colors INTO lv_colors.
      SPLIT lv_colors AT ',' INTO ls_color-name ls_color-background ls_color-foreground.
      INSERT ls_color INTO TABLE mt_colors.
    ENDLOOP.

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
