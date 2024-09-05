CLASS zcl_abapgit_label_designer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* abapGit Label Designer
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
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
      IMPORTING
        iv_dark_mode  TYPE abap_bool
      RETURNING
        VALUE(result) TYPE REF TO zcl_abapgit_label_designer
      RAISING
        zcx_abapgit_exception.

    METHODS render
      IMPORTING
        ii_event TYPE REF TO zif_abapgit_gui_event OPTIONAL
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
        VALUE(result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS free
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      c_modes TYPE i VALUE 3,
      " Dark and full modes aren't much different so we ignore them (to see them, change c_modes = 5, c_cols = 10 to see them)
      BEGIN OF c_color_mode,
        light TYPE string VALUE ' light',
        white TYPE string VALUE ' on white',
        black TYPE string VALUE ' on black',
        dark  TYPE string VALUE ' dark',
        full  TYPE string VALUE '',
      END OF c_color_mode.

    TYPES:
      ty_p TYPE p LENGTH 10 DECIMALS 4,
      BEGIN OF ty_group,
        id    TYPE n LENGTH 2,
        group TYPE string,
      END OF ty_group,
      ty_groups TYPE STANDARD TABLE OF ty_group WITH KEY id,
      BEGIN OF ty_group_color,
        id    TYPE n LENGTH 2,
        name  TYPE string,
        order TYPE n LENGTH 2,
      END OF ty_group_color,
      ty_group_colors TYPE STANDARD TABLE OF ty_group_color WITH KEY id,
      BEGIN OF ty_color,
        id     TYPE string,
        name   TYPE string,
        mode   TYPE string,
        fg     TYPE string,
        bg     TYPE string,
        border TYPE string,
      END OF ty_color,
      ty_colors TYPE STANDARD TABLE OF ty_color WITH KEY id,
      BEGIN OF ty_map_color,
        rl_style TYPE string,
        name     TYPE string,
        mode     TYPE string,
      END OF ty_map_color,
      ty_map_colors TYPE STANDARD TABLE OF ty_map_color WITH KEY rl_style,
      BEGIN OF ty_label,
        id    TYPE string,
        text  TYPE string,
        color TYPE ty_color,
      END OF ty_label,
      ty_labels TYPE SORTED TABLE OF ty_label WITH UNIQUE KEY id,
      BEGIN OF ty_gh_label,
        r      TYPE i,
        g      TYPE i,
        b      TYPE i,
        h      TYPE ty_p,
        s      TYPE ty_p,
        l      TYPE ty_p,
        fg     TYPE string,
        bg     TYPE string,
        border TYPE string,
      END OF ty_gh_label.

    DATA:
      mx_error        TYPE REF TO zcx_abapgit_exception,
      mo_settings     TYPE REF TO zcl_abapgit_settings,
      ms_settings     TYPE zif_abapgit_definitions=>ty_s_user_settings,
      mo_colors       TYPE REF TO zcl_abapgit_string_map,
      mt_groups       TYPE ty_groups,
      mt_group_colors TYPE ty_group_colors,
      mt_colors       TYPE ty_colors,
      mt_map_colors   TYPE ty_map_colors,
      mv_dark_mode    TYPE abap_bool,
      mt_labels       TYPE ty_labels,
      mi_viewer       TYPE REF TO zif_abapgit_html_viewer,
      mv_cols         TYPE i,
      mv_page         TYPE i.

    METHODS render_page_1
      IMPORTING
        ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception.

    METHODS render_page_2
      IMPORTING
        ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception.

    METHODS _get_styles
      RETURNING
        VALUE(result) TYPE string.

    METHODS _get_header
      RETURNING
        VALUE(result) TYPE string.

    METHODS _get_mode_class
      RETURNING
        VALUE(result) TYPE string.

    METHODS _get_groups.
    METHODS _get_group_colors.
    METHODS _get_colors.
    METHODS _get_map_colors.

    METHODS _load_settings.
    METHODS _save_settings
      IMPORTING
        iv_label_colors TYPE string.

    METHODS _hsl
      IMPORTING
        h             TYPE ty_p
        s             TYPE ty_p
        l             TYPE ty_p
      RETURNING
        VALUE(result) TYPE string.

    METHODS _hsla
      IMPORTING
        h             TYPE ty_p
        s             TYPE ty_p
        l             TYPE ty_p
        a             TYPE ty_p
        dark          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

    METHODS _rgb
      IMPORTING
        r             TYPE i
        g             TYPE i
        b             TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS _rgba
      IMPORTING
        r             TYPE i
        g             TYPE i
        b             TYPE i
        a             TYPE ty_p
        dark          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE string.

    METHODS _hue_to_rgb
      IMPORTING
        t1            TYPE ty_p
        t2            TYPE ty_p
        VALUE(hue)    TYPE ty_p
      RETURNING
        VALUE(result) TYPE ty_p.

    METHODS _hsl_to_rgb
      CHANGING
        label TYPE ty_gh_label.

    METHODS _rgb_to_hsl
      CHANGING
        label TYPE ty_gh_label.

    METHODS _gh_label
      IMPORTING
        col           TYPE string
      RETURNING
        VALUE(result) TYPE ty_gh_label.

    METHODS _gh_dark_mode
      IMPORTING
        col           TYPE string
        dark          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE ty_gh_label.

    METHODS _gh_light_mode
      IMPORTING
        col           TYPE string
        dark          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE ty_gh_label.

    METHODS _get_screen_size
      RETURNING
        VALUE(result) TYPE i.

    METHODS _get_buttons_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS _get_buttons_2
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_label_designer IMPLEMENTATION.


  METHOD back.
    mv_page = mv_page - 1.
    IF mv_page <= 0.
      result = abap_true.
    ENDIF.
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


  METHOD free.

    SET HANDLER on_event FOR mi_viewer ACTIVATION space.
    mi_viewer->close_document( ).
    mi_viewer->free( ).
    FREE mi_viewer.

  ENDMETHOD.


  METHOD handle_action.

    DATA li_event TYPE REF TO zif_abapgit_gui_event.

    CREATE OBJECT li_event TYPE zcl_abapgit_gui_event
      EXPORTING
        iv_action   = iv_action
        iv_getdata  = iv_getdata
        it_postdata = it_postdata.

    TRY.
        CASE iv_action.
          WHEN 'main'.
            mv_page = 1.
            render( li_event ).
          WHEN 'preview'.
            mv_page = 2.
            render( li_event ).
          WHEN 'toggle'.
            mv_dark_mode = boolc( mv_dark_mode = abap_false ).
            render( li_event ).
          WHEN 'save'.
            save( li_event ).
            mv_page = 1.
            render( li_event ).
          WHEN 'back'.
            IF back( ) IS INITIAL.
              render( li_event ).
            ENDIF.
          WHEN OTHERS.
            BREAK-POINT.
        ENDCASE.
      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD.


  METHOD on_event.

    handle_action(
      iv_action   = action
      iv_getdata  = getdata
      it_postdata = postdata ).

  ENDMETHOD.


  METHOD render.

    _get_screen_size( ).

    CASE mv_page.
      WHEN 1.
        render_page_1( ii_event ).
      WHEN 2.
        render_page_2( ii_event ).
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.


  METHOD render_page_1.

    DATA:
      lv_url         TYPE string,
      lv_html        TYPE string,
      lv_tabix       TYPE sy-tabix,
      ls_group       TYPE ty_group,
      ls_group_color TYPE ty_group_color,
      ls_color       TYPE ty_color,
      ls_label       TYPE ty_label.

    lv_html = _get_header( ) &&
      |<form method="post" id="form" action="sapevent:preview" class="form { _get_mode_class( ) }">\n| &&
      |<p class="pad">Enter a label text into the labels you want to use in abapGit and select "Preview" at the bottom of the page.</p>\n|.

    lv_html = lv_html && _get_buttons_1( ).

    " All color groups except "White" since these colors are simply too light
    LOOP AT mt_groups INTO ls_group WHERE group <> 'white'.

      lv_html = lv_html &&
        |<h2 class="pad">{ to_upper( ls_group-group(1) ) }{ ls_group-group+1 } Colors</h2>| &&
        |<table>\n|.

      lv_tabix = 1.

      LOOP AT mt_group_colors INTO ls_group_color WHERE id = ls_group-id.
        LOOP AT mt_colors INTO ls_color WHERE name = ls_group_color-name.
          IF lv_tabix MOD mv_cols = 1.
            lv_html = lv_html && |<tr>\n|.
          ENDIF.

          READ TABLE mt_labels INTO ls_label WITH TABLE KEY id = ls_color-id.
          IF sy-subrc <> 0.
            CLEAR ls_label.
          ENDIF.

          lv_html = lv_html &&
            |<td style="padding:10px;">\n| &&
            |<div style="| &&
            |font-size:12px;| &&
            |width:140px;height:40px;| &&
            |border-radius:6px;border:solid 1px;| &&
            |padding:3px;text-align:center;| &&
            |color:#{ ls_color-fg };| &&
            |background-color:#{ ls_color-bg };| &&
            |border-color:{ ls_color-border };" | &&
            |title="#{ ls_color-fg }/{ ls_color-bg }/{ ls_color-border }">| &&
            |{ ls_color-name }<br>\n| &&
            |<input type="text" value="{ ls_label-text }" name="{ ls_color-id }" class="input">\n| &&
            |</div>\n| &&
            |</td>\n|.

          IF lv_tabix MOD c_modes = 0.
            lv_html = lv_html &&
              |<td style="padding-left:20px;padding-right:20px;">\n| &&
              |</td>\n|.
          ENDIF.

          IF lv_tabix MOD mv_cols = 0.
            lv_html = lv_html && |</tr>\n|.
          ENDIF.
          lv_tabix = lv_tabix + 1.
        ENDLOOP.
      ENDLOOP.

      lv_html = lv_html && |</table>\n|.
    ENDLOOP.

    lv_html = lv_html && _get_buttons_1( ) &&
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

    lv_html = _get_header( ) &&
      |<form method="post" id="form" action="sapevent:save" class="form { _get_mode_class( ) }">\n| &&
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

    lv_html = lv_html && _get_buttons_2( ) &&
      |</form>\n| &&
      |</body></html>\n|.

    lv_url = cache_html( lv_html ).

    show_url( lv_url ).

  ENDMETHOD.


  METHOD save.

    DATA:
      ls_label  TYPE ty_label,
      lt_labels TYPE STANDARD TABLE OF ty_label WITH DEFAULT KEY,
      lv_color  TYPE string,
      lv_labels TYPE string.

    lt_labels = mt_labels.
    SORT lt_labels BY text.

    LOOP AT lt_labels INTO ls_label.
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


  METHOD show_url.
    mi_viewer->show_url( iv_url ).
  ENDMETHOD.


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

    mv_page      = 1.
    mv_cols      = _get_screen_size( ).
    mv_dark_mode = iv_dark_mode.

    _get_styles( ).
    _get_header( ).
    _get_groups( ).
    _get_group_colors( ).
    _get_colors( ).
    _get_map_colors( ).

    _load_settings( ).

    result = me.

  ENDMETHOD.


  METHOD _get_buttons_1.

    result =
      |<div class="pad">| &&
      |<input type="submit" value="Preview Selected Labels">\n| &&
      |<input type="submit" value="Toggle Dark/Light" formaction="sapevent:toggle">\n| &&
      |</div>|.

  ENDMETHOD.


  METHOD _get_buttons_2.

    result =
      |<div class="pad">| &&
      |<input type="submit" value="Save Label Designs to Personal Settings" formaction="sapevent:save">&nbsp;&nbsp;| &&
      |<input type="submit" value="Back" formaction="sapevent:back">\n| &&
      |</div>|.

  ENDMETHOD.


  METHOD _get_colors.

    DATA:
      lv_colors   TYPE string,
      lt_colors   TYPE string_table,
      ls_color    TYPE ty_color,
      ls_gh_label TYPE ty_gh_label.

    CLEAR mt_colors.

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
      'darkturquoise,00ced1,fff;' &&
      'darkviolet,9400d3,fff;' &&
      'deeppink,ff1493,fff;' &&
      'deepskyblue,00bfff,1f2d3d;' &&
      'dimgray,696969,fff;' &&
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
      'lightpink,ffb6c1,1f2d3d;' &&
      'lightsalmon,ffa07a,1f2d3d;' &&
      'lightseagreen,20b2aa,fff;' &&
      'lightskyblue,87cefa,1f2d3d;' &&
      'lightslategray,778899,fff;' &&
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
      'white,ffffff,1f2d3d;' &&
      'whitesmoke,f5f5f5,1f2d3d;' &&
      'yellow,ffff00,1f2d3d;' &&
      'yellowgreen,9acd32,1f2d3d'.

    SPLIT lv_colors AT ';' INTO TABLE lt_colors.
    LOOP AT lt_colors INTO lv_colors.
      DO c_modes TIMES.
        CLEAR ls_color.
        SPLIT lv_colors AT ',' INTO ls_color-name ls_color-bg ls_color-fg.

        READ TABLE mt_group_colors TRANSPORTING NO FIELDS
          WITH KEY name = ls_color-name.
        ASSERT sy-subrc = 0.

        CASE sy-index.
          WHEN 1.
            ls_color-id     = |light-{ sy-tabix }|.
            ls_color-mode   = c_color_mode-light.
            ls_gh_label     = _gh_light_mode( col = ls_color-bg dark = mv_dark_mode ).
            ls_color-fg     = ls_gh_label-fg.
            ls_color-bg     = ls_gh_label-bg.
            ls_color-border = ls_gh_label-border.
          WHEN 2.
            ls_color-id     = |wht-{ sy-tabix }|.
            ls_color-mode   = c_color_mode-white.
            ls_color-fg     = ls_color-bg.
            ls_color-bg     = 'fff'.
            ls_color-border = ls_color-fg.
          WHEN 3.
            ls_color-id     = |blk-{ sy-tabix }|.
            ls_color-mode   = c_color_mode-black.
            ls_color-fg     = ls_color-bg.
            ls_color-bg     = '000'.
            ls_color-border = ls_color-fg.
          WHEN 4.
            ls_color-id     = |dark-{ sy-tabix }|.
            ls_color-mode   = c_color_mode-dark.
            ls_gh_label     = _gh_dark_mode( col = ls_color-bg dark = mv_dark_mode ).
            ls_color-fg     = ls_gh_label-fg.
            ls_color-bg     = ls_gh_label-bg.
            ls_color-border = ls_gh_label-border.
          WHEN 5.
            ls_color-id     = |col-{ sy-tabix }|.
            ls_color-mode   = c_color_mode-full.
            ls_color-border = ls_color-bg.
        ENDCASE.
        INSERT ls_color INTO TABLE mt_colors.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_groups.

    DATA:
      lv_groups TYPE string,
      lt_groups TYPE string_table,
      ls_group  TYPE ty_group.

    CLEAR mt_groups.

    lv_groups =
      '01,red;' &&
      '02,orange;' &&
      '03,brown;' &&
      '04,yellow;' &&
      '05,green;' &&
      '06,cyan;' &&
      '07,blue;' &&
      '08,pink;' &&
      '09,purple;' &&
      '10,gray;' &&
      '11,white'.

    SPLIT lv_groups AT ';' INTO TABLE lt_groups.
    LOOP AT lt_groups INTO lv_groups.
      CLEAR ls_group.
      SPLIT lv_groups AT ',' INTO ls_group-id ls_group-group.
      INSERT ls_group INTO TABLE mt_groups.
    ENDLOOP.


  ENDMETHOD.


  METHOD _get_group_colors.

    DATA:
      lv_group_colors TYPE string,
      lt_group_colors TYPE string_table,
      ls_group_color  TYPE ty_group_color.

    CLEAR mt_group_colors.

    lv_group_colors =

      '01,lightsalmon,01;' &&
      '01,salmon,02;' &&
      '01,darksalmon,03;' &&
      '01,lightcoral,04;' &&
      '01,indianred,05;' &&
      '01,crimson,06;' &&
      '01,red,07;' &&
      '01,firebrick,08;' &&
      '01,darkred,09;' &&
      '02,orange,01;' &&
      '02,darkorange,02;' &&
      '02,coral,03;' &&
      '02,tomato,04;' &&
      '02,orangered,05;' &&
      '03,cornsilk,01;' &&
      '03,blanchedalmond,02;' &&
      '03,bisque,03;' &&
      '03,navajowhite,04;' &&
      '03,wheat,05;' &&
      '03,burlywood,06;' &&
      '03,tan,07;' &&
      '03,rosybrown,08;' &&
      '03,sandybrown,09;' &&
      '03,goldenrod,10;' &&
      '03,darkgoldenrod,11;' &&
      '03,peru,12;' &&
      '03,chocolate,13;' &&
      '03,olive,14;' &&
      '03,saddlebrown,15;' &&
      '03,sienna,16;' &&
      '03,brown,17;' &&
      '03,maroon,18;' &&
      '04,gold,01;' &&
      '04,yellow,02;' &&
      '04,lightyellow,03;' &&
      '04,lemonchiffon,04;' &&
      '04,lightgoldenrodyellow,05;' &&
      '04,papayawhip,06;' &&
      '04,moccasin,07;' &&
      '04,peachpuff,08;' &&
      '04,palegoldenrod,09;' &&
      '04,khaki,10;' &&
      '04,darkkhaki,11;' &&
      '05,greenyellow,01;' &&
      '05,chartreuse,02;' &&
      '05,lawngreen,03;' &&
      '05,lime,04;' &&
      '05,limegreen,05;' &&
      '05,palegreen,06;' &&
      '05,lightgreen,07;' &&
      '05,mediumspringgreen,08;' &&
      '05,springgreen,09;' &&
      '05,mediumseagreen,10;' &&
      '05,seagreen,11;' &&
      '05,forestgreen,12;' &&
      '05,green,13;' &&
      '05,darkgreen,14;' &&
      '05,yellowgreen,15;' &&
      '05,olivedrab,16;' &&
      '05,darkolivegreen,17;' &&
      '05,mediumaquamarine,18;' &&
      '05,darkseagreen,19;' &&
      '05,lightseagreen,20;' &&
      '05,darkcyan,21;' &&
      '05,teal,22;' &&
      '06,aqua,01;' &&
      '06,cyan,02;' &&
      '06,lightcyan,03;' &&
      '06,paleturquoise,04;' &&
      '06,aquamarine,05;' &&
      '06,turquoise,06;' &&
      '06,mediumturquoise,07;' &&
      '06,darkturquoise,08;' &&
      '07,cadetblue,01;' &&
      '07,steelblue,02;' &&
      '07,lightsteelblue,03;' &&
      '07,lightblue,04;' &&
      '07,powderblue,05;' &&
      '07,lightskyblue,06;' &&
      '07,skyblue,07;' &&
      '07,cornflowerblue,08;' &&
      '07,deepskyblue,09;' &&
      '07,dodgerblue,10;' &&
      '07,royalblue,11;' &&
      '07,blue,12;' &&
      '07,mediumblue,13;' &&
      '07,darkblue,14;' &&
      '07,navy,15;' &&
      '07,midnightblue,16;' &&
      '08,pink,01;' &&
      '08,lightpink,02;' &&
      '08,hotpink,03;' &&
      '08,deeppink,04;' &&
      '08,palevioletred,05;' &&
      '08,mediumvioletred,06;' &&
      '09,lavender,01;' &&
      '09,thistle,02;' &&
      '09,plum,03;' &&
      '09,orchid,04;' &&
      '09,violet,05;' &&
      '09,fuchsia,06;' &&
      '09,magenta,07;' &&
      '09,mediumorchid,08;' &&
      '09,darkorchid,09;' &&
      '09,darkviolet,10;' &&
      '09,blueviolet,11;' &&
      '09,darkmagenta,12;' &&
      '09,purple,13;' &&
      '09,mediumpurple,14;' &&
      '09,mediumslateblue,15;' &&
      '09,slateblue,16;' &&
      '09,darkslateblue,17;' &&
      '09,rebeccapurple,18;' &&
      '09,indigo,19;' &&
      '10,gainsboro,01;' &&
      '10,lightgray,02;' &&
      '10,silver,03;' &&
      '10,darkgray,04;' &&
      '10,dimgray,05;' &&
      '10,gray,06;' &&
      '10,lightslategray,07;' &&
      '10,slategray,08;' &&
      '10,darkslategray,09;' &&
      '10,black,10;' &&
      '11,white,01;' &&
      '11,snow,02;' &&
      '11,honeydew,03;' &&
      '11,mintcream,04;' &&
      '11,azure,05;' &&
      '11,aliceblue,06;' &&
      '11,ghostwhite,07;' &&
      '11,whitesmoke,08;' &&
      '11,seashell,09;' &&
      '11,beige,10;' &&
      '11,oldlace,11;' &&
      '11,floralwhite,12;' &&
      '11,ivory,13;' &&
      '11,antiquewhite,14;' &&
      '11,linen,15;' &&
      '11,lavenderblush,16;' &&
      '11,mistyrose,17'.

    SPLIT lv_group_colors AT ';' INTO TABLE lt_group_colors.
    LOOP AT lt_group_colors INTO lv_group_colors.
      CLEAR ls_group_color.
      SPLIT lv_group_colors AT ',' INTO ls_group_color-id ls_group_color-name ls_group_color-order.
      INSERT ls_group_color INTO TABLE mt_group_colors.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_header.

    result =
      |<html><head>\n| &&
      |<style>{ _get_styles( ) }</style>| &&
      |</head>\n| &&
      |<body class="main">\n| &&
      |<h1>| &&
      |<img src="https://abapgit.org/img/logo.svg" width="110px" style="vertical-align:middle;">| &&
      |<span style="vertical-align:middle;padding:10px;">Label Designer</span>| &&
      |</h1>\n|.

  ENDMETHOD.


  METHOD _get_map_colors.

    DATA:
      lv_map_colors TYPE string,
      lt_map_colors TYPE string_table,
      ls_map_color  TYPE ty_map_color.

    CLEAR mt_map_colors.

    lv_map_colors =
      'white,dimgray,' && c_color_mode-white && ';' &&
      'white-b,dodgerblue,' && c_color_mode-white && ';' &&
      'white-r,crimson,' && c_color_mode-white && ';' &&
      'grey,gray,' && c_color_mode-light && ';' &&
      'dark-w,white,' && c_color_mode-black && ';' &&
      'dark-y,gold,' && c_color_mode-black && ';' &&
      'dark-r,crimson,' && c_color_mode-black && ';' &&
      'dark-b,dodgerblue,' && c_color_mode-black && ';' &&
      'lightblue,lightblue,' && c_color_mode-light && ';' &&
      'darkblue,darkblue,' && c_color_mode-light && ';' &&
      'lightgreen,lightgreen,' && c_color_mode-light && ';' &&
      'darkgreen,darkgreen,' && c_color_mode-light && ';' &&
      'lightred,lightsalmon,' && c_color_mode-light && ';' &&
      'darkred,darkred,' && c_color_mode-light && ';' &&
      'yellow,yellow,' && c_color_mode-light && ';' &&
      'darkyellow,gold,' && c_color_mode-light && ';' &&
      'orange,orange,' && c_color_mode-light && ';' &&
      'brown,brown,' && c_color_mode-light && ';' &&
      'pink,pink,' && c_color_mode-light && ';' &&
      'teal,lightseagreen,' && c_color_mode-light && ';' &&
      'darkviolet,mediumslateblue,' && c_color_mode-light.

    SPLIT lv_map_colors AT ';' INTO TABLE lt_map_colors.
    LOOP AT lt_map_colors INTO lv_map_colors.
      CLEAR ls_map_color.
      SPLIT lv_map_colors AT ',' INTO ls_map_color-rl_style ls_map_color-name ls_map_color-mode.
      READ TABLE mt_colors TRANSPORTING NO FIELDS WITH KEY name = ls_map_color-name.
      ASSERT sy-subrc = 0.
      INSERT ls_map_color INTO TABLE mt_map_colors.
    ENDLOOP.

  ENDMETHOD.


  METHOD _get_mode_class.
    IF mv_dark_mode = abap_true.
      result = 'dark'.
    ELSE.
      result = 'light'.
    ENDIF.
  ENDMETHOD.


  METHOD _get_screen_size.

    DATA screen_width TYPE i.

    screen_width = cl_gui_props_consumer=>create_consumer( )->get_metric_factors( )-screen-x.

    IF screen_width >= 1920.
      result = 3.
    ELSEIF screen_width >= 1024.
      result = 2.
    ELSE.
      result = 1.
    ENDIF.

    result = result * c_modes.

  ENDMETHOD.


  METHOD _get_styles.

    result =
      '.main {' &&
      '  font-family:Arial;' &&
      '  font-size:16px;' &&
      '  background-color:#e8e8e8;' &&
      '}' &&
      '.light {' &&
      '  color:#000000;' &&
      '  background-color:#f2f2f2;' &&
      '}' &&
      '.dark {' &&
      '  color:#ffffff;' &&
      '  background-color:#333333;' &&
      '}' &&
      'h1 {' &&
      '  font-size:28px;' &&
      '  color:gray;' &&
      '}' &&
      'h2 {' &&
      '  font-size:14px;' &&
      '  color:gray;' &&
      '}' &&
      '.pad {' &&
      '  padding:10px 10px 0px;' &&
      '}' &&
      '.form {' &&
      '  padding-bottom:20px;' &&
      '}' &&
      '.input {' &&
      '  border:1px solid;' &&
      '  border-color:lightgray;' &&
      '  color:red;' &&
      '  width:120px;' &&
      '  text-align: center;' &&
      '  font-weight:bold;' &&
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
      '  border-radius: 8px;' &&
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


  METHOD _gh_dark_mode.

    DATA:
      label               TYPE ty_gh_label,
      perceived_lightness TYPE ty_p,
      lightness_switch    TYPE ty_p,
      lightness_threshold TYPE ty_p,
      background_alpha    TYPE ty_p,
      border_threshold    TYPE ty_p,
      border_alpha        TYPE ty_p,
      lighten_by          TYPE ty_p.

    label = _gh_label( col ).
    _rgb_to_hsl( CHANGING label = label ).

    lightness_threshold = '0.8'. " 0.6
    background_alpha    = '0.8'." 0.18
    border_threshold    = '0.96'.
    border_alpha        = '0.8'.
    perceived_lightness = ( ( label-r * '0.2126' ) + ( label-g * '0.7152' ) + ( label-b * '0.0722' ) ) / 255.
    lightness_switch    = nmax( val1 = 0 val2 = nmin( val1 = ( ( 1 / ( lightness_threshold - perceived_lightness ) ) ) val2 = 1 ) ).
    lighten_by          = ( lightness_threshold - perceived_lightness ) * lightness_switch.

    label-fg     = _hsl( h = label-h s = label-s l = label-l + lighten_by ).
    label-bg     = _rgba( r = label-r g = label-g b = label-b a = background_alpha dark = dark ).
    label-border = _hsla( h = label-h s = label-s l = label-l + lighten_by a = border_alpha dark = dark ).

    result = label.

  ENDMETHOD.


  METHOD _gh_label.

    DATA:
      x TYPE x LENGTH 1,
      y TYPE x LENGTH 1,
      z TYPE x LENGTH 1.

    ASSERT strlen( col ) = 6.

    x = to_upper( col(2) ).
    y = to_upper( col+2(2) ).
    z = to_upper( col+4(2) ).

    result-r = x.
    result-g = y.
    result-b = z.

  ENDMETHOD.


  METHOD _gh_light_mode.

    DATA:
      label               TYPE ty_gh_label,
      perceived_lightness TYPE ty_p,
      lightness_switch    TYPE ty_p,
      lightness_threshold TYPE ty_p,
      border_threshold    TYPE ty_p,
      border_alpha        TYPE ty_p,
      darken_by           TYPE ty_p.

    label = _gh_label( col ).
    _rgb_to_hsl( CHANGING label = label ).

    lightness_threshold = '0.453'.
    border_threshold    = '0.96'.
    perceived_lightness = ( ( label-r * '0.2126' ) + ( label-g * '0.7152' ) + ( label-b * '0.0722' ) ) / 255.
    lightness_switch    = nmax( val1 = 0 val2 = nmin( val1 = ( ( 1 / ( lightness_threshold - perceived_lightness ) ) ) val2 = 1 ) ).
    border_alpha        = nmax( val1 = 0 val2 = nmin( val1 = ( ( perceived_lightness - border_threshold ) ) val2 = 1 ) ).
    darken_by           = '0.25'.

    label-fg     = _hsl( h = 0 s = 0 l = lightness_switch ).
    label-bg     = _rgb( r = label-r g = label-g b = label-b ).
    label-border = _hsla( h = label-h s = label-s l = label-l - darken_by a = border_alpha dark = dark ).

    result = label.

  ENDMETHOD.


  METHOD _hsl.
    result = _hsla( h = h s = s l = l a = 1 ).
  ENDMETHOD.


  METHOD _hsla.

    DATA label TYPE ty_gh_label.

    label-h = h.
    label-s = s.
    label-l = l.
    _hsl_to_rgb( CHANGING label = label ).

    result = _rgba( r = label-r g = label-g b = label-b a = a dark = dark ).

  ENDMETHOD.


  METHOD _hsl_to_rgb.

    DATA:
      hue TYPE ty_p,
      t1  TYPE ty_p,
      t2  TYPE ty_p.

    hue = label-h / 60.
    IF label-l <= '0.5'.
      t2 = label-l * ( label-s + 1 ).
    ELSE.
      t2 = label-l + label-s - ( label-l * label-s ).
    ENDIF.
    t1 = label-l * 2 - t2.
    label-r = _hue_to_rgb( t1 = t1 t2 = t2 hue = hue + 2 ) * 255.
    label-g = _hue_to_rgb( t1 = t1 t2 = t2 hue = hue ) * 255.
    label-b = _hue_to_rgb( t1 = t1 t2 = t2 hue = hue - 2 ) * 255.

  ENDMETHOD.


  METHOD _hue_to_rgb.
    IF hue < 0.
      hue = hue + 6.
    ENDIF.
    IF hue >= 6.
      hue = hue - 6.
    ENDIF.
    IF hue < 1.
      result = ( t2 - t1 ) * hue + t1.
    ELSEIF hue < 3.
      result = t2.
    ELSEIF hue < 4.
      result = ( t2 - t1 ) * ( 4 - hue ) + t1.
    ELSE.
      result = t1.
    ENDIF.
  ENDMETHOD.


  METHOD _load_settings.

    DATA:
      ls_entry     TYPE zcl_abapgit_string_map=>ty_entry,
      ls_color     TYPE ty_color,
      ls_map_color TYPE ty_map_color,
      ls_label     TYPE ty_label.

    CLEAR mt_labels.

    TRY.
        mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
        ms_settings = mo_settings->get_user_settings( ).
        mo_colors   = zcl_abapgit_repo_labels=>split_colors_into_map( ms_settings-label_colors ).

        LOOP AT mo_colors->mt_entries INTO ls_entry.
          CLEAR ls_label.
          ls_label-text = ls_entry-k.
          IF ls_entry-v(1) = '#'.
            SPLIT ls_entry-v+1 AT '/' INTO ls_label-color-fg ls_label-color-bg ls_label-color-border.
            READ TABLE mt_colors INTO ls_color WITH KEY
              fg     = ls_label-color-fg
              bg     = ls_label-color-bg
              border = ls_label-color-border.
            IF sy-subrc = 0.
              ls_label-id = ls_color-id.
              INSERT ls_label INTO TABLE mt_labels.
            ELSE.
              " unknown color
            ENDIF.
          ELSE.
            READ TABLE mt_map_colors INTO ls_map_color WITH KEY rl_style = ls_entry-v.
            IF sy-subrc = 0.
              READ TABLE mt_colors INTO ls_color WITH KEY
                name = ls_map_color-name
                mode = ls_map_color-mode.
              ASSERT sy-subrc = 0.
              ls_label-id = ls_color-id.
              ls_label-color = ls_color.
              INSERT ls_label INTO TABLE mt_labels.
            ELSE.
              " unknown color
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH zcx_abapgit_exception INTO mx_error.
        MESSAGE mx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD _rgb.
    result = _rgba( r = r g = g b = b a = 1 ).
  ENDMETHOD.


  METHOD _rgba.

    " Background light mode #e8e8e8, dark mode #333333
    CONSTANTS:
      BEGIN OF c_bg,
        r TYPE i VALUE 232,
        g TYPE i VALUE 232,
        b TYPE i VALUE 232,
      END OF c_bg,
      BEGIN OF c_bg_dark,
        r TYPE i VALUE 51,
        g TYPE i VALUE 51,
        b TYPE i VALUE 51,
      END OF c_bg_dark.

    DATA:
      x TYPE x LENGTH 1,
      y TYPE x LENGTH 1,
      z TYPE x LENGTH 1.

    IF dark IS INITIAL.
      x =  a * r + ( 1 - a ) * c_bg-r.
      y =  a * g + ( 1 - a ) * c_bg-g.
      z =  a * b + ( 1 - a ) * c_bg-b.
    ELSE.
      x =  a * r + ( 1 - a ) * c_bg_dark-r.
      y =  a * g + ( 1 - a ) * c_bg_dark-g.
      z =  a * b + ( 1 - a ) * c_bg_dark-b.
    ENDIF.

    result = to_lower( |{ x }{ y }{ z }| ).

  ENDMETHOD.


  METHOD _rgb_to_hsl.

    DATA:
      min      TYPE ty_p,
      max      TYPE ty_p,
      maxcolor TYPE i,
      r0       TYPE ty_p,
      r1       TYPE ty_p,
      r2       TYPE ty_p.

    r0 = label-r / 255.
    r1 = label-g / 255.
    r2 = label-b / 255.
    min = nmin( val1 = r0 val2 = r1 val3 = r2 ).
    max = nmax( val1 = r0 val2 = r1 val3 = r2 ).
    maxcolor = 0.
    IF r1 >= r0.
      maxcolor = 1.
    ENDIF.
    IF r2 >= r0 AND r2 >= r1.
      maxcolor = 2.
    ENDIF.
    label-h = 0.
    IF max <> min.
      CASE maxcolor.
        WHEN 0.
          label-h = ( r1 - r2 ) / ( max - min ).
        WHEN 1.
          label-h = 2 + ( r2 - r0 ) / ( max - min ).
        WHEN 2.
          label-h = 4 + ( r0 - r1 ) / ( max - min ).
      ENDCASE.
    ENDIF.
    label-h = label-h * 60.
    IF label-h < 0.
      label-h = label-h + 360.
    ENDIF.
    label-l = ( min + max ) / 2.
    IF min = max.
      label-s = 0.
    ELSEIF label-l < '0.5'.
      label-s = ( max - min ) / ( max + min ).
    ELSE.
      label-s = ( max - min ) / ( 2 - max - min ).
    ENDIF.

    label-h = round( val = label-h dec = 0 ).
    label-s = round( val = label-s dec = 2 ).
    label-l = round( val = label-l dec = 2 ).

  ENDMETHOD.


  METHOD _save_settings.

    TRY.
        ms_settings-label_colors = iv_label_colors.
        zcl_abapgit_persistence_user=>get_instance( )->set_settings( ms_settings ).
      CATCH zcx_abapgit_exception INTO mx_error.
        MESSAGE mx_error TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
