CLASS ltcl_tests DEFINITION DEFERRED.
CLASS zcl_abapgit_label_designer DEFINITION LOCAL FRIENDS ltcl_tests.

CLASS ltcl_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    TYPES ty_gh_label TYPE zcl_abapgit_label_designer=>ty_gh_label.

    DATA mo_cut TYPE REF TO zcl_abapgit_label_designer.

    METHODS:
      setup,
      _hsl FOR TESTING,
      _hsla FOR TESTING,
      _rgb FOR TESTING,
      _rgba FOR TESTING,
      _hsl_to_rgb FOR TESTING,
      _rgb_to_hsl FOR TESTING,
      _gh_label FOR TESTING,
      _gh_dark_mode FOR TESTING,
      _gh_light_mode FOR TESTING.

ENDCLASS.

CLASS ltcl_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD _hsl.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsl( h = 75 s = 1 l = '0.7' )
      exp = 'd9ff66' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsl( h = 0 s = 1 l = '0.9' )
      exp = 'ffcccc' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsl( h = 180 s = 1 l = '0.2' )
      exp = '006666' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsl( h = 300 s = '0.35' l = '0.85' )
      exp = 'e6cbe6' ).

  ENDMETHOD.

  METHOD _hsla.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsla( h = 180 s = 1 l = '0.2' a = '0.4' )
      exp = '8bb4b4' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsla( h = 300 s = '0.35' l = '0.85' a = '0.9' )
      exp = 'e6cee6' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsla( h = 180 s = 1 l = '0.2' a = '0.4' dark = abap_true )
      exp = '1f4747' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsla( h = 300 s = '0.35' l = '0.85' a = '0.9' dark = abap_true )
      exp = 'd4bcd4' ).

  ENDMETHOD.

  METHOD _rgb.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgb( r = 232 g = 232 b = 232 )
      exp = 'e8e8e8' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgb( r = 255 g = 26 b = 198 )
      exp = 'ff1ac6' ).

  ENDMETHOD.

  METHOD _rgba.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgba( r = 255 g = 0 b = 0 a = '0.3' )
      exp = 'efa2a2' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgba( r = 192 g = 24 b = 214 a = '0.5' )
      exp = 'd480df' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgba( r = 255 g = 0 b = 0 a = '0.3' dark = abap_true )
      exp = '702424' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgba( r = 192 g = 24 b = 214 a = '0.5' dark = abap_true )
      exp = '7a2685' ).

  ENDMETHOD.

  METHOD _hsl_to_rgb.

    DATA act TYPE ty_gh_label.
    DATA exp TYPE ty_gh_label.

    act-h = 195.
    act-s = 1.
    act-l = '0.5'.

    mo_cut->_hsl_to_rgb( CHANGING label = act ).

    exp-h = 195.
    exp-s = 1.
    exp-l = '0.5'.
    exp-r = 0.
    exp-g = 191.
    exp-b = 255.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act-h = 40.
    act-s = 1.
    act-l = '0.7'.

    mo_cut->_hsl_to_rgb( CHANGING label = act ).

    exp-h = 40.
    exp-s = 1.
    exp-l = '0.7'.
    exp-r = 255.
    exp-g = 204.
    exp-b = 102.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act-h = 180.
    act-s = 1.
    act-l = '0.35'.

    mo_cut->_hsl_to_rgb( CHANGING label = act ).

    exp-h = 180.
    exp-s = 1.
    exp-l = '0.35'.
    exp-r = 0.
    exp-g = 179.
    exp-b = 179.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

  ENDMETHOD.

  METHOD _rgb_to_hsl.

    DATA act TYPE ty_gh_label.
    DATA exp TYPE ty_gh_label.

    act-r = 0.
    act-g = 191.
    act-b = 255.

    mo_cut->_rgb_to_hsl( CHANGING label = act ).

    exp-r = 0.
    exp-g = 191.
    exp-b = 255.
    exp-h = 195.
    exp-s = 1.
    exp-l = '0.5'.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act-r = 255.
    act-g = 204.
    act-b = 102.

    mo_cut->_rgb_to_hsl( CHANGING label = act ).

    exp-r = 255.
    exp-g = 204.
    exp-b = 102.
    exp-h = 40.
    exp-s = 1.
    exp-l = '0.7'.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act-r = 0.
    act-g = 179.
    act-b = 179.

    mo_cut->_rgb_to_hsl( CHANGING label = act ).

    exp-r = 0.
    exp-g = 179.
    exp-b = 179.
    exp-h = 180.
    exp-s = 1.
    exp-l = '0.35'.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act-r = 255.
    act-g = 0.
    act-b = 0.

    mo_cut->_rgb_to_hsl( CHANGING label = act ).

    exp-r = 255.
    exp-g = 0.
    exp-b = 0.
    exp-h = 0.
    exp-s = 1.
    exp-l = '0.5'.

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

  ENDMETHOD.

  METHOD _gh_label.

    DATA act TYPE ty_gh_label.
    DATA exp TYPE ty_gh_label.

    act = mo_cut->_gh_label( 'e8e8e8' ).
    exp = VALUE ty_gh_label( r = 232 g = 232 b = 232 ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = mo_cut->_gh_label( '123456' ).
    exp = VALUE ty_gh_label( r = 18 g = 52 b = 86 ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

  ENDMETHOD.

  METHOD _gh_dark_mode.

    DATA act TYPE ty_gh_label.
    DATA exp TYPE ty_gh_label.

    " r = 199 g = 225 b = 139
    act = mo_cut->_gh_dark_mode( col = 'c7e18b' dark = abap_true ).

    exp-fg     = mo_cut->_rgb( r = 199 g = 225 b = 137 ).
    exp-bg     = mo_cut->_rgba( r = 199 g = 225 b = 139 a = '0.18' dark = abap_true ).
    exp-border = mo_cut->_rgba( r = 199 g = 225 b = 137 a = '0.3' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = exp-fg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = exp-bg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = exp-border ).

    " r = 127, g = 43, b = 76
    act = mo_cut->_gh_dark_mode( col = '7f2b4c' dark = abap_true ).

    exp-fg     = mo_cut->_rgb( r = 214 g = 134 b = 166 ).
    exp-bg     = mo_cut->_rgba( r = 127 g = 43 b = 76 a = '0.18' dark = abap_true ).
    exp-border = mo_cut->_rgba( r = 214 g = 134 b = 166 a = '0.3' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = exp-fg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = exp-bg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = exp-border ).

    " r = 251, g = 202, b = 4
    act = mo_cut->_gh_dark_mode( col = 'fbca04' dark = abap_true ).

    exp-fg     = mo_cut->_rgb( r = 251 g = 202 b = 4 ).
    exp-bg     = mo_cut->_rgba( r = 251 g = 202 b = 4 a = '0.18' dark = abap_true ).
    exp-border = mo_cut->_rgba( r = 251 g = 202 b = 4 a = '0.3' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = exp-fg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = exp-bg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = exp-border ).

    " r = 0, g = 107, b = 117
    act = mo_cut->_gh_dark_mode( col = '006b75' dark = abap_true ).

    exp-fg     = mo_cut->_rgb( r = 0 g = 232 b = 253 ).
    exp-bg     = mo_cut->_rgba( r = 0 g = 107 b = 117 a = '0.18' dark = abap_true ).
    exp-border = mo_cut->_rgba( r = 0 g = 232 b = 253 a = '0.3' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = exp-fg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = exp-bg ).

    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = exp-border ).

  ENDMETHOD.

  METHOD _gh_light_mode.

    DATA act TYPE ty_gh_label.

    act = mo_cut->_gh_light_mode( 'ff0000' ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = 'ffffff' ).
    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = mo_cut->_rgb( r = 255 g = 0 b = 0 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = mo_cut->_rgba( r = 128 g = 0 b = 0 a = 0 ) ).

    act = mo_cut->_gh_light_mode( '7db174' ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = '000000' ).
    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = mo_cut->_rgb( r = 125 g = 177 b = 116 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = mo_cut->_rgba( r = 66 g = 104 b = 59 a = 0 ) ).

    act = mo_cut->_gh_light_mode( 'fdb409' ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = '000000' ).
    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = mo_cut->_rgb( r = 253 g = 180 b = 9 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = mo_cut->_rgba( r = 131 g = 92 b = 1 a = 0 ) ).

  ENDMETHOD.

ENDCLASS.
