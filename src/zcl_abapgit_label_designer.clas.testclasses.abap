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
      exp = '0d393d' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_hsla( h = 300 s = '0.35' l = '0.85' a = '0.9' dark = abap_true )
      exp = 'd1b9d2' ).

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
      exp = '5c1318' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->_rgba( r = 192 g = 24 b = 214 a = '0.5' dark = abap_true )
      exp = '6b1a7c' ).

  ENDMETHOD.

  METHOD _hsl_to_rgb.

    DATA(act) = VALUE ty_gh_label( h = 195 s = 1 l = '0.5' ).
    mo_cut->_hsl_to_rgb( CHANGING label = act ).
    DATA(exp) = VALUE ty_gh_label( r = 0 g = 191 b = 255 h = 195 s = 1 l = '0.5' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = VALUE ty_gh_label( h = 40 s = 1 l = '0.7' ).
    mo_cut->_hsl_to_rgb( CHANGING label = act ).
    exp = VALUE ty_gh_label( r = 255 g = 204 b = 102 h = 40 s = 1 l = '0.7' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = VALUE ty_gh_label( h = 180 s = 1 l = '0.35' ).
    mo_cut->_hsl_to_rgb( CHANGING label = act ).
    exp = VALUE ty_gh_label( r = 0 g = 179 b = 179 h = 180 s = 1 l = '0.35' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

  ENDMETHOD.

  METHOD _rgb_to_hsl.

    DATA(act) = VALUE ty_gh_label( r = 0 g = 191 b = 255 ).
    mo_cut->_rgb_to_hsl( CHANGING label = act ).
    DATA(exp) = VALUE ty_gh_label( r = 0 g = 191 b = 255 h = 195 s = 1 l = '0.5' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = VALUE ty_gh_label( r = 255 g = 204 b = 102 ).
    mo_cut->_rgb_to_hsl( CHANGING label = act ).
    exp = VALUE ty_gh_label( r = 255 g = 204 b = 102 h = 40 s = 1 l = '0.7' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = VALUE ty_gh_label( r = 0 g = 179 b = 179 ).
    mo_cut->_rgb_to_hsl( CHANGING label = act ).
    exp = VALUE ty_gh_label( r = 0 g = 179 b = 179 h = 180 s = 1 l = '0.35' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

    act = VALUE ty_gh_label( r = 255 g = 0 b = 0 ).
    mo_cut->_rgb_to_hsl( CHANGING label = act ).
    exp = VALUE ty_gh_label( r = 255 g = 0 b = 0 h = 0 s = 1 l = '0.5' ).

    cl_abap_unit_assert=>assert_equals(
      act = act
      exp = exp ).

  ENDMETHOD.

  METHOD _gh_label.

    DATA(act) = mo_cut->_gh_label( 'e8e8e8' ).
    DATA(exp) = VALUE ty_gh_label( r = 232 g = 232 b = 232 ).

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

    DATA(act) = mo_cut->_gh_dark_mode( col = 'ff0000' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = 'ffc6c6' ).
    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = mo_cut->_rgba( r = 255 g = 0 b = 0 a = '0.18' dark = abap_true ) ).
    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = mo_cut->_rgba( r = 255 g = 198 b = 198 a = '0.3' dark = abap_true ) ).

    act = mo_cut->_gh_dark_mode( col = 'fdb409' dark = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = act-fg
      exp = 'fdb308' ).
    cl_abap_unit_assert=>assert_equals(
      act = act-bg
      exp = mo_cut->_rgba( r = 253 g = 180 b = 9 a = '0.18' dark = abap_true ) ).
    cl_abap_unit_assert=>assert_equals(
      act = act-border
      exp = mo_cut->_rgba( r = 253 g = 180 b = 8 a = '0.3' dark = abap_true ) ).

  ENDMETHOD.

  METHOD _gh_light_mode.

    DATA(act) = mo_cut->_gh_light_mode( 'ff0000' ).

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
