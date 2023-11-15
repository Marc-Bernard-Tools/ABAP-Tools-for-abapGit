CLASS ltcl_tests DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    TYPES:
      ty_p TYPE p LENGTH 10 DECIMALS 4,
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

    DATA mo_cut TYPE REF TO zcl_abapgit_label_designer.

    METHODS:
      setup,
      _hsl FOR TESTING,
      _hsla FOR TESTING,
      _rgb FOR TESTING,
      _rgba FOR TESTING,
      _hue_to_rgb FOR TESTING,
      _hsl_to_rgb FOR TESTING,
      _rgb_to_hsl FOR TESTING,
      _gh_label FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_label_designer DEFINITION LOCAL FRIENDS ltcl_tests.

CLASS ltcl_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD _hsl.

    cl_abap_unit_assert=>assert_equals(
      act = 1
      exp = 1 ).

  ENDMETHOD.

  METHOD _hsla.

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

  ENDMETHOD.

  METHOD _hue_to_rgb.

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

ENDCLASS.
