REPORT zabapgit_repo_labels.

********************************************************************************
* abapGit Repository Labels
*
* A tool for doing mass-maintenance of repository labels. Quickly display, add,
* or remove labels based on free selection of repository name, package, or URL.
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

TABLES: tdevc, tdevct.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl0.
  SELECTION-SCREEN:
  SKIP,
  COMMENT /1(77) sc_txt1,
  COMMENT /1(77) sc_txt2,
  COMMENT /1(77) sc_txt3.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_repo WITH FRAME TITLE sc_titl1.
  SELECT-OPTIONS:
    s_name FOR tdevct-ctext LOWER CASE,
    s_pack FOR tdevc-devclass,
    s_url  FOR tdevct-ctext LOWER CASE.
SELECTION-SCREEN END OF BLOCK sc_repo.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_label WITH FRAME TITLE sc_titl2.
  PARAMETERS:
    p_list   RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND lar,
    p_add    RADIOBUTTON GROUP g1,
    p_remove RADIOBUTTON GROUP g1.
  SELECT-OPTIONS:
    s_label FOR tdevct-ctext LOWER CASE NO INTERVALS MODIF ID lab.
SELECTION-SCREEN END OF BLOCK sc_label.

DATA gt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.

FORM get.

  DATA:
    lo_online  TYPE REF TO zcl_abapgit_repo_online,
    lv_name    TYPE string,
    lv_package TYPE devclass,
    lv_url     TYPE string,
    lx_error   TYPE REF TO zcx_abapgit_exception.

  FIELD-SYMBOLS <li_repo> TYPE REF TO zif_abapgit_repo.

  TRY.
      gt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

      LOOP AT gt_repos ASSIGNING <li_repo>.
        lv_name    = <li_repo>->get_name( ).
        lv_package = <li_repo>->get_package( ).

        IF <li_repo>->is_offline( ) = abap_false.
          lo_online ?= <li_repo>.
          lv_url = lo_online->get_url( ).
        ELSE.
          lv_url = ''.
        ENDIF.

        IF NOT ( lv_name IN s_name AND lv_package IN s_pack AND lv_url IN s_url ).
          DELETE gt_repos.
        ENDIF.
      ENDLOOP.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

FORM list.

  TYPES:
    BEGIN OF ty_alv,
      name    TYPE string,
      package TYPE devclass,
      url     TYPE string,
      labels  TYPE string,
    END OF ty_alv.

  DATA:
    lo_online   TYPE REF TO zcl_abapgit_repo_online,
    ls_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
    ls_alv      TYPE ty_alv,
    lt_alv      TYPE STANDARD TABLE OF ty_alv WITH DEFAULT KEY.

  FIELD-SYMBOLS <li_repo> TYPE REF TO zif_abapgit_repo.

  LOOP AT gt_repos ASSIGNING <li_repo>.
    ls_settings = <li_repo>->get_local_settings( ).

    CLEAR ls_alv.
    ls_alv-name    = <li_repo>->get_name( ).
    ls_alv-package = <li_repo>->get_package( ).
    IF <li_repo>->is_offline( ) = abap_false.
      lo_online ?= <li_repo>.
      ls_alv-url = lo_online->get_url( ).
    ENDIF.
    ls_alv-labels  = ls_settings-labels.
    INSERT ls_alv INTO TABLE lt_alv.
  ENDLOOP.

  SORT lt_alv.

  CALL FUNCTION 'RSDU_CALL_ALV_TABLE'
    EXPORTING
      i_title   = 'Repository Labels'
      i_ta_data = lt_alv.

ENDFORM.

FORM add_remove.

  DATA:
    li_repo     TYPE REF TO zcl_abapgit_repo,
    ls_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
    lv_label    TYPE string,
    lt_labels   TYPE STANDARD TABLE OF string,
    lx_error    TYPE REF TO zcx_abapgit_exception.

  FIELD-SYMBOLS:
    <li_repo>  TYPE REF TO zif_abapgit_repo,
    <lv_label> LIKE LINE OF s_label.

  IF s_label IS INITIAL.
    MESSAGE 'Enter at least one label' TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT gt_repos ASSIGNING <li_repo>.
    li_repo ?= <li_repo>.

    ls_settings = li_repo->get_local_settings( ).

    SPLIT ls_settings-labels AT ',' INTO TABLE lt_labels.

    LOOP AT s_label ASSIGNING <lv_label>.
      lv_label = condense( <lv_label>-low ).
      READ TABLE lt_labels TRANSPORTING NO FIELDS WITH KEY table_line = lv_label.
      IF sy-subrc <> 0 AND p_add = abap_true.
        INSERT lv_label INTO TABLE lt_labels.
      ELSEIF sy-subrc = 0 AND p_remove = abap_true.
        DELETE lt_labels WHERE table_line = lv_label.
      ENDIF.
    ENDLOOP.

    CONCATENATE LINES OF lt_labels INTO lv_label SEPARATED BY ','.

    ls_settings-labels = zcl_abapgit_repo_labels=>normalize( lv_label ).

    TRY.
        li_repo->set_local_settings( ls_settings ).
        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'I'.
    ENDTRY.
  ENDLOOP.

ENDFORM.

FORM screen.

  DATA lv_input TYPE abap_bool.

  LOOP AT SCREEN.
    IF screen-group1 = 'LAB'.
      lv_input = boolc( p_add = abap_true OR p_remove = abap_true ).
    ELSE.
      lv_input = abap_true.
    ENDIF.

    IF lv_input = abap_true.
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

INITIALIZATION.

  sc_titl0 = 'Description'.
  sc_txt1  = 'This is a tool for mass-maintenance of repository labels. Quickly'.
  sc_txt2  = 'display, add, or remove labels based on free selection of repository'.
  sc_txt3  = 'name, package, or URL.'.
  sc_titl1 = 'Repository Selection'.
  sc_titl2 = 'Action'.

AT SELECTION-SCREEN.

  PERFORM screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen.

START-OF-SELECTION.

  PERFORM get.

  CASE abap_true.
    WHEN p_list.
      PERFORM list.
    WHEN p_add.
      PERFORM add_remove.
      PERFORM list.
    WHEN p_remove.
      PERFORM add_remove.
      PERFORM list.
  ENDCASE.
